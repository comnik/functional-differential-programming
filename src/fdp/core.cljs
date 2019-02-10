(ns ^:figwheel-hooks fdp.core
  (:require
   [goog.object :as gobj]
   [clj-3df.core :as d])
  (:require-macros [fdp.core :refer [html]]))

(enable-console-print!)

(def schema
  {:counter/click   {:db/valueType :Bool :db/semantics :db.semantics/raw}
   :degrees/celsius {:db/valueType :Number :db/semantics :db.semantics.cardinality/one}
   :mouse/x         {:db/valueType :Number :db/semantics :db.semantics.cardinality/one}
   :mouse/y         {:db/valueType :Number :db/semantics :db.semantics.cardinality/one}})

(def db (d/create-db schema))
(def counter 0)
(def temp 1)
(def mouse 2)

(defonce registry (atom {}))

;; The wasm engine will call this function on all output batches.
(gobj/set
 js/window "__UGLY_DIFF_HOOK"
 (fn [name batch]
   (let [unwrap-type  (fn [boxed] (second (first boxed)))
         unwrap-tuple (fn [[tuple time diff]] [(mapv unwrap-type tuple) time diff])]
     (if-some [observers (vals (get @registry name))]
       (let [batch (->> batch
                        (js->clj)
                        (map unwrap-tuple))]
         (doseq [observer observers]
           (observer batch)))
       (println "unconsumed results for" name)))))

(defn dbg! [x] (println x) x)

(declare engine)

(def ^:private reconcile? (volatile! false))

(def ^:private schedule
  (or (and (exists? js/window)
           (or js/window.requestAnimationFrame
               js/window.webkitRequestAnimationFrame
               js/window.mozRequestAnimationFrame
               js/window.msRequestAnimationFrame))
      #(js/setTimeout % 16)))

(defn reconcile [t]
  (.handle engine (clj->js [{:AdvanceDomain [nil (js/Date.now)]}]))
  (.reconcile engine)
  (vreset! reconcile? false))

(defn request-reconcile
  "Schedules engine steps on animation frames until all dataflows are
  caught up with all inputs."
  []
  (when-not @reconcile?
    (schedule reconcile))
  (vreset! reconcile? true))

(defn exec!
  [requests]
  (->> requests #_(dbg!) (clj->js) (.handle engine))
  (request-reconcile))

(defn result-time [[tuple time diff]] time)
(defn result-diff [[tuple time diff]] diff)

(defn classify [batch]
  (let [diffs (into #{} (map result-diff) batch)]
    (case [(contains? diffs -1) (contains? diffs 1)]
      [true true]   :diff/change
      [true false]  :diff/retract
      [false true]  :diff/add
      [false false] :diff/noop)))

(defn most-recent [v batch]
  (let [next (->> batch
                  last
                  (filter (comp pos? result-diff))
                  last
                  first)]
    (if (some? next) next v)))

(defn use-snapshot
  [f name]
  (let [[value update-value] (js/React.useState nil)]
    (js/React.useEffect
     (fn []
       (let [key (gensym "use-snapshot")]
         (swap! registry assoc-in [name key] (fn [batch] (f value batch)))
         (fn []
           (swap! registry update name dissoc key))))
     [name])
    value))

(defn use-diff
  [f name]
  (let [[[value time diff :as current] update-value] (js/React.useState [nil 0 nil])]
    (js/React.useEffect
     (fn []
       (let [key (gensym "use-diff")]
         (swap! registry assoc-in [name key] (fn [batch]
                                               (if (empty? batch)
                                                 (update-value current)
                                                 (let [batch (->> batch
                                                                  (sort-by result-time)
                                                                  (partition-by result-time))
                                                       next  (-> batch last first result-time)]
                                                   (update-value [(f value batch) next batch])))))
         (fn []
           (swap! registry update name dissoc key))))
     [name])
    [value time diff]))

(defn fahrenheit->celsius [f]
  (when (some? f) (* (- f 32) (/ 5 9))))

(defn celsius->fahrenheit [c]
  (when (some? c) (+ (* c (/ 9 5)) 32)))

(defn Counter
  []
  (let [[[count] time diff] (use-diff most-recent "counter")
        change-class        (classify (last diff))
        ;; q                   '[:find ?counter (count ?click)
        ;;                       :where [?counter :counter/click ?click]]
        ;; [[count] time diff] (use-diff most-recent q)
        on-inc              (fn [e] (exec! (d/transact db [[:db/add counter :counter/click true]])))
        on-dec              (fn [e] (exec! (d/transact db [[:db/retract counter :counter/click true]])))]
    (println "re-rendering Counter" change-class diff)
    (html
     [:.example
      [:p nil "The task is to build a frame containing a label or
      read-only textfield T and a button B. Initially, the value in T
      is “0” and each click of B increases the value in T by one."]
      (str "Count: " (or count 0) " (at time " time ")")
      [:button {:on-click on-inc} "+"]
      [:button {:on-click on-dec} "-"]])))

(defn Temp
  []
  (let [[celsius]     (use-snapshot most-recent "celsius")
        on-celsius    (fn [e]
                        (when-some [c (js/parseInt (.. e -target -value))]
                          (when-not (js/isNaN c)
                            (exec! (d/transact db [[:db/add temp :degrees/celsius c]])))))
        on-fahrenheit (fn [e]
                        (when-some [f (js/parseInt (.. e -target -value))]
                          (when-not (js/isNaN f)
                            (let [c (js/Math.round (fahrenheit->celsius f))]
                              (exec! (d/transact db [[:db/add temp :degrees/celsius c]]))))))]
    (println "re-rendering Temp" celsius)
    (html
     [:.example
      [:p nil "The task is to build a frame containing two textfields
      TC and TF representing the temperature in Celsius and
      Fahrenheit, respectively. Initially, both TC and TF are
      empty. When the user enters a numerical value into TC the
      corresponding value in TF is automatically updated and vice
      versa. When the user enters a non-numerical string into TC the
      value in TF is not updated and vice versa."]
      [:input {:type        "text"
               :placeholder "Celsius"
               :value       (or celsius "")
               :on-change   on-celsius}]
      [:input {:type        "text"
               :placeholder "Fahrenheit"
               :value       (or (celsius->fahrenheit celsius) "")
               :on-change   on-fahrenheit}]])))

(def bbox-cache (atom nil))

(defn Circles
  []
  (let [ref           (js/React.useRef)
        [[x y] _ _]   (use-diff most-recent "mouse")
        on-mouse-move (fn [e]
                        (let [bbox (if-some [bbox @bbox-cache]
                                     bbox
                                     (when-some [current (.-current ref)]
                                       (let [rect (.getBoundingClientRect current)]
                                         (reset! bbox-cache rect))))]
                          (when (some? bbox)
                            (exec!
                              (d/transact db [[:db/add mouse :mouse/x (js/Math.round (- (.-clientX e) (.-left bbox)))]
                                              [:db/add mouse :mouse/y (js/Math.round (- (.-clientY e) (.-top bbox)))]]))))
                        (.stopPropagation e))]
    (println "re-rendering Circles" x y)
    (html
     [:.example
      [:svg#canvas {:ref ref :on-mouse-move on-mouse-move}
       [:g
        [:circle {:cx x :cy y :r 30 :stroke "black" :fill "red"}]]]])))

(defn Test
  [props]
  (let [[x setX] (js/React.useState 0)]
    (println "re-rendering" (.-id props))
    (html
     [:button {:on-click #(setX (inc x))} (str "button " x)])))

(defn Root
  []
  (println "re-rendering Root")
  (html
   [:#examples
    [:> Counter {}]
    [:> Temp {}]
    [:> Circles {}]]))

(defn mount
  [component node]
  (js/ReactDOM.render (js/React.createElement component) node))

(defn ^:after-load setup []
  (println "setup"))

(defn ^:before-load teardown []
  (println "teardown"))

(defonce initialization
  (do
    (doto js/Rust.wasm
      (.then
       (fn [df]
         (def engine df)
         (gobj/set js/window "__ENGINE" df)
         (println "Loaded")

         (exec!
           (concat
            (d/create-db-inputs db)

            (d/register-query
             db "counter"
             '[:find (count ?click)
               :where [?counter :counter/click ?click]])

            (d/register-query
             db "celsius"
             '[:find ?celsius :where [?temp :degrees/celsius ?celsius]])

            (d/register-query
             db "mouse"
             '[:find ?x ?y
               :where [?mouse :mouse/x ?x] [?mouse :mouse/y ?y]])
            ))

         (setup)

         (mount Root (.getElementById js/document "app-container")))))
    true))
