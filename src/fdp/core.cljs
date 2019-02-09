(ns ^:figwheel-hooks fdp.core
  (:require
   [goog.object :as gobj]
   [clj-3df.core :as d])
  (:require-macros [fdp.core :refer [html]]))

(enable-console-print!)

(def schema
  {:counter/click   {:db/valueType :Bool :db/semantics :db.semantics/raw}
   :degrees/celsius {:db/valueType :Number :db/semantics :db.semantics.cardinality/one}})

(def db (d/create-db schema))
(def counter 0)
(def temp 1)

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

(defn pipe-log [x]
  (println x)
  x)

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
  (->> requests (clj->js) (pipe-log) (.handle engine))
  (request-reconcile))

(defn use-snapshot
  [name]
  (let [[value update-value] (js/React.useState nil)]
    (js/React.useEffect
     (fn []
       (let [key  (gensym "use-snapshot")
             time (fn [[tuple time diff]] time)
             diff (fn [[tuple time diff]] diff)]
         (swap! registry assoc-in [name key] (fn [batch]
                                               (let [next (->> batch
                                                               (sort-by time)
                                                               (partition-by time)
                                                               last
                                                               (filter (comp pos? diff))
                                                               last
                                                               first)]
                                                 (when (some? next)
                                                   (update-value next)))))
         (fn []
           (swap! registry update name dissoc key)))))
    value))

(defn fahrenheit->celsius [f]
  (when (some? f) (* (- f 32) (/ 5 9))))

(defn celsius->fahrenheit [c]
  (when (some? c) (+ (* c (/ 9 5)) 32)))

(comment
  (defn Counter
    []
    (let [[_ count] (use-snapshot
                     '[:find ?counter (count ?click)
                       :where [?counter :counter/click ?click]])
          on-click  (fn [e]
                      (exec!
                        (d/transact db [[:db/add counter :counter/click true]])))]
      (html
       [:div
        [:button {:on-click on-click} (str "Count: " (or count 0))]]))))

(defn Root
  []
  (let [[_ count]     (use-snapshot "counter")
        [celsius]     (use-snapshot "celsius")
        on-click      (fn [e] (exec! (d/transact db [[:db/add counter :counter/click true]])))
        on-celsius    (fn [e]
                        (when-some [c (js/parseInt (.. e -target -value))]
                          (when-not (js/isNaN c)
                            (exec! (d/transact db [[:db/add temp :degrees/celsius c]])))))
        on-fahrenheit (fn [e]
                        (when-some [f (js/parseInt (.. e -target -value))]
                          (when-not (js/isNaN f)
                            (let [c (js/Math.round (fahrenheit->celsius f))]
                              (exec! (d/transact db [[:db/add temp :degrees/celsius c]]))))))]
    (html
     [:div
      [:div
       [:p nil "The task is to build a frame containing a label or
      read-only textfield T and a button B. Initially, the value in T
      is “0” and each click of B increases the value in T by one."]
       [:button {:on-click on-click} (str "Count: " (or count 0))]]
      [:div
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
                :on-change   on-fahrenheit}]]])))

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
             '[:find ?counter (count ?click)
               :where [?counter :counter/click ?click]])

            (d/register-query
             db "celsius"
             '[:find ?celsius :where [?temp :degrees/celsius ?celsius]])
            ))

         (setup)
         
         (mount Root (.getElementById js/document "app-container")))))
    true))
