(ns ^:figwheel-hooks fdp.core
  (:require
   [goog.object :as gobj]
   [clj-3df.core :as d])
  (:require-macros [fdp.core :refer [html]]))

(enable-console-print!)

(defonce registry (atom {}))

(defn add-watch! [name key f]
  (swap! registry assoc-in [name key] f))

(defn remove-watch! [name key]
  (swap! registry update name dissoc key))

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
         (doseq [f observers]
           (f batch)))
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

(def next-tx (atom 0))

(defn reconcile [t]
  (let [work-remaining? (.step engine)]
    (if work-remaining?
      (schedule reconcile)
      (vreset! reconcile? false))))

(defn request-reconcile
  "Schedules engine steps on animation frames until all dataflows are
  caught up with all inputs."
  []
  (when-not @reconcile?
    (schedule reconcile))
  (vreset! reconcile? true))

(defn exec!
  [requests]
  (->> requests (clj->js) #_(pipe-log) (.handle engine))
  (request-reconcile))

(def v-width (.-innerWidth js/window))
(def v-height (.-innerHeight js/window))

(defn use-query
  [name]
  (let [[value update-value] (js/React.useState nil)]
    (js/React.useEffect
     (fn []
       (let [key  (gensym "use-query")
             time (fn [[tuple time diff]] time)
             diff (fn [[tuple time diff]] diff)]
         (add-watch! name key (fn [batch]
                                (let [next (->> batch
                                                (sort-by time)
                                                (partition-by time)
                                                last
                                                (filter (comp pos? diff))
                                                last)]
                                  (when (some? next)
                                    (update-value next)))))
         (fn []
           (remove-watch! name key)))))
    value))

(defn Root
  []
  (let [[[x y] _ _] (use-query "mouse")]
    (html
     [:svg#canvas
      [:g
       [:circle {:cx x :cy y :r 30 :stroke "black" :fill "red"}]]])))

(defn mount
  [component node]
  (js/ReactDOM.render ((js/React.createFactory component)) node))

(def schema
  {:key/pressed? {:db/valueType :Bool}
   :mouse/x      {:db/valueType :Number :db/semantics :db.semantics.cardinality/one}
   :mouse/y      {:db/valueType :Number :db/semantics :db.semantics.cardinality/one}})

(def db (d/create-db schema))

(def nextId (atom 0))
(defn next-id [] (swap! nextId inc))

(def mouse (next-id))

(defn- handle-mousemove [e]
  (exec!
    (d/transact db [[:db/add mouse :mouse/x (. e -clientX)]
                    [:db/add mouse :mouse/y (. e -clientY)]]))
  (.stopPropagation e)
  e)

(defn ^:after-load setup []
  (println "setup")
  (.addEventListener js/document "mousemove" handle-mousemove))

(defn ^:before-load teardown []
  (println "teardown")
  (.removeEventListener js/document "mousemove" handle-mousemove))

(defonce initialization-block
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
             db "mouse"
             '[:find ?x ?y :where [?mouse :mouse/x ?x] [?mouse :mouse/y ?y]])))
         
         (setup)
         (mount Root (.getElementById js/document "app-container"))))
      )
    true))
