(ns ^:figwheel-hooks fdp.core
  (:require
   [goog.object :as gobj]
   [clj-3df.core :as d])
  (:require-macros [fdp.core :refer [html]]))

(enable-console-print!)

(defonce registry (atom {}))

(defprotocol IWatcher
  (-apply-diff [this batch])
  (-notify-of-diff [this batch]))

(deftype Watcher [watches state apply-diff]
  IWatcher
  (-apply-diff [this batch]
    (set! (.-state this) (apply-diff state batch))
    (-notify-of-diff this batch))
  (-notify-of-diff [this batch]
    (doseq [[key f] watches]
      (f key this batch)))
  
  cljs.core/IDeref
  (-deref [_] state)
  
  cljs.core/IWatchable
  (-notify-watches [this oldval newval]
    (doseq [[key f] watches]
      (f key this oldval newval)))
  (-add-watch [this key f]
    (set! (.-watches this) (assoc watches key f))
    this)
  (-remove-watch [this key]
    (set! (.-watches this) (dissoc watches key))))

(defn watcher [apply-diff] (Watcher. nil (apply-diff) apply-diff))

(defn set-watcher []
  (let [apply-diff (fn
                     ([] #{})
                     ([v batch] (into v batch)))]
    (watcher apply-diff)))

(defn tuple-watcher [attributes]
  (let [apply-diff (fn
                     ([] {})
                     ([v batch]
                      (let [tuple   (first (last batch))
                            changes (zipmap attributes tuple)]
                        (merge v changes))))]
    (watcher apply-diff)))

(defn add-watch! [name key f]
  (assert (contains? @registry name) "no watcher registered under that name")
  (swap! registry update name (fn [watcher] (add-watch watcher key f))))

(defn remove-watch! [name key]
  (assert (contains? @registry name) "no watcher registered under that name")
  (swap! registry update name (fn [watcher] (remove-watch watcher key))))

;; The wasm engine will call this function on all output batches.
(gobj/set
 js/window "__UGLY_DIFF_HOOK"
 (fn [name batch]
   (let [unwrap-type  (fn [boxed] (second (first boxed)))
         unwrap-tuple (fn [[tuple diff timestamp]] [(mapv unwrap-type tuple) diff timestamp])]
     (if-some [watcher (get @registry name)]
       (let [batch (->> batch
                        (js->clj)
                        (map unwrap-tuple))]
         (-apply-diff watcher batch))
       #_(println name)))))

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
       (let [key (gensym "use-query")]
         (add-watch! name key (fn [_ _ batch] (update-value batch)))
         (fn []
           (remove-watch! name key)))))
    value))

(defn Root
  []
  (let [[count set-count] (js/React.useState 0)
        v                 (use-query "mouse")]
    (println v)
    (html
     [:div
      [:svg#canvas
       [:g
        [:circle {:cx 400 :cy 400 :r 30 :stroke "black" :fill "red"}]]]
      [:button {:onClick (fn [e] (set-count (inc count)))}
       (str "Click Me :: " count)]])))

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
    (d/transact db [[:db/add mouse :mouse/x (. e -screenX)]
                    [:db/add mouse :mouse/y (. e -screenY)]]))
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
         
         (swap! registry assoc "mouse" (tuple-watcher [:mouse/x :mouse/y]))

         (setup)
         (mount Root (.getElementById js/document "app-container"))))
      )
    true))
