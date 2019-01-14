(ns ^:figwheel-hooks fdp.core
  (:require
   [goog.object :as gobj]
   [clj-3df.core :as d]
   [rum.core :as rum]))

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
       (println name batch)))))

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

(defn pipe-log [x]
  (println x)
  x)

(defn exec!
  [requests]
  (->> requests (clj->js) (pipe-log) (.handle engine))
  (request-reconcile))

(def v-width (.-innerWidth js/window))
(def v-height (.-innerHeight js/window))

(rum/defc root
  < {:init
     (fn [state props]
       (assoc state :fdp/key (random-uuid)))
     :will-mount
     (fn [state]
       (let [key       (:fdp/key state)
             component (:rum/react-component state)]
         (add-watch! "mouse" key (fn [_ _ batch] (rum/request-render component)))))}
  [watcher]
  (let [{:mouse/keys [x y]} @watcher]
    [:svg#canvas
     [:g
      [:circle {:cx x :cy y :r 30 :stroke "black" :fill "red"}]]]))

(def schema
  {:key/pressed? {:db/valueType :Bool}
   :mouse/x      {:db/valueType :Number}
   :mouse/y      {:db/valueType :Number}})

(def db (d/create-db schema))

(def nextId (atom 0))
(defn next-id [] (swap! nextId inc))

(def mouse (next-id))

(defn- handle-mousemove [e]
  (exec!
    (concat
     (let [{:mouse/keys [x y]} @(get @registry "mouse")]
       (when (and (some? x) (some? y))
         (d/transact db [[:db/retract mouse :mouse/x x]
                         [:db/retract mouse :mouse/y y]])))
     (d/transact db [[:db/add mouse :mouse/x (. e -screenX)]
                     [:db/add mouse :mouse/y (. e -screenY)]])))
  (.stopPropagation e)
  e)

(defn- handle-keyup [e]
  (exec! (d/transact db [[:db/retract (. e -keyCode) :key/pressed? true]]))
  (.stopPropagation e)
  e)

(defn- handle-keydown [e]
  (exec! (d/transact db [[:db/add (. e -keyCode) :key/pressed? true]]))
  (.stopPropagation e)
  e)

(defn ^:after-load setup []
  (println "setup")
  #_(.addEventListener js/document "mousemove" handle-mousemove)
  #_(.addEventListener js/document "keyup" handle-keyup)
  #_(.addEventListener js/document "keydown" handle-keydown))

(defn ^:before-load teardown []
  (println "teardown")
  #_(.removeEventListener js/document "mousemove" handle-mousemove)
  #_(.removeEventListener js/document "keyup" handle-keyup)
  #_(.removeEventListener js/document "keydown" handle-keydown))

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
            
            ;; (d/register-query
            ;;  db "keys"
            ;;  '[:find ?key :where [?key :key/pressed? true]])
            
            (d/register-query
             db "mouse"
             '[:find ?x ?y :where [?mouse :mouse/x ?x] [?mouse :mouse/y ?y]])

            #_(d/register-query
               db "dosome"
               '[:find ?v
                 :where
                 [32 :key/pressed? ?v]
                 [13 :key/pressed? ?v]])))

         (swap! registry assoc "mouse" (tuple-watcher [:mouse/x :mouse/y]))

         (setup)
         (rum/mount (root (get @registry "mouse")) (.getElementById js/document "app-container")))))
    true))
