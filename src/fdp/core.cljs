(ns ^:figwheel-hooks fdp.core
  (:require
   [goog.object :as gobj]
   [clj-3df.core :as d]
   [rum.core :as rum]))

(enable-console-print!)

(def registry (atom {}))

(deftype Watcher [watches]
  cljs.core/IWatchable
  (-notify-watches [this oldval newval]
    (doseq [[key f] (.-watches this)]
      (f key this oldval newval)))
  (-add-watch [this key f]
    (set! (.-watches this) (assoc watches key f))
    this)
  (-remove-watch [this key]
    (set! (.-watches this) (dissoc watches key))))

(defn watcher [] (Watcher. nil))

(defn notify-watches-of-diff [^Watcher watcher batch]
  (doseq [[key f] (.-watches watcher)]
    (f key watcher batch)))

(defn add-watch! [name key f]
  (if (contains? @registry name)
    (swap! registry update name (fn [watcher] (add-watch watcher key f)))
    (swap! registry assoc name (-> (watcher) (add-watch key f)))))

;; The wasm engine will call this function on all output batches.
(gobj/set js/window "__UGLY_DIFF_HOOK" (fn [name batch]
                                         (if-some [watcher (get @registry name)]
                                           (notify-watches-of-diff watcher batch)
                                           (println name batch))))

(declare engine)

(defn reconcile!
  "Schedules engine steps on animation frames until all dataflows are
  caught up with all inputs."
  [t]
  (let [work-remaining? (.step engine)]
    (when work-remaining?
      (.requestAnimationFrame js/window reconcile!))))

(defn exec!
  [requests]
  (->> requests (clj->js) (.handle engine))
  (reconcile! nil))

(def schema
  {:key/pressed? {:db/valueType :Bool}
   :mouse/x      {:db/valueType :Number}
   :mouse/y      {:db/valueType :Number}})

(def db (d/create-db schema))

(def nextId (atom 0))
(defn next-id [] (swap! nextId inc))

(def mouse (next-id))

(defn- handle-mousemove [e]
  #_(let [x (. e -screenX)
        y (. e -screenY)]
    (exec!
      (d/transact db [[:db/add mouse :mouse/x x]
                      [:db/add mouse :mouse/y y]]))
    (.stopPropagation e)
    e))

(defn- handle-key-up [e]
  (exec! (d/transact db [[:db/retract (. e -keyCode) :key/pressed? true]]))
  (.stopPropagation e)
  e)

(defn- handle-key-down [e]
  (exec! (d/transact db [[:db/add (. e -keyCode) :key/pressed? true]]))
  (.stopPropagation e)
  e)

(defn ^:after-load setup []
  (println "setup")
  (.addEventListener js/document "mousemove" handle-mousemove)
  (.addEventListener js/document "keyup" handle-key-up)
  (.addEventListener js/document "keydown" handle-key-down))

(defn ^:before-load teardown []
  (println "teardown")
  (.removeEventListener js/document "mousemove" handle-mousemove)
  (.removeEventListener js/document "keyup" handle-key-up)
  (.removeEventListener js/document "keydown" handle-key-down))

(defonce initialization-block
  (do
    (doto js/Rust.wasm
      (.then
       (fn [df]
         (def engine df)
         (gobj/set js/window "__ENGINE" df)
         (println "Loaded")

         ;; (add-watch! "keys" :debug (fn [key watcher batch] (println "keys result" batch)))

         (exec!
           (concat
            (d/create-db-inputs db)
            
            (d/register-query
             db "keys"
             '[:find ?key :where [?key :key/pressed? true]])

            #_(d/register-query
               db "dosome"
               '[:find ?v
                 :where
                 [32 :key/pressed? ?v]
                 [13 :key/pressed? ?v]])))

         (setup)
         
         (.requestAnimationFrame js/window reconcile!))))
    true))

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
         (add-watch! "keys" key (fn [_ _ _] (rum/request-render component)))))}
  []
  [:svg#canvas
   [:g
    [:circle {:cx (rand-int v-width) :cy (rand-int v-height) :r 30 :stroke "black" :fill "red"}]]])

(rum/mount (root) (.getElementById js/document "app-container"))

(comment

  (exec! (d/transact db [{:db/id 100 :mouse/x 100 :mouse/y 200}]))
  
  (exec!
   (d/register-query
    db "mouse-coords"
    '[:find ?x ?y
      :where
      [?mouse :mouse/x ?x]
      [?mouse :mouse/y ?y]]))

  
  )
