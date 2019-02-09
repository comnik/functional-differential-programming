(ns ^:figwheel-hooks fdp.circle-datascript
  (:require
   [goog.object :as gobj]
   [datascript.core :as d])
  (:require-macros [fdp.core :refer [html]]))

(enable-console-print!)

(defn pipe-log [x]
  (println x)
  x)

(def schema
  {:mouse/x {:db/cardinality :db.cardinality/one}
   :mouse/y {:db/cardinality :db.cardinality/one}})

(def conn (d/create-conn schema))

(d/transact conn [[:db/add -1 :db/ident :mouse]])

(defn use-query
  [conn q]
  (let [[value update-value] (js/React.useState nil)]
    (js/React.useEffect
     (fn []
       (let [key (gensym "use-query")]
         (d/listen! conn key (fn [tx-log]
                               (-> (d/q q (:db-after tx-log))
                                   (update-value))))
         (fn []
           (d/unlisten! conn key)))))
    value))

(defn Root
  []
  (let [[x y] (first (use-query conn '[:find ?x ?y :where [?mouse :mouse/x ?x] [?mouse :mouse/y ?y]]))]
    (html
     [:svg#canvas
      [:g
       [:circle {:cx x :cy y :r 30 :stroke "black" :fill "red"}]]])))

(defn mount
  [component node]
  (js/ReactDOM.render ((js/React.createFactory component)) node))

(defn- handle-mousemove [e]
  (d/transact conn [[:db/add :mouse :mouse/x (. e -clientX)]
                    [:db/add :mouse :mouse/y (. e -clientY)]])
  (.stopPropagation e)
  e)

(defn ^:after-load setup []
  (println "setup")
  (.addEventListener js/document "mousemove" handle-mousemove)
  (mount Root (.getElementById js/document "app-container")))

(defn ^:before-load teardown []
  (println "teardown")
  (.removeEventListener js/document "mousemove" handle-mousemove))
