(ns ^:figwheel-hooks fdp.device
  (:require
   [goog.object :as gobj]
   [clj-3df.core :as df])
  (:require-macros
   [clj-3df.core :refer [exec!]]
   [fdp.core :refer [html]]))

;; We are modeling devices that are reporting their current speed via
;; some sensors. The speed is supposed to match the settings for that
;; device. We are interested in reporting any devices that are
;; deviating from their settings.

(def schema
  {:settings/speed {:db/valueType :Number}
   :device/name    {:db/valueType :String}
   :device/speed   {:db/valueType :Number}})

(def db (df/create-db schema))

(comment

  (do
    (def conn (df/create-debug-conn "ws://127.0.0.1:6262"))
    (exec! conn (df/create-db-inputs db)))

  (exec! conn
    (df/transact db [[:db/add 111 :device/name "dev0"]
                     [:db/add 111 :settings/speed 100]
                     [:db/add 222 :device/name "dev1"]
                     [:db/add 222 :settings/speed 130]]))

  (exec! conn
    (df/query
     db "current-speed"
     '[:find ?device ?speed 
       :where [?device :device/speed ?speed]]))

  (exec! conn
    (df/query
     db "current-settings"
     '[:find ?device ?speed 
       :where [?device :settings/speed ?speed]]))

  (exec! conn
    (df/query
     db "devicemgr/deviating"
     '[:find ?device
       :where
       [?device :settings/speed ?target]
       [?device :device/speed ?speed]
       [(< ?speed ?target)]]))

  (exec! conn
    (df/query
     db "devicemgr/alerts"
     '[:find ?device ?name ?deviation
       :where
       (devicemgr/deviating ?device)
       [?device :device/name ?name]
       [?device :settings/speed ?target]
       [?device :device/speed ?speed]
       [(subtract ?target ?speed) ?deviation]]))

  (exec! conn
    (df/transact db [[:db/add 111 :device/speed 80]]))

  (exec! conn
    (df/transact db [[:db/add 222 :device/speed 120]]))

  )
