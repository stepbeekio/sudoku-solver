(ns user
  (:require [flow-storm.api :as flow-storm]
            [portal.api :as portal]))

(defn start-flowstorm!
  "Opens the FlowStorm debugger UI in this JVM. Close the window to dismiss it."
  []
  (flow-storm/local-connect)
  nil)

(defonce ^:private portal-instance (atom nil))

(defn start-portal!
  "Opens Portal and sends all `tap>` values to it."
  []
  (add-tap #'portal/submit)
  (reset! portal-instance (portal/open)))

(defn stop-portal!
  "Closes Portal and stops sending `tap>` values to it."
  []
  (remove-tap #'portal/submit)
  (when-let [instance @portal-instance]
    (portal/close instance)
    (reset! portal-instance nil)))

(comment (do (start-flowstorm!) (start-portal!)))
