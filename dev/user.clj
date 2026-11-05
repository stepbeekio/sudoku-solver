(ns user
  (:require [portal.api :as portal]))

;; FlowStorm: evaluate :dbg at the REPL to open the debugger UI.

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
