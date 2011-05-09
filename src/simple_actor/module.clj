(ns simple-actor.module
  (:require [clojure.contrib.logging :as log]))


;;TODO one message can register serveral handle-function
(defn mk-main-handle
  "return [f-register f-main-handle]
   f-register [header f-handle] : register the f-handle function to handle the message with the header, the f-handle must have the same form as f-main-handle
   f-main-handler [actor msg] : main handle function that according the header of message to pass on to sub handler. Mainly used in making a actor"
  []
  (let [handlers (ref {})]
    (letfn [(f-register [header f-handle]
                        (log/info (str "register message : " header
                                       " with handle: " f-handle) )
                        (dosync (alter handlers assoc header f-handle) ))
            (f-main-handle [actor msg]
                           (log/debug (str "handle message : " msg))
                           (if-let [f-handle (get @handlers (:type msg))]
                             (f-handle actor msg)
                             (log/warn (str "not registed msg : " msg))))]
      [f-register f-main-handle])))

