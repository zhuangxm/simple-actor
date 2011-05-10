(ns simple-actor.module
  (:require [clojure.contrib.logging :as log]))


(defn append-last [coll item]
  (conj (or coll []) item))

(defn mk-main-handle
  "define a handle that can invoke different handle function
   according to registered  message header, one message header can have more than one handle function. and every handle function will be execute in the order that they registered.  
   return [f-register f-main-handle]
   f-register [header f-handle] : register the f-handle function to handle the message with the header, the f-handle must have the same form as f-main-handle
   f-main-handler [actor msg] : main handle function that according the header of message to pass on to sub handler. Mainly used in making a actor"
  []
  (let [handlers (atom {})]
    (letfn [(f-register [header f-handle]
                        (log/info (str "register message : " header
                                       " with handle: " f-handle) )
                        (swap! handlers update-in [header] append-last f-handle))
            (f-main-handle [actor msg]
                           (log/debug (str "handle message : " msg))
                           (if-let [f-handles (get @handlers (:type msg))]
                             (mapcat #(% actor msg) f-handles)
                             (log/warn (str "not registed msg : " msg))))]
      [f-register f-main-handle])))

