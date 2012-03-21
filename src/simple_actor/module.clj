(ns simple-actor.module
  (:require [clojure.tools.logging :as log]))


(defn append-last [coll item]
  (conj (or coll []) item))

(defn mk-main-handle
  "define a handle that can invoke different handle function
   according to registered  signal type, one signal type can have more than one handle function. and every handle function will be execute in the order that they registered.  
   return [f-register f-main-handle]
   f-register [type f-handle] : register the f-handle function to handle the signal with the type, the f-handle must have the same form as f-main-handle
   f-main-handler [actor signal] : main handle function that according the type of a signal to pass on to sub handler function. Mainly used in making a actor"
  []
  (let [handlers (atom {})]
    (letfn [(f-register [type f-handle]
                        (log/info (str "register signal type : " type
                                       " with handle: " f-handle) )
                        (swap! handlers update-in [type] append-last f-handle))
            (f-main-handle [actor signal]
                           (log/debug (str "handle signal : " signal))
                           (if-let [f-handles (get @handlers (:type signal))]
                             (mapcat #(% actor signal) f-handles)
                             (log/warn (str "not registed signal : " signal))))]
      [f-register f-main-handle])))

