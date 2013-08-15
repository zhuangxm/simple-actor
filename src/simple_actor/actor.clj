(ns simple-actor.actor
  (:require [clojure.tools.logging :as log])
  (:import [java.util.concurrent LinkedBlockingQueue
            ThreadPoolExecutor TimeUnit]))


;;In order to distinguish the messages used by actor and the messages
;;used in the  communication between client and server.
;; The messages used in actor are  called signal.
;; The messages used between client and server are callled message.


;; the actor is a send function in real. so when we define a parameter
;; either actor or f-send represents the same meaning.
;; the actor need a external function f-handle to handle signals
;; the handler function has the form f-handler [actor signal]
;; or f-handler [f-send signal]
;; and return a list of signals

(defn loop-handle
  "loop using f-handle handles the signal and the returned signals
  that the f-handle fucntion returns"
  [actor f-handler signal a-state]
  (log/debug (str "handle signal : " signal))
  (let [signals (filter identity (f-handler actor (assoc signal :state @a-state)))]
    (if-not (empty? signals)
      (doseq [m signals] (loop-handle actor f-handler m a-state)))))

(defn loop-receive
  "loop receive signals and handle it
   fn-receive [] : function that receives singal with zero parameter
   fn-handle [actor signal] : function that handle signal with an actor and a signal and return a list of signals"
  [actor f-receive f-handler a-state]
  (loop [signal (f-receive)]
    (log/debug (str "receive signal : " signal))
    (if-not (= (:type signal) :stop)
      (do
        (try
          (do (loop-handle actor f-handler signal a-state))
          (catch Exception e (log/error (str "handle singal error : " signal) e)))
        (recur (f-receive))))))

(defn stop-actor
  "stop a actor"
  [actor]
  (actor {:type :stop}))

(defn mk-actor
  "start a actor, loop receive and handle signal
   fn-handle  [signal] : function that handle signal
   return the function that sends signal : f-send"
  ([f-handler]
     (mk-actor f-handler nil))
  ([f-handler init-state]
     (let [queue (LinkedBlockingQueue.)
           a-state (atom init-state)]
       (letfn [(f-send [msg] (do (log/debug (str "send signal : " msg))
                                 (.put queue msg)))
               (f-receive [] (.take queue))
               (new-handler [actor signal] (if (= (:type signal) :update-state)
                                             (do
                                               (log/debug "update state to " (:new-state signal) )
                                               (reset! a-state (:new-state signal))
                                               nil)
                                             (f-handler actor signal)) )]
           (.start (Thread. #(do (log/info (str "actor thread start"
                                               (Thread/currentThread)))
                                (loop-receive f-send f-receive new-handler a-state)
                                (log/info (str "actor thread stop"
                                               (Thread/currentThread)))) ))
          f-send))))
