(ns simple-actor.actor
  (:require [clojure.contrib.logging :as log])
  (:import [java.util.concurrent LinkedBlockingQueue
            ThreadPoolExecutor TimeUnit]))

;; the actor is a send function in real. so when we define a parameter
;; either actor or f-send represents the same meaning.
;; the actor need a external function f-handle to handle messages
;; the handler function has the form f-handler [actor msg]
;; or f-handler [f-send msg]
;; and return a list of messages
;; TODO add debug log code

(defn loop-handle
  "loop using f-handle handles the msg and the returned messages
  that the f-handle fucntion returns"
  [actor f-handle msg]
  (log/debug (str "handle message : " msg))
  (let [msgs (f-handle actor msg)]
    (if-not (empty? msgs)
      (doseq [m msgs] (loop-handle actor f-handle m)))))

(defn loop-receive
  "loop receive messages and handle it
   fn-receive [] : receive message function with zero parameter
   fn-handle [actor msg] : handle message function with an actor and a msg and return a list of messages"
  [actor f-receive f-handle]
  (loop [msg (f-receive)]
    (log/debug (str "receive message : " msg))
    (if-not (= (:type msg) :stop)
      (do 
        (try 
          (do (loop-handle actor f-handle msg))
          (catch Exception e (log/error (str "handle-msg error : " msg) e) )  )
        (recur (f-receive))))))

(defn stop-actor
  "stop a actor"
  [actor]
  (actor {:type :stop}))

(defn actor
  "start a actor, loop receive and handle msg
   fn-handle  [msg] : function that handle msg
   return the send message function f-send"
  [f-handle]
  (let [queue (LinkedBlockingQueue.)]
    (letfn [(f-send [msg] (.put queue msg))
            (f-receive [] (.take queue))]
      
      (.start (Thread. #(do (log/info (str "actor thread start"
                                           (Thread/currentThread)))
                            (loop-receive f-send f-receive f-handle)
                            (log/info (str "actor thread stop"
                                           (Thread/currentThread)))) ))
      f-send)))


