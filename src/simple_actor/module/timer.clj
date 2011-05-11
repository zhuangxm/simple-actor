(ns simple-actor.module.timer
  (:import [java.util.concurrent ScheduledExecutorService TimeUnit
            ScheduledThreadPoolExecutor ScheduledFuture])
  (:require [clojure.contrib.logging :as log]))


;;define timer module can execute delayed or repeatly.

(defmacro with-delay
  "body will execute after delay-ms time
   cancel-tag is a tag that used to cancel the timer
   application must guantee the cancel-tag is only."
  [cancel-tag delay-ms & body]
  {:type :timer :tag cancel-tag :delay delay-ms :code `(fn [] ~@body)})

(defmacro with-timer
  "body will execute every repeat-ms time
   cancel-tag is a tag that used to cancel the timer
   application must guantee the cancel-tag is only."
  [cancel-tag repeat-ms & body]
  {:type :timer :tag cancel-tag :repeat repeat-ms :code `(fn [] ~@body)})

(defn cancel-timer
  "return a cancel timer message
   cancel-tag is the tag of timer that want to be cancelled"
  [cancel-tag]
  {:type :timer-cancel :tag cancel-tag})

(defn timer-execute-register_
  "delayed or repeated to send sync execute code to actor"
  [futures-m #^ScheduledExecutorService scheduleExecutor actor msg]
  (log/debug (str "ask to register timer execute : " msg))
  (let [f-sync (fn [] (log/debug (str "timer-up send to actor to execute :" msg))
                 (actor {:type :timer-sync :tag (:tag msg) :code (:code msg)}))
        future_ (if-let [repeat-ms (:repeat msg)]
                 (.scheduleAtFixedRate scheduleExecutor
                                       #^Runnable f-sync repeat-ms repeat-ms
                                       TimeUnit/MILLISECONDS)
                 (.schedule scheduleExecutor
                            #^Runnable f-sync (long (:delay msg))
                            TimeUnit/MILLISECONDS))]
    (swap! futures-m assoc (:tag msg) future_)
    nil))

(defn timer-execute_
  "just execute the code (:code msg) of the message"
  [timer-futures-m actor msg]
  (let [tag (:tag msg)]
    (if-not (get @timer-futures-m tag)
      (do (log/debug (str "timer has been cancelled" msg))
          (swap! timer-futures-m dissoc tag)
          nil)
      (do (log/debug (str "invoke timer-exectue : " msg))
          ((:code msg))))))

(defn cancel-timer_
  "cacnel a timer of specitfy tag of (:msg msg) "
  [timer-futures-m actor msg]
  (log/debug (str "cancel timer : " msg))
  (let [tag (:tag msg)]
    (if-let [future_ (get @timer-futures-m tag)]
      (do (.cancel #^ScheduledFuture future_ false)
          (swap! timer-futures-m dissoc tag)))
    nil))

(defn cancel-all-timer_
  "cancel all unexecuted timer"
  [timer-futures-m actor msg]
  (log/debug "cancel all timer")
  (doseq [future_ (vals @timer-futures-m)] (.cancel #^ScheduledFuture future_) )
  (reset! timer-futures-m {})
  nil)

(defn init-module
  "init timer execute module -- register timer execute handle"
  [f-register]
  (let [scheduleExecutor (ScheduledThreadPoolExecutor. 1)
        timer-futures-m (atom {})]
    (f-register :timer (partial timer-execute-register_ timer-futures-m
                                scheduleExecutor))
    (f-register :timer-sync (partial timer-execute_ timer-futures-m))
    (f-register :timer-cancel (partial cancel-timer_ timer-futures-m))
    (f-register :timer-cancel-all (partial cancel-all-timer_ timer-futures-m))))
