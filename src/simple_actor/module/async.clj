(ns simple-actor.module.async
  (:import [java.util.concurrent LinkedBlockingQueue
            ThreadPoolExecutor TimeUnit])
  (:require [clojure.tools.logging :as log]))

;;the :code will be a function that can be execute
(def signal-async-example {:type :async :code (partial inc 1)})

;;the :code will be a function that can be execute
(def signal-sync-example {:type :sync :code #(inc 1)})

;;(def *executor-example* (ThreadPoolExecutor. 3 3 0 TimeUnit/MINUTES
;;                                             (LinkedBlockingQueue. 1024))) 

(defn async-execute_
  "execute code of the signal in another thread"
  [#^ThreadPoolExecutor executor actor signal]
  (log/debug (str "invoke async-execute : " signal))
  (.execute executor #^Runnable #(actor {:type :sync :code ((:code signal))})))

(defn sync-execute_
  "just execute the code (:code signal) of the signal"
  [actor signal]
  (log/debug (str "invoke sync-exectue : " signal))
  ((:code signal)))

(defmacro with-async
  "reutrn a message contains a function that can execute async.
   the function that executes in asyn exeuctes the bindings part and
   return another function that can execute sync and executes the body part."
  [bindings & body]
  {:type :async :code `(fn []
                              (let ~bindings
                                (fn [] ~@body)))})

(defn init-module
  "init async execute module -- register async and sync execute handle"
  [#^ThreadPoolExecutor executor f-register]
  (do 
    (f-register :async (partial async-execute_ executor))
    (f-register :sync sync-execute_)))



