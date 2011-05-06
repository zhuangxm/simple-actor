(ns simple-actor.module.async
  (:import [java.util.concurrent LinkedBlockingQueue
            ThreadPoolExecutor TimeUnit])
  (:require [clojure.contrib.logging :as log]))

;the :code will be execute in a thread pool the :code must be a
;binding form like [a 5 b (inc a)]
;and the :after will be execute in the actor thread 
(def msg-async-example {:type :async :code '[a 5 b (inc a)]
                        :after (list '(prn (+ a b))) })

;the :code will be execute in the actor thread
;the :code must be normal clojure form
(def msg-sync-example {:type :sync :code '(let [a 5 b (inc a)] (prn (+ a b)))})

;;(def *executor-example* (ThreadPoolExecutor. 3 3 0 TimeUnit/MINUTES
;;                                             (LinkedBlockingQueue. 1024)))

; 
(defmacro eval-bindings 
  "eval the bindings
   for example [a 5 b (inc a)] ==> [a 5 b 6] "
  [bindings]
  (let [ks (vec (mapcat identity (partition 1 2 bindings)))
        vs `(let ~bindings ~ks)]
    `(vec (mapcat #(list %1 %2) '~ks ~vs))))

(defn pre-eval-let 
  "eval the bindings and transform to result body form a code
   that may be eval later"
  [bindings & body]
  (let [exp `(eval-bindings ~bindings)
        new-ctx (eval `(eval-bindings ~bindings))]
    `(let ~new-ctx ~@body)))

(defn execute-msg-with-after
  "execute the code of the message and
   return a new message with code that will be execute next time"
  [msg]
  (log/debug (str "execute-msg-with-after : " msg))
  (let [code (apply pre-eval-let (:code msg) (:after msg))]
    {:type :sync :code code}))

(defn async-execute
  "execute code of the message in another thread"
  [executor actor msg]
  (log/debug (str "invoke async-execute : " msg))
  (.execute executor #(actor (execute-msg-with-after msg)) ))

(defn sync-execute
  "just execute the code (:code msg) of the message"
  [actor msg]
  (eval `(:code ~msg))
  nil)

(defmacro with-async 
  "send a message with the code to be execute async"
  [f-send bindings & body]
  `(~f-send {:type :async :code '~bindings :after '~body}))

(defn init-module
  "init async execute module -- register async and sync execute handle"
  [executor f-register]
  (do 
    (f-register :async (partial async-execute executor))
    (f-register :sync sync-execute)))
