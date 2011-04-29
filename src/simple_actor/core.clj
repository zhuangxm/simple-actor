(ns simple-actor.core
  (:import [java.util.concurrent LinkedBlockingQueue
            ThreadPoolExecutor TimeUnit ]))

;the :code will be execute in a thread pool the :code must be a
;binding form like [a 5 b (inc a)]
;and the :after will be execute in the actor thread 
(def msg-async-example {:type :async :code '[a 5 b (inc a)]
                        :after (list '(prn (+ a b))) })

;the :code will be execute in the actor thread
;the :code must be normal clojure form
(def msg-sync-example {:type :sync :code '(let [a 5 b (inc a)] (prn (+ a b)))})

(def *queue* (LinkedBlockingQueue.))

(def *executor* (ThreadPoolExecutor. 3 3 0 TimeUnit/MINUTES
                                    (LinkedBlockingQueue. 1024)))
(defn receive-msg []
  (prn "waitting message")
  (let [msg (.take *queue*)
        _ (prn "received : " msg)]
    msg))

(defn send-msg [msg]
  (println "send-msg : " msg)
  (.put *queue* msg))

(defmacro eval-bindings 
  "eval the bindings
   for example [a 5 b (inc a)] ==> [a 5 b 6] "
  [bindings]
  (let [ks (vec (mapcat identity (partition 1 2 bindings)))
        vs (eval `(let ~bindings  ~ks))]
    `(vec (mapcat #(list %1 %2) '~ks ~vs))))

(defn pre-eval-let 
  "eval the bindings and transform to result body form a code
   that may be eval later"
  [bindings & body]
  (let [new-ctx (eval `(let [ctx# (eval-bindings ~bindings)] ctx#))]
    `(let ~new-ctx ~@body)))

(defn execute-msg-with-after
  "execute the code of the message and
   return a new message with code that will be execute next time"
  [msg]
  (let [code (apply pre-eval-let (:code msg) (:after msg))]
    {:type :sync :code code}))

(defmulti handle-msg :type)

(defmethod handle-msg :default
  [msg]
  (prn msg "not match method."))

(defmethod handle-msg :async
  [msg]
  (.execute *executor* #(let [after-code (execute-msg-with-after  msg)]
                          (send-msg after-code)) ))

;"just execute the code of the message"
(defmethod handle-msg :sync
  [msg]
  (eval (:code msg)))

(defn loop-receive
  "loop receive messages and handle it"
  [fn-receive]
  (println "loop-receive start ")
  (loop [msg (fn-receive)]
    (println "loop-recevie : " msg)
    (try 
      (handle-msg msg)
      (catch Exception e (prn "handle-msg error " e) )  )
    (recur (fn-receive))))

(defn actor 
  "start a actor thread
   fn-recevie must be a function with zero paramter"
  [fn-receive]
  (do
    (.start (Thread. #(loop-receive fn-receive) ))))

(defmacro async 
  "send a message with the code to be execute async"
  [bindings & body]
  (send-msg {:type :async :code bindings :after body}))












