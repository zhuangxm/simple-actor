(ns simple-actor.core
  (:import [java.util.concurrent LinkedBlockingQueue
            ThreadPoolExecutor TimeUnit ]))

(def msg-example {:type :execute :result {'a 1 'b 2} :program (list 'a 'b) })

(def *queue* (LinkedBlockingQueue.))

(def *executor* (ThreadPoolExecutor. 3 3 0 TimeUnit/MINUTES
                                    (LinkedBlockingQueue. 1024)))

(defn receive-msg []
  (.take *queue*))

(defn send-msg [msg]
  (println "send-msg : " msg)
  (.put *queue* msg))


(defmacro test-let [bindings]
  `(let ~bindings 3))


(defn pre-let- [bindings]
  (let [ks (vec (mapcat identity (partition 1 2 bindings)))
        vs (eval `(let ~bindings  ~ks))
        _ (println ks)
        _ (println vs)]
    `(vec (mapcat #(list %1 %2) '~ks ~vs))))

(defn pre-let [bindings & body]
  (let [new-ctx (eval `(let [ctx# (pre-eval-let- ~bindings)] ctx#))]
    `(let ~new-ctx ~@body)))

(defmulti handle-msg :type)

(defmethod handle-msg nil
  [msg]
  (prn msg "not match method."))

(defn async-execute
  [msg]
  (prn "async-execute : " (Thread/currentThread))
  (let [code (apply pre-let (:code msg) (:after msg))]
    {:type :sync :code code}))


(defmethod handle-msg :async
  [msg]
  (.execute *executor* #(let [after-code (async-execute msg)]
                          (send-msg after-code)) ))

(defmethod handle-msg :sync
  [msg]
  (eval (:code msg)))

(defn handle-msg- [msg]
  (println msg)
  (let [results (map eval (:program msg))
        after (:after msg)]
    (println "eval results : " results)
    (when after
      (if-let [result-params (:result msg)]
        (send-msg {:type :execute :program (list 'let (map #(vec %1 %2) result-params  results  ) after)})
        results))))

(defn loop-receive [fn-receive]
  (println "loop-receive start ")
  (loop [msg (fn-receive)]
    (println "loop-recevie : " msg)
    (handle-msg msg)
    (recur (fn-receive))))

(defn actor [fn-receive]
  (do
    (.start (Thread. #(loop-receive fn-receive) ))))

(defn with-async
  "execute asynchronize call"
  [bindings & body]
  (prn (type body) " " body)
  (send-msg {:type :async :code bindings :after body}))















