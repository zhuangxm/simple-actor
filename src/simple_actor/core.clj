(ns simple-actor.core
  (:import [java.util.concurrent LinkedBlockingQueue]))

(def msg-example {:type :execute :result {'a 1 'b 2} :program (list 'a 'b) })

(def *queue* (LinkedBlockingQueue.))

(defn receive-msg []
  (.take *queue*))

(defn send-msg [msg]
  (println "send-msg : " msg)
  (.put *queue* msg))


(defmacro pre-eval-ctx- [ctx]
  (let [ks (vec (mapcat identity (partition 1 2 ctx)))
        vs (eval `(let ~(vec ctx)  ~ks))
        _ (println ks)
        _ (println vs)]
    `(vec (mapcat #(list %1 %2) '~ks ~vs))))

(defmacro pre-eval-ctx [ctx & body]
  (let [new-ctx (eval `(let [ctx# (pre-eval-ctx- ~ctx)] ctx#))]
    `(let ~new-ctx ~@body)))


;TODO complete this function to ##########################################
(defmacro contextual-eval [ctx expr]
  (println ctx))

(defn handle-msg- [msg]
  (println msg)
  (let [results (map eval (:program msg))
        after (:after msg)]
    (println "eval results : " results)
    (when after
      (if-let [result-params (:result msg)]
        (send-msg {:type :execute :program (list 'let (map #(vec %1 %2) result-params  results  ) after)})
        results
        ))) )


(defn loop-receive [fn-receive]
  (println "loop-receive start ")
  (loop [msg (fn-receive)]
    (println "loop-recevie : " msg)
    (handle-msg msg)
    (recur (fn-receive))))



(defn actor [fn-receive]
  (do
    (.start (Thread. #(loop-receive fn-receive) ))))


(defmacro async-one
  "execute an synchronize call and execute others"
  [sym-result statement & body]
  (if sym-result 
    `(send-msg {:type :execute :result ~sym-result :program ~statement :after ~@body})
    `(send-msg {:type :execute :program ~@body}) ))

(defmacro with-async
  "execute asynchronize call"
  [[var- code- & rest :as bindings] & body]
  (println var-)
  (let [r-bindings (drop 2 bindings)]
    (if var- 
      `(async-one '~var- ~code- (with-async ~r-bindings ~@body))
      `(async-one nil nil ~@body) )))
