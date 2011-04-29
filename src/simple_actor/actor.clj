(ns simple-actor.actor
  (:import [java.util.concurrent LinkedBlockingQueue
            ThreadPoolExecutor TimeUnit]))

(defn loop-receive
  "loop receive messages and handle it
   fn-receive [] : receive message function with zero parameter
   fn-handle [msg] : handle message function with one parameter msg"
  [fn-receive fn-handle]
  (loop [msg (fn-receive)]
    (if-not (= (:type msg) :stop)
      (do 
        (try 
          (fn-handle msg)
          (catch Exception e (prn "handle-msg error " e) )  )
        (recur (fn-receive))))))

(defn stop-actor
  "stop a actor"
  [actor]
  ((:send actor) {:type :stop}))

(defn actor
  "start a actor, loop receive and handle msg
   fn-handle  [msg] : function that handle msg
   return send and receive message function {:send fn-send :receive fn-receive}"
  [fn-handle]
  (let [queue (LinkedBlockingQueue.)]
    (letfn [(f-send [msg] (.put queue msg))
            (f-receive [] (.take queue))]
      
      (.start (Thread. #(do (prn "actor thread start" (Thread/currentThread))
                            (loop-receive f-receive fn-handle)
                            (prn "actor thread stop" (Thread/currentThread))) ))
      {:send f-send :receive f-receive})))


