(ns simple-actor.test.module.async
  (:use [simple-actor.actor] :reload)
  (:use [simple-actor.module] :reload)
  (:use [simple-actor.module.async] :reload)
  (:use [clojure.test])
  (:import [java.util.concurrent LinkedBlockingQueue
            ThreadPoolExecutor TimeUnit]))

(def *executor-example* (ThreadPoolExecutor. 3 3 0 TimeUnit/MINUTES
                                             (LinkedBlockingQueue. 1024)))

(deftest test-async-module
    "test async module can work"
    (let [[f-register f-main-handle] (mk-main-handle)
          f-send (actor f-main-handle)]
      (do 
        (init-module *executor-example* f-register)
        (do
          (f-send (with-async [_ (prn "hello")
                                a 5
                                b (+ a 2)
                                t (Thread/currentThread)]
                     (is (not= t (Thread/currentThread))"execute in different thread" )
                     (is (= 12 (+ a b)))
                     (prn (+ a b)))))
        (Thread/sleep 1000)
        (stop-actor f-send))))
