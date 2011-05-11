(ns simple-actor.test.module.timer
  (:use [simple-actor.module.timer] :reload)
  (:use [simple-actor.actor] :reload)
  (:use [simple-actor.module] :reload)
  (:use [clojure.test]))

(deftest test-timer-module
  "test timer module work correctly"
  (let [[f-register f-main-handle] (mk-main-handle)
        f-send (actor f-main-handle)
        timer1-counter (atom 0)
        timer2-counter (atom 0)]
    (do
      (init-module f-register)
      (do (f-send (with-delay "timer1" 100 (swap! timer1-counter inc) nil))
          (f-send (with-timer "timer2" 100 (swap! timer2-counter inc) nil))
          (Thread/sleep 50)
          (f-send (cancel-timer "timer1"))
          (Thread/sleep 100)
          (is (= @timer1-counter 0) "timer1 can not be execute")
          (is (= @timer2-counter 1) "timer2 has been execute once")
          (f-send (with-delay "timer1" 100 (swap! timer1-counter inc) nil))
          (Thread/sleep 110)
          (is (= @timer1-counter 1) "timer1 has been execute once")
          (is (= @timer2-counter 2) "timer2 has been execute twice")
          (Thread/sleep 1000)
          (is (= @timer1-counter 1) "timer1 can noly execute once")
          (is (= @timer2-counter 12) "timer2 has been execute 12 times")
          (f-send (cancel-timer "timer2"))
          (Thread/sleep 1000)
          (is (= @timer2-counter 12) "timer2 can not execute again")
          (stop-actor f-send)))))
