(ns simple-actor.test.module
  (:use [simple-actor.module] :reload)
  (:use [clojure.test]))


(def v (atom 0))

(defn handle-signal1 [actor signal]
  (is (= (:type signal) :test))
  (swap! v inc)
  (is (= @v 1) "first execute"))

(defn handle-signal2 [actor signal]
  (is (= (:type signal) :test))
  (swap! v inc)
  (is (= @v 2) "second execute"))

(defn handle-signal-other [actor signal]
  (is (= (:type signal) :other))
  (reset! v 0))

(deftest test-register
  "test can register handle function correctly"
  (let [[f-register f-main-handle] (mk-main-handle)]
    (f-register :test handle-signal1 )
    (f-register :test handle-signal2 )
    (f-register :other handle-signal-other)
    (f-main-handle nil {:type :test})
    (is (= @v 2))
    (f-main-handle nil {:type :other})
    (is (= @v 0))))
