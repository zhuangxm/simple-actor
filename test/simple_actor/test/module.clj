(ns simple-actor.test.module
  (:use [simple-actor.module] :reload)
  (:use [clojure.test]))


(def v (atom 0))

(defn handle-msg1 [actor msg]
  (is (= (:type msg) :test))
  (swap! v inc)
  (is (= @v 1) "first execute"))

(defn handle-msg2 [actor msg]
  (is (= (:type msg) :test))
  (swap! v inc)
  (is (= @v 2) "second execute"))

(defn handle-msg-other [actor msg]
  (is (= (:type msg) :other))
  (reset! v 0))

(deftest test-register
  "test can register handle function correctly"
  (let [[f-register f-main-handle] (mk-main-handle)]
    (f-register :test handle-msg1 )
    (f-register :test handle-msg2 )
    (f-register :other handle-msg-other)
    (f-main-handle nil {:type :test})
    (is (= @v 2))
    (f-main-handle nil {:type :other})
    (is (= @v 0))))
