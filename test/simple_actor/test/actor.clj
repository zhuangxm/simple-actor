(ns simple-actor.test.actor
  (:use [simple-actor.actor] :reload)
  (:use [clojure.test])
  (:require [simple-actor.signal :as s]
            [clojure.tools.logging :as log]))


(def signals_ (ref '()))

(defmulti test-handle
  (fn [actor signal] (:type signal)))

(defmethod test-handle :single
  [actor signal]
  (dosync (alter signals_ conj :single))
  [{:type :none}])

(defmethod test-handle :double
  [actor signal]
  (dosync (alter signals_ conj :double))
  [{:type :single} {:type :single}])


(defmethod test-handle :four
  [actor signal]
  (dosync (alter signals_ conj :four))
  (repeat 2 {:type :double }))

(defmethod test-handle :none
  [actor signal]
  (dosync (alter signals_ conj :none))
  [])

(defmethod test-handle :update
  [actor signal]
  [(s/update-state-signal 3)])

(def state (atom nil))

(defmethod test-handle :check
  [actor signal]
  (reset! state (:state signal))
  nil)

(deftest test-actor
  "test actor handles signals should use the perdefined order"
  (let [actor (mk-actor test-handle)]
    (try
      (actor {:type :four})
      (actor {:type :none})
      (actor {:type :single})
      (Thread/sleep 1000)
      (is (= @signals_  (reverse '(:four :double :single :none :single :none
                                          :double :single :none :single :none
                                          :none
                                          :single
                                          :none)) )
          "check the order the signalss that was handled")
      (finally (stop-actor actor)))))

(deftest test-update-state
  "test handle update state"
  (let [actor (mk-actor test-handle)]
    (actor {:type :update})
    (actor {:type :check})
    (Thread/sleep 1000)
    (is (= 3 @state))))
