(ns simple-actor.test.actor
  (:use [simple-actor.actor] :reload)
  (:use [clojure.test]))


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

(deftest test-actor
  "test actor handles signals should use the perdefined order"
  (let [actor (actor test-handle)]
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

