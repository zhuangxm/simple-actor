(ns simple-actor.test.actor
  (:use [simple-actor.actor] :reload)
  (:use [clojure.test]))


(def messages_ (ref '()))

(defmulti test-handle
  (fn [actor msg] (:type msg)))

(defmethod test-handle :single
  [actor msg]
  (dosync (alter messages_ conj :single))
  [{:type :none}])

(defmethod test-handle :double
  [actor msg]
  (dosync (alter messages_ conj :double))
  [{:type :single} {:type :single}])


(defmethod test-handle :four
  [actor msg]
  (dosync (alter messages_ conj :four))
  (repeat 2 {:type :double }))

(defmethod test-handle :none
  [actor msg]
  (dosync (alter messages_ conj :none))
  [])

(deftest test-actor
  "test actor handles message should use the perdefined order"
  (let [actor (actor test-handle)]
    (try
      (actor {:type :four})
      (actor {:type :none})
      (actor {:type :single})
      (Thread/sleep 1000)
      (is (= @messages_  (reverse '(:four :double :single :none :single :none
                                          :double :single :none :single :none
                                          :none
                                          :single
                                          :none)) )
          "check the order the messages that was handled") 
      (finally (stop-actor actor)))))

