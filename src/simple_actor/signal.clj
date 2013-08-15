(ns simple-actor.signal)

(defn update-state-signal
  "generate update state signal"
  [new-state]
  {:type :update-state :new-state new-state})
