(ns adventofcode-2022-clj.day11)
(require '[clojure.java.io :as io])

(defn divisible?
  "Determine if a number is divisible by the divisor with no remainders."
  [div num]
  (zero? (mod num div)))

(def monkeys {
              :0 {
                  :items [52, 60, 85, 69, 75, 75]
                  :operation (partial * 17)
                  :test (partial divisible? 13)
                  :if-true :6
                  :if-false :7
                  :num-inspected 0
                  }
              :1 {
                  :items [96, 82, 61, 99, 82, 84, 85]
                  :operation (partial + 8)
                  :test (partial divisible? 7)
                  :if-true :0
                  :if-false :7
                  :num-inspected 0
                  }
              :2 {
                  :items [95, 79]
                  :operation (fn [old] (+ old 6))
                  :test (partial divisible? 19)
                  :if-true :5
                  :if-false :3
                  :num-inspected 0
                  }
              :3 {
                  :items [88, 50, 82, 65, 77]
                  :operation (fn [old] (* old 19))
                  :test (partial divisible? 2)
                  :if-true :4
                  :if-false :1
                  :num-inspected 0
                  }
              :4 {
                  :items [66, 90, 59, 90, 87, 63, 53, 88]
                  :operation (fn [old] (+ old 7))
                  :test (partial divisible? 5)
                  :if-true :1
                  :if-false :0
                  :num-inspected 0
                  }
              :5 {
                  :items [92, 75, 62]
                  :operation (fn [old] (* old old))
                  :test (partial divisible? 3)
                  :if-true :3
                  :if-false :4
                  :num-inspected 0
                  }
              :6 {
                  :items [94, 86, 76, 67]
                  :operation (fn [old] (+ old 1))
                  :test (partial divisible? 11)
                  :if-true :5
                  :if-false :2
                  :num-inspected 0
                  }
              :7 {
                  :items [57]
                  :operation (fn [old] (+ old 2))
                  :test (partial divisible? 17)
                  :if-true :6
                  :if-false :2
                  :num-inspected 0
                  }
              }
  )

; d is our strategy to keep the numbers low. It is the multiplication of all the numbers each monkey checks divisibility
; for. If we take the mod of worry and d, we keep worry short without disrupting the monkey business
(def d (* 17 11 3 5 2 19 7 13))

(defn run-turn [monkeys curr-monkey]
  (reduce
    (fn [ms item]
      (let [{operation :operation test :test if-true :if-true if-false :if-false} (get monkeys curr-monkey)
            worry (mod (operation item) d) ; the magic mod referenced above to keep worry low
            test-result (test worry)
            target-monkey (if test-result if-true if-false)]
        ; 3 chained changes to the monkeys state map: add the item to the next monkey, remove it from the current monkey,
        ; and increase the number of times the current monkey has inspected an item
        (update-in
          (update-in
            (update-in ms [target-monkey :items] conj worry)
            [curr-monkey :items]
            rest)
          [curr-monkey :num-inspected]
          inc)))
    monkeys
    (get-in monkeys [curr-monkey :items])))

(defn run-round [monkeys]
  (reduce
    run-turn
    monkeys
    (sort (keys monkeys))))

(defn run-rounds []
  (reduce
    (fn [ms _] (run-round ms))
    monkeys
    (range 1 10001)))

(defn sort-by-monkey-business [monkeys]
  (into
    (sorted-map-by (fn [key1 key2] (compare
                                     (get-in monkeys [key2 :num-inspected])
                                     (get-in monkeys [key1 :num-inspected]))))
    monkeys))

(defn execute []
  (->> (run-rounds)
       (sort-by-monkey-business)
       (take 2)
       (map second)
       (map #(get % :num-inspected))
       (reduce *)))