(ns adventofcode-2022-clj.day17
  (:require [clojure.string :as str]))

(require '[clojure.java.io :as io])

(defn print-chamber [chamber] (clojure.pprint/pprint (reverse chamber)))

(defn cross
  "Given the coordinates of the bottom leftmost part of a cross, returns a sequence of
  coordinates of all other parts of the cross."
  [[x y]]
  [[x y] [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)] [x (+ 2 y)]])

(defn h-bar
  "Given the coordinates of the bottom leftmost part of a horizontal bar, returns a
  sequence of coordinates of all other parts of the bar."
  [[x y]]
  [[x y] [(inc x) y] [(+ 2 x) y] [(+ 3 x) y]])

(defn v-bar
  "Given the coordinates of the bottom leftmost part of a vertical bar, returns a
  sequence of coordinates of all other parts of the bar."
  [[x y]]
  [[x y] [x (inc y)] [x (+ 2 y)] [x (+ 3 y)]])

(defn l
  "Given the coordinates of the bottom leftmost part of an L, returns a
  sequence of coordinates of all other parts of the L."
  [[x y]]
  [[x y] [(inc x) y] [(+ 2 x) y] [(+ 2 x) (inc y)] [(+ 2 x) (+ 2 y)]])

(defn square
  "Given the coordinates of the bottom leftmost part of a square, returns a
  sequence of coordinates of all other parts of the square."
  [[x y]]
  [[x y] [(inc x) y] [x (inc y)] [(inc x) (inc y)]]
  )

(def new-chamber-row [0 0 0 0 0 0 0])

(def starting-chamber [])

(def shape-heights {:cross 3 :h-bar 1 :v-bar 4 :l 3 :square 2})

(def shape-bottom-x-initial-positions {:cross 3 :h-bar 2 :v-bar 2 :l 2 :square 2})

(defn spawn-shape [chamber highest-rock-height shape-type]
  (let [chamber-height (count chamber)
        shape-height (get shape-heights shape-type)
        number-new-rows (+ shape-height (- 3 (- chamber-height highest-rock-height)))
        expanded-chamber (vec (concat chamber
                                      (repeat
                                       number-new-rows
                                       new-chamber-row)))
        shape-y (- (+ chamber-height number-new-rows) shape-height)
        shape-x (get shape-bottom-x-initial-positions shape-type)]
    [expanded-chamber [shape-x shape-y]]))

(defn move-shape [chamber [shape-x shape-y] shape-type direction]
  (let [shape (case shape-type
                :h-bar h-bar
                :v-bar v-bar
                :cross cross
                :l l
                :square square)]
    (if (some (fn [[x y]]
                (let [in-destination (get-in chamber (reverse (map + direction [x y])))]
                  (or (= 1 in-destination) (nil? in-destination))))
              (shape [shape-x shape-y]))
      [shape-x shape-y]
      (map + direction [shape-x shape-y]))))

(defn settle-shape [chamber shape-pos shape-type]
  (let [shape (case shape-type
                :h-bar h-bar
                :v-bar v-bar
                :cross cross
                :l l
                :square square)
        rest-shape (shape shape-pos)]
    (reduce (fn [c [x y]] (assoc-in c [y x] 1))
            chamber
            rest-shape)))

(defn direction-from-jet [jet]
  (case jet
    \< [-1 0]
    \> [1 0]))

(defn spawn-and-settle-shape [chamber highest-rock-height shape-type jets]
  (loop [next-jets jets
         [new-chamber [shape-x shape-y]] (spawn-shape chamber highest-rock-height shape-type)]
    (let [jet-direction (direction-from-jet (first next-jets))
          [new-shape-x new-shape-y] (move-shape new-chamber [shape-x shape-y] shape-type jet-direction)
          [new-new-shape-x new-new-shape-y] (move-shape new-chamber [new-shape-x new-shape-y] shape-type [0 -1])]
      (if (= shape-y new-new-shape-y)
        (let [new-new-chamber (settle-shape new-chamber [new-new-shape-x new-new-shape-y] shape-type)
              new-highest-rock-height (max highest-rock-height (+ new-new-shape-y (get shape-heights shape-type)))]
          [new-new-chamber new-highest-rock-height (drop 1 next-jets)])
        (recur (drop 1 next-jets) [new-chamber [new-new-shape-x new-new-shape-y]])))))


(defn run [n]
  (let [input (slurp (str (io/resource "day17.txt")))]
    (loop [chamber starting-chamber
           shapes (cycle [:h-bar :cross :l :v-bar :square])
           jets (cycle (vec (char-array (str/trim-newline input))))
           num-rocks 0
           highest-rock-height 0
           heights []]
      (if (= n num-rocks)
        highest-rock-height
        (let [[new-chamber new-highest-rock-height next-jets] (spawn-and-settle-shape chamber highest-rock-height (first shapes) jets)]
          (recur new-chamber
                 (drop 1 shapes)
                 next-jets
                 (inc num-rocks)
                 new-highest-rock-height
                 (conj heights new-highest-rock-height)))))))

(defn part2-sample []
  (let [
        pattern [4 0 1 2 3 0 1 1 3 2 2 0 0 2 3 4 0 1 2 1 2 0 1 2 1 2 0 1 3 2 0 0 1 3 3]
        pattern-size (count pattern)
        pattern-sum (reduce + pattern)
        start 17
        m (mod (- 2022 start) pattern-size)
        rest-sum (reduce + (take m pattern))
        ]
    (+ (run start) (* (/ (- 2022 start m) pattern-size) pattern-sum) rest-sum)))

(defn part2-real []
  (let [
        pattern [4 0 1 3 3 4 0 0 0 1 2 0 1 2 2 0 2 0 2 3 0 0 1 3 0 3 0 0 1 3 2 0 1 3 2 0 2 1 3 3 2 0 1 3 2 2 0 1 3 2 2 0 1 3 2 2 2 1 2 3 0 0 1 3 2 1 2 1 3 2 4 0 0 0 2 2 0 1 3 2 2 0 1 2 1 2 2 1 3 2 4 0 1 2 2 2 2 1 2 2 2 0 1 2 3 4 0 1 2 2 2 0 1 2 3 4 2 1 3 3 0 0 1 3 2 0 0 1 3 3 0 0 0 2 2 2 0 1 3 3 0 0 1 2 2 2 0 1 3 3 2 0 1 3 2 2 0 1 2 2 4 0 1 2 1 3 0 1 3 3 2 0 1 3 3 2 2 1 2 3 0 1 0 3 3 0 0 1 3 3 2 2 1 3 3 4 0 0 0 3 1 2 1 0 0 1 1 0 3 0 2 0 1 3 2 4 2 1 2 3 0 0 1 3 3 4 2 0 3 0 0 1 1 0 0 2 0 1 3 3 2 0 1 3 3 0 2 1 3 2 0 1 1 3 2 2 0 1 3 2 4 0 1 3 0 3 2 1 3 3 2 0 0 3 0 1 0 1 3 2 2 0 1 3 2 0 0 1 3 2 1 0 0 3 2 4 0 1 3 2 2 0 1 3 2 4 0 1 3 2 4 2 1 2 3 2 0 1 3 3 2 0 1 3 2 1 2 0 3 0 0 2 1 2 2 2 0 1 3 2 2 2 0 3 0 3 2 0 0 1 0 0 1 3 2 1 0 0 3 3 0 0 1 3 3 0 0 0 2 3 2 2 1 3 3 0 0 1 3 0 4 0 1 3 3 0 0 1 3 3 0 2 1 3 0 4 2 0 0 3 0 1 0 3 3 2 2 1 3 3 0 0 1 2 2 2 0 1 2 3 0 2 1 3 0 2 0 1 3 3 4 0 1 2 1 3 0 1 3 2 0 0 1 3 0 0 0 1 3 2 0 0 1 3 2 4 0 1 3 2 2 2 1 3 3 2 0 1 3 3 0 2 1 3 3 2 0 1 3 3 2 0 1 3 3 0 0 1 2 1 2 0 1 3 3 2 0 1 3 3 2 2 1 3 2 2 0 1 3 3 2 0 1 3 3 0 0 0 2 2 2 0 1 2 3 0 0 1 2 2 2 0 1 3 0 0 2 1 2 1 2 0 1 3 2 1 2 1 2 2 0 0 1 3 0 4 0 0 2 3 0 0 1 3 3 0 0 1 3 3 2 2 1 3 3 2 0 1 3 3 2 0 0 2 2 2 0 1 3 3 2 0 1 2 2 4 0 0 0 3 2 0 1 3 2 2 0 1 2 2 1 1 1 3 3 0 0 1 3 3 0 0 1 1 3 0 0 1 2 1 3 2 0 0 3 2 0 1 3 2 4 0 1 2 1 2 0 0 2 0 2 0 1 3 2 2 0 1 0 3 2 2 1 2 1 2 2 0 0 3 0 0 1 3 2 1 0 0 3 3 4 0 1 3 2 0 0 1 3 3 4 0 1 3 0 0 0 1 3 3 2 2 0 3 2 0 0 1 3 2 2 0 1 0 3 2 2 1 3 3 2 0 1 3 0 3 2 0 3 0 3 2 0 0 3 2 0 1 3 2 2 0 0 2 2 2 0 1 2 3 4 0 1 3 2 2 2 0 0 0 4 0 1 2 3 0 0 0 3 3 0 0 1 3 2 2 0 1 3 3 2 0 1 2 3 0 0 1 3 0 2 0 0 3 0 3 0 1 2 3 2 0 1 3 2 0 0 1 2 3 4 2 1 2 1 2 2 0 0 3 0 2 0 0 3 0 2 1 2 3 2 0 1 3 3 0 2 1 0 3 2 0 0 2 3 0 2 1 3 3 0 0 1 2 3 4 2 1 2 2 2 0 0 3 3 0 0 0 2 2 2 0 1 2 2 1 1 1 3 2 2 0 1 3 2 4 0 0 2 3 4 0 1 3 0 3 2 1 3 3 0 2 1 1 3 0 0 1 3 2 0 0 1 2 3 0 0 1 1 3 0 0 1 0 3 1 2 1 3 0 0 2 1 2 3 0 0 1 3 3 4 0 1 0 1 4 2 1 2 1 3 2 1 3 2 2 0 1 2 1 2 0 1 3 3 0 0 1 3 3 2 2 1 3 3 4 0 1 3 0 3 0 0 3 0 3 0 0 3 2 4 0 0 0 1 2 2 1 3 0 3 2 0 2 3 4 0 0 0 3 1 2 0 3 0 0 2 1 3 2 1 0 1 3 3 0 0 1 3 3 0 0 1 2 1 4 0 1 3 3 0 0 1 3 3 2 0 1 3 0 2 0 1 2 1 2 2 1 2 2 0 2 1 2 2 2 0 1 3 0 1 1 0 3 3 0 2 1 2 3 0 0 1 3 3 2 0 1 3 3 2 2 1 2 2 4 0 1 3 3 4 2 1 2 1 0 0 1 3 2 0 0 1 3 2 0 2 1 3 3 0 0 0 2 2 2 0 1 3 0 2 2 1 3 0 0 2 1 3 0 1 1 1 0 3 2 2 1 2 3 0 0 1 2 2 0 0 1 3 3 0 0 1 3 3 0 0 1 2 3 4 0 1 3 3 2 2 1 3 3 0 0 1 3 2 0 2 0 3 2 0 2 1 2 1 2 2 1 3 2 2 0 0 2 3 0 0 1 3 0 2 2 0 2 3 2 2 1 3 3 2 2 1 0 3 1 1 1 3 3 0 0 1 3 0 0 2 1 3 2 2 0 1 3 2 2 0 1 2 1 2 0 1 2 2 2 0 1 2 3 0 0 1 3 3 2 0 1 2 2 2 0 1 3 2 2 2 0 2 3 4 0 0 2 0 0 0 1 3 3 2 0 1 0 3 1 2 0 3 0 3 0 0 2 3 4 2 1 3 2 0 0 1 3 2 2 2 1 3 2 2 2 0 2 3 2 2 1 2 2 1 1 1 3 3 2 0 1 2 3 0 0 1 2 2 2 2 1 0 3 2 0 1 2 2 2 2 0 3 0 0 1 1 2 3 0 2 0 2 1 3 0 0 1 2 2 0 1 3 3 0 0 1 3 3 2 2 1 3 2 2 2 1 3 2 2 2 1 2 3 2 2 1 3 3 0 0 0 2 3 2 0 1 3 3 0 0 1 2 3 2 0 1 3 3 0 0 1 3 3 2 0 1 0 1 2 2 1 3 3 2 0 1 3 3 4 0 1 3 0 2 0 1 3 3 0 0 1 3 3 2 0 0 2 2 4 2 1 0 0 4 2 1 3 3 2 0 1 3 3 2 0 1 3 3 0 0 1 3 3 4 0 0 0 3 0 0 1 3 0 0 0 1 3 2 2 2 1 1 2 1 2 1 2 1 3 0 0 3 3 0 0 1 3 2 0 0 1 2 1 1 1 1 3 3 0 0 1 3 3 2 0 1 3 2 0 0 1 2 1 2 2 0 0 3 1 2 1 3 3 2 0 1 3 3 0 2 1 3 0 2 2 0 0 3 1 1 1 2 2 1 2 0 1 2 2 0 1 2 3 0 0 1 3 3 4 0 1 3 3 2 2 1 3 3 0 0 1 3 2 0 2 1 3 0 4 0 1 3 2 0 1 1 3 3 2 0 1 2 1 4 2 1 3 3 0 0 0 0 3 2 2 1 3 0 3 0 1 3 2 1 2 1 3 3 2 2 1 3 3 2 2 1 3 3 2 2 1 3 0 1 0 1 3 3 0 0 1 2 3 2 0 0 3 2 2 2 1 3 3 0 0 1 3 3 2 0 1 3 0 1 2 1 2 1 2 2 1 2 3 0 2 1 2 1 2 0 1 3 3]
        pattern-size (count pattern)
        pattern-sum (reduce + pattern)
        start 1517
        m (mod (- 1000000000000 start) pattern-size)
        rest-sum (reduce + (take m pattern))
        ]
    (+ (run start) (* (/ (- 1000000000000 start m) pattern-size) pattern-sum) rest-sum)))
