(ns adventofcode-2022-clj.day9
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))
(require '[clojure.java.io :as io])

(defn move-tail
  "Changes the position of a rope segment 'tail' in relation to the segment in front of it in the rope, 'head'.
  Returns the fixed tail, which may or may not have changed"
  [head tail]
  (let [[dist-row dist-col] (map - head tail)
        ; The vector [dir-row dir-col] points from tail to head, and its coordinates never exceed 1 in magnitude
        dir-row (Integer/compare dist-row 0)
        dir-col (Integer/compare dist-col 0)
        [tail-row tail-col] tail]
    ; If head is more than 1 unit away from tail in any coordinate, we need to move tail
    (if (or (>= (Math/abs dist-row) 2) (>= (Math/abs dist-col) 2))
      ; Move tail in the direction of head
      [(+ dir-row tail-row) (+ dir-col tail-col)]
      ; Otherwise, return tail unchanged
      [tail-row tail-col])))

(defn move-tails
  "For every rope segment in tails, starting from the one right after the head, change its position based on the segment
  in front of it. Returns the updated tails."
  [head tails]
  (loop [h head
         [t & ts] tails
         new-tails []]
    (if (nil? t)
      new-tails
      (let [new-t (move-tail h t)]
        (recur new-t ts (conj new-tails new-t))))))

(defn parse-command
  "Parses a command into a sequence of vectors representing sequential movements the head of the rope will make.
  Example: R 5 becomes [0 1] [0 2]  [0 3] [0 4] [0 5]"
  [command]
  (condp re-find command
    #"R (\d+)" :>> (fn [[_ steps]] (for [i (range 1 (inc (read-string steps)))] [0 i]))
    #"L (\d+)" :>> (fn [[_ steps]] (for [i (range 1 (inc (read-string steps)))] [0 (* -1 i)]))
    #"D (\d+)" :>> (fn [[_ steps]] (for [i (range 1 (inc (read-string steps)))] [i 0]))
    #"U (\d+)" :>> (fn [[_ steps]] (for [i (range 1 (inc (read-string steps)))] [(* -1 i) 0]))))

(defn apply-command
  "Applies a single command to head, moving all rope segments in tails accordingly. Returns a set of positions visited
  by the last element of tails during the movements."
  [head tails command]
  (let [parsed-command (parse-command command)]
    (let [[new-head new-tail positions] (reduce
                                          (fn [[_ t positions] step]
                                            (let [new-head (map + head step)
                                                  new-tails (move-tails new-head t)]
                                              [new-head new-tails (conj positions new-tails)]))
                                          [head tails []]
                                          parsed-command)]
      ; map last beloew because we only care about the positions visited by the last rope segment
      [new-head new-tail (set (map last positions))])))

(defn apply-commands
  "Applies a sequence of commands to head, moving all rope segments in tails accordingly. Returns a set of positions
  visited by the last element of tails during the movements."
  [head tails commands]
  (loop [[command & rest-commands] commands
         positions (set [])
         h head
         ts tails]
    (cond
      (nil? command)
      positions

      :else
      (let [[new-head new-tails new-positions] (apply-command h ts command)]
        (recur rest-commands (set/union positions new-positions) new-head new-tails))))
  )

(defn execute [head tails]
  (let [input (slurp (str (io/resource "day9.txt")))]
    (->> (str/split-lines input)
         (apply-commands head tails)
         (count)
         ))
  )


(defn part1 [] (execute [0 0] [[0 0]]))
(defn part2 [] (execute [0 0] [[0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] ]))