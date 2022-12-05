(ns adventofcode-2022-clj.day5
  (:require [clojure.string :as str]))
(require '[clojure.java.io :as io])

(def stacks
  [["H" "R" "B" "D" "Z" "F" "L" "S"],
   ["T" "B" "M" "Z" "R"],
   ["Z" "L" "C" "H" "N" "S"],
   ["S" "C" "F" "J"],
   ["P" "G" "H" "W" "R" "Z" "B"],
   ["V" "J" "Z" "G" "D" "N" "M" "T"],
   ["G" "L" "N" "W" "F" "S" "P" "Q"],
   ["M" "Z" "R"],
   ["M" "C" "L" "G" "V" "R" "T"]])

(defn move [n origin dest all-at-once? stacks]
  (let [origin-stack (nth stacks origin)
        size-origin-stack (count origin-stack)
        dest-stack (nth stacks dest)
        to-move (if all-at-once? (take-last n origin-stack) (reverse (take-last n origin-stack)))]
    (assoc
      (assoc stacks origin (take (- size-origin-stack n) origin-stack))
      dest
      (concat dest-stack to-move))))

(defn parse-move [move]
  (let [[_ n _ from _ to] (str/split move #" ")]
    (map read-string [n from to])))

(defn execute [move-all-at-once?]
  (let [input (slurp (str (io/resource "day5.txt")))]
    (->> (str/split-lines input)
         (map parse-move)
         (reduce (fn [acc [n origin dest]] (move n (dec origin) (dec dest) move-all-at-once? acc)) stacks)
         (map last)
         (apply str)
         (println)))
  )

(defn part1 [] (execute false))
(defn part2 [] (execute true))