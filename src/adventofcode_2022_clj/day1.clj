(ns adventofcode-2022-clj.day1
  (:require [clojure.string :as str]))
(require '[clojure.java.io :as io])

(defn part1 []
  (let [input (slurp (str (io/resource "day1.txt")))]
    (->> (str/split input #"\n\n")
         (map str/split-lines)
         (map #(map read-string %))
         (map #(reduce + %))
         (apply max)
         (println))))

(defn top-three [[big1 big2 big3 :as acc] x]
  (cond
    (> x big1) [x big1 big2]
    (> x big2) [big1 x big2]
    (> x big3) [big1 big2 x]
    :else acc)
  )

(defn part2 []
  (let [input (slurp (str (io/resource "day1.txt")))]
    (->> (str/split input #"\n\n")
         (map str/split-lines)
         (map #(map read-string %))
         (map #(reduce + %))
         (reduce top-three [0 0 0])
         (reduce +)
         (println))))