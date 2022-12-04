(ns adventofcode-2022-clj.day4
  (:require [clojure.string :as str]))
(require '[clojure.java.io :as io])

(defn fully-contains? [[[range1-start range1-end] [range2-start range2-end]]]
  (cond
    (and (>= range2-start range1-start) (<= range2-end range1-end)) true
    (and (>= range1-start range2-start) (<= range1-end range2-end)) true
    :else false))

(defn overlaps? [[[range1-start range1-end] [range2-start range2-end]]]
  (cond
    (and (<= range1-start range2-end) (<= range2-start range1-end)) true
    :else false))

(defn split-range-and-cast-to-int [range]
  (map read-string (str/split range #"-")))

(defn part1 []
  (let [input (slurp (str (io/resource "day4.txt")))]
    (->> (str/split-lines input)
         (map #(str/split % #","))
         (map #(map split-range-and-cast-to-int %))
         (filter fully-contains?)
         (count)
         (println))))

(defn part2 []
  (let [input (slurp (str (io/resource "day4.txt")))]
    (->> (str/split-lines input)
         (map #(str/split % #","))
         (map #(map split-range-and-cast-to-int %))
         (filter overlaps?)
         (count)
         (println))))