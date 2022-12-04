(ns adventofcode-2022-clj.day3)
(require '[clojure.java.io :as io])

(defn get-priority [item]
  (if (Character/isLowerCase item)
    (- (int item) 96)
    (+ 26 (- (int item) 64))))

(defn part1 []
  (with-open [rdr (io/reader (str (io/resource "day3.txt")))]
    (->> (line-seq rdr)
         (map (fn [rucksack] (split-at (/ (count rucksack) 2) rucksack)))
         (map #(first (apply clojure.set/intersection (map set %))))
         (map get-priority)
         (reduce +)
         (println))))

(defn part2 []
  (with-open [rdr (io/reader (str (io/resource "day3.txt")))]
    (->> (line-seq rdr)
         (partition 3)
         (map #(first (apply clojure.set/intersection (map set %))))
         (map get-priority)
         (reduce +)
         (println))))