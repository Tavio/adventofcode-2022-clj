(ns adventofcode-2022-clj.day6)
(require '[clojure.java.io :as io])

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn is-marker? [marker-length s]
  (= marker-length (count (set s))))

(defn execute [marker-length]
  (let [input (slurp (str (io/resource "day6.txt")))]
    (->> (partition marker-length 1 input)
         (indices #(is-marker? marker-length %))
         (first)
         (+ marker-length)
         (println)))
  )

(defn part1 [] (execute 4))
(defn part2 [] (execute 14))