(ns adventofcode-2022-clj.day14
  (:require [clojure.string :as str]))
(require '[clojure.java.io :as io])

(defn generate-coords [[start-col start-row] [end-col end-row]]
  (cond
    (= start-col end-col)
    (let [[from to] (sort [start-row end-row])]
      (map (fn [row] [start-col row]) (range from (inc to))))

    :else
    (let [[from to] (sort [start-col end-col])]
      (map (fn [col] [col start-row]) (range from  (inc to))))))


(defn parse-line [cave-map [line-start line-end]]
  (let [coords (generate-coords line-start line-end)]
    (reduce
      (fn [m coord-key] (assoc-in m coord-key "#"))
      cave-map
      coords)))

(defn parse-path [cave-map path]
  (->> (str/split path #"->")
       (map (fn [p] (str "[" p "]")))
       (map read-string)
       (partition 2 1)
       (reduce parse-line cave-map)))

(defn out-of-bounds? [[_ row] max-depth]
  (> row max-depth))

(defn step-sand [[sand-col sand-row] max-depth cave-map]
  (let [below [sand-col (inc sand-row)]
        left [(dec sand-col) (inc sand-row)]
        right [(inc sand-col) (inc sand-row)]]
    (cond
      (nil? (get-in cave-map below))
      (if (out-of-bounds? below max-depth) nil below)

      (nil? (get-in cave-map left))
      (if (out-of-bounds? left max-depth) nil left)

      (nil? (get-in cave-map right))
      (if (out-of-bounds? right max-depth) nil right)

      :else
      [sand-col sand-row])))

(defn drop-sand-till-rest [max-depth cave-map]
  (loop [m cave-map
         sand [500 0]]
    (let [new-sand (step-sand sand max-depth m)]
      (cond
        (nil? new-sand) ; sand fell into abyss
        [false, (assoc-in m sand nil)]

        (= sand new-sand) ; sand came to rest
        [true, m]

        :else ; sand moved
        (recur (-> cave-map
                   (assoc-in sand nil)
                   (assoc-in new-sand "o"))
               new-sand)))))

(defn calc-max-depth [cave-map]
  (apply max (flatten (map keys (vals cave-map)))))

(defn drop-sands-till-full [cave-map]
  (let [max-depth (calc-max-depth cave-map)]
    (loop [m cave-map
           n 0]
      (let [[can-drop-more-sand new-m] (drop-sand-till-rest max-depth m)]
        (if can-drop-more-sand (recur new-m (inc n)) n)))))

(defn part1 []
  (let [input (slurp (str (io/resource "day14.txt")))]
    (->> (str/split-lines input)
         (reduce parse-path {})
         (drop-sands-till-full))))



; Part 2




(defn is-floor? [[_ row] max-depth]
  (= row max-depth))

(defn step-sand2 [[sand-col sand-row] max-depth cave-map]
  (let [below [sand-col (inc sand-row)]
        left [(dec sand-col) (inc sand-row)]
        right [(inc sand-col) (inc sand-row)]]
    (cond
      (is-floor? below max-depth)
      [sand-col sand-row]

      (nil? (get-in cave-map below))
      below

      (nil? (get-in cave-map left))
      left

      (nil? (get-in cave-map right))
      right

      :else
      [sand-col sand-row])))

(defn drop-sand-till-rest2 [max-depth cave-map]
  (loop [m cave-map
         sand [500 0]]
    (let [new-sand (step-sand2 sand max-depth m)]
      (cond
        (= sand new-sand) ; sand came to rest
        [(not (= sand [500 0])) m]

        :else ; sand moved
        (recur (-> cave-map
                   (assoc-in sand nil)
                   (assoc-in new-sand "o"))
               new-sand)))))

(defn drop-sands-till-full2 [cave-map]
  (let [max-depth (+ 2 (calc-max-depth cave-map))]
    (loop [m cave-map
           n 0]
      (let [[can-drop-more-sand new-m] (drop-sand-till-rest2 max-depth m)]
        (if can-drop-more-sand (recur new-m (inc n)) (inc n))))))

(defn part2 []
  (let [input (slurp (str (io/resource "day14.txt")))]
    (->> (str/split-lines input)
         (reduce parse-path {})
         (drop-sands-till-full2))))