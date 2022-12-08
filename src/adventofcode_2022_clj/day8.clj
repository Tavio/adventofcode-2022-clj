(ns adventofcode-2022-clj.day8
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))
(require '[clojure.java.io :as io])

; Part 1

(defn visible-from-start
  "Given a sequence of trees, determines which ones are visible from the start of the sequence"
  [trees]
  (->>
    (reduce-kv
      (fn [[res height] idx tree]
        (if (> tree height)
          [(conj res idx) tree]
          [res height]))
      [[] -1]
      trees
      )
    (first)))

(defn visible-from-left
  "For all trees, determines which are visible from the left. Returns the coordinates of those trees"
  [trees]
  (let [from-start (map visible-from-start (map vec trees))]
    (map-indexed
      (fn [idx ts]
        (map (fn [t] [idx t]) ts))
      from-start)))

(defn visible-from-right
  "For all trees, determines which are visible from the right. Returns the coordinates of those trees"
  [trees]
  (let [num-trees (count (first trees))
        from-start (map (comp visible-from-start vec reverse) trees)]
    (map-indexed
      (fn [idx ts]
        (map (fn [t] [idx (- num-trees t 1)]) ts)) ; When generating the coordinates, fix them to the right orientation
      from-start)))

(defn trees-from-top
  "Generates a sequence of trees as seen from the top."
  [trees]
  (let [r (range 0 (count (first trees)))]
    (reduce (fn [res idx] (conj res (map #(nth % idx) trees))) ; When generating the coordinates, fix them to the right orientation
      [] r)))

(defn trees-from-bottom
  "Generates a sequence of trees as seen from the bottom"
  [trees]
  (let [r (range 0 (count (first trees)))]
    (reduce (fn [res idx] (conj res (reverse (map #(nth % idx) trees))))
            [] r)))

(defn visible-from-top
  "For all trees, determines which are visible from the top. Returns the coordinates of those trees"
  [trees]
  (let [trees-from-top (trees-from-top trees)
        visible (map (comp visible-from-start vec) trees-from-top)]
    (map-indexed
      (fn [idx ts]
        (map (fn [t] [t idx]) ts)) ; When generating the coordinates, fix them to the right orientation
      visible)))

(defn visible-from-bottom
  "For all trees, determines which are visible from the bottom. Returns the coordinates of those trees"
  [trees]
  (let [num-trees (count trees)
        trees-from-bottom (trees-from-bottom trees)
        visible (map (comp visible-from-start vec) trees-from-bottom)]
    visible
    (map-indexed
      (fn [idx ts]
        (map (fn [t] [(- num-trees t 1) idx]) ts)) ; When generating the coordinates, fix them to the right orientation
      visible)))

(defn calculate-visibility [trees]
  (let [from-left (set (reduce concat (visible-from-left trees)))
        from-right (set (reduce concat (visible-from-right trees)))
        from-top (set (reduce concat (visible-from-top trees)))
        from-bottom (set (reduce concat (visible-from-bottom trees)))]
    ; The functions above returns the coordinates of the trees that are visible, so we can add them to a set
    ; and not count them twice.
    (set/union from-left from-right from-top from-bottom)))

(defn part1 []
  (let [input (slurp (str (io/resource "day8.txt")))]
    (->> (str/split-lines input)
         (map #(str/split % #""))
         (map #(map read-string %))
         (calculate-visibility)
         (count))))

; Part 2

(defn viewing-distance-from-start
  "Given a sequence of trees and a tree that exists before the start of that sequence, determines the viewing distance
  of tree in relation to trees"
  [tree trees]
  (loop [ts trees
         count 0]
    (let [[next-tree & rest-trees] ts]
      (cond
        (nil? next-tree)
        count

        (>= next-tree tree)
        (inc count)

        :else
        (recur rest-trees (inc count))))))

(defn viewing-distance-left
  "Given a tree and its coordinates specified by row and col, determines the viewing distance to the left of tree"
  [row col tree trees]
  (let [trees-to-left
        (->> (nth trees row)
             (take col)
             (reverse))]
    (viewing-distance-from-start tree trees-to-left)))

(defn viewing-distance-right
  "Given a tree and its coordinates specified by row and col, determines the viewing distance to the right of tree"
  [row col tree trees]
  (let [trees-to-right
        (->> (nth trees row)
             (drop (inc col)))]
    (viewing-distance-from-start tree trees-to-right)))

(defn viewing-distance-up
  "Given a tree and its coordinates specified by row and col, determines the viewing distance above the tree"
  [row col tree trees]
  (let [trees-above
        (->> (map #(nth % col) trees)
             (take row)
             (reverse))]
    (viewing-distance-from-start tree trees-above)))

(defn viewing-distance-down
  "Given a tree and its coordinates specified by row and col, determines the viewing distance below the tree"
  [row col tree trees]
  (let [trees-below
        (->> (map #(nth % col) trees)
             (drop (inc row)))]
    (viewing-distance-from-start tree trees-below)))

(defn calculate-viewing-distances [trees]
  ; For every coordinate...
  (let [coords (for [row (range 0 (count trees))
                     col (range 0 (count (first trees)))]
                 [row col])]
    ; get its viewing distances in all directions...
    (map (fn [[row col]]
           (let [tree (first (drop col (nth trees row)))
                 view-left (viewing-distance-left row col tree trees)
                 view-right (viewing-distance-right row col tree trees)
                 view-up (viewing-distance-up row col tree trees)
                 view-down (viewing-distance-down row col tree trees)]
             ; and multiply them
             (* view-left view-right view-up view-down)))
         coords)))

(defn part2 []
  (let [input (slurp (str (io/resource "day8.txt")))]
    (->>
      (str/split-lines input)
      (map #(str/split % #""))
      (map #(map read-string %))
      (calculate-viewing-distances)
      (apply max))))