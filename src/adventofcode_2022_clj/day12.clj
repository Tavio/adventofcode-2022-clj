(ns adventofcode-2022-clj.day12
  (:require [clojure.string :as str]))
(require '[clojure.java.io :as io])
(require '[clojure.data.priority-map :refer [priority-map]])

; Horribly inefficient, but I've copied my Dijkstra solution from last year and it works. Saves me an evening of coding,
; so I'll count this as a win. Yes, I know you can solve it with a simple DFS. Sue me.

(def elevations "SabcdefghijklmnopqrstuvwxyzE")

(defn into-2d-vec
  "Transforms an input matrix of chars into a 2d vector, transforming each element with cast-fn"
  [cast-fn matrix]
  (vec
    (map (fn [array]
           (vec (map cast-fn array)))
         matrix)))

(defn get-neighbours
  "Gets neighbours of element with coords i,j in matrix. We only consider as neighbours tiles that have height less than
  that of the element, or at most 1 higher.
  Returns a map where keys are encoded coordinates of each neighbour (i.e. :0-4) and values are the cost to move to that
  neighbour from position i,j."
  [i j matrix]
  (into {}
        (map (fn [[k _]] [k 1]) ; costs are always one for this exercise.
             (remove
               (fn [neighbour] (or (nil? (second neighbour)) (>= (- (second neighbour) (get-in matrix [i j])) 2)))
               [[(keyword (str (- i 1) "-" j)) (get-in matrix [(- i 1) j])]
                [(keyword (str (+ i 1) "-" j)) (get-in matrix [(+ i 1) j])]
                [(keyword (str i "-" (- j 1))) (get-in matrix [i (- j 1)])]
                [(keyword (str i "-" (+ j 1))) (get-in matrix [i (+ j 1)])]]))))

(defn build-adjacency-map
  "Builds a graph represented as an adjacency map from matrix."
  [matrix]
  (loop [limit-y (- (count matrix) 1)
         limit-x (- (count (first matrix)) 1)
         i 0
         j 0
         m {}]
    (if (> i limit-y)
      m
      (let [new-i (if (= j limit-x) (inc i) i)
            new-j (if (= j limit-x) 0 (inc j))
            neighbours (get-neighbours i j matrix)
            k (keyword (str i "-" j))]
        (recur limit-y limit-x new-i new-j (into m {k neighbours}))))))

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisited neighbors by using curr's shortest path"
  [g costs costs-hash curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
      (fn [[c c-h] nbr nbr-cost]
        (if (contains? costs-hash nbr)
          [(update-in c [nbr] min (+ curr-cost nbr-cost))
           (update-in c-h [nbr] min (+ curr-cost nbr-cost))]
          [c c-h]))
      [costs costs-hash]
      (get g curr))))

(def ^:private inf Double/POSITIVE_INFINITY)

(defn dijkstra
  "Returns a map of nodes to minimum cost from src using Dijkstra's algorithm. Graph is a map of nodes to map of
  neighboring nodes and associated cost. Optionally, specify destination node to return once cost is known"
  ([src g]
   (dijkstra src nil g))
  ([src dst g]
   (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0) ; cost to get to 0 starts as 0, everywhere else start at Inf
          ; We use a priority queue so at every iteration we always look at the next node with the shortest path to it.
          ; This is the heart of this algo.
          costs-hash (pop (reduce (fn [m [k v]] (assoc m k v)) (priority-map) costs))
          curr src]
     (cond
       (= curr dst)
       (select-keys costs [dst]) ; We are done! Return cost to go from src to dst.

       (or (empty? costs-hash) (= inf (get costs curr))) ; no path from src to dst
       nil

       :else
       (let [[next-costs next-costs-hash] (update-costs g costs costs-hash curr)
             next-node (first (first next-costs-hash))]
         (recur next-costs (pop next-costs-hash) next-node))))))

(defn part1 []
  (let [input (slurp (str (io/resource "day12.txt")))]
    (->> (str/split-lines input)
         (into-2d-vec (fn [c] (str/index-of elevations c)))
         (build-adjacency-map)
         (dijkstra :0-20 :20-55)
         )))

; Part 2

(defn a-positions
  "Returns coordinates of all positions where there's an 'a' in matrix"
  [matrix]
  (loop [limit-y (- (count matrix) 1)
         limit-x (- (count (first matrix)) 1)
         i 0
         j 0
         res []]
    (if (> i limit-y)
      res
      (let [new-i (if (= j limit-x) (inc i) i)
            new-j (if (= j limit-x) 0 (inc j))
            new-res (if (= 1 (get-in matrix [i j]))
                      (conj res (keyword (str i "-" j)))
                      res)
            ]
        (recur limit-y limit-x new-i new-j new-res)))))

(defn shortest-path-from-as [matrix]
  (let [adjacency-map (build-adjacency-map matrix)
        a-positions (a-positions matrix)
        paths-from-a (remove nil? (map (partial #(dijkstra % :20-55 adjacency-map))
                                       a-positions))]
    (first (sort (flatten (map vals paths-from-a))))))

(defn part2 []
  (let [input (slurp (str (io/resource "day12.txt")))]
    (->> (str/split-lines input)
         (into-2d-vec (fn [c] (str/index-of elevations c)))
         (shortest-path-from-as))))