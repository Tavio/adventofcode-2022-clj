(ns adventofcode-2022-clj.day16
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]))
(require '[clojure.java.io :as io])

(defn dist-to-other-valves
  "Returns a map where each pair is a node in valves and their distance from src-valve"
  [valves src-valve]
  (loop [q (list [src-valve 0]) ; keep a queue of pairs of nodes and their "depths" in relation to src-valve
         visited (list src-valve)
         distances {}]
    (if (empty? q)
      distances
      (let [[[next n] & rest] q
            neighbours (map
                        (fn [neighbour][neighbour (inc n)]) ; pair neighbour with their distance from src-valve
                        (remove (into #{} visited) (get-in valves [next :tunnels]))) ; only non-visited neighbours
            ]
        (recur (concat rest neighbours)
               (conj visited next)
               (reduce
                (fn [ds [neighbour n]] (assoc ds neighbour n))
                distances
                neighbours))))))

(defn map-distances
  "For every valve v, generates a map of the distances from v to every other valve"
  [valves]
  (reduce
   (fn [m [k v]]
     (assoc m k (assoc v :distances (dist-to-other-valves valves k))))
   {}
   valves))

(defn potential-pressure
  "Returns the pressure over time we get if we move from from-valve to to-valve and open to-valve. We expect all valves but the initial one to have
  positive rate (zero rates have been filtered out already)."
  [from-valve to-valve time-left valves]
  (let [time-to-move (get-in valves [from-valve :distances to-valve]) ; time to move from this valve to the next
        new-time-left (- time-left time-to-move 1) ; time left after moving and opening valve
        pressure (* (get-in valves [to-valve :rate]) new-time-left) ; pressure the next valve will release over time
        ]
    [pressure new-time-left]))

(defn run-best-path [to-visit visited time-left total-pressure valves]
  (if
      (or (empty? to-visit) (zero? time-left))
      total-pressure
      (let [[last-visited & _] visited]
        (apply max (map
                    (fn [next]
                      (let [[pressure new-time-left] (potential-pressure last-visited next time-left valves)]
                        (if (<= new-time-left 0)
                          total-pressure
                          (run-best-path (remove #(= % next) to-visit) (conj visited next) new-time-left (+ total-pressure pressure) valves))))
                    to-visit)))))

(defn best-path
  "Calculates the best path between the starting valve :AA and all the other valves. Valves with zero rate are used only for moving between valves that
  can be opened."
  [valves]
  (let [start-valve :AA
        to-visit (remove
                  (fn [valve] (zero? (get-in valves [valve :rate])))
                  (keys valves))]
    (run-best-path to-visit (list start-valve) 30 0 valves)))

(defn parse-valve [valve-str]
  (condp re-find valve-str
    #"Valve (\w{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)"
    :>> (fn [[_ valve rate tunnels]]
          [(keyword valve) {:rate (read-string rate) :tunnels (map keyword (str/split tunnels #", "))}])))

(defn part1 []
  (let [input (slurp (str (io/resource "day16.txt")))]
    (->> (str/split-lines input)
         (map parse-valve)
         (into {})
         (map-distances)
         (best-path))))

(defn best-path2
  "For every possible partitioning of valves between me and elephant, run best path for both and add them up, taking the best partition at the end.
  Takes a long time to run."
  [valves]
  (let [start-valve :AA
        non-zero-rate-valves (remove
                              (fn [valve] (zero? (get-in valves [valve :rate])))
                              (keys valves))
        partitions (combo/partitions non-zero-rate-valves :min 2 :max 2)]
    (apply max (map
               (fn [[me-to-visit elephant-to-visit]] (+
                                                      (run-best-path me-to-visit (list start-valve) 26 0 valves)
                                                      (run-best-path elephant-to-visit (list start-valve) 26 0 valves)))
               partitions))))

(defn part2 []
  (let [input (slurp (str (io/resource "day16.txt")))]
    (->> (str/split-lines input)
         (map parse-valve)
         (into {})
         (map-distances)
         (best-path2))))
