(ns adventofcode-2022-clj.day15
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))
(require '[clojure.java.io :as io])

(defn in?
  "true if coll contains elem"
  [coll elem]
  (some #(= elem %) coll))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn parse-line [m line]
  (condp re-find line
    #"Sensor at x=(-?\d+), y=(-?\d+).*is at x=(-?\d+), y=(-?\d+)"
    :>> (fn [[_ sensor-x sensor-y beacon-x beacon-y]]
          (let [sensor-coords [(read-string sensor-x) (read-string sensor-y)]
                beacon-coords [(read-string beacon-x) (read-string beacon-y)]
                dist-to-beacon (manhattan-distance sensor-coords beacon-coords)
                sensor [sensor-coords dist-to-beacon]]
            (-> m
                (update :sensors conj sensor)
                (update :beacons (comp distinct conj) beacon-coords))))))

(defn covered-by [row [[sensor-x sensor-y] radius]]
  (let [dist-x-remaining (- radius (Math/abs (- sensor-y row)))]
    (if (> dist-x-remaining 0)
      (let [start (max 0 (- sensor-x dist-x-remaining))
            end (max 0 (+ sensor-x dist-x-remaining))]
        (if (= [0 0] [start end]) [] [start end]))
      [])))

(defn intersect? [[a-start a-end] [b-start b-end]]
  (or (and (>= a-start (dec b-start)) (<= a-start (inc b-end)))
      (and (>= b-start (dec a-start)) (<= b-start (inc a-end)))))

(defn merge-intersecting-ranges [[a-start a-end] [b-start b-end]]
  [(min a-start b-start) (max a-end b-end)])

(defn merge-all-intersecting [cover covers]
  (reduce
    merge-intersecting-ranges
    (conj covers cover)))

(defn merge-covers [cover covers]
  (cond
    (empty? cover)
    covers

    (empty? covers)
    [cover]

    :else
    (let [ {intersecting true non-intersecting false} (group-by #(intersect? % cover) covers)
          merged (merge-all-intersecting cover intersecting)]
      (sort-by first (conj non-intersecting merged)))))

(defn beacons-in-cover [row [cover-start cover-end] grid]
  (filter (fn [[beacon-x beacon-y]]
            (and (= row beacon-y)
                 (and (>= beacon-x cover-start) (<= beacon-x cover-end))
                 ))
          (get grid :beacons))
  )

(defn cover-size [[cover-start cover-end]]
  (inc (- cover-end cover-start)))

(defn divisible?
  "Determine if a number is divisible by the divisor with no remainders."
  [div num]
  (zero? (mod num div)))

(defn covers-in-row [row grid]
  (if (divisible? 10000 row) (println row))
  (reduce
    (fn [covers sensor]
      (let [next-cover (covered-by row sensor)]
        (merge-covers next-cover covers)))
    []
    (get grid :sensors)))

(defn positions-without-beacons [row grid]
  (let [cover (first (covers-in-row row grid))
        cover-size (cover-size cover)
        beacons-in-cover (beacons-in-cover row cover grid)
        ]
    (- cover-size (count beacons-in-cover))))

(defn part1 []
  (let [input (slurp (str (io/resource "day15.txt")))]
    (->> (str/split-lines input)
         (reduce parse-line {})
         (positions-without-beacons 2000000))))


;  Part 2

(defn find-distress-beacon [grid]
  (loop [row 0]
    (let [covers (covers-in-row row grid)]
      (cond
        (= 2 (count covers))
        (+ (* 4000000 (inc (second (first covers)))) row)

        (= row 4000000)
        nil

        :else
        (recur (inc row))))))

(defn part2 []
  (let [input (slurp (str (io/resource "day15.txt")))]
    (->> (str/split-lines input)
         (reduce parse-line {})
         (find-distress-beacon)
         ))
  )