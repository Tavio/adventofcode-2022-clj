(ns adventofcode-2022-clj.day13
  (:require [clojure.string :as str]))
(require '[clojure.java.io :as io])
(require '[clojure.data.priority-map :refer [priority-map]])

; Multimethods are clojure's way of doing function overloading. Haskell we are not...
(defmulti compare-packets (fn [packet-a packet-b]
                            (cond
                              (and (coll? packet-a) (coll? packet-b))
                              :both-lists

                              (and (coll? packet-a) (not (coll? packet-b)))
                              :a-list

                              (and (coll? packet-b) (not (coll? packet-a)))
                              :b-list

                              :else
                              :both-single)))

(defmethod compare-packets :both-single [packet-a packet-b]
  (compare packet-a packet-b))

(defmethod compare-packets :a-list [packet-a packet-b]
  (compare-packets packet-a [packet-b]))

(defmethod compare-packets :b-list [packet-a packet-b]
  (compare-packets [packet-a] packet-b))

(defmethod compare-packets :both-lists [packet-a packet-b]
  (cond
    (and (empty? packet-b) (not (empty? packet-a)))
    1

    (and (empty? packet-a) (empty? packet-b))
    0

    (and (empty? packet-a) (not (empty? packet-b)))
    -1

    :else
    (case (compare-packets (first packet-a) (first packet-b))
      0 (compare-packets (rest packet-a) (rest packet-b))
      -1 -1
      1 1)))

(defn parse-packets [[packet-a-str  packet-b-str]]
  [(read-string packet-a-str) (read-string packet-b-str)])

(defn part1 []
  (let [input (slurp (str (io/resource "day13.txt")))]
    (->> (str/split input #"\n\n")
         (map #(str/split-lines %))
         (map parse-packets)
         (map #(apply compare-packets %))
         (keep-indexed #(if (< %2 0) (inc %1) nil))
         (reduce +))))

(defn decoder-key [packets]
  (let [first (inc (.indexOf packets [[2]]))
        second (inc (.indexOf packets [[6]]))]
    (* first second)))

(defn part2 []
  (let [input (slurp (str (io/resource "day13.txt")))]
    (->> (str/split input #"\n\n")
         (map #(str/split-lines %))
         (flatten)
         (map #(read-string %))
         (concat [[[2]]] [[[6]]])
         (sort-by identity #(compare-packets %1 %2))
         (decoder-key))))