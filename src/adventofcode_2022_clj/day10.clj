(ns adventofcode-2022-clj.day10
  (:require
    [clojure.string :as str]))
(require '[clojure.java.io :as io])

(defn run-command
  "Given cpu-state which is a list where indexes are cycle numbers and values are the values of x for each cycle,
  runs command and returns the updated cpu state"
  [cpu-state command]
  (let [x (last cpu-state)]
    (condp re-find command
      #"noop" :>> (fn [_] (conj cpu-state x))
      #"addx (-?\d+)" :>> (fn [[_ val-str]] (let [val (read-string val-str)
                                                  new-x (+ x val)]
                                              ; update cpu-state for the next two cycles
                                              (conj cpu-state x new-x))))))

(defn run-commands
  "Runs a sequence of commands and returns the resulting cpu state, is a list where indexes are cycle numbers and values
   are the values of x for each cycle"
  [commands]
  (reduce
    run-command
    [1] ; initial state is cycle 1 with x value of 1
    commands))

(defn signal-strength [cpu-state]
  (let [cycles (select-keys cpu-state [19 59 99 139 179 219])]
    (reduce-kv (fn [acc k v] (+ acc (* (inc k) v)))
               0 cycles)))

(defn crt-state
  "Derives the crt state from the cpu state. Returns a flat vector of 240 pixels."
  [cpu-state]
  (reduce
    (fn [crt pixel-idx]
      (let [pixel-idx-mod (mod pixel-idx 40)  ; mod because we only care about the horizontal position
            x (get cpu-state pixel-idx)
            pixel (if (<= (dec x) pixel-idx-mod (inc x)) `\# \.)] ; <= here checks that the pixel is in range of x
        (conj crt pixel)))
    []
    (range 0 240)))

(defn print-crt [crt-state]
  (println (str/join \newline (map #(apply str %) (partition-all 40 crt-state)))))

(defn execute []
  (let [input (slurp (str (io/resource "day10.txt")))
        cpu-state (->> (str/split-lines input)
                       (run-commands))
        signal-strength (signal-strength cpu-state)
        crt (crt-state cpu-state)]
    (println signal-strength)
    (print-crt crt)))