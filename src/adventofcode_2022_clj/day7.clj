(ns adventofcode-2022-clj.day7
  (:require
    [clojure.string :as str]))
(require '[clojure.java.io :as io])
(require '[clojure.walk :as walk])

; the two functions below help us to generate unique ids for every member of the
; the map which we will use to represent our  tree
(def max-node-id (atom 0))
(defn get-node-id []
  (let [id @max-node-id]
    (swap! max-node-id inc)
    (keyword (str id))))

(defn parse-ls
  "Parses an ls command which was run from the parent-node directory in tree, and whose output is the
  lines parameter. Adds the files and directories in lines as children of parent-node in tree, and returns tree."
  [parent-node-id tree lines]
  (reduce
    (fn [t line]
      (let [current-children (get-in t [parent-node-id :children])
            new-id (get-node-id)]
        (condp re-find line
          #"dir (.*)" :>> (fn [[_ dir-name]]
                            ; add the new directory in the tree, and update the children of parent-node to contain it
                            (assoc-in
                              (assoc t new-id {:name dir-name :children () :parent parent-node-id})
                              [parent-node-id :children] (conj current-children new-id)))
          #"(\d+) (.*)" :>> (fn [[_ file-size file-name]]
                              ; add the new file in the tree, and update the children of parent-node to contain it
                              (assoc-in
                                (assoc t new-id {:name file-name :size (read-string file-size) :parent parent-node-id})
                                [parent-node-id :children] (conj current-children new-id))))))
    tree
    lines)
  )

(defn find-child-dir-id
  "Finds a directory node in tree under curr-node-id that has name dir-name and returns the id of that node"
  [tree curr-node-id dir-name]
  (let [children-ids (get-in tree [curr-node-id :children])]
    (first (filter
             (fn [child-id] (let [child (get tree child-id)]
                              (and
                                (not (nil? (:children child)))
                                (= dir-name (:name child)))))
             children-ids))))

(defn parse-next-command
  "Parses the next command from a list of commands, and returns: the node in the tree we are at after the parsing,
  the remaining commands that still have to be parsed, and the tree (which may have changed as the result of parsing
  the next command)"
  [root-id curr-node-id tree commands]
  (let [[next-line & rest-commands] commands]
    (condp re-find next-line
      ; The forms below match the regex on the left against next-line and provide the matches as arguments to the
      ; function on the right
      #"\$ cd /" :>> (fn [_] [root-id rest-commands tree])
      #"\$ cd \.\." :>> (fn [_] (let [parent-node-id (get-in tree [curr-node-id :parent])]
                                  [parent-node-id rest-commands tree]))
      #"\$ cd (.*)" :>> (fn [[_ dir-name]] (let [dir-id (find-child-dir-id tree curr-node-id dir-name)]
                                             [dir-id rest-commands tree]))
      ; The ls command requires us to read more commands from the commands list until we've read all files and
      ; directories listed
      #"\$ ls" :>> (fn [_] (let [[next-lines rest-rest-commands] (split-with #(not (= \$ (first %))) rest-commands)
                                 next-tree (parse-ls curr-node-id tree next-lines)]
                             [curr-node-id rest-rest-commands next-tree])))))

(defn build-tree
  "Parses the input and builds a tree of files and directories. The tree is represented
  by a map, where every entry maps a unique key to a node. A node has a name, a parent
  (which is an id pointing to another entry in the map) and may have children (a sequence
   of ids pointing to other entries in the map).
   Before any parsing of the input is done, the tree already starts with the node
   {:name '/' :parent nil :children []}"
  []
  (let [root-id (get-node-id)]
    (loop [curr-node-id root-id ; curr-node-id represents the current node of the tree we are in as we perform the parsing
           commands (str/split-lines (slurp (str (io/resource "day7.txt"))))
           tree (assoc {} root-id {:name " / " :parent nil :children []})]
      (if (empty? commands)
        ;no more commands to parse, return tree
        tree
        ;otherwise, parse the next command and recur
        (let [[next-node-id next-commands next-tree] (parse-next-command root-id curr-node-id tree commands)]
          (recur next-node-id next-commands next-tree))))))

(defn is-leaf? [node-id tree]
   (nil? (:children (get tree node-id))))

(defn calculate-sizes
  "Calculates the size of a node in tree and updates the node's size attribute.
  Returns the modified tree."
  [node-id tree]
  (cond
    ; The size of a file is its own size
    (is-leaf? node-id tree)
    [(get-in tree [node-id :size]) tree]

    ; The size of a directory is the size of everything under it
    :else
    (let [[children-size new-tree]
          ; Call calculate-sizes for each child and merge each of their changed trees into tree. Also adds all of their
          ; sizes and change node's size with the result.
          (reduce
            (fn [[sum-acc t-acc] [new-sum new-t]]
              [(+ sum-acc new-sum) (merge-with into t-acc new-t)])
            [0 tree]
            (map #(calculate-sizes % tree)
                 (get-in tree [node-id :children])))]
      [children-size (assoc-in new-tree [node-id :size] children-size)])
    )
  )

(defn prewalk
  "Produces a list of tree starting from node-id in pre order"
  [node-id tree]
  (cond
    (is-leaf? node-id tree)
    (get tree node-id)

    :else
    ; this is probably inefficient
    (flatten
      (conj
        (map #(prewalk % tree) (get-in tree [node-id :children]))
        (get tree node-id)))
    )
  )

(defn part1 []
  (let [tree (build-tree)
        [_ calculated-tree] (calculate-sizes :0 tree)]
    (->> calculated-tree
         (prewalk :0)
         (filter #(not (nil? (:children %))))
         (map #(:size %))
         (filter #(< % 100000))
         (reduce +)
         )))

(defn part2 []
  (let [tree (build-tree)
        [_ calculated-tree] (calculate-sizes :0 tree)
        total-size-available 70000000
        needed-for-update 30000000
        outmost-size (get-in calculated-tree [:0 :size])
        unused-size (- total-size-available outmost-size)
        to-delete (- needed-for-update unused-size)
        ]
    (->> calculated-tree
         (prewalk :0)
         (filter #(not (nil? (:children %))))
         (map #(:size %))
         (filter #(>= % to-delete))
         (apply min))

    ))