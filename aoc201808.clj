(ns first-clojure.aoc201808
  (:require [clojure.string :as str]
            [first-clojure.utils :as u]))

(def input
  (u/read-input "src/first_clojure/input/aoc201808"))

(def input-test
  (u/read-input "src/first_clojure/input/aoc201808test"))

(defn parse
  [raw]
  (as-> raw raw
    (first raw)
    (str/split raw #" ")
    (mapv #(Integer/parseInt %) raw)))

(defn sum-of-metadata
  [[sum sum2 data]]
  (if (empty? data)
    [0 0 []]
    (let [child (first data)
          meta (second data)
          rest-data (vec (nthrest data 2))
          child-nodes (take (+ child 1) (iterate sum-of-metadata [sum sum2 rest-data]))
          [sum-of-child _ remaining] (last child-nodes)
          meta-entries (take meta remaining)
          filtered-entries (filter #(<= % child) meta-entries)
          value-of-sum2 (if (= child 0)
                          (reduce + meta-entries)
                          (reduce (fn [acc [_ s _]]
                                    (+' acc s)) 0 (map #(nth child-nodes %)
                                                       filtered-entries)))]
      [(+ (reduce + meta-entries) sum-of-child) value-of-sum2 (drop meta (vec remaining))])))

(defn part1_2
  [raw]
  (->> raw
       parse
       (conj [0 0])
       sum-of-metadata))

(comment
  ;; tree-seq
  ;; zipper
  (empty? (filter #(<= % 1) [2]))
  (parse input)
  (take 10 (iterate (fn [x]
                      (* x 2)) 1))
  (empty? '(1))
  (first [])
  (vec (nthrest [0 1 2 3] 1))
  (drop 2 [0 1 2 3 4])
  (sum-of-metadata [0 (-> input
                          parse)])
  (last (take 2 (iterate (fn [x]
                           (* x 2)) 3)))
  (reduce + (subvec [0 1 2 3 4 5] 0 3))
  (reduce (fn [acc [s rem]]
            (+ acc s)) 0 '([1 10] [2 10] [3 10]))
  (part1_2 input) ;; => [49602 25656 ()]
  )
