(ns first-clojure.aoc201801
  (:require [first-clojure.utils :as u]))

(defn read-input []
  (with-open [rdr (clojure.java.io/reader "src/first_clojure/input/aoc201801")]
    (reduce conj [] (line-seq rdr))))

(def input-vector
  (map (fn [x] (read-string x)) (read-input)))

;; part1
(def part1
  (reduce + 0 input-vector))

(defn next-frequency [acc, num]
  (+ (if (empty? acc) 0 (last acc)) num))

(defn generate-freq [first freqs]
  (reduce (fn [acc num]
            (conj acc (next-frequency acc num))) first freqs))

;; part2
(defn find-dups [freqs changes]
  (let [latest-freq (if (empty? freqs) 0 (last freqs))
        change (first changes)
        new-freq (+ latest-freq change)
        following-changes (rest changes)]
    (if (u/ccontains? freqs new-freq)
      new-freq
      (if (empty? following-changes)
        (recur (conj freqs new-freq) input-vector)
        (recur (conj freqs new-freq) following-changes))
      )))

(comment
  ;; part1
  part1
  ;; part2
  (find-dups [0] input-vector)
  ;; tests
  (find-dups [0] [1 -1])
  (find-dups [0] [3 3 4 -2 -4])
  (find-dups [0] [-6 3 8 5 -6])
  (find-dups [0] [7 7 -2 -7 -4])
  )
