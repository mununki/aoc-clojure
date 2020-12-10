(ns first-clojure.aoc201809
  (:require [clojure.string :as str]
            [first-clojure.utils :as u]
            [data.deque :as dq]))

(def input
  (u/read-input "src/first_clojure/input/2018/d9"))

(def input-test
  (u/read-input "src/first_clojure/input/2018/d9test"))

(defn parse
  [raw]
  (let [trimmed (str/trim raw)
        [players _ _ _ _ _ last _] (str/split trimmed #" ")]
    [players last]))

(defn insertv
  [v pos x]
  {:pre [(< pos (count v))]}
  (into (conj (subvec v 0 pos) x) (subvec v pos (count v))))

(defn omitv
  [v pos]
  {:pre [(< pos (count v))]}
  (into (subvec v 0 pos) (subvec v (+ pos 1) (count v))))

(defn next-pos
  [marbles pos offset dir]
  {:pre [(< pos (count marbles)) (or (= dir "CW") (= dir "CCW"))]}
  (let [c (count marbles)
        sum (+ pos offset)
        diff (- pos offset)]
    (cond
      (= dir "CW") (if (<= c sum)
                     (mod sum c)
                     sum)
      (= dir "CCW") (if (< pos offset)
                      (let [rem-offset (rem diff c)]
                        (if (= rem-offset 0)
                          pos
                          (+ c rem-offset)))
                      diff)
      :else pos)))

(defn generate-marbles
  [cur pos end scores marbles]
  (if (< end cur)
    scores
    (let [next (inc cur)
          multipled? (= (mod cur 23) 0)
          next-p (if multipled?
                   (next-pos marbles pos 7 "CCW")
                   (next-pos marbles pos 2 "CW"))
          next-scores (if multipled?
                        (assoc scores cur (+ cur (marbles next-p)))
                        scores)
          next-m (if multipled?
                   (omitv marbles next-p)
                   (insertv marbles next-p cur))
          _ (when (= (mod cur 1000000) 0)
              (prn cur))]
      (recur next next-p end next-scores next-m))))

(defn best-score
  [num-of-player last-marble]
  (sort-by (fn [[_ v]]
             v) > (reduce (fn [acc [k v]]
                            (let [player (mod k num-of-player)]
                              (if (contains? acc player)
                                (update-in acc [player] + v)
                                (assoc-in acc [player] v)))) {} (generate-marbles 1 0 last-marble {} [0]))))

(defn toss-to-front
  [marbles]
  (let [end (dq/peek-last marbles)
        cutted (dq/remove-last marbles)
        tossed (dq/add-first cutted end)]
    tossed))

(defn toss-to-end
  [marbles]
  (let [front (dq/peek-first marbles)
        cutted (dq/remove-first marbles)
        tossed (dq/add-last cutted front)]
    tossed))

(defn generate-marbles-deque
  [cur end scores marbles]
  (if (< end cur)
    scores
    (let [multipled? (= (mod cur 23) 0)
          rolled-front (last (take 2 (iterate toss-to-front marbles)))
          rolled-back (last (take 8 (iterate toss-to-end marbles)))
          second-score (dq/peek-first rolled-back)
          new-marbles (if multipled?
                        (toss-to-front (dq/remove-first rolled-back))
                        (dq/add-first rolled-front cur))
          new-scores (if multipled?
                       (assoc scores cur (+ cur second-score))
                       scores)
          _ (when (= (mod cur 1000000) 0)
              (prn cur))]
      (recur (inc cur) end new-scores new-marbles))))

(defn best-score-deque
  [num-of-player last-marble]
  (sort-by (fn [[_ v]]
             v) > (reduce (fn [acc [k v]]
                            (let [player (mod k num-of-player)]
                              (if (contains? acc player)
                                (update-in acc [player] + v)
                                (assoc-in acc [player] v)))) {} (generate-marbles-deque 1 last-marble {} (dq/deque 0)))))

(defn part1
  []
  (first (best-score-deque 425 70848)))

(defn part2
  []
  (first (best-score-deque 425 7084800)))

(comment
  ;; Execution error (ArrayIndexOutOfBoundsException) at clojure.core.rrb-vector.rrbt/slice-left$fn (rrbt.clj:393).
  ;; Index 33 out of bounds for length 33
  ;;
  ;; It seems okay with around 2M items.
  (generate-marbles 1 0 3000000 {} [0])
  (generate-marbles-deque 1 23 {} (dq/deque 0))

  (time (part1))
;; => [186 413188]
  (time (part2))
;; => [341 3377272893]
  (insertv [0] 0 1)
  (omitv [0 2 1] 1)
  (next-pos [0 16 8] 4 2 "CW")
  (nth [0 3 2] 1)
  (as-> input input
    (map parse input)))
