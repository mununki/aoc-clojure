(ns first-clojure.aoc201802
  (:require [first-clojure.utils :as u]))

(def input (u/read-input "src/first_clojure/input/aoc201802"))

(defn dups-counter [line]
  (filter #(> (second %) 1) (frequencies line)))

(defn count-by-freqs [freqs]
  (reduce (fn [acc [char freq]]
            (assoc acc freq 1)) {} freqs))

(defn aggregate [x]
  (reduce #(merge-with + %1 %2) {} x))

(defn result [x]
  (reduce (fn [acc [freqs count]]
            (* acc count)) 1 x))

(def part1
  (->> input
       (map dups-counter)
       (map count-by-freqs)
       (aggregate)
       (result)))

(use 'clojure.data)

(defn check-diff-one [x, y]
  (let [[only-x only-y common] (diff (vec x) (vec y))
        diff-x (remove nil? only-x)
        diff-y (remove nil? only-y)]
    (and (= (count diff-x) 1) (= (count diff-y) 1))))

(defn finder [id]
  (reduce (fn [acc target]
            (if (check-diff-one id target)
              (conj acc id target)
              acc)) #{} input))

(def found-pair
  (reduce (fn [acc item]
            (let [pair (finder item)
                  is-pair (> (count pair) 0)]
              (if is-pair
                (conj acc pair)
                acc))) #{} input))

(def part2
  ;; let으로 절차형처럼 하는 건 좀 별로 아닐까..??
  (let [pair found-pair
        ;; pair = #{#{"aaa" "bbb"}}
        ;; 깔끔하게 꺼낼 수 있는 방법은??
        pair-vec (vec pair)
        id1 (first (vec (first pair-vec)))
        id2 (second (vec (first pair-vec)))
        [only-id1 only-id2 common] (diff (vec id1) (vec id2))]
    (clojure.string/join (remove nil? common))))

(comment
  (u/read-input "src/first_clojure/input/aoc201802")
  (result (aggregate (map count-by-freqs (map dups-counter input))))
  (diff (vec "aaaa") (vec "aaac"))
  (check-diff-one "aaa" "ncb")
  (clojure.string/join [\a \b]))

