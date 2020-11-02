(ns first-clojure.aoc201803
  (:require [first-clojure.utils :as u]
            [clojure.string :as str]))

(def input
  (u/read-input "src/first_clojure/input/aoc201803"))

(defn coordinates [start offset with]
  (let [[start-x start-y] start
        [offset-x offset-y] offset]
    (for [x (range start-x (+ start-x offset-x))
          y (range start-y (+ start-y offset-y))]
      {[x y] with})))

(defn merge-coords [acc coords]
  (if (empty? coords)
    acc
    (recur (merge-with + acc (first coords)) (rest coords))))

(defn mark-claims [acc claim]
  (let [[id at start area] (str/split claim #" ")
        x (str/split (subs start 0 (- (count start) 1)) #",")
        y (str/split area #"x")
        coords (coordinates (map #(Integer/parseInt %) x) (map #(Integer/parseInt %) y) 1)]
    (merge-coords acc coords)))

(defn count-overlap [claims]
  (count (filter (fn [[_ v]]
                   (> v 1)) (reduce mark-claims {} claims))))

(def part1
  (count-overlap input))

(defn overwrite-coords [acc coords]
  (if (empty? coords)
    acc
    (recur (merge acc (let [coord (first (keys (first coords)))]
                        (if (contains? acc coord)
                          {coord 0}
                          (first coords)))) (rest coords))))

(defn mark-claims-with-id [acc claim]
  (let [[id at start area] (str/split claim #" ")
        i (Integer/parseInt (subs id 1))
        x (str/split (subs start 0 (- (count start) 1)) #",")
        y (str/split area #"x")
        coords (coordinates (map #(Integer/parseInt %) x) (map #(Integer/parseInt %) y) i)]
    (overwrite-coords acc coords)))

(defn area-by-id [acc claim]
  (let [[id at start area] (str/split claim #" ")
        i (Integer/parseInt (subs id 1))
        y (map #(Integer/parseInt %) (str/split area #"x"))
        sqaure (reduce * y)]
    (merge acc {i sqaure})))

(defn count-by-id [claims id]
  (reduce #(if (= %2 id)
             (inc %1)
             %1) 0 (vals claims)))

(defn find-not-overlapped [claims]
  (let [claims-by-id (reduce mark-claims-with-id {} claims)
        areas-by-id (reduce area-by-id {} claims)]
    (reduce (fn [acc item]
              ;; map의 item을 받아 destructuring을 할 때 vector로 받을 수 있다!?
              (let [[id area] item
                    count (count-by-id claims-by-id id)]
                (if (= area count)
                  (conj acc id)
                  acc))) [] areas-by-id)))

(def part2
  (find-not-overlapped input))

(comment
  ;; part1
  (coordinates [1 3] [4 4] 1)
  (merge-coords {} (coordinates [1 3] [4 4]))
  (mark-claims {} "#1 @ 306,433: 2x2")
  (count-overlap input)
  ;; part2
  (reduce mark-claims-with-id {} input)
  (reduce area-by-id {} input)
  (count-by-id (reduce mark-claims-with-id {} input) 1)
  ;; ArrayMap을 다룰 때 map의 한 item을 어떻게 구조분해 해야하나..
  ;; 더 좋은 방법은??
  (let [coord (first (keys (first '({[1 2] 1} {[1 3] 2}))))] coord)
  (find-not-overlapped input))
