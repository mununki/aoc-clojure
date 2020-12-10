(ns first-clojure.aoc201806
  (:require [first-clojure.utils :as u]
            [clojure.string :as str]))

(def input
  (u/read-input "src/first_clojure/input/2018/d6"))

(defn parse
  ;; => ([1 [262 196]] [2 [110 109]] [3 [58 188]])
  [coordinates]
  (map-indexed #(let [str-of-coord (str/split %2 #", ")
                      coord (vec (map (fn [c] (Integer/parseInt c)) str-of-coord))]
                  [(+ %1 1) coord]) coordinates))

(defn max-x-y
  ;; => [352 356]
  [coordinates]
  (reduce (fn [acc coord]
            (let [str-of-coord (str/split coord #", ")
                  [x y] (map #(Integer/parseInt %) str-of-coord)
                  [max-x max-y] acc]
              [(max max-x x) (max max-y y)])) [0 0] coordinates))

(defn manhattan-dist
  [a b]
  (let [[x-a y-a] a
        [x-b y-b] b
        x-dist (Math/abs (- x-a x-b))
        y-dist (Math/abs (- y-a y-b))]
    (+ x-dist y-dist)))

(defn all-points
  [vec-of-x-y]
  (let [[x y] vec-of-x-y
        z (max x y)]
    (for [coll-x (range (+ z 1))
          coll-y (range (+ z 1))]
      [coll-x coll-y])))

(defn measure-dists
  ;; parsed coordinates에 대하여 max-of-x max-of-y 안의 모든 point의
  ;; manhattan distance구한 거리를 vector로 담아 value로 갖는 map을 구한다
  ;; => {[0 0] {1 50, 2 29, 3 20, ... } [0 1] {1 49, 2 30, 3 27, ... } ... }
  [coordinates max-of-x-y]
  (let [[max-x max-y] max-of-x-y
        all-area (all-points max-of-x-y)]
    (reduce (fn [acc [x y]]
              (assoc acc [x y] (->> coordinates
                                    (reduce (fn [acc-dists [name [coord-x coord-y]]]
                                              (let [dist (manhattan-dist [x y] [coord-x coord-y])]
                                                ;; 거리가 0인 건 자기 자신
                                                (if (not= dist 0)
                                                  (assoc acc-dists name dist)
                                                  acc-dists)))
                                            {}))))
            {} all-area)))

(defn overlapped?
  ;; {1 50, 2 29, 3 20, ... } 중 가장 가까운 거리가 2개 이상인 지 찾는 함수
  ;; => false
  [marked]
  (let [x (sort (vals marked))]
    (= (first x) (second x))))

(defn find-nearest
  ;; {1 50, 2 29, 3 20, ... } 거리 값 중 가장 작은 값을 구하는 함수
  ;; => [3 20]
  [marked]
  (reduce (fn [acc [name dist]]
            (let [[min-name min-dist] acc]
              (if (> min-dist dist)
                [name dist]
                acc))) marked))

(defn mark-on-map
  ;; => {[0 0] [2 23], [0 1] [0 0], ... }
  ;; overlapped면 [0 0]
  ;; overlapped가 없으면 [name dist]
  [all-area]
  (reduce (fn [acc [point dists]]
            (if (overlapped? dists)
              ;; [0 0]는 중복된 빈 지역
              (assoc acc point [0 0])
              (assoc acc point (find-nearest dists)))) {} all-area))

(defn safe?
  [marked]
  (let [dists (vals marked)
        total-dist (reduce + dists)]
    (< total-dist 10000)))

(defn check-if-safe
  [all-area]
  (reduce (fn [acc [point dists]]
            (if (safe? dists)
              (conj acc point)
              acc)) [] all-area))

(defn names-of-infinite
  ;; grid의 각 모서리 => infinite여서 제거할 item들을 찾는 함수
  [all-area]
  (reduce (fn [acc [[x y] [name dist]]]
            (cond
              (and (or (= x 0) (= y 0) (= x 356) (= y 356)) (> name 0)) (conj acc name)
              :else acc)) #{} all-area))

(defn largest-area
  ;; => [[0 0] [28 41], [0 1] [17 24], ... ]
  [all-area]
  (->> (vals all-area)
       (map (fn [[name dist]]
              name))
       frequencies
       (sort #(compare (second %2) (second %1)))))

(defn points-out-of-grid
  ;; infinite에 해당하는 coord name들
  ;; => #{7 29 31 ... }
  [points]
  (-> (measure-dists (-> points
                         parse)
                     (-> points
                         max-x-y))
      mark-on-map
      names-of-infinite))

(defn largest-area-in-grid
  ;; => [[0 0] [28 41], [0 1] [17 24], ... ]
  [points]
  (-> (measure-dists (-> points
                         parse)
                     (-> points
                         max-x-y))
      mark-on-map
      largest-area))

(defn part1
  [points]
  (let [p (points-out-of-grid points)
        l (largest-area-in-grid points)
        names-largest (remove p (map (fn [[name area]]
                                       name) l))]
    (keep (fn [[name area]]
            (if (= name (first names-largest))
              area)) l)))
(defn part2
  [points]
  (-> (measure-dists (-> points
                         parse)
                     (-> points
                         max-x-y))
      check-if-safe
      count))

(comment
  (count input)
  (-> input
      parse)
  (-> input
      max-x-y)
  (manhattan-dist [5 5] [4 6])
  (-> (measure-dists (-> input
                         parse)
                     (-> input
                         max-x-y))
      mark-on-map
      names-of-infinite)
  (-> (measure-dists (-> input
                         parse)
                     (-> input
                         max-x-y))
      mark-on-map
      largest-area)
  (overlapped? {0 7 1 10 2 10})
  (find-nearest {0 10 1 20 3 21})
  (remove #{1 2} [1 2 3 4])
  (part1 input);; => (4143)
  (-> (measure-dists (-> input
                         parse)
                     (-> input
                         max-x-y))
      check-if-safe
      count)
  (part2 input) ;; => 35039
  )
