(ns first-clojure.2018.d14
  (:require [clojure.string :as str]
            [first-clojure.utils :as u]))

(def input-test 2018)
(def input 209231)

(defn gen-recipies
  [init len]
  (fn [offset-recipies]
    (loop [recipies init
           pos-a 0
           pos-b 1]
      (let [score-a (nth recipies pos-a)
            score-b (nth recipies pos-b)
            sum (+ score-a score-b)
            added-recipies (if (> sum 9)
                             [(quot sum 10) (mod sum 10)]
                             [sum])
            new-recipies (apply conj recipies added-recipies)
            total (count new-recipies)
            new-pos-a (mod (+ pos-a score-a 1) total)
            new-pos-b (mod (+ pos-b score-b 1) total)]
        (if (> total (+ offset-recipies len))
          (take len (nthrest new-recipies offset-recipies))
          (recur new-recipies new-pos-a new-pos-b))))))

(defn lookup
  [recipies value]
  (let [total         (count recipies)
        len           (count value)
        last-five     (subvec recipies (max (- total len) 0))
        last-six      (subvec recipies (max (- total (+ len 1)) 0) (- total 1))]
    (cond
      (= last-five value) (- total len)
      (= last-six value) (- total (+ len 1))
      :else nil)))

(defn gen-recipies2
  [{:keys [r a b v]}]
  (loop [recipies r
         pos-a a
         pos-b b]
    (let [score-a         (first (subvec recipies pos-a (+ pos-a 1)))
          score-b         (first (subvec recipies pos-b (+ pos-b 1)))
          sum             (+ score-a score-b)
          new-recipies    (if (> sum 9)
                            [(quot sum 10) (mod sum 10)]
                            [sum])
          next-recipies   (apply conj recipies new-recipies)
          total           (count next-recipies)
          next-pos-a      (mod (+ pos-a score-a 1) total)
          next-pos-b      (mod (+ pos-b score-b 1) total)
          result          (lookup next-recipies v)
          _               (when (= (mod total 10000) 0)
                            (prn total))]
      (if (not (nil? result))
        result
        (recur next-recipies next-pos-a next-pos-b)))))

(defn part1
  [offset]
  ((gen-recipies [3 7 1 0] 10) offset))

(defn part2
  [data]
  (gen-recipies2 data))

(comment
  (part2 {:r [3 7 1 0] :a 0 :b 1 :v [2 0 9 2 3 1]})
  (part1 input)
  ((gen-recipies [3 7 1 0] 5) input-test)
  (nthrest [0 1 2 3 4 5] 3)
  (apply conj [0 1] [2 3])
  (quot 12 10)
  (mod 12 10))
