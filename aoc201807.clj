(ns first-clojure.aoc201807
  (:require [first-clojure.utils :as u]
            [clojure.string :as str]))

(def input
  (u/read-input "src/first_clojure/input/aoc201807"))

(def input-test
  (u/read-input "src/first_clojure/input/aoc201807test"))

(defn parse-all-steps
  ;; => {"M" "D" ... }
  [lines]
  (reduce (fn [acc line]
            (let [[_ prev _ _ _ _ _ next _ _] (str/split line #" ")]
              (conj acc prev next))) #{} lines))

(defn parse-backward
  ;; Step M must be finished before step D can begin.
  ;; => {"D" ["M" ...], ... }
  [lines]
  (reduce (fn [acc line]
            (let [[_ prev _ _ _ _ _ next _ _] (str/split line #" ")
                  prevs (acc next)]
              (if prevs
                (update-in acc [next] conj prev)
                (assoc acc next [prev])))) {} lines))

(defn parse-steps
  ;; 선행 step이 필요없는 step들도 map data에 추가한다
  [lines]
  (let [backward-steps (parse-backward lines)
        all-steps (parse-all-steps lines)]
    (reduce (fn [acc step]
              (if (acc step)
                acc
                (assoc acc step []))) backward-steps all-steps)))

(defn remove-steps
  ;; 선행되어야하는 step들이 담긴 vector에 이미 orders에 포함된 step은 제거한다.
  [orders steps]
  (into {} (map (fn [[k v]]
                  [k (vec (remove (set orders) v))]) steps)))

(defn find-next-order
  ;; 선행되어야하는 step이 없는 step을 찾아 알파벳 순으로 제일 빠른 step을 찾는다.
  [steps]
  (->> steps
       (filter (fn [[k v]] (= (count v) 0)))
       (map (fn [[k v]] k))
       sort
       first))

(defn make-order
  [orders steps]
  (let [removed-by-key (apply dissoc steps orders)
        removed-by-value (remove-steps orders removed-by-key)]
    (if (= (count removed-by-value) 0)
      orders
      (recur (conj orders (find-next-order removed-by-value)) removed-by-value))))

(defn part1
  [steps]
  (->> steps
       parse-steps
       (make-order [])
       (apply str)))

(defn make-queue
  ;; "A" => ["A" "A" "A" ... ] => count 61개
  ;; replicate는 deprecated => take와 repeat의 조합으로 사용할 수 있음
  ;; repeatedly라는 함수도 있음
  [a]
  (vec (repeat (- (->> a
                       (map int)
                       first) 4) a)))

(defn process-workers
  [workers]
  (reduce (fn [acc [w queue]]
            (cond
              (> (count queue) 1) (update-in acc [:has-many] conj [w queue])
              (= (count queue) 1) (update-in acc [:has-one] conj [w queue])
              (= (count queue) 0) (update-in acc [:idle] conj [w queue])))
          {:idle [] :has-one [] :has-many []}
          workers))

(comment (process-busy [[:w1 ["E" "E"]] [:w2 ["F" "F" "F"]]]))
(defn process-busy
  [workers]
  (into {} (map (fn [[w queue]]
                  [w (pop queue)]) workers)))

(comment (process-hanging ["E"] [[:w1 ["A"]] [:w2 ["B"]]] {"C" ["A" "B"] "D" ["F"]}))
(defn process-hanging
  [done workers steps]
  (reduce (fn [[done-cur workers-cur steps-cur] [w queue]]
            ;; 일이 끝나면 done에 추가하고, steps에서 key도 지우고, value들 중에서도 제거한다.
            (let [next-done (conj done-cur (peek queue))
                  removed-by-key (apply dissoc steps-cur next-done)
                  removed-by-value (remove-steps next-done removed-by-key)]
              [next-done (conj workers-cur [w []]) removed-by-value])) [done [] steps] workers))

(comment (process-idle ["E"] [[:w1 []] [:w2 []]] {"A" ["C"], "F" ["C"], "B" ["A"], "D" ["A"], "E" ["B" "D" "F"], "C" []}))
(defn process-idle
  [done workers steps]
  (let [[_ refreshed-workers refreshed-steps] (reduce (fn [[done-cur workers-cur steps-cur] [w queue]]
                                                        (let [next-step (find-next-order steps-cur)
                                                              ;; 처리 시작된 일을 steps에서 key만 찾아 지워서 다른 worker가 중복해서 받지 않게 한다
                                                              ;; value들에서 지우지 않는다.
                                                              removed-by-key (if (nil? next-step)
                                                                               steps-cur
                                                                               (apply dissoc steps-cur (conj done next-step)))]
                                                          (if (nil? next-step)
                                                            [done-cur (conj workers-cur [w []]) steps-cur]
                                                            [done-cur (conj workers-cur [w (make-queue next-step)]) removed-by-key])))
                                                      [done [] steps] workers)]
    [refreshed-workers refreshed-steps]))

(defn handle-queue
  [done workers steps]
  (let [w (process-workers workers)
        busy-workers (w :has-many)
        hanging-workers (w :has-one)
        idle-workers (w :idle)
        busy-w-next (process-busy busy-workers)
        [done-hanging released-workers steps-hanging] (process-hanging done hanging-workers steps)
        [refreshed-workers refreshed-steps] (process-idle done-hanging idle-workers steps-hanging)]
    [done-hanging (merge (into {} busy-w-next) (into {} released-workers) (into {} refreshed-workers)) refreshed-steps]))

(defn counter
  [sec done workers steps]
  (let [sec-cur (inc sec)
        [done-next workers-next steps-next] (handle-queue done workers steps)
        all-idle? (= (count (filter (fn [[k v]]
                                      (not= (count v) 0)) workers-next)) 0)]
    (if (and (= 0 (count steps-next)) all-idle?)
      sec-cur
      (recur sec-cur done-next workers-next steps-next))))

(defn part2
  ;; 맨 처음 queue가 다 빈 체로 시작하는 1초를 지운다.
  [steps]
  (- (->> steps
          parse-steps
          (counter 0 [] {:w1 [] :w2 [] :w3 [] :w4 [] :w5 []})) 1))

(comment
  (first (map int "A"))
  (remove #{1 2} [1 2 3 4])
  (seq? [1 2 4])
  (sequential? [1 2 4])
  (map vector [1 2 4] [2 6 7])
  (-> input
      parse-backward)
  (remove-steps ["E"] {"A" ["E" "T"]})
  (apply dissoc {1 2 3 4 5 6} (keys (filter (fn [[k v]]
                                              (#{2} v)) {1 2 3 4 5 6})))
  (part1 input) ;; => "EFHLMTKQBWAPGIVXSZJRDUYONC"
  (part2 input) ;; => 1056
  )
