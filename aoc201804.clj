(ns first-clojure.aoc201804
  (:require [first-clojure.utils :as u]
            [clojure.string :as str]))

(def input
  (u/read-input "src/first_clojure/input/aoc201804"))

(defn sort-logs
  ;; guard post log를 시간 순으로 정렬하는 함수
  ;; sort-by는 sort와는 달리 keyfn을 사용할 수 있다.
  [logs]
  (sort-by #(let [[date time a b c d] (str/split % #" ")]
              (str date time)) logs))

(defn parser
  ;; 시간순으로 정렬된 log를 parsing
  ;; 
  ;; 반환하는 data shape
  ;; { 2557 { :asleep [ "00:07" "00:41" ], :up [ "00:28" "00:51" ] }, ... }
  [logs]
  (reduce (fn [acc log]
            (let [[dirty-date dirty-time a b c d] (str/split log #" ")
                  date (subs dirty-date 1)
                  time (subs dirty-time 0 (- (count dirty-time) 1))
                  [hour minute] (map #(Integer/parseInt %) (str/split time #":"))]
              (case a
                "Guard" (let [id (Integer/parseInt (subs b 1))]
                          (if (contains? acc id)
                            (assoc-in acc [:cursor] id)
                            (assoc acc id {:asleep [] :up []} :cursor id)))
                "falls" (let [id (:cursor acc)]
                          (update-in acc [id :asleep] conj time))
                "wakes" (let [id (:cursor acc)]
                          (update-in acc [id :up] conj time))))) {} logs))

(defn compute-time
  ;; a "23:57" b "00:23" => 26
  [a b]
  (let [start (map #(Integer/parseInt %) (str/split a #":"))
        end (map #(Integer/parseInt %) (str/split b #":"))
        [diff-h diff-m] (map - end start)]
    (cond
      (< diff-h 0) (+ 60 diff-m)
      (>= diff-h 0) (+ (* diff-h 60) diff-m))))

(defn mark-asleep-time
  ;; a "23:57" b "00:01" => {57 1, 58 1, 59 1, 0 1, 1 1}
  ;; a "00:12" b "00:15" => {12 1, 13 1, 14 1, 15 1}
  [a b]
  (let [[start-h start-m] (map #(Integer/parseInt %) (str/split a #":"))
        [end-h end-m] (map #(Integer/parseInt %) (str/split b #":"))
        [diff-h diff-m] (map - [end-h end-m] [start-h start-m])]
    (cond
      (< diff-h 0) (reduce (fn [acc item]
                             (if (contains? acc item)
                               (update-in acc [item] inc)
                               (assoc acc item 1))) {} (into (range start-m 60) (range 0 end-m)))
      (>= diff-h 0) (reduce (fn [acc item]
                              (if (contains? acc item)
                                (update-in acc [item] inc)
                                (assoc acc item 1))) {} (range start-m end-m)))))

(defn compute-asleep
  ;; { 2557 { :asleep [ "00:07" "00:41" ], :up [ "00:28" "00:51" ] }, ... }
  ;; =>
  ;; [[2557 31] ... ]
  [logs]
  (map (fn [log]
         (let [[id post] log
               asleep (:asleep post)
               up (:up post)
               max-of-asleep (reduce max 0 (map compute-time asleep up))]
           [id max-of-asleep])) logs))

(defn aggregate-asleep
  ;; { :asleep [ "00:07" "00:41" ], :up [ "00:28" "00:51" ] }
  ;; => {0 2, 7 6, 20 9, 27 12, 1 3, ... }
  [log]
  (let [asleep (:asleep log)
        up (:up log)]
    (reduce (fn [acc map-of-asleep]
              (merge-with + acc map-of-asleep)) {} (map mark-asleep-time asleep up))))

(defn sleeper
  ;; max asleep 구하기
  [logs]
  (reduce (fn [acc guard]
            (let [[id total-asleep] guard
                  [_ max-asleep] acc]
              (cond
                (> total-asleep max-asleep) guard
                :else acc))) [0 0] logs))

(def most-sleeper
  (->> input
       sort-logs
       parser
       compute-asleep
       sleeper
       first))

(defn max-dups-min-by
  [logs id]
  (reduce (fn [acc item]
            (let [[max-min max-freq] acc
                  [min freq] item]
              (cond
                (> max-freq freq) acc
                :else [min freq]))) [0 0] (aggregate-asleep (get (parser (sort-logs input)) id))))

(def dups-min-of-most-sleeper
  (first (max-dups-min-by input most-sleeper)))

(def part1
  (* most-sleeper dups-min-of-most-sleeper))

(defn asleep-mins-by-guard
  [logs]
  (map (fn [log]
         (let [[id post] log]
           [id (aggregate-asleep post)])) (parser (sort-logs logs))))

(defn max-dups-min
  [log]
  (reduce (fn [acc item]
            (let [[max-min max-freq] acc
                  [min freq] item]
              (cond
                (> max-freq freq) acc
                :else [min freq]))) [0 0] log))

(defn most-asleep-min-by
  [logs]
  (map (fn [log]
         (let [[id mins] log]
           [id (max-dups-min mins)])) logs))

(defn best-freq-sleeper
  [logs]
  (reduce (fn [acc guard]
            (let [[max-id max-post] acc
                  [max-min max-freq] max-post
                  [id post] guard
                  [min freq] post]
              (if (> freq max-freq)
                guard
                acc))) [0 [0 0]] logs))

(defn answer2
  [x]
  (let [[id [min freq]] x]
    (* id min)))

(def part2
  (-> input
      asleep-mins-by-guard
      most-asleep-min-by
      best-freq-sleeper
      answer2))

(comment
  "[1518-11-02 23:56] Guard #3463 begins shift"
  "[1518-04-22 00:32] falls asleep"
  "[1518-09-18 00:19] wakes up"
  (let [[date time a b c d] (str/split "[1518-11-02 23:56] Guard #3463 begins shift" #" ")]
    [date time a b c d])
  (sort-logs input)
  (parser (sort-logs input))
  (compute-time "23:57" "00:23")
  (mark-asleep-time "23:57" "00:2")
  (compute-asleep (take 3 (parser (sort-logs input))))
  (sleeper (compute-asleep (take 3 (parser (sort-logs input)))))
  (into (range 57 59) (range 0 2))
  (best-freq-sleeper (most-asleep-min-by (asleep-mins-by-guard input)))

  ;; 대박!
  (map - [0 33] [0 23])
  ;; 왜 #()으로는 안될까?
  (map (fn [i]
         (let [[k v] i]
           v)) {1 2 2 3}))
