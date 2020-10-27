(ns first-clojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn greet [name]
  (println (str "Hello " name)))

(+ 1 1)

(def foo (for [x [:a :b], y (range 5) :when (odd? y)]
  [x y]))

(comment (def make-set
   (fn [x y]
     (println "making a set")
     #{x y})))

(defn make-set
  ([x] #{x})
  ([x y] #{x y}))

(let [r 5
      pi 3.1415
      r-squared (* r r)]
  (println "radius is" r)
  (* pi r-squared))

(defn print-down-from [x]
  (when (pos? x)
    (println x)
    (recur (dec x))))

(defn sum-down-from [initial-x]
  (loop [sum 0, x initial-x]
    (if (pos? x)
         (recur (+ sum x) (dec x))
         sum)))
