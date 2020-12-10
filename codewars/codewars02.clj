(ns first-clojure.codewars02)

(defn desc-order [n]
  (->> n
       str
       sort
       reverse
       (apply str)
       Integer/parseInt))

;; apply를 생각해내서 잘 쓰자.

(comment
  (str 1234)
  (reverse (sort "1234"))
  (clojure.string/join '(5 1))
  (desc-order 15)
  )
