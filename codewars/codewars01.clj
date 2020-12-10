(ns first-clojure.codewars01)

(defn remover [col pred]
  (remove #(= %1 pred) col))

(defn array-diff [c1 c2]
  (reduce (fn [acc pred]
            (remover acc pred)) c1 c2))

;; Best answer
;; (defn array-diff [a b]
;;   (remove (set b) a))

(comment
  (remover [1 2 2 2 3] 2)
  (array-diff [1 2 2 2 3] [2 3])
  (remove #{1 2} [2 3 4])
  (remove (set [1 2]) [1 2 3 2])
  )
