;;Challenge: palindrome detector ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [coll]
   (= (seq coll) (reverse coll)))
 "racecar")
;; => true

;;Challenge: flatten a seq ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s]
   (reverse
    (reduce (fn extract [col elm]
              (if (coll? elm)
                (reduce extract col elm)
                (conj col elm))) '() s)))
 '((1 2) 3 [4 [5 6]]))
;; => (1 2 3 4 5 6)

;;Challenge: get the caps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s]
   (clojure.string/join
    (filter #(re-matches #"[A-Z]" (str %))
            (seq s))))
 "HeLlO, WoRlD!")
;; => "HLOWRD"

;;Challenge: compress a seq ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s]
  (loop [x 0
         y []]
    (if (< x (dec (count s)))
      (if (= (nth s x) (nth s (+ x 1)))
        (recur (inc x) y)
        (recur (inc x) (conj y (nth s x))))
      (conj y (nth s (dec (count s)))))))
 [1 1 2 3 3 2 2 3])
;; => [1 2 3 2 3]

;;Challenge: pack a sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s]
   (partition-by identity s))
 [1 1 2 1 1 1 3 3])
;; => ((1 1) (2) (1 1 1) (3 3))

;;Challenge: duplicate a sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [x]
  (reduce
   (fn [col elm]
     (concat col (list elm elm)))
   '() x))
 '([1 2] 3))
;; => ([1 2] [1 2] 3 3)

;;Challenge: replicat\e a sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [x c]
  (reduce
   (fn [col elm]
     (concat col (replicate c elm)))
   '() x))
 [1 2 3] 4)
;; => (1 1 1 1 2 2 2 2 3 3 3 3)

;;Challenge: implement range ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [x y]
  (loop [c x
        l []]
    (if (and (>= c x) (< c y))
      (recur (inc c) (concat l (list c)))
      l)))
 5 8)
;; => (5 6 7)

;;Challenge: maximum value ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [& args]
   (last (sort args)))
 45 67 11)
;; => 67

;;Challenge: interleave two seqs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s1 s2]
   (loop [c 0
          x []]
     (if (< c (min (count s1) (count s2)))
       (recur (inc c)
              (concat x (list
                         (nth s1 c)
                         (nth s2 c))))
       x)))
 [1 2 3] [:a :b :c])
;; => (1 :a 2 :b 3 :c)

;;Challenge: interpose a seq ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [d s]
   (reverse
    (rest
     (reverse
      (flatten (for [x s]
                 [x d]))))))
 0 [1 2 3])
;; => (1 0 2 0 3)

;;Challenge: drop every nth item ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s n]
   (for [x s
         :when (not
                (zero?
                 (mod
                  (inc
                   (.indexOf s x)) n)))]
     x))
 [:a :b :c :d :e :f] 2)
;; => (:a :c :e)
