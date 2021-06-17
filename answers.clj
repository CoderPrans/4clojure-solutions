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

;;Challenge: factorial fun ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn factorial [n]
   (if (= n 1)
     n
     (* n (factorial (dec n)))))
 8)
;; => 40320

;;Challenge: reverse interleave ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s n]
   (let [gm (group-by #(mod % n) s)]
     (map #(apply list (get gm %)) (keys gm))))
 (range 10) 5)
;; => ((0 5) (1 6) (2 7) (3 8) (4 9))

;;Challenge: rotate sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [n s]
   (let [i (if (< (count s) (Math/abs n))
             (- (Math/abs n) (count s))
             (Math/abs n))]
     (concat
      (filter #(> (.indexOf s %)
                  (if (> n 0) (dec i)
                      (- (count s) (inc i)))) s)
      (filter #(< (.indexOf s %)
                  (if (> n 0) i
                      (- (count s) i))) s))))
 -4 '(:a :b :c))
;; => (:c :a :b)

;;Challenge: Flipping out ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(((fn [f]
    (fn [& args] (apply f (reverse args))))
  quot) 2 8)
;; => 4

;;Challenge: Split by Type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set ((fn [s]
         (vals (group-by type s)))
      [1 :a 2 :b 3 :c]))
;; => #{[:a :b :c] [1 2 3]}

;;Challenge: Split a sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [n s]
   (partition-by #(>= (.indexOf s %) n) s))
 1 [:a :b :c :d])
;; => ((:a) (:b :c :d))

;;Challenge: Advanced Destructuring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let
    [[a b & c :as d] [1 2 3 4 5]]
  [a b c d])
;; => [1 2 (3 4 5) [1 2 3 4 5]]

;; Challenge 53: Longest Increasing Sub-seq ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; method 1 (loop recur)
((fn [s]
   (loop [temp_coll []
          sub_colls []
          remaining s]

     (if (empty? remaining)

       (cond
         (> (count temp_coll) (count sub_colls)) temp_coll
         (< (count temp_coll) (count sub_colls)) sub_colls
         (= 1 (count temp_coll) (count sub_colls)) [])

       (recur

        (cond
          (= temp_coll []) [(first remaining)]
          (= (inc (last temp_coll)) (first remaining)) (conj temp_coll (first remaining))
          (not= (inc (last temp_coll)) (first remaining)) [(first remaining)])

        (if (> (count temp_coll) (count sub_colls))
          temp_coll
          sub_colls)

        (rest remaining)
        ))))
 [2 3 3 4 5])
;; => [3 4 5]

; method 2 (modify to suitable form filter, sort, concat)
((fn [sequence]
   (let [sorted (->> sequence
                     (partition 2 1)
                     (partition-by #(apply < %))
                     (filter (fn [[[n1 n2]]] (< n1 n2)))
                     (sort-by count >)
                     first)]
     (vec (concat (first sorted) (map last (rest sorted))))))
 [2 3 3 4 5])
;; => [3 4 5]

;; Challenge 54: Partition a Sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [n s]
   (->> (rest s)
        (reduce
         #(if (< (count (first %1)) n)
            (cons (cons %2 (first %1)) (rest %1))
            (cons (list %2) %1))
         (->> (first s) list list))
        reverse
        (map #(reverse %))
        (filter #(= (count %) n))))
 3 (range 8))
;; => ((0 1 2) (3 4 5))

;; Challenge 55: Count Occurences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s]
   (->> (sort s)
        (partition-by identity)
        (map (fn [v] [(first v) (count v)]))
        (mapcat (fn [s] s))
        (apply hash-map)))
 '([1 2] [1 3] [1 3]))
;; => {[1 3] 2, [1 2] 1}

;; Challenge 56: Find Distinct Items ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#(keys (frequencies %)) [1 2 1 3 1 2 4])
;; => (1 2 3 4)

((fn [s]
   (if (empty?
        (filter #(> (count %) 1)
                (partition-by identity (sort s))))
     s (keys (frequencies s))))
 '([2 4] [1 2] [1 3] [1 3]))
;; => ([2 4] [1 2] [1 3])

