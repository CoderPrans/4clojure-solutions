;;; Challenge 27: Palindrome Detector ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [coll]
   (= (seq coll) (reverse coll)))
 "racecar")
;; => true

;;; Challenge 28: Flatten a Sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s]
   (reverse
    (reduce (fn extract [col elm]
              (if (coll? elm)
                (reduce extract col elm)
                (conj col elm))) '() s)))
 '((1 2) 3 [4 [5 6]]))
;; => (1 2 3 4 5 6)

;;; Challenge 29: Get the Caps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s]
   (clojure.string/join
    (filter #(re-matches #"[A-Z]" (str %))
            (seq s))))
 "HeLlO, WoRlD!")
;; => "HLOWRD"

;;; Challenge 30: Compress a Seq ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Challenge 31: Pack a Sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s]
   (partition-by identity s))
 [1 1 2 1 1 1 3 3])
;; => ((1 1) (2) (1 1 1) (3 3))

;;; Challenge 32: Duplicate a Sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [x]
  (reduce
   (fn [col elm]
     (concat col (list elm elm)))
   '() x))
 '([1 2] 3))
;; => ([1 2] [1 2] 3 3)

;;; Challenge 33: Replicat/e a Sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [x c]
  (reduce
   (fn [col elm]
     (concat col (replicate c elm)))
   '() x))
 [1 2 3] 4)
;; => (1 1 1 1 2 2 2 2 3 3 3 3)

;;; Challenge 34: Implement Range ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [x y]
  (loop [c x
        l []]
    (if (and (>= c x) (< c y))
      (recur (inc c) (concat l (list c)))
      l)))
 5 8)
;; => (5 6 7)

;;; Challenge 38: Maximum Value ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [& args]
   (last (sort args)))
 45 67 11)
;; => 67

;;; Challenge 39: Interleave Two Seqs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Challenge 40: Interpose a Seq ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [d s]
   (reverse
    (rest
     (reverse
      (flatten (for [x s]
                 [x d]))))))
 0 [1 2 3])
;; => (1 0 2 0 3)

;; Challenge 41: Drop Every nth Item ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Challenge 42: Factorial Fun ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn factorial [n]
   (if (= n 1)
     n
     (* n (factorial (dec n)))))
 8)
;; => 40320

;;; Challenge 43: Reverse Interleave ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s n]
   (let [gm (group-by #(mod % n) s)]
     (map #(apply list (get gm %)) (keys gm))))
 (range 10) 5)
;; => ((0 5) (1 6) (2 7) (3 8) (4 9))

;;; Challenge 44: Rotate Sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Challenge 46: Flipping Out ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(((fn [f]
    (fn [& args] (apply f (reverse args))))
  quot) 2 8)
;; => 4

;;; Challenge 49: Split a sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [n s]
   (partition-by #(>= (.indexOf s %) n) s))
 1 [:a :b :c :d])
;; => ((:a) (:b :c :d))

;;; Challenge 50: Split By Type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set ((fn [s]
         (vals (group-by type s)))
      [1 :a 2 :b 3 :c]))
;; => #{[:a :b :c] [1 2 3]}

;;; Challenge 51: Advanced Destructuring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let
    [[a b & c :as d] [1 2 3 4 5]]
  [a b c d])
;; => [1 2 (3 4 5) [1 2 3 4 5]]

;;; Challenge 53: Longest Increasing Sub-seq ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; method 2 (modify to suitable form, filter, sort, concat)
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

;;; Challenge 54: Partition a Sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Challenge 55: Count Occurences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; Challenge 58: Function Composition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(((defn -comp [& funcs]
    (fn [& v]
      (reduce (fn [acc cur]
                (if (= v acc) (apply cur acc) (cur acc)))
              v (reverse funcs))))
  rest reverse) 
 [1 2 3 4])
;; => (3 2 1)

((-comp zero? #(mod % 8) +) 3 5 7 9)
;; => true

((-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world")
;; => "HELLO"

;; Challenge 59: Juxtaposition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(((defn -juxt [& funcs]
    (fn [& v]
      (reduce (fn [acc cur]
                (conj acc (apply cur v)))
              [] funcs)))
  + max min)
 2 3 5 1 6 4)
;; => [21 6 1]

((-juxt #(.toUpperCase %) count) "hello")
;; => ["HELLO" 5]

((-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})
;; => [2 6 4]

;; Challenge 60: Sequence Reduction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(take 5
      ((fn -reduction
         ([f s] (-reduction f (first s) (next s)))
         ([f d s]
          (cons d
                (lazy-seq 
                 (when (seq s) (-reduction f (f d (first s)) (next s)))))
          ))
       + (range)))
;; => (0 1 3 6 10)

                                        ; TODO find how to lazy-seq this.
                                        ; original solution. only solves 2 and 3.
#_(let [m (atom [d])]
    (reduce (fn [a c] (do (swap! m conj (f a c)) (f a c))) d s)
    @m)

;; Challenge 61: Map Construction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [c1 c2]
   (loop [i 0
          m []]
     (if (= i (min (count c1) (count c2)))
       (apply hash-map m)
       (recur (inc i) (conj m (c1 i) (c2 i))))))
 [:a :b :c] [1 2 3])
;; => {:c 3, :b 2, :a 1}

; NOTE: map can take multiple collections.

((fn [c1 c2]
   (apply merge (map (fn [k v] {k v}) c1 c2)))
 [:a :b :c] [1 2 3])
;; => {:a 1, :b 2, :c 3}

;; Challenge 62: Re-implement iterate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(take 5 ((fn -iterate [f x]
           (cons x (lazy-seq
                    (-iterate f (f x)))))
         #(* 2 %) 1))
;; => (1 2 4 8 16)

;; Challenge 63: Group A Sequnce ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn -group-by [f s]
   (reduce
    (fn [acc cur]
      (assoc acc (cur 0)
             (if (acc (cur 0))
               (conj (acc (cur 0)) (cur 1))
               [(cur 1)])))
    {}
    (mapcat (fn [e] [[(f e) e]]) s)))
 #(> % 5) [1 3 6 8])
;; => {false [1 3], true [6 8]}

;; Challenge 65: Black Box Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [s]
   (cond
     (= (conj s {}) s) :map
     (empty? s) (cond
                  (= (clojure.set/union s #{}) #{}) :set
                  (= (conj (conj s 0) 1) [0 1]) :vector
                  (= (conj (conj s 0) 1) [1 0]) :list)
     (= (clojure.set/union s s) s) :set
     (= (last (conj s s)) s) :vector
     (= (first (conj s s)) s) :list
     ))
 #{10 (rand-int 5)})

;; Challenge 66: Greatest Common Divisor ;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

((fn [a b]
   (if (zero? b)
     a
     (recur b (mod a b))))
 1023 858)
;; => 33

;; Challenge 67: Prime Numbers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [n]
   (take n (lazy-seq
            (filter (fn [j]
                      (= (count (filter (fn [i] (= 0 (mod j i))) (rest (range j))))
                         1))
                    (rest (rest (range))))))
   ) 5)
;; => (2 3 5 7 11)

((fn [n]
   (take n (lazy-seq
            (->> (rest (rest (range)))
                 (filter
                  (fn [j]
                    (->> (rest (range j))
                         (filter #(= 0 (mod j %)))
                         count
                         (= 1)))))
            )))
 5)
;; => (2 3 5 7 11)

;; Challenge 69: Merge with a Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [f & maps]
   (reduce
    (fn [acc cur]
      (if (empty? acc)
        (conj acc cur)
        (->> (set (apply concat (map keys [acc cur])))
             (map #(cond
                     (and (acc %)
                          (cur %)) {% (f (acc %) (cur %))}
                     (acc %) {% (acc %)}
                     (cur %) {% (cur %)}))
             (apply merge))))
    {} maps))
 concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
;; => {:c [8 9], :b (6 7), :a (3 4 5)}

;; Challenge 70: Word Sorting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#(sort
   (clojure.string/split % #"\s|[.!,]"))
 "Have a nice day!")
;; => ("Have" "a" "day" "nice")

((fn [s]
   (let [caps (re-seq #"[A-Z][a-z]+" s)
         cap-map (->> caps
                      (map #(hash-map (.toLowerCase %) %))
                      (apply merge))]
     (map #(if (contains? cap-map %) (cap-map %) %)
          (-> (.toLowerCase s)
              (clojure.string/split #"\s|[.!,]")
              sort))))
 "Clojure is a fun language!")
;; => ("a" "Clojure" "fun" "is" "language")

;; Challenge 73: Analayze a Tic-Tac-Toe Board ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [l]
   (let [ac-r #(vector ((l 0) %1)
                    ((l 1) %2)
                    ((l 2) %3))
         hori #(when (apply = (l %))
                 ((l %) 0))
         vert #(when (apply = (ac-r % % %))
                 ((ac-r % % %) 0))
         diag #(let [s (if (= % \r)
                         (ac-r 0 1 2)
                         (ac-r 2 1 0))]
                 (when (apply = s)
                   (s 0)))
         res (or (hori 0) (hori 1) (hori 2)
                 (vert 0) (vert 1) (vert 2)
                 (diag \r) (diag \l))]
     (when-not (= res :e) res)))

 [[:x :e :e]
  [:o :x :e]
  [:o :e :x]])
;; => :x

;; Challenge 74: Filter Perfect Squares ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((fn [num]
   (->> (clojure.string/split num #",")
        (map #(Long/parseLong %))
        (filter
         (fn [n]
           (when ((->> (range n)
                       (map #(* % %))
                       set) n) n)))
        (clojure.string/join #",")))
 "15,16,25,36,37")
;; => "16,25,36"

