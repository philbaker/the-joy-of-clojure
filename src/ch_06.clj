(ns ch-06)

; Structural sharing: a persistent toy
(def baselist (list :barnabas :adam))
(def lst1 (cons :willie baselist))
(def lst2 (cons :phoenix baselist))

lst1 ; (:willie :barnabas :adam)
lst2 ; (:phoenix :barnabas :adam)
; baselist is like a historical version of both lst1 and lst2
; more than being equal, the next parts of both lists are identical -
; the same instance
(= (next lst1) (next lst2)) ; true
(identical? (next lst1) (next lst2)) ; true

(defn xconj [t v]
  (cond
    (nil? t) {:val v, :L nil, :R nil}))

(xconj nil 5) ; {:val 5, :L nil, :R nil}

(defn xconj2 [t v]
  (cond
    (nil? t) {:val v, :L nil, :R nil}
    (< v (:val t)) {:val (:val t),
                    :L (xconj2 (:L t) v),
                    :R (:R t)}))

(def tree1 (xconj2 nil 5))
tree1 ; {:val 5, :L nil, :R nil}

(def tree1 (xconj2 tree1 3))
tree1 ; {:val 5, :L {:val 3, :L nil, :R nil}, :R nil}

(def tree1 (xconj2 tree1 2))
tree1
; {:val 5, :L {:val 3, :L {:val 2, :L nil, :R nil}, :R nil}, :R nil}

(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

(xseq tree1) ; (2 3 5)

(defn xconj3 [t v]
  (cond
    (nil? t)       {:val v, :L nil, :R nil} ; nil nodes start with v
    (< v (:val t)) {:val (:val t), ; when v is less than the value at
                                   ; the current node, it's pushed left
                    :L (xconj3 (:L t) v),
                    :R (:R t)}
    :else          {:val (:val t), ; otherwise it's pushed right
                    :L (:L t),
                    :R (xconj3 (:R t) v)}))

(def tree2 (xconj3 tree1 7))
(xseq tree2) ; (2 3 5 7)

(identical? (:L tree1) (:L tree2)) ; true

; Laziness
; Short-circuiting is an example of laziness
; Note: these examples are not idiomatic Clojure code
(defn if-chain [x y z]
  (if x
    (if y
      (if z
        (do
          (println "Made it!")
          :all-truthy)))))
; The call to println is evaluated only in the case of thee truthy arguments

(if-chain () 42 true)
; (out) Made it!
; :all-truthy

(if-chain true true false) ; nil

; You can perform the equivalent action given only the `and` macro 
(defn and-chain [x y z]
  (and x y z (do (println "Made it!") :all-truthy)))

(and-chain () 42 true)
; (out) Made it!
:all-truthy

; Understanding the lazy-seq recipe
(defn rec-step [[x & xs]]
  (if x
    [x (rec-step xs)]
    []))

(rec-step [1 2 3 4]) ; [1 [2 [3 [4 []]]]]

(rec-step (range 200000))
; (err) Execution error (StackOverflowError) at ch-06/rec-step (user.clj:88).
; (err) null

; The lazy-seq recipe
; 1. Use the `lazy-seq` macro at the outermost level of your lazy sequence
; producting expression(s)
; 2. If you happen to be consuming another sequence during your operations,
; then use `rest` instead of `next`
; 3. Prefer higher-order functions when processing sequences
; 4. Don't hold on to your head

; using lazy-seq to avoid stack overflow
(defn lz-rec-step [s]
  (lazy-seq
    (if (seq s)
      [(first s) (lz-rec-step (rest s))]
      [])))

(lz-rec-step [1 2 3 4]) ; (1 (2 (3 (4 ()))))

(class (lz-rec-step [1 2 3 4])) ; clojure.lang.LazySeq

(dorun (lz-rec-step (range 200000))) 
; nil
; no longer produces a stack overflow

(defn simple-range [i limit]
  (lazy-seq
    (when (< i limit)
      (cons i (simple-range (inc i) limit)))))

(simple-range 0 9) ; (0 1 2 3 4 5 6 7 8)

; Losing your head
; The primary advantage of laziness in Clojure is that it prevents the
; full realization of interim results during a calculation
; If you hold onto the head of a sequence somewhere in a function, the 
; sequence will not be garbage collected
(let [r (range 1e9)]
  (first r)
  (last r)) ; 999999999

(let [r (range 1e9)]
  (last r)
  (first r))
; (err) Execution error (OutOfMemoryError) at ch-06/eval3672 (user.clj:139).
; (err) Java heap space
;
; The first example eventually evaluates because Clojure can deduce that r
; is no longer needed. In the second case Clojure can't rearrange the
; the operations because it has no way to guarantee that order is unimportant

; Infinite sequences
(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(triangle 10) ; 55

; This example works but it only does what it does - it's not flexible
(map triangle (range 1 11)) ; (1 3 6 10 15 21 28 36 45 55)

(def tri-nums (map triangle (iterate inc 1)))

(take 10 tri-nums) ; (1 3 6 10 15 21 28 36 45 55)
(take 10 (filter even? tri-nums)) ; (6 10 28 36 66 78 120 136 190 210)
(nth tri-nums 99) ; 5050
(double (reduce + (take 1000 (map / tri-nums)))) ; 1.998001998001998
(take 2 (drop-while #(< % 10000) tri-nums)) ; (10011 10153)

(defn defer-expensive [cheap expensive]
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))

(defer-expensive 
  (delay :cheap)
  (delay (do (Thread/sleep 5000) :expensive))) ; :cheap

(defer-expensive 
  (delay false)
  (delay (do (Thread/sleep 5000) :expensive))) ; :expensive

; if-let and when-let
; useful when you'd like to bind the results of an expression based on
; whether it returns a truthy value
(if :truthy-thing
  (let [res :truthy-thing] (println res)))
; (out) :truthy-thing
; nil

(if-let [res :truthy-thing] (println res) nil)
; (out) :truthy-thing
; nil

(defn inf-triangles [n]
  {:head (triangle n)
   :tail (delay (inf-triangles (inc n)))})

(defn head [l] (:head l))
(defn tail [l] (force (:tail l)))

(def tri-nums-2 (inf-triangles 1))
(head tri-nums-2) ; 1
(head (tail tri-nums-2)) ; 3
(head (tail (tail tri-nums-2))) ; 6

(defn taker [n l]
  (loop [t n, src l, ret []]
    (if (zero? t)
      ret
      (recur (dec t) (tail src) (conj ret (head src))))))

(defn nthr [l n]
  (if (zero? n)
    (head l)
    (recur (tail l) (dec n))))

(taker 10 tri-nums-2) ; [1 3 6 10 15 21 28 36 45 55]

(nthr tri-nums-2 99) ; 5050

; Quicksort implementation
(defn rand-ints [n]
  (take n (repeatedly #(rand-int n))))

(rand-ints 10) ; (7 6 0 5 8 8 1 1 3 6)

(defn sort-parts [work]
  (lazy-seq
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)] ; pull apart work
              (let [smaller? #(< % pivot)] ; define pivot comparison fn
                (recur (list*
                         (filter smaller? xs) ; work all < pivot
                         pivot ; work the pivot itself
                         (remove smaller? xs) ; work all > pivot
                         parts))) ; concat parts
              (when-let [[x & parts] parts]
                (cons x (sort-parts parts))))))) ; sort the rest if more parts

(defn qsort [xs]
  (sort-parts (list xs))) ; #'ch-06/qsort

(qsort [2 1 4 3]) ; (1 2 3 4)
(qsort (rand-ints 20)) ; (0 1 1 2 2 3 4 4 5 6 7 8 8 9 9 9 10 12 14 15)
(first (qsort (rand-ints 100))) ; 1
