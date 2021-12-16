(ns ch-15-performance
  (:require [clojure.core.reducers :as r]
            [criterium.core :as crit]))

(defn asum-sq [xs]
  (let [dbl (amap xs i ret
                  (* (aget xs i)
                     (aget xs i)))]
    (areduce dbl i ret 0
             (+ ret (aget dbl i)))))

;; (time (dotimes [_ 10000] (asum-sq (float-array [1 2 3 4 5]))))


(defn ^Float asum-sq [^floats xs]
  (let [^floats dbl (amap xs i ret
                  (* (aget xs i)
                     (aget xs i)))]
    (areduce dbl i ret 0
             (+ ret (aget dbl i)))))

(time (dotimes [_ 10000] (asum-sq (float-array [1 2 3 4 5]))))
;; "Elapsed time: 5.827774 msecs"
;; => nil

;; (.intValue (asum-sq (float-array [1 2 3 4 5])))

;; Transients
(defn zencat1 [x y]
  (loop [src y, ret y]
    (if (seq src)
      (recur (next src) (conj ret (first src)))
      ret)))

(time (dotimes [_ 1000000] (zencat1 [1 2 3] [4 5 6])))
;; "Elapsed time: 631.99679 msecs"
;; => nil

(defn zencat2 [x y]
  (loop [src y, ret (transient x)]
    (if src
      (recur (next src) (conj! ret (first src)))
      (persistent! ret))))

(time (dotimes [_ 1000000] (zencat2 [1 2 3] [4 5 6])))
;; "Elapsed time: 563.48425 msecs"
;; => nil

(def bv (vec (range 1e6)))

(first (time (zencat1 bv bv)))
;; "Elapsed time: 190.081667 msecs"
;; => 0

(first (time (zencat2 bv bv)))
;; "Elapsed time: 52.882511 msecs"
;; => 0

(def gimme #(do (print \.) %))

(take 1 (map gimme (range 32)))
;; => (0)
;; .........

(take 1 (drop 32 (map gimme (range 64))))
;; => (32)
;; ..................

(defn seql [s]
  (lazy-seq
   (when-let [[x] (seq s)]
     (cons x (seql (rest s))))))

(take 1 (map gimme (seql (range 32))))
;; => (0)

(take 1 (drop 32 (map gimme (seql (range 64)))))
;; => (32)

;; Memoization
;; Storing a cache of values local to a function so that its
;; arguments can be retrieved rather than calculated on every call
(def gcd (memoize
          (fn [x y]
            (cond
              (> x y) (recur (- x y) y)
              (< x y) (recur x (- y x))
              :else x))))


(gcd 1000645475 56130776629010010)
;; => 215

(defprotocol CacheProtocol
  (lookup [cache e])
  (has? [cache e])
  (hit [cache e])
  (miss [cache e ret]))

(deftype BasicCache [cache]
  CacheProtocol(lookup [_ item]
                 (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (BasicCache. (assoc cache item result))))

(def cache (BasicCache. {}))

(lookup (miss cache '(servo) :robot) '(servo))
;; => :robot

(defn through [cache f item]
  (if (has? cache item)
    (hit cache item)
    (miss cache item (delay (apply f item)))))


(deftype PluggableMemoization [f cache]
  CacheProtocol
  (has? [_ item] (has? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (PluggableMemoization. f (miss cache item result)))
  (lookup [_ item]
    (lookup cache item)))

(defn memoization-impl [cache-impl]
  (let [cache (atom cache-impl)]
    (with-meta
      (fn [& args]
        (let [cs (swap! cache through (.f cache-impl) args)]
          @(lookup cs args)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 3000) x))

(def sometimes-slowly (memoization-impl
                       (PluggableMemoization.
                        slowly
                        (BasicCache. {}))))

(time [(sometimes-slowly 108) (sometimes-slowly 108)])
;; "Elapsed time: 3009.126237 msecs"
;; => [108 108]

(time [(sometimes-slowly 108) (sometimes-slowly 108)])
;; "Elapsed time: 0.650759 msecs"
;; => [108 108]

;; Tail-recursive factorial, with no type declarations
(defn factorial-a [original-x]
  (loop [x original-x, acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))

(factorial-a 10)
;; => 3628800

(factorial-a 20)
;; => 2432902008176640000

(time (dotimes [_ 1e5] (factorial-a 20)))
;; "Elapsed time: 136.619295 msecs"
;; => nil

;; Factorial with a coerced local
(defn factorial-b [original-x]
  (loop [x (long original-x), acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))

(time (dotimes [_ 1e5] (factorial-b 20)))
;; "Elapsed time: 88.275216 msecs"
;; => nil

;; Factorial with a primitive long argument
(defn factorial-c [^long original-x]
  (loop [x original-x, acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))

(time (dotimes [_ 1e5] (factorial-b 20)))
;; "Elapsed time: 75.361903 msecs"
;; => nil

;; Factorial without overflow checking
(set! *unchecked-math* true)

(defn factorial-d [^long original-x]
  (loop [x original-x, acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))

(set! *unchecked-math* false)

(time (dotimes [_ 1e5] (factorial-d 20)))
;; "Elapsed time: 83.077542 msecs"
;; => nil

;; (factorial-d 21)

;; Factorial with a primitive double argument
(defn factorial-e [^double original-x]
  (loop [x original-x, acc 1.0]
    (if (>= 1.0 x)
      acc
      (recur (dec x) (* x acc)))))

(factorial-e 10.0)
;; => 3628800.0

(factorial-e 20.0)
;; => 2.43290200817664E18

(factorial-e 30.0)
;; => 2.652528598121911E32

(factorial-e 171.0)
;; => ##Inf

(time (dotimes [_ 1e5] (factorial-e 20.0)))
;; "Elapsed time: 26.054243 msecs"
;; => nil
;; Doubles give up accuracy but they deliver excellent performance
(defn factorial-f [^long original-x]
  (loop [x original-x, acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (*' x acc)))))

(factorial-f 20)
;; => 2432902008176640000

(factorial-f 30)
;; => 265252859812191058636308480000000N

(factorial-f 171)
;; => 1241018070217667823424840524103103992616605577501693185388951803611996075221691752992751978120487585576464959501670387052809889858690710767331242032218484364310473577889968548278290754541561964852153468318044293239598173696899657235903947616152278558180061176365108428800000000000000000000000000000000000000000N

(time (dotimes [_ 1e5] (factorial-f 20)))
;; "Elapsed time: 98.626724 msecs"
;; => nil

;; Reimlementing Clojure's range function using lazy-seq
(defn empty-range? [start end step]
  (or (and (pos? step) (>= start end))
      (and (neg? step) (<= start end))))

(defn lazy-range [i end step]
  (lazy-seq
   (if (empty-range? i end step)
     nil
     (cons i
           (lazy-range (+ i step)
                       end
                       step)))))

(lazy-range 5 10 2)
;; => (5 7 9)

(lazy-range 6 0 -1)
;; => (6 5 4 3 2 1)

(reduce conj [] (lazy-range 6 0 -1))
;; => [6 5 4 3 2 1]

(reduce + 0 (lazy-range 6 0 -1))
;; => 21

;; Reimplementation of range that returns a reducible
(defn reducible-range [start end step]
  (fn [reducing-fn init]
    (loop [result init, i start]
      (if (empty-range? i end step)
        result
        (recur (reducing-fn result i)
               (+ i step))))))

(defn half [x]
  (/ x 2))

(half 4)
;; => 2

(half 7)
;; => 7/2

(defn sum-half [result input]
  (+ result (half input)))

(reduce sum-half 0 (lazy-range 0 10 2))
;; => 10

((reducible-range 0 10 2) sum-half 0)
;; => 10

(defn half-transformer [f1]
  (fn f1-helf [result input]
    (f1 result (half input))))

((reducible-range 0 10 2) (half-transformer +) 0)
;; => 10

((reducible-range 0 10 2) (half-transformer conj) [])
;; => [0 1 2 3 4]

;; Essence of mapping, bundled to be used with a reducible
(defn mapping [map-fn]
  (fn map-transformer [f1]
    (fn [result input]
      (f1 result (map-fn input)))))

((reducible-range 0 10 2) ((mapping half) +) 0)
;; => 10

((reducible-range 0 10 2) ((mapping half) conj) [])
;; => [0 1 2 3 4]

((reducible-range 0 10 2) ((mapping list) conj) [])
;; => [(0) (2) (4) (6) (8)]

;; Essence of filtering, bundled to be used with a reducible
(defn filtering [filter-pred]
  (fn [fl]
    (fn [result input]
      (if (filter-pred input)
        (fl result input)
        result))))

((reducible-range 0 10 2) ((filtering #(not= % 2)) +) 0)
;; => 18

((reducible-range 0 10 2) ((filtering #(not= % 2)) conj) [])
;; => [0 4 6 8]

((reducible-range 0 10 2)
 ((filtering #(not= % 2))
  ((mapping half) conj))
 [])
;; => [0 2 3 4]

((reducible-range 0 10 2)
 ((mapping half)
  ((filtering #(not= % 2)) conj))
 [])
;; => [0 1 3 4]

;; Essence of mapcatting, bundled to be used with a reducible
(defn mapcatting [map-fn]
  (fn [fl]
    (fn [result input]
      (let [reducible (map-fn input)]
        (reducible fl result)))))

(defn and-plus-ten [x]
  (reducible-range x (+ 11 x) 10))

((and-plus-ten 5) conj [])
;; => [5 15]

((reducible-range 0 10 2) ((mapcatting and-plus-ten) conj) [])
;; => [0 10 2 12 4 14 6 16 8 18]

(filter #(not= % 2)
        (map half
             (lazy-range 0 10 2)))
;; => (0 1 3 4)

;; A couple of reducible transformers
(defn r-map [mapping-fn reducible]
  (fn new-reducible [reducing-fn init]
    (reducible ((mapping mapping-fn) reducing-fn) init)))

(defn r-filter [filter-pred reducible]
  (fn new-reducible [reducing-fn init]
    (reducible ((filtering filter-pred) reducing-fn) init)))

(def our-final-reducible
  (r-filter #(not= % 2)
            (r-map half
                   (reducible-range 0 10 2))))

(our-final-reducible conj [])
;; => [0 1 3 4]

;; Performance of reducibles
;; (crit/bench
;;  (reduce + 0 (filter even? (map half (lazy-range 0 (* 10 1000 1000) 2)))))
;; Evaluation count : 60 in 60 samples of 1 calls.
;;              Execution time mean : 2.416207 sec
;;     Execution time std-deviation : 481.890683 ms
;;    Execution time lower quantile : 2.005460 sec ( 2.5%)
;;    Execution time upper quantile : 3.188402 sec (97.5%)
;;                    Overhead used : 10.806846 ns

;; Found 2 outliers in 60 samples (3.3333 %)
;; 	low-severe	 1 (1.6667 %)
;; 	low-mild	 1 (1.6667 %)
;;  Variance from outliers : 91.1165 % Variance is severely inflated by outliers

;; (crit/bench
;;  (reduce + (filter even? (map half (range 0 (* 10 1000 1000) 2)))))
;; Evaluation count : 120 in 60 samples of 2 calls.
;;              Execution time mean : 591.282903 ms
;;     Execution time std-deviation : 18.737365 ms
;;    Execution time lower quantile : 572.454516 ms ( 2.5%)
;;    Execution time upper quantile : 640.558565 ms (97.5%)
;;                    Overhead used : 10.806846 ns

;; Found 5 outliers in 60 samples (8.3333 %)
;; 	low-severe	 2 (3.3333 %)
;; 	low-mild	 3 (5.0000 %)
;;  Variance from outliers : 18.9627 % Variance is moderately inflated by outliers

;; (crit/bench
;;  ((r-filter even? (r-map half
;;                          (reducible-range 0 (* 10 1000 1000) 2))) + 0))
;; Evaluation count : 180 in 60 samples of 3 calls.
;;              Execution time mean : 632.920490 ms
;;     Execution time std-deviation : 144.543277 ms
;;    Execution time lower quantile : 428.749236 ms ( 2.5%)
;;    Execution time upper quantile : 830.525741 ms (97.5%)
;;                    Overhead used : 10.806846 ns

;; Converting transformers to core reducibles
(defn core-r-map [mapping-fn core-reducible]
  (r/reducer core-reducible (mapping mapping-fn)))

(defn core-r-filter [filter-pred core-reducible]
  (r/reducer core-reducible (filtering filter-pred)))

(reduce conj []
        (core-r-filter #(not= % 2)
                       (core-r-map half [0 2 4 6 8])))
;; => [0 1 3 4]

;; The fold function: reducing in parallel
(reduce + [1 2 3 4 5])
;; => 15

(r/fold + [1 2 3 4 5])
;; => 15

;; Converting transformers to core foldables
(defn core-f-map [mapping-fn core-reducible]
  (r/folder core-reducible (mapping mapping-fn)))

(defn core-f-filter [filter-pred core-reducible]
  (r/folder core-reducible (filtering filter-pred)))

(r/fold +
        (core-f-filter #(not= % 2)
                       (core-f-map half
                                   [0 2 4 6 8])))
;; => 8

(r/fold +
        (r/filter #(not= % 2)
                  (r/map half
                         [0 2 4 6 8])))
;; => 8

(r/fold (fn ([] 100) ([a b] (+ a b))) (range 10))
;; => 145

(r/fold (r/monoid + (constantly 100)) (range 10))
;; => 145

(r/fold 512
        (r/monoid + (constantly 100))
        +
        (range 10))
;; => 145

(r/fold 4 (r/monoid conj (constantly [])) conj (vec (range 10)))
;; => [0 1 [2 3 4] [5 6 [7 8 9]]]

(r/fold 4 (r/monoid into (constantly [])) conj (vec (range 10)))
;; => [0 1 2 3 4 5 6 7 8 9]

(r/foldcat (r/filter even? (vec (range 1000))))
;; => #object[clojure.core.reducers.Cat 0x5bcf3787 "clojure.core.reducers.Cat@5bcf3787"]

(seq (r/foldcat (r/filter even? (vec (range 10)))))
;; => (0 2 4 6 8)

;; (def big-vector (vec (range 0 (* 10 1000 1000) 0)))

;; (crit/bench
;;  (r/fold + (core-f-filter even? (core-f-map half big-vector))))
