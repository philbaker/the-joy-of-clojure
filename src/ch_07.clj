(ns ch-07
  (:require clojure.test))

;; Functional programming

([:a :b] 0) ;; => :a

(map [:cthon :pthor :beowulf :grendel] #{0 3})
;; => (:cthon :grendel)

; comp allows you to build a new function from chains of other functions
(def fifth (comp first rest rest rest rest))

(fifth [1 2 3 4 5]) ;; => 5

(defn fnth [n]
  (apply comp
         (cons first
               (take (dec n) (repeat rest)))))

((fnth 5) '[a b c d e]) ;; => e

(map (comp
      keyword
      #(.toLowerCase %)
      name)
     '(a B C))
;; => (:a :b :c)


; partial allows you to build a function from the partial application
; of another
((partial + 5) 100 200) ;; => 305
; is equivalent to
(#(apply + 5 %&) 100 200) ;; => 305


; complement takes a function that returns a truthy value and returns
; the opposite truthy value;

(let [truthiness (fn [v] v)]
  [((complement truthiness) true)
   ((complement truthiness) 42)
   ((complement truthiness) false)
   ((complement truthiness) nil)])
;; => [false false true true]

((complement even?) 2) ;; => false
; is equivalent to
((comp not even?) 2) ;; => false
; and
(#(not (even? %)) 2) ;; => false

;; Using functions as data

;; defn metadata

; placing a map before a functions parameters is one way
; of assiging metadata to a function
(defn join
  {:test (fn []
           (assert
            (= (join "," [1 2 3]) "1,3,3")))}
  [sep s]
  (apply str (interpose sep s)))

(clojure.test/run-tests)
;; => {:test 1, :pass 0, :fail 0, :error 1, :type :summary}

; shorthand notation
(defn ^:private ^:dynamic sum [nums]
  (map + nums))

; is the same as
(defn ^{:private true, :dynamic true} sum [nums]
  (map + nums))

; and the same as
(defn sum {:private true, :dynamic :true} [nums]
  (map + nums))

; and the same as
(defn sum
  ([nums]
   (map + nums))
  {:private true, :dynamic true})

; The different choices are useful for metaprogramming -
; favour the shorthand in most cases

;; Higher order functions
; Takes one or more funcions as arguments
; Returns a function as a result

(sort [1 5 7 0 -42 13]) ;; => (-42 0 1 5 7 13)

(sort ["z" "x" "a" "aa"]) ;; => ("a" "aa" "x" "z")

(sort [(java.util.Date.) (java.util.Date. 100)])
; => (#inst "1970-01-01T00:00:00.100-00:00" #inst "2021-10-18T06:51:22.754-00:00")

(sort [[1 2 3], [-1 0 1], [3 2 1]])
; => ([-1 0 1] [1 2 3] [3 2 1])

(sort > [7 1 4])
; => (7 4 1)

(sort ["z" "x" "a" "aa" 1 5 8])
; => Unhandled java.lang.ClassCastException

(sort [{:age 99}, {:age 13}, {:age 7}])
; => Unhandled java.lang.ClassCastException

(sort [[:a 7], [:c 13], [:b 21]])
; => ([:a 7] [:b 21] [:c 13])

(sort second [[:a 7], [:c 13], [:b 21]])
; => Unhandled clojure.lang.ArityException

(sort-by second [[:a 7], [:c 13], [:b 21]])
; => ([:a 7] [:c 13] [:b 21])

(sort-by str ["z" "x" "a" "aa" 1 5 8])
; => (1 5 8 "a" "aa" "x" "z")

(sort-by :age [{:age 99}, {:age 13}, {:age 7}])
; => ({:age 7} {:age 13} {:age 99})

(def plays [{:band "Burial" :plays 979 :loved 9}
            {:band "Eno" :plays 2333 :loved 15}
            {:band "Bill Evans" :plays 979 :loved 9}
            {:band "Magma" :plays 2665 :loved 31}])

(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))

(sort-by-loved-ratio plays)
; => ({:band "Magma", :plays 2665, :loved 31}
;     {:band "Burial", :plays 979, :loved 9}
;     {:band "Bill Evans", :plays 979, :loved 9}
;     {:band "Eno", :plays 2333, :loved 15})

(defn columns [column-names]
  (fn [row]
    (vec (map row column-names))))

((columns [:plays :loved :band])
 {:band "Burial" :plays 979 :loved 9})
; => [979 9 "Burial"]

(vec (map (plays 0) [:plays :loved :band]))
; => [979 9 "Burial"]

;; Pure functions
(defn keys-apply [f ks m]
  (let [only (select-keys m ks)]
    (zipmap (keys only)
            (map f (vals only)))))

(keys-apply #(.toUpperCase %) #{:band} (plays 0))
; => {:band "BURIAL"}

(defn manip-map [f ks m]
  (merge m (keys-apply f ks m)))

(manip-map #(int (/ % 2)) #{:plays :loved} (plays 0))
; => {:band "Burial", :plays 489, :loved 4}

(defn mega-love! [ks]
  (map (partial manip-map #(int (* % 1000)) ks) plays))

(mega-love! [:loved])
; => ({:band "Burial", :plays 979, :loved 9000}
;     {:band "Eno", :plays 2333, :loved 15000}
;     {:band "Bill Evans", :plays 979, :loved 9000}
;     {:band "Magma", :plays 2665, :loved 31000})

;; Named arguments
(defn slope
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(slope :p1 [4 15] :p2 [3 21])
; => -6.0

(slope :p2 [2 1])
; => 0.5

(slope)
; => 1.0

;; Constraining functions with pre- and postconditions

(defn slope2 [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

(slope2 [10 10] [10 10])
; => Assert failed: (not= p1 p2)

(slope2 [10 1] '(1 20))
; => Assert failed: (vector? p2)

(slope2 [10 1] [1 20])
; => Assert failed: (float? %)

(slope2 [10.0 1] [1 20])
; => -2.111111111111111

(defn put-things [m]
  (into m {:meat "beef" :veggie "broccoli"}))

(put-things {})
; => {:meat "beef", :veggie "broccoli"}

(defn vegan-constraints [f m]
  {:pre [(:veggie m)]
   :post [(:veggie %) (nil? (:meat %))]}
  (f m))

(vegan-constraints put-things {:veggie "carrot"})
; => Assert failed: (nil? (:meat %))

(defn balanced-diet [f m]
  {:post [(:meat %) (:veggie %)]}
  (f m))

(balanced-diet put-things {})
; => {:meat "beef", :veggie "broccoli"}

(defn finicky [f m]
  {:post [(= (:meat %) (:meat m))]}
  (f m))

(finicky put-things {})
; => Assert failed: (= (:meat %) (:meat m))

;; Closures
; A closure is a function that has access to local s from the context
; where it was created
(def times-two
  (let [x 2]
    (fn [y] (* y x))))

(times-two 5)
; => 10

(def add-and-get
  (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [y] (.addAndGet ai y))))

(add-and-get 2)
; => 2

(add-and-get 2)
; => 4

(add-and-get 7)
; => 11

; Functions returning closures
(defn times-n [n]
  (let [x n]
    (fn [y] (* y x))))

(times-n 4)
; => #function[ch-07/times-n/fn--10156]

(def times-four (times-n 4))

(times-four 10)
; => 40

; Closing over parameters
(defn times-n [n]
  (fn [y] (* y n)))

(defn divisible [denom]
  (fn [num]
    (zero? (rem num denom))))

((divisible 3) 6)
; => true

((divisible 3) 7)
; => false

; Passing closures as functions

(filter even? (range 10))
; => (0 2 4 6 8)

(filter (divisible 4) (range 10))
; => (0 4 8)

(defn filter-divisible [denom s]
  (filter (fn [num] (zero? (rem num denom))) s))

(filter-divisible 4 (range 10))
; => (0 4 8)

(defn filter-divisible-2 [denom s]
  (filter #(zero? (rem % denom)) s))

(filter-divisible-2 5 (range 20))
;; => (0 5 10 15)

(def bearings [{:x 0, :y 1} ; north
               {:x 1, :y 0} ; east
               {:x 0, :y -1} ; south
               {:x -1, :y 0}]) ; west

(defn forward [x y bearing-num]
  [(+ x (:x (bearings bearing-num)))
   (+ y (:y (bearings bearing-num)))])

(forward 5 5 0)
; => [5 6]

(forward 5 5 1)
; => [6 5]

(forward 5 5 2)
; => [5 4]

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))})

(:coords (bot 5 5 0))
; => [5 5]

(:bearing (bot 5 5 0))
; => :north

(:coords ((:forward (bot 5 5 0))))
; => [5 6]

(defn bot-2 [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left (fn [] (bot x y (mod (- 1 bearing-num) 4)))})

(:bearing ((:forward ((:forward ((:turn-right (bot-2 5 5 0))))))))
; => :east

(:coords ((:forward ((:forward ((:turn-right (bot-2 5 5 0))))))))
; => [7 5]


(defn mirror-bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :south :west] bearing-num)
   :forward (fn [] (mirror-bot (- x (:x (bearings bearing-num)))
                               (- y (:y (bearings bearing-num)))
                               bearing-num))
   :turn-right (fn [] (mirror-bot x y (mod (- 1 bearing-num) 4)))
   :turn-left (fn [] (mirror-bot x y (mod (+ 1 bearing-num) 4)))})


;; Recursion

(defn pow [base exp]
  (if (zero? exp)
    1
    (* base (pow base (dec exp)))))

(pow 2 10)
; => 1024

(pow 1.01 925)
; => 9937.353723241924

(pow 2 10000)
; StackOverflowError

(defn pow-2 [base exp]
  (letfn [(kapow [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* base acc))))]
    (kapow base exp 1)))

(pow-2 2N 10000)
;; => 199506311688075838488374216268358508382349683188...

(def simple-metric {:meter 1
                    :km 1000
                    :cm 1/100
                    :mm [1/10 :cm]})

; How many meters are in 3 kilometers, 10 meters, 80 centimeters,
; 10 millimeters?
(->    (* 3  (:km simple-metric))
    (+ (* 10 (:meter simple-metric)))
    (+ (* 80 (:cm simple-metric)))
    (+ (* (:cm simple-metric)
          (* 10 (first (:mm simple-metric)))))
    float)
; => 3010.81


(defn convert [context descriptor]
  (reduce (fn [result [mag unit]]
            (+ result
               (let [val (get context unit)]
                 (if (vector? val)
                   (* mag (convert context val))
                   (* mag val)))))
            0
            (partition 2 descriptor)))


(convert simple-metric [1 :meter])
; => 1

(convert simple-metric [50 :cm])
; => 1/2

(convert simple-metric [100 :mm])
; => 1/10

(float (convert simple-metric [3 :km 10 :meter 80 :cm 10 :mm]))
; => 3010.81

(convert {:bit 1 :byte 8 :nibble [1/2 :byte]} [32 :nibble])
; => 128N

;; Tail recursion
; Greatest common denominator
; No tail call optimisation - JVM doesn't provide it so this will
; not be optimised
(defn gcd [x y]
  (cond
    (> x y) (gcd (- x y) y)
    (< x y) (gcd x (- y x))
    :else x))

(gcd 100 5)
; => 5


(defn gcd-tco [x y]
  (cond
    (> x y) (recur (- x y) y)
    (< x y) (recur x (- y x))
    :else x))

(gcd-tco 100 5)
; => 5

(defn elevator [commands]
  (letfn
   [(ff-open [[_ & r]]
      "When the elevator is open on the 1st floor
              it can either close or be done."
      #(case _
         :close (ff-closed r)
         :done true
         false))
    (ff-closed [[_ & r]]
      "When the elevator is closed on the 1st floor
                it can either open or go up."
      #(case _
         :open (ff-open r)
         :up (sf-closed r)
         false))
    (sf-closed [[_ & r]]
      "When the elevator is clsoed on the 2nd floor
                it can either go down or open."
      #(case _
         :down (ff-closed r)
         :open (sf-open r)
         false))
    (sf-open [[_ & r]]
      "When the elevator is open on the 2nd floor
              it can either close or be done."
      #(case _
         :close (sf-closed r)
         :done true
         false))]
    (trampoline ff-open commands)))

(elevator [:close :open :close :up :open :open :done])
; => false

(elevator [:close :up :open :close :down :open :done])
; => true

;; Continuation-passing style
; Not prevalent in Clojure but is part of the functional tradition
; A way of generalising a computation by viewing it in terms of
; up to three functions:
; - Accept: Decides when a computation should terminate
; - Return: Wraps the return values
; - Continuation: Provides the next step in the computation
; Factorial is a illustrative example
(defn fac-cps [n k]
  (letfn [(cont [v] (k (* v n)))]
    (if (zero? n)
      (k 1)
      (recur (dec n) cont))))

(defn fac [n]
  (fac-cps n identity))

(fac 10)
; => 3628800

; The power of CPS is that you can extract more generic function
; builders using CPS
(defn mk-cps [accept? kend kont] ;; Next
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v]
                    (k ((partial kont v) n)))]
         (if (accept? n) ;; Accept
           (k 1) ;; Return
           (recur (dec n) cont))))
     n kend)))


(def fac-2
  (mk-cps zero?
          identity
          #(* %1 %2)))

(fac-2 10)
; => 3628800

(def tri
  (mk-cps #(== 1 %)
          identity
          #(+ %1 %2)))

(tri 10)
; => 55

;; A* pathfinding algorithm
; Maintains a set of candidate poths through a "world" with the
; purpose of finding the least difficult path to some goal
(def world [[  1   1   1   1   1]
            [999 999 999 999   1]
            [  1   1   1   1   1]
            [  1 999 999 999 999]
            [  1   1   1   1   1]])

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
                        size
                        yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %))
                deltas))))

(neighbors 5 [0 0])
; => ([1 0] [0 1])

; Estimate the straight-line remaining path cost
(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))

(estimate-cost 900 5 0 0)
; => 7200

(estimate-cost 900 5 4 4)
; => 0

; Calculate the cost of the path traversed so far
(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (or (:cost cheapest-nbr) 0)))

(path-cost 900 {:cost 1})
; => 901

; Calculate the estimated total cost of the path
(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(total-cost 0 900 5 0 0)
; => 7200

(total-cost 1000 900 5 3 4)
; => 1900

(total-cost (path-cost 900 {:cost 1}) 900 5 3 4)
; => 1801

; Retrieve the minimum value based on a criteria function
(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min other]
              (if (> (f min) (f other))
                other
                min))
            coll)))

(min-by :cost [{:cost 100} {:cost 36} {:cost 9}])

; A* algorithm to navigate the world. Tail-recursive solution
(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %)
                                         nbr-yxs))
              newcost (path-cost (get-in cell-costs yx)
                                 cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs (conj (:yxs cheapest-nbr [])
                                         yx)})

                   (into rest-work-todo
                         (map
                          (fn [w]
                            (let [[y x] w]
                              [(total-cost newcost step-est size y x) w]))
                          nbr-yxs)))))))))


; The Z world
(astar [0 0]
       900
       world)
; => [{:cost 17,
;     :yxs
;     [[0 0] [0 1] [0 2] [0 3] [0 4] [1 4] [2 4]
;      [2 3] [2 2] [2 1] [2 0] [3 0] [4 0] [4 1]
;      [4 2] [4 3] [4 4]]}
;    :steps 94]


; Shrubbery world
(astar [0 0]
       900
       [[ 1 1 1   2 1]
        [ 1 1 1 999 1]
        [ 1 1 1 999 1]
        [ 1 1 1 999 1]
        [ 1 1 1   1 1]])
; => [{:cost 9,
;     :yxs [[0 0] [0 1] [0 2]
;           [1 2]
;           [2 2]
;           [3 2]
;           [4 2] [4 3] [4 4]]}
;    :steps 134]


; Bunny world
(astar [0 0]
       900
       [[ 1 1 1   2 1]
        [ 1 1 1 999 1]
        [ 1 1 1 999 1]
        [ 1 1 1 999 1]
        [ 1 1 1 666 1]])
; => [{:cost 10,
;     :yxs [[0 0] [0 1] [0 2] [0 3] [0 4]
;           [1 4]
;           [2 4]
;           [3 4]
;           [4 4]]}
;    :steps 132]
