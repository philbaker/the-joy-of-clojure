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
