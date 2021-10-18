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
;; => (#inst "1970-01-01T00:00:00.100-00:00" #inst "2021-10-18T06:51:22.754-00:00")

(sort [[1 2 3], [-1 0 1], [3 2 1]])
;; => ([-1 0 1] [1 2 3] [3 2 1])

(sort > [7 1 4])
;; => (7 4 1)

(sort ["z" "x" "a" "aa" 1 5 8])
;; => Unhandled java.lang.ClassCastException

(sort [{:age 99}, {:age 13}, {:age 7}])
;; => Unhandled java.lang.ClassCastException
