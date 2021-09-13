(ns joy.ch-02
  (:require clojure.string
            [clojure.set :as s]
            [clojure.test :refer [is]])
  (:refer clojure.pprint :rename {pprint print-that-stuff})
  (:import [java.util HashMap]
           [java.util.concurrent.atomic AtomicLong]))

[127 0x7F 0177 32r3V 2r01111111] ; [127 127 127 127 127]

(def yucky-pi 22/7)
yucky-pi ; 22/7

(fn [x y]
  (println "Making a set")
  #{x y}) ; #function[joy.ch-02/eval7373/fn--7374]

; Anonymous function that is called immediately
((fn [x y]
   (println "Making a set")
   #{x y})
 1 2) ; #{1 2}

(def make-set
  (fn [x y]
    (println "Making a set")
    #{x y}))

(make-set 1 2) ; #{1 2}

(defn make-set-f
  "Takes two values and makes a set from them."
  [x y]
  (println "Making a set")
  #{x y})

(make-set-f 1 2) ; #{1 2}

(defn make-set-f-2
  ([x] #{x})
  ([x y] #{x y}))

(make-set-f-2 42) ; #{42}

(arity2+ defn [first second & more]
  (vector first second more))

(arity2+ 1 2) ; [1 2 nil]
(arity2+ 1 2 3 4) ; [1 2 (3 4)]

;; #() is replaced with fn at read time
(def make-list0 #(list))
(make-list0) ; ()

(def make-list2 #(list %1 %2))
(make-list2 1 2) ; (1 2)

(def make-list2+ #(list %1 %2 %&))
(make-list2+ 1 2 3 4 5) ; (1 2 (3 4 5))

;; Blocks
;; use the `do` form when you have a series or block of expressions that need to
;; be treated as one
(do
  (def x 5)
  (def y 4)
  (+ x y)
  [x y]) ; [5 4]

(let [r 5
      pi 3.1415
      r-squared (* r r)]
  (println "radius is" r)
  (* pi r-squared)) ; 78.53750000000001

;; Loops 
;; Recur

(defn print-down-from [x]
  (when (pos? x) ; perform while still positive
    (println x) ; print the current x
    (recur (dec x)))) ; recurn with x minus 1

(print-down-from 10)

(defn sum-down-from [sum x] ; take counter
  (if (pos? x) ; if positive
    (recur (+ sum x) (dec x)) ; then recurse
    sum)) ; else return sum

(sum-down-from 0 10) ; 55
(sum-down-from 10 1) ; 11

;; Loop
(defn sum-down-from-2 [initial-x]
  (loop [sum 0, x initial-x] ; set up recursion target
    (if (pos? x)
      (recur (+ sum x) (dec x)) ; jump to recursion target
      sum)))

(sum-down-from-2 10) ; 55

;; Evalutian
cons ; #function[clojure.core/cons--5361]
1 ; 1
[2 3] ; [2 3]
(cons 1 [2 3]) ; (1 2 3)

;; Quoting
;; The quote special operator prevents its argument, and all of its sub-forms,
;; from being evaluated.
(quote age) ; age
(def age 9) ; #'joy.ch-02/age
(quote age) ; age
(quote (cons 1 [2 3])) ; (cons 1 [2 3])
(cons 1 (quote (2 3))) ; (1 2 3)
(cons 1 '(2 3)) ; (1 2 3)
[1 (+ 2 3)] ; [1 5]
'(1 (+ 2 3)) ; (1 (+ 2 3))

;; The syntax-quote (`) prevents its argument and subforms from being evaluated
;; unlike quote it has a few extra features that make it ideal for constructing
;; collections to be used as code
`(1 2 3) ; (1 2 3)
`map ; clojure.core/map
`Integer ; java.lang.Integer
`(map even? [1 2 3]) ; (clojure.core/map clojure.core/even? [1 2 3])
'(map even? [1 2 3]) ; (map even? [1 2 3])

;; Unquote
;; An unquote is used to demarcate specific forms as requiring evaluation by
;; prefixing them with the symbol ~ within the body of a syntax quote
`(+ 10 (* 3 2)) ; (clojure.core/+ 10 (clojure.core/* 3 2))
`(+ 10 ~(* 3 2)) ; (clojure.core/+ 10 6)

(let [x 2]
  `(1 ~x 3)) ; (1 2 3)

(let [x '(2 3)] `(1 ~x)) ; (1 (2 3))

;; Unquote-splicing
;; the @ tells clojure to splice the sequence into the resulting list rather
;; than inserting it as a nested list
(let [x '(2 3)] `(1 ~@x)) ; (1 2 3)

;; Auto-gensym
`potion# ; potion__7600__auto__

;; Interop
;; Accessing static class members (Java only)
java.util.Locale/JAPAN

(java.lang.Math/sqrt 9) ; 3.0
(Math/sqrt 9) ; 3.0

;; Creating instances
(new java.awt.Point 0 1) ; #object[java.awt.Point 0x30b3f866 "java.awt.Point[x=0,y=1]"]

(new java.util.HashMap {"foo" 42 "bar" 9 "baz" "quux"}) ; {"bar" 9, "baz" "quux", "foo" 42}
;; prefered: (the dot signifies a constructor call)
(java.util.HashMap. {"foo" 42 "bar" 9 "baz" "quux"}) ; {"bar" 9, "baz" "quux", "foo" 42}

;; to access public instance variables, precede the field name with a dot and
;; a hyphen
(.x (java.awt.Point. 10 20)) ; 10

;; to access instance methods, the dot form allows an additional argument to be 
;; passed to the method
(.divide (java.math.BigDecimal. "42") 2M) ; 21M

;; Instance fields can be set via the set! function:
(let [origin (java.awt.Point. 0 0)]
  (set! (.-x origin) 15)
  (str origin)) ; "java.awt.Point[x=15,y=0]"

;; method chaining
;; new java.util.Date().toString().endsWith("2021);
(.endsWith (.toString (java.util.Date.)) "2021") ; true
;; the .. macro makes method chaining in clojure easier to read
(.. (java.util.Date.) toString (endsWith "2021")) ; true

;; the doto macro
;; when working with Java, it's common to initialise a fresh instance by
;; calling a set of mutators
; java.util.HashMap props = new java.util.HashMap();
; props.put ("HOME", "/home/me");
; props.put ("SRC", "src");
; props.put ("BIN", "classes");
;; this code can be streamlined in clojure using the doto macro
(doto (java.util.HashMap.)
  (.put "HOME" "/home/me")
  (.put "SRC" "src")
  (.put "BIN" "classes")) ; {"SRC" "src", "BIN" "classes", "HOME" "/home/me"}

;; Error handling
;; Throwing and catching
(throw (Exception. "I done throwed")) ; Error: "I done throwed"

;; the syntax for catching exceptions in Clojure is similar to Java
(defn throw-catch [f]
  [(try
     (f)
     (catch ArithmeticException e "No dividing by zero!")
     (catch Exception e (str "You are so bad " (.getMessage e)))
     (finally (println "returning... ")))])

(throw-catch #(/ 10 5)) ; [2]

(throw-catch #(/ 10 0)) ; ["No dividing by zero!"]

(throw-catch #(throw (Exception. "Crybaby"))) ; ["You are so bad Crybaby"]

;; CLJS error handling is similar too
; (try
;   (throw (Error. "I done throwed in CLJS"))
;   (catch js/Error err "I done catched in CLJS"))
(throw-catch #(throw (Exception. "Crybaby"))) ; ["You are so bad Crybaby"]

;; Require and namespaces
(clojure.string/trim "    hello   ") ; "hello"
(s/intersection #{1 2 3} #{3 4 5}) ; #{3}
(is (true? true)) ; true
(print-that-stuff "hello!") ; "Hello"
(HashMap. {"happy?" true}) ; {"happy?" true}
(AtomicLong. 42) ; 42

