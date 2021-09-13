(ns joy.ch-04)

(let [imadeupapi 3.1415926535879323846264338327950288419716939937M]
  (println (class imadeupapi))
  imadeupapi)
; java.math.BigDecimal 
; 3.1415926535879323846264338327950288419716939937M

(let [butieatedit 3.1415926535879323846264338327950288419716939937]
  (println (class butieatedit))
  butieatedit)
; java.lang.Double 
; 3.1415926535879324

(def clueless 9)

(class clueless) ; java.lang.Long

(class (+ clueless 9000000000000000)) ; java.lang.Long

(class (+ clueless 90000000000000000000)) ; clojure.lang.BigInt

(class (+ clueless 9.0)) ; java.lang.Double

; Overflow
(+ Long/MAX_VALUE Long/MAX_VALUE) ; exception: integer overflow

(unchecked-add (Long/MAX_VALUE) (Long/MAX_VALUE)) ; -2

; Underflow
(float 0.0000000000000000000000000000000000000000000001) ; 0.0
1.0E-430 ; 0.0

; Rounding errors
(let [approx-interval (/ 209715 2097152)
      actual-interval (/ 1 10)
      hours (* 3600 100 10)
      actual-total (double (* hours actual-interval))
      approx-total (double (* hours approx-interval))]
  (- actual-total approx-total)) ; 0.34332275390625

; In Clojure, any computation involving even a single double results in a value
; that's a double
(+ 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M) ; 1.0M
(+ 0.1M 0.1M 0.1M 0.1 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M) ; 0.9999999999999999

; Rationals
; Clojure provides a data type representing a rational number, and all of its
; core mathematical functions operate with rational numbers
; For absolutely precise calculations rationals (represented in fraction form)
; are the best choice. Floating point calculations can be unreliable
1.0E-430000000M ; 1.0E-430000000M
1.0E-4300000000M ; Erro: Exponent overflow

(def a 1.0e50)
(def b -1.0e50)
(def c 17.0e00)

(+ (+ a b) c) ; 17.0
(+ a (+ b c)) ; 0.0

; The best way to ensure that your calculations remain as accurate as possible
; is to do them all using rational numbers
(def d (rationalize 1.0e50))
(def e (rationalize -1.0e50))
(def f (rationalize 17.0e00))

(+ (+ d e) f) ; 17N
(+ d (+ e f)) ; 17N

(numerator (/ 123 10)) ; 123
(denominator (/ 123 10)) ; 10

; Keywords
:a-keyword ; :a-keyword
::also-a-keyword ; :joy.ch-04/also-a-keyword
:user/also-a-keyword ; :user/also-a-keyword
; Keywords always refer to themselves, whereas symbols don't
; In Clojure code keywords are almost always used as map keys

(def population {:zombies 2700, :humans 9})

(get population :zombies) ; 2700

(println (/ (get population :zombies)
            (get population :humans))
         "zombies per capita") ; 300 zombies per capita

; Using a keyword as a function directive
(defn pour [lb ub]
  (cond
    (= ub :toujours) (iterate inc lb)
    :else (range lb ub)))

(pour 1 10) ; (1 2 3 4 5 6 7 8 9)

; Qualifying keywords
; Keywords don't belong to any specific namespace, although they may appear
; to if you start them with two colons rather than one
::not-in-ns ; :joy.ch-04/not-in-ns
; The prefix on a keyword is arbitrary and in no way associates it with a
; namespace
