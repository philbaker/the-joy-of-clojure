(ns ch-08
  (:require [clojure.walk :as walk]))

;; Macros

;; Data is code is data
; With Clojure there's no distinction between the textual form
; and the actual form of a program. When a program is the data
; that composes the program, then you can write programs to write
; programs.

; Eval
; Take a data structure representing a clojure expression, evaluate
; it, and return the result.

(eval 42)
; => 42

(eval '(list 1 2))
; => (1 2)

(eval (list 1 2))
; Unhandled java.l ang.ClassCastException
; class java.lang.Long cannot be cast to class clojure.lang.IFn

(eval (list (symbol "+") 1 2))
; => 3

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(contextual-eval '{a 1, b 2} '(+ a b))
; => 3

(contextual-eval '{a 1, b 2} '(let [b 1000] (+ a b)))
; => 1001


; Nested syntax-quotes
(let [x 9 y '(- x)]
  (println `y)
  (println ``y)
  (println ``~y)
  (println ``~~y)
  (contextual-eval {'x 36} ``~~y))
; ch-08/y
; (quote ch-08/y)
; ch-08/y
; (- x)
; => -36

; Macros are useful for creating control structures
(defmacro do-until [& clauses]
  (when clauses
    (list 'clojure.core/when (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                    "do-until requires an even number of forms")))
          (cons 'do-until (nnext clauses)))))

(do-until
 (even? 2) (println "Even")
 (odd? 3) (println "Odd")
 (zero? 1) (println "You never see me")
 :lollipop (println "Truthy thing"))
; Even
; Odd
; => nil

(macroexpand-1 '(do-until true (prn 1) false (prn 2)))
; => (clojure.core/when true (prn 1) (do-until false (prn 2)))

(walk/macroexpand-all '(do-until true (prn 1) false (prn 2)))
; => (if true (do (prn 1) (if false (do (prn 2) nil))))

(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

(unless (even? 3) "Now we see it...")
; => "Now we see it..."

(unless (even? 2) "Now we don't.")
; => nil
