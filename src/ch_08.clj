(ns ch-08
  (:require [clojure.walk :as walk]
            [clojure.xml :as xml])
  (:import [java.io BufferedReader InputStreamReader]
          [java.net URL]))

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

(unless true (println "nope"))
; => nil

(unless false (println "yep!"))
; => nil

(macroexpand `(if (not condition) "got it"))
; => (if (clojure.core/not ch-08/condition) "got it")

(eval `(if (not condition) "got it"))
; Caused by java.lang.RuntimeException
; No such var: ch-08/condition

(def condition false)

(eval `(if (not condition) "got it"))
; => "got it"

(defmacro def-watched [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#]
                  (println old# " -> " new#)))))

(def-watched x (* 12 12))
x
; => 144

(def x 0)
; 144  ->  144
; 144  ->  0

; Using macros to change forms
(defmacro domain [name & body]
  `{:tag :domain
    :attrs {:name (str '~name)}
    :content [~@body]})

(declare handle-things)

(defmacro grouping [name & body]
  `{:tag :grouping,
    :attrs {:name (str '~name)}
    :content [~@(handle-things body)]})


(declare grok-attrs grok-props)

(defn handle-things [things]
  (for [t things]
    {:tag :thing
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
            (list? a) [:isa (str (second a))]
            (string? a) [:comment a]))))


(defn grok-props [props]
  (when props
    {:tag :properties :attrs nil
     :content (apply vector (for [p props]
                              {:tag :property
                               :attrs {:name (str (first p))}
                               :content nil}))}))

(def d
  (domain man-vs-monster
          (grouping people
                    (Human "A stock human")
                    (Man (isa Human)
                         "A man, baby"
                         [name]
                         [has-beard?]))
          (grouping monsters
                    (Chupacabra
                     "A fierce, yet elusive creature"
                     [eats-goats?]))))

(:tag d)
; => :domain

(:tag (first (:content d)))
; => :grouping

(xml/emit d)
; <?xml version='1.0' encoding='UTF-8'?>
; <domain name='man-vs-monster'>
;   <grouping name='people'>
;     <thing name='Human' comment='A stock human'>
;       <properties>
;       </properties>
;     </thing>
;     <thing name='Man' isa='Human' comment='A man, baby'>
;       <properties>
;         <property name='name'/>
;         <property name='has-beard?'/>
;       </properties>
;     </thing>
;   </grouping>
;   <grouping name='monsters'>
;     <thing name='Chupacabra' comment='A fierce, yet elusive creature'>
;       <properties>
;         <property name='eats-goats?'/>
;       </properties>
;     </thing>
;   </grouping>
; </domain>

; Using macros to control symbolic resolution time
(defmacro resolution [] `x)

(macroexpand '(resolution))
; => ch-08/x

(def x2 9)
(let [x2 109] (resolution))
; => 9

; Anaphora
(defmacro awhen [expr & body]
  `(let [~'it ~expr]
     (if ~ 'it
       (do ~@body))))

(awhen [1 2 3] (it 2))
; => 3

(awhen nil (println "Will never get here"))
; => nil

(awhen 1 (awhen 2 [it]))
; => [2]

; Contract top-level macro
(declare collect-bodies)

(defmacro contract [name & forms]
  (list* `fn name (collect-bodies forms)))


(declare build-contract)

(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

; Contract auxiliary function build-contract
(defn build-contract [c]
  (let [args (first c)]
    (list
     (into '[f] args)
     (apply merge
            (for [con (rest c)]
              (cond (= (first con) 'require)
                    (assoc {} :pre (vec (rest con)))
                    (= (first con) 'ensure)
                    (assoc {} :post (vec (rest con)))
                    :else (throw (Exception.
                                  (str "Unknown tag "
                                       (first con)))))))
     (list* 'f args))))

; Composition of the contract function and the constrained function
(def doubler-contract
  (contract doubler
            [x]
            (require
             (pos? x))
            (ensure
             (= (* 2 x) %))))

(def times2 (partial doubler-contract #(* 2 %)))

(times2 9)

(def times3 (partial doubler-contract #(* 3 %)))

(times3 9)
; Unhandled java.lang.AssertionError
; Assert failed: (= (* 2 x) %)

; Contract for multiple-arity functions
(def doubler-contract
  (contract doubler-contract
            [x]
            (require
             (pos? x))
            (ensure
             (= (* 2 x) %))
            [x y]
            (require
             (pos? x)
             (pos? y))
            (ensure
             (= (* 2 (+ x y)) %))))

((partial doubler-contract #(* 2 (+ %1 %2))) 2 3)
; => 10

((partial doubler-contract #(+ %1 %1 %2 %2)) 2 3)
; => 10

((partial doubler-contract #(* 3 (+ %1 %2))) 2 3)
; Unhandled java.lang.AssertionError
; Assert failed: (= (* 2 (+ x y)) %)
