(ns ch-17-change-thinking
  (:require [clojure.set :as ra]
            [ch-11-parallelism :as joy]
            [ch-08 :as contracts])
  (:use [clojure.string :as str :only []]
        [clojure.test]))


(def artists
  #{{:artist "Burial" :genre-id 1}
    {:artist "Magma" :genre-id 2}
    {:artist "Can" :genre-id 3}
    {:artist "Faust" :genre-id 3}
    {:artist "Ikonika" :genre-id 1}
    {:artist "Grouper"}})

(def genres
  #{{:genre-id 1 :genre-name "Dubstep"}
    {:genre-id 2 :genre-name "Zeuhl"}
    {:genre-id 3 :genre-name "Prog"}
    {:genre-id 4 :genre-name "Drone"}})

;; select * example using Clojure's relational algebra functions
(def ALL identity)

(ra/select ALL genres)
;; => #{{:genre-id 4, :genre-name "Drone"} {:genre-id 2, :genre-name "Zeuhl"} {:genre-id 3, :genre-name "Prog"} {:genre-id 1, :genre-name "Dubstep"}}

(ra/select (fn [m] (#{1 3} (:genre-id m))) genres)
;; => #{{:genre-id 3, :genre-name "Prog"} {:genre-id 1, :genre-name "Dubstep"}}

(defn ids [& ids]
  (fn [m] ((set ids) (:genre-id m))))

(ra/select (ids 1 3) genres)
;; => #{{:genre-id 3, :genre-name "Prog"} {:genre-id 1, :genre-name "Dubstep"}}

(take 2 (ra/select ALL (ra/join artists genres)));; => ({:genre-id 2, :genre-name "Zeuhl", :artist "Magma"} {:genre-id 1, :genre-name "Dubstep", :artist "Ikonika"})

;; Implementing a SQL-like DSL to generate queries
;; (defn fantasy-query [max]
;;   (SELECT [a b c]
;;           (FROM X
;;                 (LEFT-JOIN Y :ON (= X.a Y.b)))
;;           (WHERE (< a 5) AND (< b max))))

;; Shuffling SQL-like operators into infix position
(defn shuffle-expr [expr]
  (if (coll? expr)
    (if (= (first expr) `unquote)
      "?"
      (let [[op & args] expr]
        (str "("
             (str/join (str " " op " ")
                       (map shuffle-expr args)) ")")))
    expr))

(shuffle-expr 42)
;; => 42

(shuffle-expr `(unquote max))
;; => "?"

(read-string "~max")
;; => (clojure.core/unquote max)

(shuffle-expr '(= X.a Y.b))
;; => "(X.a = Y.b)"

(shuffle-expr '(AND (< a 5) (< b ~max)))
;; => "((a < 5) AND (b < ?))"

(shuffle-expr '(AND (< a 5) (OR (> b 0) (< b ~max))))
;; => "((a < 5) AND ((b > 0) OR (b < ?)))"

(defn process-where-clause [processor expr]
  (str " WHERE " (processor expr)))

(process-where-clause shuffle-expr '(AND (< a 5) (< b ~max)))
;; => " WHERE ((a < 5) AND (b < ?))"

(defn process-left-join-clause [processor table _ expr]
  (str " LEFT JOIN " table
       " ON " (processor expr)))

(apply process-left-join-clause
       shuffle-expr
       '(Y :ON (= X.a = Y.b)))
;; => " LEFT JOIN Y ON (X.a = = = Y.b)"

(let [LEFT-JOIN (partial process-left-join-clause shuffle-expr)]
  (LEFT-JOIN 'Y :ON '(= X.a Y.b)))
;; => " LEFT JOIN Y ON (X.a = Y.b)"

(defn process-from-clause [processor table & joins]
  (apply str " FROM " table
         (map processor joins)))

(process-from-clause shuffle-expr 'X
                     (process-left-join-clause shuffle-expr 'Y :ON '(= X.a Y.b)))
;; => " FROM X LEFT JOIN Y ON (X.a = Y.b)"

(defn process-select-clause [processor fields & clauses]
  (apply str "SELECT " (str/join ", " fields)
         (map processor clauses)))

(process-select-clause shuffle-expr
                       '[a b c]
                       (process-from-clause shuffle-expr 'X
                                            (process-left-join-clause shuffle-expr 'Y :ON '(= X.a Y.b)))
                       (process-where-clause shuffle-expr '(AND (< a 5) (< b ~max))))
;; => "SELECT a, b, c FROM X LEFT JOIN Y ON (X.a = Y.b) WHERE ((a < 5) AND (b < ?))"

(declare apply-syntax)

(def ^:dynamic *clause-map*
  {'SELECT (partial process-select-clause apply-syntax)
   'FROM (partial process-from-clause apply-syntax)
   'LEFT-JOIN (partial process-left-join-clause shuffle-expr)
   'WHERE (partial process-where-clause shuffle-expr)})

;; Looking up syntax processors in the processor table
(defn apply-syntax [[op & args]]
  (apply (get *clause-map* op) args))

;; Building a SQL-like SELECT statement DSL
(defmacro SELECT [& args]
  {:query (apply-syntax (cons 'SELECT args))
   :bindings (vec (for [n (tree-seq coll? seq args)
                        :when (and (coll? n)
                                   (= (first n) `unquote))]
                    (second n)))})

(defn example-query [max]
  (SELECT [a b c]
          (FROM X
                (LEFT-JOIN Y :ON (= X.a Y.b)))
          (WHERE (AND (< a 5) (< b ~max)))))

(example-query 9)
;; => {:query "SELECT a, b, c FROM X LEFT JOIN Y ON (X.a = Y.b) WHERE ((a < 5) AND (b < ?))", :bindings [9]}

;; Using with-redefs to create stubs
(def stubbed-feed-children
  (constantly [{:content [{:tag :title
                           :content ["Stub"]}]}]))

(defn count-feed-entries [url]
  (count (joy/feed-children url)))

(count-feed-entries "http://blog.fogus.me/feed/")
;; => 5

(with-redefs [joy/feed-children stubbed-feed-children]
  (count-feed-entries "dummy url"))
;; => 1

(with-redefs [joy/feed-children stubbed-feed-children]
  (joy/occurrences joy/title "Stub" "a" "b" "c"))
;; => 3

;; clojure.test as a partial specification
(deftest feed-tests
  (with-redefs [joy/feed-children stubbed-feed-children]
    (testing "Child Counting"
      (is (= 1000 (count-feed-entries "Dummy URL"))))
    (testing "Occurence Counting"
      (is (= 0 (joy/count-text-task
                joy/title
                "ZOMG"
                "Dummy URL"))))))

(run-tests)
;; => {:test 1, :pass 1, :fail 1, :error 0, :type :summary}
;; FAIL in (feed-tests) (NO_SOURCE_FILE:165)
;; Child Counting
;; expected: (= 1000 (count-feed-entries "Dummy URL"))
;;   actual: (not (= 1000 1))

;; Ran 1 tests containing 2 assertions.
;; 1 failures, 0 errors.

;; Creating formulas that are like spreadsheet cells
(defmacro defformula [nm bindings & formula]
  `(let ~bindings
     (let [formula# (agent ~@formula)
           update-fn# (fn [key# ref# o# n#]
                        (send formula# (fn [_#] ~@formula)))]
       (doseq [r# ~(vec (map bindings
                             (range 0 (count bindings) 2)))]
         (add-watch r# :update-formula update-fn#))
       (def ~nm formula#))))

;; defformula to track baseball averages
(def h (ref 25))
(def ab (ref 100))

(defformula avg
  [at-bats ab, hits h]
  (float (/ @hits @at-bats)))

@avg
;; => 0.25

(dosync (ref-set h 33))

@avg
;; => 0.33
