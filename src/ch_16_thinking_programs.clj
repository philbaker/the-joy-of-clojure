(ns ch-16-thinking-programs
  (:require [joy.ch-05 :refer [pos]]
            [clojure.walk :as walk]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]))

(def b1 '[3 - - - - 5 - 1 -
          - 7 - - - 6 - 3 -
          1 - - - 9 - - - -
          7 - 8 - - - - 9 -
          9 - - 4 - 8 - - 2
          - 6 - - - - 5 - 1
          - - - - 4 - - - 6
          - 4 - 7 - - - 2 -
          - 2 - 6 - - - - 3])
;; => #'ch-16-thinking-programs/b1

(defn prep [board]
  (map #(partition 3 %)
       (partition 9 board)))

(defn print-board [board]
  (let [row-sep (apply str (repeat 37 "-"))]
    (println row-sep)
    (dotimes [row (count board)]
      (print "| ")
      (doseq [subrow (nth board row)]
        (doseq [cell (butlast subrow)]
          (print (str cell "   ")))
        (print (str (last subrow) " | ")))
      (println)
      (when (zero? (mod (inc row) 3))
        (println row-sep)))))

(-> b1 prep print-board)
;; -------------------------------------
;; | 3   -   - | -   -   5 | -   1   - |
;; | -   7   - | -   -   6 | -   3   - |
;; | 1   -   - | -   9   - | -   -   - |
;; -------------------------------------
;; | 7   -   8 | -   -   - | -   9   - |
;; | 9   -   - | 4   -   8 | -   -   2 |
;; | -   6   - | -   -   - | 5   -   1 |
;; -------------------------------------
;; | -   -   - | -   4   - | -   -   6 |
;; | -   4   - | 7   -   - | -   2   - |
;; | -   2   - | 6   -   - | -   -   3 |
;; -------------------------------------

(defn rows [board sz]
  (partition sz board))

(defn row-for [board index sz]
  (nth (rows board sz) (/ index 9)))

(row-for b1 1 9)
;; => (3 - - - - 5 - 1 -)

(defn column-for [board index sz]
  (let [col (mod index sz)]
    (map #(nth % col)
         (rows board sz))))

(column-for b1 2 9)
;; => (- - - 8 - - - - -)

(defn subgrid-for [board i]
  (let [rows (rows board 9)
        sgcol (/ (mod i 9) 3)
        sgrow (/ (/ i 9) 3)
        grp-col (column-for (mapcat #(partition 3 %) rows) sgcol 3)
        grp (take 3 (drop (* 3 (int sgrow)) grp-col))]
    (flatten grp)))

(subgrid-for b1 0)
;; => (3 - - - 7 - 1 - -)

;; Sudoku rules
;; Place a number into the first empty square
;; Check if the constraints hold
;; - If so, then start this algorithm again
;; - If not, then remove the number and start this algorithm again
;; Repeat

(defn numbers-present-for [board i]
  (set
   (concat (row-for board i 9)
           (column-for board i 9)
           (subgrid-for board i))))

(numbers-present-for b1 1)
;; => #{7 1 4 - 6 3 2 5}

(numbers-present-for (assoc b1 1 8) 1)
;; => #{7 1 4 - 6 3 2 5 8}

(clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                (numbers-present-for b1 1))
;; => #{9 8}

(defn possible-placements [board index]
  (clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                  (numbers-present-for board index)))

;; Brute-force Sudoku solver
(defn solve [board]
  (if-let [[i & _]
           (and (some '#{-} board)
                (pos '#{-} board))]
    (flatten (map #(solve (assoc board i %))
                  (possible-placements board i)))
    board))

;; (-> b1
;;     solve
;;     prep
;;     print-board)

;; Identifying logic variables
(defn lvar?
  "Determines if a value represents a logic variable"
  [x]
  (boolean
   (when (symbol? x)
     (re-matches #"^\?.*" (name x)))))

(lvar? '?x)
;; => true

(lvar? 'a)
;; => false

(lvar? 2)
;; => false

;; Simplified satisfiability function
(defn satisfyl
  [l r knowledge]
  (let [L (get knowledge l l)
        R (get knowledge r r)]
    (cond
      (= L R) knowledge
      (lvar? L) (assoc knowledge L R)
      (lvar? R) (assoc knowledge R L)
      :default nil)))

(satisfyl '?something 2 {})
;; => {?something 2}

(satisfyl 2 '?something {})
;; => {?something 2}

(satisfyl '?x '?y {})
;; => {?x ?y}

(->> {}
     (satisfyl '?x '?y)
     (satisfyl '?x 1))
;; => {?x ?y, ?y 1}

;; Satisfying seqs
(= '(1 2 3) '(1 2 3))
;; => true

(= '(1 2 3) '(100 200 300))
;; => false

;; Function that satisfies seqs
(defn satisfy
  [l r knowledge]
  (let [L (get knowledge l l)
        R (get knowledge r r)]
    (cond
      (not knowledge) nil
      (= L R) knowledge
      (lvar? L) (assoc knowledge L R)
      (lvar? R) (assoc knowledge R L)
      (every? seq? [L R])
      (satisfy (rest L)
               (rest R)
               (satisfy (first L)
                        (first R)
                        knowledge))
      :default nil)))

(satisfy '(1 2 3) '(1 ?something 3) {})
;; => {?something 2}

(satisfy '((((?something)))) '((((2)))) {})
;; => {?something 2}

(satisfy '(?x 2 3 (4 5 ?z))
         '(1 2 ?y (4 5 6))
         {})
;; => {?x 1, ?y 3, ?z 6}

(satisfy '?x '(?y) {})
;; => {?x (?y)}

(satisfy '(?x 10000 3) '(1 2 ?y) {})
;; => nil

;; Walking a data structure and substituting logic variables for
;; bound values
(defn subst [term binds]
  (walk/prewalk
   (fn [expr]
     (if (lvar? expr)
       (or (binds expr) expr)
       expr))
   term))

(subst '(1 ?x 3) '{?x 2})
;; => (1 2 3)

(subst '((((?x)))) '{?x 2})
;; => ((((2))))

(subst '[1 ?x 3] '{?x 2})
;; => [1 2 3]

(subst '{:a ?x, :b [1 ?x 3]} '{?x 2})
;; => {:a 2, :b [1 2 3]}

(subst '(1 ?x 3) '{})
;; => (1 ?x 3)

(subst '(1 ?x 3) '{?x ?y})
;; => (1 ?y 3)

(def page
  '[:html
    [:head [:title ?title]]
    [:body [:h1 ?title]]])

(subst page '{?title "Hi!"})
;; => [:html [:head [:title "Hi!"]] [:body [:h1 "Hi!"]]]

;; Unification
;; A function that takes two terms and unifies them in the
;; empty context, finally returning a new substitution

;; Melding two seqs, substituting logic variables
(defn meld [term1 term2]
  (->> {}
       (satisfy term1 term2)
       (subst term1)))

(meld '(1 ?x 3) '(1 2 ?y))
;; => (1 2 3)

(meld '(1 ?x) '(?y (?y 2)))
;; => (1 (1 2))

(satisfy '?x 1 (satisfy '?x '?y {}))
;; => {?x ?y, ?y 1}

(satisfy '(1 ?x) '(?y (?y 2)) {})
;; => {?y 1, ?x (?y 2)}

;; core.logic
(logic/run* [answer]
  (logic/== answer 5))
;; => (5)

(logic/run* [val1 val2]
  (logic/== {:a val1, :b 2}
            {:a 1,    :b val2}))
;; => ([1 2])

(logic/run* [x y]
  (logic/== x y))
;; => ([_0 _0])

(logic/run* [q]
  (logic/== q 1)
  (logic/== q 2))
;; => ()

(logic/run* [george]
  (logic/conde
   [(logic/== george :born)]
   [(logic/== george :unborn)]))
;; => (:born :unborn)

;; An introduction to constraint programming
(defn doubler [n] (* n 2))

(doubler 2)
;; => 4

(doubler Math/E)
;; => 5.43656365691809

(doubler 1/8)
;; => 1/4

(doubler [])
;; Unhandled java.lang.ClassCastException

(doubler #(%))
;; Unhandled java.lang.ClassCastException

(doubler "")
;; Unhandled java.lang.ClassCastException


(logic/run* [q]
  (logic/fresh [x y]
    (logic/== q [x y])))
;; => ([_0 _1])


(logic/run* [q]
  (logic/fresh [x y]
    (logic/== [:pizza "Java"] [x y])
    (logic/== q [x y])))
;; => ([:pizza "Java"])

(logic/run* [q]
  (logic/fresh [x y]
    (logic/== q [x y])
    (logic/!= y "Java")))
;; => (([_0 _1] :- (!= (_1 "Java"))))

(logic/run* [q]
  (logic/fresh [x y]
    (logic/== [:pizza "Java"] [x y])
    (logic/== q [x y])
    (logic/!= y "Java")))

(logic/run* [q]
  (logic/fresh [x y]
    (logic/== [:pizza "Scala"] [x y])
    (logic/== q [x y])
    (logic/!= y "Java")))
;; => ([:pizza "Scala"])

(logic/run* [q]
  (logic/fresh [n]
    (logic/== q n)))
;; => (_0)

(logic/run* [q]
  (logic/fresh [n]
    (logic/!= 0 n)
    (logic/== q n)))
;; => ((_0 :- (!= (_0 0))))

(logic/run* [q]
  (logic/fresh [n]
    (fd/in n (fd/domain 0 1))
    (logic/== q n)))
;; => (0 1)

(logic/run* [q]
  (let [coin (fd/domain 0 1)]
    (logic/fresh [heads tails]
      (fd/in heads 0 coin)
      (fd/in tails 1 coin)
      (logic/== q [heads tails]))))
;; => ([0 0] [1 0] [0 1] [1 1])

;; Solving Sudoku with finite domains
(defn rowify [board]
  (->> board
       (partition 9)
       (map vec)
       vec))

(clojure.pprint/pprint (rowify b1))
;; [[3 - - - - 5 - 1 -]
;;  [- 7 - - - 6 - 3 -]
;;  [1 - - - 9 - - - -]
;;  [7 - 8 - - - - 9 -]
;;  [9 - - 4 - 8 - - 2]
;;  [- 6 - - - - 5 - 1]
;;  [- - - - 4 - - - 6]
;;  [- 4 - 7 - - - 2 -]
;;  [- 2 - 6 - - - - 3]]

(defn colify [rows]
  (apply map vector rows))

(colify (rowify b1))
;; => ([3 - 1 7 9 - - - -] [- 7 - - - 6 - 4 2] [- - - 8 - - - - -] [- - - - 4 - - 7 6] [- - 9 - - - 4 - -] [5 6 - - 8 - - - -] [- - - - - 5 - - -] [1 3 - 9 - - - 2 -] [- - - - 2 1 6 - 3])

(defn subgrid [rows]
  (partition 9
             (for [row (range 0 9 3)
                   col (range 0 9 3)
                   x (range row (+ row 3))
                   y (range col (+ col 3))]
               (get-in rows [x y]))))

(subgrid (rowify b1))
;; => ((3 - - - 7 - 1 - -) (- - 5 - - 6 - 9 -) (- 1 - - 3 - - - -) (7 - 8 9 - - - 6 -) (- - - 4 - 8 - - -) (- 9 - - - 2 5 - 1) (- - - - 4 - - 2 -) (- 4 - 7 - - 6 - -) (- - 6 - 2 - - - 3))

(def logic-board #(repeatedly 81 logic/lvar))

;; Recursively initializing a Sudoku board filled with logic variables
(defn init [[lv & lvs] [cell & cells]]
  (if lv
    (logic/fresh []
      (if (= '- cell)
        logic/succeed
        (logic/== lv cell))
      (init lvs cells))
    logic/succeed))

;; A core.logic Sudoku solver
(defn solve-logically [board]
  (let [legal-nums (fd/interval 1 9)
        lvars (logic-board)
        rows (rowify lvars)
        cols (colify rows)
        grids (subgrid rows)]
    (logic/run 1 [q]
      (init lvars board)
      (logic/everyg #(fd/in % legal-nums) lvars)
      (logic/everyg fd/distinct rows)
      (logic/everyg fd/distinct cols)
      (logic/everyg fd/distinct grids)
      (logic/== q lvars))))

(-> b1
    solve-logically
    first
    prep
    print-board)
;; -------------------------------------
;; | 3   8   6 | 2   7   5 | 4   1   9 |
;; | 4   7   9 | 8   1   6 | 2   3   5 |
;; | 1   5   2 | 3   9   4 | 8   6   7 |
;; -------------------------------------
;; | 7   3   8 | 5   2   1 | 6   9   4 |
;; | 9   1   5 | 4   6   8 | 3   7   2 |
;; | 2   6   4 | 9   3   7 | 5   8   1 |
;; -------------------------------------
;; | 8   9   3 | 1   4   2 | 7   5   6 |
;; | 6   4   1 | 7   5   3 | 9   2   8 |
;; | 5   2   7 | 6   8   9 | 1   4   3 |
;; -------------------------------------
;; => nil
