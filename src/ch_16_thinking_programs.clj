(ns ch-16-thinking-programs)

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
