(ns ch-10-mutation-concurrency
  (:require [joy.ch-05 :as ch-05 :refer [neighbors]])
  (:import java.util.concurrent.Executors))

;; Concurrency
;; Refers to the execution of disparate tasks at roughly the same time,
;; each sharing a common resource, but not necessarily performing
;; related tasks.
;; The results of concurrent tasks often affect the behaviour of other
;; concurrent tasks and therefore contains an element of nondeterminism

;; Parallelism
;; Refers to partitioning a task into multiple parts, each run at the
;; same time.
;; Typically, parallel tasks work toward an aggregate goal, and the
;; result of one doesn't affect the behaviour of any other parallel
;; task, thus maintaining determinacy.

(def thread-pool
  (Executors/newFixedThreadPool
   (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads!
  [f & {thread-count :threads
        exec-count :times
        :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit thread-pool
             #(dotimes [_ exec-count] (f)))))

(dothreads! #(.print System/out "Hi ") :threads 2 :times 2)

(def initial-board
  [[:- :k :-]
   [:- :- :-]
   [:- :K :-]])

(defn board-map [f board]
  (vec (map #(vec (for [s %] (f s)))
            board)))

(defn reset-board!
  "Resets the board state. Generally these types of functions are
  a bad idea"
  []
  (def board (board-map ref initial-board))
  (def to-move (ref [[:K [2 1]] [:k [0 1]]]))
  (def num-moves (ref 0)))

(def king-moves
  (partial neighbors
           [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]] 3))

(defn good-move?
  [to enemy-sq]
  (when (not= to enemy-sq)
    to))

(defn choose-move
  "Randomly choose a legal move"
  [[[mover mpos] [_ enemy-pos]]]
  [mover (some #(good-move? % enemy-pos)
               (shuffle (king-moves mpos)))])

(reset-board!)
(take 5 (repeatedly #(choose-move @to-move)))
;; => ([:K [2 2]] [:K [1 2]] [:K [1 2]] [:K [2 0]] [:K [1 2]])

(defn place [from to] to)

(defn move-piece [[piece dest] [[_ src] _]]
  (alter (get-in board dest) place piece)
  (alter (get-in board src) place :-)
  (alter num-moves inc))

(defn update-to-move [move]
  (alter to-move #(vector (second %) move)))

(defn make-move []
  (let [move (choose-move @to-move)]
    (dosync (move-piece move @to-move))
    (dosync (update-to-move move))))

(reset-board!)

(make-move)
;; => [[:k [0 1]] [:K [2 2]]]

(board-map deref board)
;; => [[:- :k :-] [:- :- :-] [:- :- :K]]

(make-move)
;; => [[:K [2 2]] [:k [1 2]]]

(board-map deref board)
;; => [[:- :- :-] [:- :- :k] [:- :- :K]]

(dothreads! make-move :threads 100 :times 100)

(board-map deref board)
;; => [[:- :- :-] [:K :K :-] [:- :- :-]]

;; io! is useful because it will throw an exception if used inside
;; a transaction. It's best to avoid IO in transactions e.g.you can
;; end up with very bloated logs
(io! (.println System/out "hola!"))

(dosync (io! (.println System/out "hello!")))
;; 1. Unhandled java.lang.IllegalStateException
;;    I/O in transaction

(defn make-move-2 []
  (let [move (choose-move @to-move)]
    (dosync (move-piece move @to-move))
    (dosync (update-to-move move))))

(defn make-move-v2 []
  (dosync
   (let [move (choose-move @to-move)]
     (move-piece move @to-move)
     (update-to-move move))))

(reset-board!)

(make-move-2)
;; => [[:k [0 1]] [:K [1 0]]]

(board-map deref board)
;; => [[:- :k :-] [:K :- :-] [:- :- :-]]

@num-moves
;; => 1

(dothreads! make-move-v2 :threads 100 :times 100)

(board-map #(dosync (deref %)) board)
;; => [[:- :- :-] [:- :k :-] [:K :- :-]]

@to-move
;; => [[:k [1 1]] [:K [2 0]]]

@num-moves
;; => 10001

(defn move-piece-2 [[piece dest] [_ src] _]
  (commute (get-in board dest) place piece)
  (commute (get-in board src) place :-)
  (commute num-moves inc))

(defn make-move-v3 []
  (dosync
   (let [move (choose-move @to-move)]
     (move-piece-2 move @to-move)
     (update-to-move move))))

(reset-board!)

(dothreads! make-move-v3 :threads 100 :times 100)

(board-map deref board)
;; => [[:- :k :-] [:- :- :-] [:- :K :-]]

@to-move
;; => [[:K [2 1]] [:k [0 1]]]

(defn update-to-move-2 [move]
  (commute to-move #(vector (second %) move)))

(defn make-move-v4 []
  (dosync
   (let [move (choose-move @to-move)]
     (move-piece-2 move @to-move)
     (update-to-move-2 move))))

(dothreads! make-move-v4 :threads 100 :times 100)

(board-map #(dosync (deref %)) board)
;; => [[:- :k :-] [:- :- :-] [:- :K :-]]

@to-move
;; => [[:K [2 1]] [:k [0 1]]]

(defn update-to-move-3 [move]
  (commute to-move #(vector (second %) move)))

(defn make-move-v5 []
  (dosync
   (let [move (choose-move @to-move)]
     (move-piece-2 move @to-move)
     (update-to-move-3 move))))

(dothreads! make-move-v5 :threads 100 :times 100)

(board-map #(dosync (deref %)) board)
;; => [[:- :k :-] [:- :- :-] [:- :K :-]]

@to-move
;; => [[:K [2 1]] [:k [0 1]]]

(dosync (ref-set to-move '[[:K [2 1]] [:k [0 1]]]))
;; => [[:K [2 1]] [:k [0 1]]]
