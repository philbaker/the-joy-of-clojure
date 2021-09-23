(ns ch-06)

; Structural sharing: a persistent toy
(def baselist (list :barnabas :adam))
(def lst1 (cons :willie baselist))
(def lst2 (cons :phoenix baselist))

lst1 ; (:willie :barnabas :adam)
lst2 ; (:phoenix :barnabas :adam)
; baselist is like a historical version of both lst1 and lst2
; more than being equal, the next parts of both lists are identical -
; the same instance
(= (next lst1) (next lst2)) ; true
(identical? (next lst1) (next lst2)) ; true

(defn xconj [t v]
  (cond
    (nil? t) {:val v, :L nil, :R nil}))

(xconj nil 5) ; {:val 5, :L nil, :R nil}

(defn xconj2 [t v]
  (cond
    (nil? t) {:val v, :L nil, :R nil}
    (< v (:val t)) {:val (:val t),
                    :L (xconj2 (:L t) v),
                    :R (:R t)}))

(def tree1 (xconj2 nil 5))
tree1 ; {:val 5, :L nil, :R nil}

(def tree1 (xconj2 tree1 3))
tree1 ; {:val 5, :L {:val 3, :L nil, :R nil}, :R nil}

(def tree1 (xconj2 tree1 2))
tree1
; {:val 5, :L {:val 3, :L {:val 2, :L nil, :R nil}, :R nil}, :R nil}

(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

(xseq tree1) ; (2 3 5)

(defn xconj3 [t v]
  (cond
    (nil? t)       {:val v, :L nil, :R nil} ; nil nodes start with v
    (< v (:val t)) {:val (:val t), ; when v is less than the value at
                                   ; the current node, it's pushed left
                    :L (xconj3 (:L t) v),
                    :R (:R t)}
    :else          {:val (:val t), ; otherwise it's pushed right
                    :L (:L t),
                    :R (xconj3 (:R t) v)}))

(def tree2 (xconj3 tree1 7))
(xseq tree2) ; (2 3 5 7)

(identical? (:L tree1) (:L tree2)) ; true

; Laziness
; Short-circuiting is an example of laziness
; Note: these examples are not idiomatic Clojure code
(defn if-chain [x y z]
  (if x
    (if y
      (if z
        (do
          (println "Made it!")
          :all-truthy)))))
; The call to println is evaluated only in the case of thee truthy arguments

(if-chain () 42 true)
; (out) Made it!
; :all-truthy

(if-chain true true false) ; nil

; You can perform the equivalent action given only the `and` macro 
(defn and-chain [x y z]
  (and x y z (do (println "Made it!") :all-truthy)))

(and-chain () 42 true)
; (out) Made it!
:all-truthy

; Understanding the lazy-seq recipe
(defn rec-step [[x & xs]]
  (if x
    [x (rec-step xs)]
    []))

(rec-step [1 2 3 4]) ; [1 [2 [3 [4 []]]]]

(rec-step (range 200000))
; (err) Execution error (StackOverflowError) at ch-06/rec-step (user.clj:88).
; (err) null

; The lazy-seq recipe
; 1. Use the `lazy-seq` macro at the outermost level of your lazy sequence
; producting expression(s)
; 2. If you happen to be consuming another sequence during your operations,
; then use `rest` instead of `next`
; 3. Prefer higher-order functions when processing sequences
; 4. Don't hold on to your head
