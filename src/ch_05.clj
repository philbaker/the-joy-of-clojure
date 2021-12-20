(ns joy.ch-05
  (:require clojure.set))

; into-array can make a Java/JavaScript array out of a vector
(def ds (into-array [:willie :barnabas :adam])) ; [:willie :barnabas :adam]
(seq ds) ; (:willie :barnabas :adam)
; aset sets the value in an array slot
(aset ds 1 :quentin) ; :quentin

(def dsv [:willie :barnabas :adam])
(def dsv1 (replace {:barnabas :quentin} ds))
; ds is unchanged
ds ; #object["[Lclojure.lang.Keyword;" 0xace9703 "[Lclojure.lang.Keyword;@ace9703"]
; dsv1 is a modified version of the original vector
dsv1 ; (:willie :quentin :adam)

; Seqs, sequentials and sequences
;
; Collection: A composite data type
[1 2] ; [1 2]
{:a 1} ; {:a 1}
#{1 2} ; #{1 2}
;
; Sequential: On ordered series of values
[1 2 3 4]
'(1 2 3 4)
;
; Sequence: A sequential collection that may or may not exist yet
(map dec [1 2 3]) ; (0 1 2)
;
; Seq: A simple API for navigating collections
(first [1 2 3]) ; 1
(rest [1 2 3]) ; (2 3)
() ; ()
;
; clojure.core/seq: A function that returns an object implementing the seq API
(seq []) ; nil
(seq [1 2]) ; (1 2)

; If two sequentials have the same values in the same order, = returns true
; for them, even if their concrete types are different
(= [1 2 3] '(1 2 3)) ; true
; Conversely, even if two collections have the same exact values, if one is a
; sequential collecton and the other isn't, = returns false
(= [1 2 3] #{1 2 3}) ; false

(class (hash-map :a 1)) ; clojure.lang.PersistentHashMap
(seq (hash-map :a 1)) ; ([:a 1])
(class (seq (hash-map :a 1))) ; clojure.lang.PersistentHashMap$NodeSeq
(seq (keys (hash-map :a 1))) ; (:a)
(class (keys (hash-map :a 1))) ; clojure.lang.APersistentMap$KeySeq

; Vectors
(vec (range 10)) ; [0 1 2 3 4 5 6 7 8 9]

(let [my-vector [:a :b :c]]
  (into my-vector (range 10))) ; [:a :b :c 0 1 2 3 4 5 6 7 8 9]

(into (vector-of :int) [Math/PI 2 1.3]) ; [3 2 1]

(into (vector-of :char) [100 101 102]) ; [\d \e \f]

;; (into (vector-of :int) [1 442042942966429420642094091111])
; (err) Value out of range for long: 442042942966429420642094091111

; Large vectors
(def a-to-j (vec (map char (range 65 75))))
(nth a-to-j 4) ; \E
(get a-to-j 4) ; \E
(a-to-j 4) ; \E
; Because vectors are indexed, they can be efficiently waled in either
; direction
(seq a-to-j) ; (\A \B \C \D \E \F \G \H \I \J)
(rseq a-to-j) ; (\J \I \H \G \F \E \D \C \B \A)

(assoc a-to-j 4 "no longer E") ; [\A \B \C \D "no longer E" \F \G \H \I \J]
(replace {2 :a, 4 :b} [1 2 3 2 3 4]) ; [1 :a 3 :a 3 :b]

(def matrix
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(get-in matrix [1 2]) ; 6

(assoc-in matrix [1 2] 'x) ; [[1 2 3] [4 5 x] [7 8 9]]

(update-in matrix [1 2] * 100) ; [[1 2 3] [4 5 600] [7 8 9]]

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
                        size
                        yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %))
                deltas))))


(neighbors 3 [0 0]) ; ([1 0] [0 1])
(neighbors 3 [1 1]) ; ([0 1] [2 1] [1 0] [1 2])

; Vectors as stacks
(def my-stack [1 2 3])
(peek my-stack) ; 3
(pop my-stack) ; [1 2]
(conj my-stack 4) ; [1 2 3 4]
(+ (peek my-stack) (peek (pop my-stack))) ; 5

; Using vectors instead of reverse
; The ability of vectors to grow efficiently on the right side and then be
; walked right means you rarely need the `reverse` function
(defn strict-map-1 [f coll]
  (loop [coll coll, acc nil]
    (if (empty? coll)
      (reverse acc)
      (recur (next coll)
             (cons (f (first coll)) acc)))))
(strict-map-1 - (range 5)) ; (0 -1 -2 -3 -4)
; One way to get rid of the reverse is to use a vector instead of a list
; as the accumulator
(defn strict-map-2 [f coll]
  (loop [coll coll, acc []]
    (if (empty? coll)
      acc
      (recur (next coll)
             (conj acc (f (first coll)))))))
(strict-map-2 - (range 5)) ; [0 -1 -2 -3 -4]

; Subvectors
(subvec a-to-j 3 6) ; [\D \E \F]

; Vectors as map entries
(first {:width 10, :height 20, :depth 15}) ; [:width 10]
(vector? (first {:width 10, :height 20, :depth 15})) ; true

(doseq [[dimension amount] {:width 10, :height 20, :depth 15}]
  (println (str (name dimension) ":") amount "inches"))
; eval (comment-current-form): (doseq [[dimension amount] {:width 10, :height 20...
; (out) width: 10 inches
; (out) height: 20 inches
; (out) depth: 15 inches
; nil

; Lists
(cons 1 '(2 3)) ; (1 2 3)
(conj '(2 3) 1) ; (1 2 3)
; The 'right' way to add to a front of a list in clojure is with conj due
; to efficiency

(+ 100 (* 0.08 (- 6000 5000)))

; Queues
(defmethod print-method clojure.lang.PersistentQueue [q, w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))
; Overload the pritner for queues so they look like fish

clojure.lang.PersistentQueue/EMPTY
; <-nil-<

(def schedule
  (conj clojure.lang.PersistentQueue/EMPTY
        :wake-up :shower :brush-teeth))

schedule
; <-(:wake-up :shower :brush-teeth)-<

; `peek` gets the front enlement in a queue
(peek schedule) ; :wake-up

; `pop` removes elements from the front of a queue
(pop schedule) ; <-(:shower :brush-teeth)-<
; Note: with queues make sure you use pop instead of rest - rest will
; return a seq rather than a queue which can cause subtle bugs

; Persistent sets
; Clojure sets work the same as mathematical sets - they are collections of
; unsorted unique elements
; Sets are functions of their elements that return the matched element or nil
(#{:a :b :c :d} :c) ; :c
(#{:a :b :c :d} :e) ; nil
; Set elements can be accessed via get which returns the queried value if
; it exists in the set
(get #{:a 1 :b 2} :b) ; :b
(get #{:a 1 :b 2} :z) ; nil
(get #{:a 1 :b 2} :z :nothing-doing) ; :nothing-doing

; How Clojure populates sets
(into #{[]} [()]) ; #{[]}
(into #{[1 2]} '[(1 2)]) ; #{[1 2]}
(into #{[] #{} {}} [()]) ; #{[] #{} {}}

; Check for a truthy value in a set
(some #{:b} [:a 1 :b 2]) ; :b
(some #{1 :b} [:a 1 :b 2]) ; 1

; Sorted sets are straightforward but will only work if the arguments
; are mutually comparable
(sorted-set :b :c :a) ; #{:a :b :c}
(sorted-set [3 4] [1 2]) ; #{[1 2] [3 4]}
;; (sorted-set :b 2 :c :a 3 1)
; (err) class clojure.lang.Keyword cannot be cast to class java.lang.Number...

(def my-set (sorted-set :a :b))
; ... some time later
;; (conj my-set "a")
; (err) class clojure.lang.Keyword cannot be cast to class java.lang.String...

; contains? will work with sets
(contains? #{1 2 3 4} 4) ; true
; contains works for sets because behind the scenes sets are implemented
; as maps with the same key and value
(contains? [1 2 3 4] 4) ; false

; The clojure.set namespace

; Intersection
; Given two sets `intersection` returns a set of the common elements
; Given n sets, it incrementally returns the intersection of resulting sets
; and the next set
(clojure.set/intersection #{:humans :fruit-bats :zombies}
                          #{:chupacabra :zombies :humans})
; #{:zombies :humans}
; The common elements between the given sets are returned

(clojure.set/intersection #{:pez :gum :dots :skor}
                          #{:pez :skor :pocky}
                          #{:pocky :gum})
; #{:skor}
; This is the result of the intersection of the first two sets, then
; intersected with the final set

; Union
(clojure.set/union #{:humans :fruit-bats :zombies}
                   #{:chupacabra :zombies :humans})
; #{:chupacabra :zombies :humans :fruit-bats}

(clojure.set/union #{:pez :gum :dots :skor}
                   #{:pez :skor :pocky}
                   #{:pocky :gum :skor})
; #{:pocky :pez :skor :dots :gum}
; Takes all distinct elements and returns in a set

; Difference
; This may work a bit differently than expected
(clojure.set/difference #{1 2 3 4} #{3 4 5 6}) ; #{1 2}
; difference checks for items in the first set that do not appear in
; the second

; Thinking in maps
; Hash maps
(hash-map :a 1 :b 2 :c 3 :d 4 :e 5)
; {:e 5, :c 3, :b 2, :d 4, :a 1}

; Clojure supports heterogeneous keys, meaning they can be of any type
(let [m {:a 1, 1 :b, [1 2 3] "4 5 6"}]
  [(get m :a) (get m [1 2 3])])
; [1 "4 5 6"]

; Clojure maps are functions of their keys
(let [m {:a 1, 1 :b, [1 2 3] "4 5 6"}]
  [(m :a) (m [1 2 3])])
; [1 "4 5 6"]

; Providing a map to the seq function returns a sequence of map entries
(seq {:a 1 :b 2}) ; ([:a 1] [:b 2])
(into {} [[:a 1] [:b 2]]) ; {:a 1, :b 2}
(apply hash-map [:a 1 :b 2]) ; {:b 2, :a 1}
(zipmap [:a :b] [1 2]) ; {:a 1, :b 2}

; Sorted maps
; Maps in Clojure have no order guarantees. If that is required use
; sorted maps
(sorted-map :thx 1138 :r2d 2) ; {:r2d 2, :thx 1138}
(sorted-map "bac" 2 "abc" 9) ; {"abc" 9, "bac" 2}
; sorted-map-by takes an additional comparison function
(sorted-map-by #(compare (subs %1 1) (subs %2 1)) "bac" 2 "abc" 9)
; {"bac" 2, "abc" 9}
;; (sorted-map :a 1, "b" 2)
; (err) class clojure.lang.Keyword cannot be cast to class java.lang.String

(assoc {1 :int} 1.0 :float) ; {1 :int, 1.0 :float}
; Hash maps treat long, int, float etc as different but sorted-map uses
; comparison to work out the order so it treats them the same
; Generally sorted-map is a drop-in replacement for hash-map though
(assoc (sorted-map 1 :int) 1.0 :float) ; {1 :float}

; Clojure provides array-map for cases when insertion order is important
(seq (hash-map :a 1, :b 2, :c 3)) ; ([:c 3] [:b 2] [:a 1])
(seq (array-map :a 1, :b 2, :c 3)) ; ([:a 1] [:b 2] [:c 3])

; Finding the position of items in a sequence
; Initial attempt:
(defn pos [e coll]
  (let [cmp (if (map? coll)
              #(= (second %1) %2) ; Map compare
              #(= %1 %2))] ; Default compare
    (loop [s coll idx 0] ; Start at the beginning
      (when (seq s)
        (if (cmp (first s) e) ; Compare
          (if (map? coll)
            (first (first s)) ; Map returns key
            idx) ; Else returns index
        (recur (next s) (inc idx)))))))

(pos 3 [:a 1 :b 2 :c 3 :d 4]) ; 5
(pos :foo [:a 1 :b 2 :c 3 :d 4]) ; nil
(pos 3 {:a 1 :b 2 :c 3 :d 4}) ; :c
(pos \3 ":a 1 :b 2 :c 3 :d 4") ; 13

; pos works but it's already quite complex - not great if we want to add new
; functionality to it
; trying to check for collection type makes the implementation too specific,
; where possible this should be avoided in favour of more generic algorithms

; This function can generate a uniform repersentation for indexed collections
(defn index [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

(index [:a 1 :b 2 :c 3 :d 4])
; ([0 :a] [1 1] [2 :b] [3 2] [4 :c] [5 3] [6 :d] [7 4])

(index {:a 1 :b 2 :c 3 :d 4})
; ([:a 1] [:b 2] [:c 3] [:d 4])

(index #{:a 1 :b 2 :c 3 :d 4})
; ([1 1] [4 4] [:c :c] [3 3] [2 2] [:b :b] [:d :d] [:a :a])

(defn pos2 [e coll]
  (for [[i v] (index coll) :when (= e v)] i))

(pos2 3 [:a 1 :b 2 :c 3 :d 4]) ; (5)
(pos2 3 {:a 1 :b 2 :c 3 :d 4}) ; (:c)
(pos2 3 [:a 3 :b 3 :c 3 :d 4]) ; (1 3 5)
(pos2 3 {:a 3 :b 3 :c 3 :d 4}) ; (:a :b :c)

(defn pos3 [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))

(pos3 #{3 4} {:a 1 :b 2 :c 3 :d 4}) ; (:c :d)
(pos3 even? [2 3 4 7]) ; (0 2)
