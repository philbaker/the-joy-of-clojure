(ns joy.ch-03
  (:require [clojure.java.javadoc :refer [javadoc]]))

;; Truthiness
(if true :truthy :falsey) ; :truthy
(if [] :truthy :falsey) ; :truthy
(if nil :truthy :falsey) ; :falsey
(if false :truthy :falsey) ; :falsey

; Never do this:
(def evil-false (Boolean. "false")) ; #'joy.ch-03/evil-false
evil-false ; false
(= false evil-false) ; true
(if evil-false :truthy :falsey) ; :truthy
; correct way
(if (Boolean/valueOf "false") :truthy :falsey) ; :falsey

;; The only falsey values in clojure are nil and false
;; nil vs false
(when (nil? nil) "Actually nil, not false") ; "Actually nil, not false"

;; nil punning
;; because empty collections act like true in boolean contexts, you need an 
;; idiom for testing whether there's anything in a collection to process
(seq [1 2 3]) ; (1 2 3)
(seq []) ; nil
;; seq is useful as a termination process - it helps you avoid using 'empty?'
(defn print-seq [s]
  (when (seq s) ; check for empty
    (prn (first s))
    (recur (rest s)))) ; recurse

(print-seq []) ; nil
(print-seq [1 2]) ; 1 2 nil

;; Destructuring
(def guys-whole-name ["Guy" "Lewis" "Steele"])

(str (nth guys-whole-name 2) ", "
     (nth guys-whole-name 0) " "
     (nth guys-whole-name 1)) ; "Steele, Guy Lewis"

(let [[f-name m-name l-name] guys-whole-name]
  (str l-name ", " f-name " " m-name)) ; "Steele, Guy Lewis"

; note: positional destructuring doesn't work on maps and sets because they're
; not logically aligned sequentially. It does work for some Java constructs though

(def date-regex #"(\d{1,2})\/(\d{1,2})\/(\d{4})")

(let [rem (re-matcher date-regex "12/02/1975")]
  (when (.find rem)
    (let [[_ m d] rem]
      {:month m :day d}))) ; {:month "12", :day "02"}

(let [[a b c & more] (range 10)]
  (println "a b c are:" a b c)
  (println "more is:" more))
; a b c are: 0 1 2
; more is: (3 4 5 6 7 8 9)
; nil

(let [range-vec (vec (range 10))
      [a b c & more :as all] range-vec]
  (println "a b c are:" a b c)
  (println "more is:" a b c)
  (println "all is:" all))
; a b c are: 0 1 2
; more is: (3 4 5 6 7 8 9)
; all is: [0 1 2 3 4 5 6 7 8 9]

; Destructuring with a map
(def guys-name-map
  {:f-name "Guy" :m-name "Lewis" :l-name "Steele"})

(let [{f-name :f-name, m-name :m-name, l-name :l-name} guys-name-map]
  (str l-name ", " f-name " " m-name)) ; "Steele, Guy Lewis"

(let [{:keys [f-name m-name l-name]} guys-name-map]
  (str l-name ", " f-name " " m-name)) ; "Steele, Guy Lewis"

(let [{f-name :f-name, :as whole-name} guys-name-map]
  (println "First name is" f-name)
  (println "Whole name is below:")
  whole-name)
; First name is Guy
; Whole name is below:
 ; {:f-name "Guy", :m-name "Lewis", :l-name "Steele"}

(let [{:keys [title f-name m-name l-name],
       :or {title "Mr."}} guys-name-map]
  (println title f-name m-name l-name))
; Mr. Guy Lewis Steele
; nil

; all of the map destructuring features also work on lists, a feature used by
; functions to accept keyword arguments:
(defn whole-name [& args]
  (let [{:keys [f-name m-name l-name]} args]
    (str l-name ", " f-name " " m-name)))

(whole-name :f-name "Guy" :m-name "Lewis" :l-name "Steele") 
; "Steele, Guy Lewis"

; Associative destructuring
; You can also destructure a vector by providing a map declaring the local name
; as indices
(let [{first-thing 0, last-thing 3} [1 2 3 4]]
  [first-thing last-thing]) ; [1 4]

; Destructuring in function parameters
; Each function parameter can destructure a map or sequence
(defn print-last-name [{:keys [l-name]}]
  (println l-name))

(print-last-name guys-name-map) ; Steele

;; REPL
(range 5) ; (0 1 2 3 4)
(for [x (range 2) y (range 2)] [x y]) 
; ([0 0] [0 1] [1 0] [1 1])

(bit-xor 1 2) ; 3

(defn xors [xs ys]
  (for [x (range xs) y (range ys)]
    [x y (rem (bit-xor x y) 256)]))

(xors 2 2) ; ([0 0 0] [0 1 1] [1 0 1] [1 1 0])

(defn f-values [f xs ys]
  (for [x (range xs) y (range ys)]
    [x y (rem (f x y) 256)]))

; The REPL is actually great for experimenting with Java libraries
(def frame (java.awt.Frame.))
frame ; #object[java.awt.Frame 0x653f1382 "java.awt.Frame[frame0,0,54,0x0,invalid,hidden,layout=java.awt.BorderLayout,title=,resizable,normal]"]

(for [meth (.getMethods java.awt.Frame) ; iterate over class methods
      :let [name (.getName meth)] ; bind a variable name
      :when (re-find #"get" name)] ; build a seq of matched names
  name) ; ("setVisible" "isVisible")

(.isVisible frame) ; false
(.setVisible frame true)
(.setSize frame (java.awt.Dimension. 200 200))
(javadoc frame)

(def gfx (.getGraphics frame))

(.fillRect gfx 100 100 50 75)
(.setColor gfx (java.awt.Color. 255 128 0))
(.fillRect gfx 100 150 75 50)

(doseq [[x y xor] (xors 200 200)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))

(doseq [[x y xor] (xors 500 500)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))

(.printStackTrace *e)

(defn clear [g] (.clearRect g 0 0 200 200))
(clear gfx)

(defn draw-values [f xs ys]
  (clear gfx)
  (.setSize frame (java.awt.Dimension. xs ys))
  (doseq [[x y v] (f-values f xs ys)]
          (.setColor gfx (java.awt.Color. v v v))
          (.fillRect gfx x y 1 1)))

(draw-values bit-and 256 256)
(draw-values + 256 256)
(draw-values * 256 256)
