(ns ch-14-data-oriented-programming
  (:require [clojure.data]
            [ch-07 :refer [convert]]))

;; Values can be reproduced and fabricated
(rand-int 1024)
;; => 846

(+ (rand-int 100) (rand-int 100))
;; => 131

;; Generating a more complex structure isn't difficult
(def ascii (map char (range 65 (+ 65 26))))

(defn rand-str [sz alphabet]
  (apply str (repeatedly sz #(rand-nth alphabet))))

(rand-str 10 ascii)
;; => "MNFWNGJLTM"

(def rand-sym #(symbol (rand-str %1 %2)))
(def rand-key #(keyword (rand-str %1 %2)))

(rand-key 10 ascii)
;; => :XNQTPCBRMP

(rand-sym 10 ascii)
;; => QXBTTUXJQF

;; You can use generators to build composite structures like vectors
(defn rand-vec [& generators]
  (into [] (map #(%) generators)))

(rand-vec #(rand-sym 5 ascii)
          #(rand-key 10 ascii)
          #(rand-int 1024))
;; => [XFSKH :MKLNSUSLPL 700]

;; And even maps
(defn rand-map [sz kgen vgen]
  (into {}
        (repeatedly sz #(rand-vec kgen vgen))))

(rand-map 3 #(rand-key 5 ascii) #(rand-int 100))
;; => {:HXRDH 9, :GZIOV 84, :QKMDV 67}

;; Values facilitate testing
(assert (= [1 2 3] (conj [1 2] 3)))
;; => nil

(clojure.data/diff [1 2 3] [1 2 4])
;; => [[nil nil 3] [nil nil 4] [1 2]]

;; Values facilitate debugging
(defn filter-rising [segments]
  (clojure.set/select
   (fn [{:keys [p1 p2]}]
     (> 0
        (/ (- (p2 0) (p1 0))
           (- (p2 1) (p1 1)))))
   segments))

(filter-rising #{{:p1 [0 0] :p2 [1 1]}
                 {:p1 [4 15] :p2 [3 21]}})
;; => #{{:p1 [4 15], :p2 [3 21]}}

;; Values are language independent

;; Tagged literals
#inst "1969-08-18"
;; => #inst "1969-08-18T00:00:00.000-00:00"

;; Function to read distance units
(def distance-reader
  (partial convert
           {:m 1
            :km 1000
            :cm 1/100
            :mm [1/10 :cm]}))
