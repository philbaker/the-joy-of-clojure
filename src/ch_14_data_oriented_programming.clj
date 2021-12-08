(ns ch-14-data-oriented-programming
  (:require [clojure.data]
            [clojure.edn :as edn]
            [clojure.set :as sql]
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

;; #unit/length [1 :km]
;; => 1000

(def time-reader
  (partial convert
           {:sec 1
            :min 60
            :hr [60 :min]
            :day [24 :hr]}))

(binding [*data-readers* {'unit/time #'ch-14-data-oriented-programming/time-reader}]
  (read-string "#unit/time [1 :min 30 :sec]"))
;; => 90

(binding [*default-data-reader-fn* #(-> {:tag %1 :payload %2})]
  (read-string "#nope [:doesnt-exist]"))
;; => {:tag nope, :payload [:doesnt-exist]}
;; Note: the use of *data-readers*, *default-data-reader-fn* and
;; read-string is not recommended for processing data from untrusted
;; sources

;; Handling Clojure's EDN data using clojure.edn
(edn/read-string "#uuid \"dae78a90-d491-11e2-8b8b-0800200c9a66\"")
;; => #uuid "dae78a90-d491-11e2-8b8b-0800200c9a66"

(edn/read-string "42")
;; => 42

(def T {'unit/time #'ch-14-data-oriented-programming/time-reader})
(edn/read-string {:readers T} "#unit/time [1 :min 30 :sec]")
;; => 90

(edn/read-string {:readers T :default vector} "#what/the :huh?")
;; => [what/the :huh?]

;; A simple event-sourced model
{:ab 5
 :h 2
 :avg 0.400}

{:result :hit}

;; Checking the form of an event
(defn valid? [event]
  (boolean (:result event)))

(valid? {})
;; => false

(valid? {:result 42})
;; => true

;; Event-sourcing function that affects state
(defn effect [{:keys [ab h] :or {ab 0 h 0}}
              event]
  (let [ab (inc ab)
        h (if (= :hit (:result event))
            (inc h)
            h)
        avg (double (/ h ab))]
    {:ab ab :h h :avg avg}))

(effect {} {:result :hit})
;; => {:ab 1, :h 1, :avg 1.0}

(effect {:ab 599 :h 180}
        {:result :out})
;; => {:ab 600, :h 180, :avg 0.3}

;; Function that applies an effect only when the event is valid
(defn apply-effect [state event]
  (if (valid? event)
    (effect state event)
    state))

(apply-effect {:ab 600 :h 180 :avg 0.3}
              {:result :hit})
;; => {:ab 601, :h 181, :avg 0.3011647254575707}

;; Event-sourcing, mass-effect function
(def effect-all #(reduce apply-effect %1 %2))
(effect-all {:ab 0 :h 0}
            [{:result :hit}
             {:result :out}
             {:result :hit}
             {:result :out}])
;; => {:ab 4, :h 2, :avg 0.5}

(def events (repeatedly 100
                        (fn []
                          (rand-map 1
                                    #(-> :result)
                                    #(if (< (rand-int 10) 3)
                                       :hit
                                       :out)))))

(effect-all {} events)
;; => {:ab 100, :h 19, :avg 0.19}

(effect-all {} (take 50 events))
;; => {:ab 50, :h 6, :avg 0.12}

(def fx-timeline #(reductions apply-effect %1 %2))

(fx-timeline {} (take 3 events))
;; => ({} {:ab 1, :h 0, :avg 0.0} {:ab 2, :h 0, :avg 0.0} {:ab 3, :h 0, :avg 0.0})

;; Simulation testing
;; Data model for representing baseball player abilities
(def PLAYERS #{{:player "Nick" :ability 32/100}
               {:player "Matt" :ability 26/100}
               {:player "Ryan" :ability 19/100}})

(defn lookup [db name]
  (first (sql/select ;; pretent sql data source
         #(= name (:player %))
         db)))

(lookup PLAYERS "Nick")
;; => {:player "Nick", :ability 8/25}

(defn update-stats [db event]
  (let [player (lookup db (:player event))
        less-db (sql/difference db #{player})]
    (conj less-db
          (merge player (effect player event)))))

(update-stats PLAYERS {:player "Nick" :result :hit})
;; => #{{:player "Matt", :ability 13/50} {:player "Nick", :ability 8/25, :ab 1, :h 1, :avg 1.0} {:player "Ryan", :ability 19/100}}

;; Transactionally applying result events to a data store
(defn commit-event [db event]
  (dosync (alter db update-stats event)))

(commit-event (ref PLAYERS) {:player "Nick" :result :hit})
;; => #{{:player "Matt", :ability 13/50} {:player "Nick", :ability 8/25, :ab 1, :h 1, :avg 1.0} {:player "Ryan", :ability 19/100}}

;; Generating a random baseball event based on player ability
(defn rand-event [{ability :ability}]
  (let [able (numerator ability)
        max (denominator ability)]
    (rand-map 1
              #(-> :result)
              #(if (< (rand-int max) able)
                 :hit
                 :out))))

;; Generating a number of random baseball events
(defn rand-events [total player]
  (take total
        (repeatedly #(assoc (rand-event player)
                            :player
                            (:player player)))))

(rand-events 3 {:player "Nick" :ability 32/100})
;; => ({:result :hit, :player "Nick"} {:result :out, :player "Nick"} {:result :hit, :player "Nick"})
