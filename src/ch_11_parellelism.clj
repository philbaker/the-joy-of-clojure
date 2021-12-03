(ns ch-11-parallelism
  (:require (clojure [xml :as xml])
            (clojure [zip :as zip])
            [ch-10-mutation-concurrency :as ch-10 :refer [dothreads!]]
            [clojure.core.reducers :as r])
  (:import (java.util.regex Pattern)))

(time (let [x (future (do (Thread/sleep 5000) (+ 41 1)))]
        [@x @x]))
;; "Elapsed time: 5001.282506 msecs"
;; => [42 42]

(defn feed->zipper [uri-str]
  (->> (xml/parse uri-str)
       zip/xml-zip))

(defn normalize [feed]
  (if (= :feed (:tag (first feed)))
    feed
    (zip/down feed)))

(defn feed-children [uri-str]
  (->> uri-str
       feed->zipper
       normalize
       zip/children
       (filter (comp #{:item :entry} :tag))))

(defn title [entry]
  (some->> entry
           :content
           (some #(when (= :title (:tag %)) %))
           :content
           first))

(defn count-text-task [extractor txt feed]
  (let [items (feed-children feed)
        re    (Pattern/compile (str "(?i)" txt))]
    (->> items
         (map extractor)
         (mapcat #(re-seq re %))
         count)))

(count-text-task
 title
 "Erlang"
 "http://feeds.feedburner.com/ElixirLang")
;; => 0

(count-text-task
 title
 "Elixir"
 "http://feeds.feedburner.com/ElixirLang")
;; => 45

;; Manually spreading tasks over a sequence of futures
(def feeds #{"http://feeds.feedburner.com/ElixirLang"
             "http://blog.fogus.me/feed/"})

(let [results (for [feed feeds]
                (future
                  (count-text-task title "Elixir" feed)))]
  (reduce + (map deref results)))
;; => 45

;; Macro to dispatch a sequence of futures
(defmacro as-futures [[a args] & body]
  (let [parts (partition-by #{'=>} body)
        [acts _ [res]] (partition-by #{:as} (first parts))
        [_ _ task] parts]
    `(let [~res (for [~a ~args] (future ~@acts))]
       ~@task)))

;; Counting text occurences in feed titles fetched in parallel
(defn occurrences [extractor tag & feeds]
  (as-futures [feed feeds]
              (count-text-task extractor tag feed)
              :as results
              =>
              (reduce + (map deref results))))


(occurrences title "released"
             "http://blog.fogus.me/feed/"
             "http://feeds.feedburner.com/ElixirLang"
             "http://www.ruby-lang.org/en/feeds/news.rss")
;; => 31

;; When to use promises
(def x (promise))
(def y (promise))
(def z (promise))

(dothreads! #(deliver z (+ @x @y)))

(dothreads!
 #(do (Thread/sleep 2000 (deliver x 52))))

(dothreads!
 #(do (Thread/sleep 4000 (deliver y 86))))

(time @z)
;; => 138
;; "Elapsed time: 0.204726 msecs"

;; Dispatching a sequence of promises across threads
(defmacro with-promises [[n tasks _ as] & body]
  (when as
    `(let [tasks# ~tasks
           n# (count tasks#)
           promises# (take n# (repeatedly promise))]
       (dotimes [i# n#]
         (dothreads!
          (fn []
            (deliver (nth promises# i#)
                     ((nth tasks# i#))))))
       (let [~n tasks#
             ~as promises#]
         ~@body))))

;; Parallel test runner using with-promises
(defrecord TestRun [run passed failed])

(defn pass [] true)
(defn fail [] false)

(defn run-tests [& all-tests]
  (with-promises
    [tests all-tests :as results]
    (into (TestRun. 0 0 0)
          (reduce #(merge-with + %1 %2) {}
                  (for [r results]
                    (if @r
                      {:run 1 :passed 1}
                      {:run 1 :failed 1}))))))

(run-tests pass fail fail fail pass)
;; => {:run 5, :passed 2, :failed 3}

;; Callback API to blocking API
(defn feed-items [k feed]
  (k
   (for [item (filter (comp #{:entry :item} :tag)
                      (feed-children feed))]
     (-> item :content first :content))))

(feed-items
 count
 "http://blog.fogus.me/feed/")
;; => 5

(let [p (promise)]
  (feed-items #(deliver p (count %))
              "http://blog.fogus.me/feed/")
  @p)
;; => 5

;; Transforming a callback-based function to a blocking call
(defn cps->fn [f k]
  (fn [& args]
    (let [p (promise)]
      (apply f (fn [x] (deliver p (k x))) args)
      @p)))

(def count-items (cps->fn feed-items count))

(count-items "http://blog.fogus.me/feed/")
;; => 5

;; The pvalues macro
;; Returns a lazy sequence of the values of the exprs, which are
;; evaluated in parallel
(pvalues 1 2 (+ 1 2))
;; => (1 2 3)
;; Because the return value is a lazy sequence your access costs might
;; not always present themselves as expected
(defn sleeper [s thing] (Thread/sleep (* 1000 s)) thing)
(defn pvs [] (pvalues
              (sleeper 2 :1st)
              (sleeper 3 :2nd)
              (keyword "3rd")))

(-> (pvs) first time)
;; "Elapsed time: 2001.476007 msecs"
;; => :1st

(-> (pvs) last time)
;; "Elapsed time: 3001.135118 msecs"
;; => :3rd

;; The pmap function
;; The pmap function is the parallel version fo the core map function
;; Given a function and a set of sequences, the application of the
;; function to each matching element happens in parallel
(->> [1 2 3]
     (pmap (comp inc (partial sleeper 2)))
     doall
     time)
;; "Elapsed time: 2003.20952 msecs"
;; => (2 3 4)

;; The pcalls function
;; takes an arbitrary number of functions taking no arguments and
;; calls them in parallel, returning a lazy sequence of all the results
(-> (pcalls
     #(sleeper 2 :first)
     #(sleeper 3 :second)
     #(keyword "3rd"))
    doall
    time)
;; "Elapsed time: 3003.132562 msecs"
;; => (:first :second :3rd)

;; reducer/fold
(def big-vec (vec (range (* 1000 1000))))

(time (reduce + big-vec))
;; => 499999500000
;; "Elapsed time: 53.61318 msecs"

(time (r/fold + big-vec))
;; => 499999500000
;; "Elapsed time: 51.632624 msecs"
