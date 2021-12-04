(ns ch-12-java-next
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.sun.net.httpserver HttpHandler HttpExchange HttpServer]
           [java.net InetSocketAddress URLDecoder URI]
           [java.io File FilterOutputStream]
           [java.util Comparator Collections ArrayList]
           [java.util.concurrent FutureTask]))

(def OK java.net.HttpURLConnection/HTTP_OK)

(defn respond
  ([exchange body]
   (respond identity exchange body))
  ([around exchange body]
   (.sendResponseHeaders exchange OK 0)
   (with-open [resp (around (.getResponseBody exchange))]
     (.write resp (.getBytes body)))))

;; Simplest possible web server using Java
(defn new-server [port path handler]
  (doto
   (HttpServer/create (InetSocketAddress. port) 0)
    (.createContext path handler)
    (.setExecutor nil)
    (.start)))

(defn default-handler [txt]
  (proxy [HttpHandler]
      []
    (handle [exchange]
      (respond exchange txt))))

(def server
  (new-server
   8213
   "/joy/hello"
   (default-handler "Hello Cleveland")))

(.stop server 0)

(def p (default-handler
        "There's no problem that can't be solved with another level of indirection"))

(def server (new-server 8213 "/" p))

(update-proxy p
              {"handle" (fn [this exchange]
                          (respond exchange (str "this is " this)))})

;; Web handler that echoes the request header
(def echo-handler
  (fn [_ exchange]
    (let [headers (.getRequestHeaders exchange)]
      (respond exchange (prn-str headers)))))

(update-proxy p {"handle" echo-handler})

(defn html-around [o]
  (proxy [FilterOutputStream]
      [o]
    (write [raw-bytes]
      (proxy-super write
                   (.getBytes (str "<html><body>"
                                   (String. raw-bytes)
                                   "</body></html>"))))))

(defn listing [file]
  (-> file .list sort))

(listing (io/file "."))
;; => (".clj-kondo"
;;     ".cpcache"
;;     ".git"
;;     ".gitignore"
;;     ".lsp"
;;     ".nrepl-port"
;;     "deps.edn"
;;     "dev"
;;     "src")

(listing (io/file "./README.md"))
;; => ()

;; Quick and dirty function to generate HTML file listings
(defn html-links [root filenames]
  (string/join
   (for [file filenames]
     (str "<a href='"
          (str root
               (if (= "/" root)
                 ""
                 File/separator)
               file)
          "'>"
          file "</a><br>"))))

(html-links "." (listing (io/file ".")))
;; => "<a href='./.clj-kondo'>.clj-kondo</a><br><a href='./.cpcache'>.cpcache</a><br><a href='./.git'>.git</a><br><a href='./.gitignore'>.gitignore</a><br><a href='./.lsp'>.lsp</a><br><a href='./.nrepl-port'>.nrepl-port</a><br><a href='./deps.edn'>deps.edn</a><br><a href='./dev'>dev</a><br><a href='./src'>src</a><br>"

;; Function to build a string representation of a file-size listing
(defn details [file]
  (str (.getName file) " is "
       (.length file) " bytes."))

(details (io/file "./deps.edn"))
;; => "deps.edn is 180 bytes."

;; Function to convert a relative-path URI into a file
(defn uri->file [root uri]
  (->> uri
       str
       URLDecoder/decode
       (str root)
       io/file))

(uri->file "." (URI. "/deps.edn"))

(details (uri->file "." (URI. "/deps.edn")))
;; => "deps.edn is 180 bytes."

;; Web handler to list and navigate a local file system
(def fs-handler
  (fn [_ exchange]
    (let [uri (.getRequestURI exchange)
          file (uri->file "." uri)]
      (if (.isDirectory file)
        (do (.add (.getResponseHeaders exchange)
                  "Content-Type" "text/html")
            (respond html-around
                     exchange
                     (html-links (str uri) (listing file))))
        (respond exchange (details file))))))

(update-proxy p {"handle" fs-handler})

;; Java arrays
(doto (StringBuilder. "abc")
  (.append (into-array [\x \y \z])))
;; => #object[java.lang.StringBuilder 0x5fd9c5d9 "abc[Ljava.lang.Character;@26fd15f7"]

(doto (StringBuilder. "abc")
  (.append (char-array [\x \y \z])))
;; => #object[java.lang.StringBuilder 0x452a5fc0 "abcxyz"]

(let [ary (make-array Long/TYPE 3 3)]
  (dotimes [i 3]
    (dotimes [j 3]
      (aset ary i j (+ i j))))
  (map seq ary))
;; => ((0 1 2) (1 2 3) (2 3 4))

(into-array Integer/TYPE [1 2 3])
;; => [1, 2, 3]

;; Creating reference arrays
;; To intentionally create an array of a particular reference type,
;; or of compatible types, use the into-array function, passing
;; a sequence of objects
(into-array ["a" "b" "c"])
;; => ["a", "b", "c"]

(into-array [(java.util.Date.) (java.sql.Time. 0)])
;; => [#inst "2021-12-03T18:25:46.675-00:00",
;;     #inst "1970-01-01T00:00:00.000-00:00"]

(into-array ["a" "b" 1M])
;; 1. Unhandled java.lang.IllegalArgumentException
;;    array element type mismatch

(into-array Number [1 2.0 3M 4/5])
;; => [1, 2.0, 3M, 4/5]

;; To create a heterogenous array of java.lang.Object use to-array
;; or to-array-2d function
(to-array-2d [[1 2 3
               4 5 6]])
;; => [[1, 2, 3, 4, 5, 6]]

(to-array ["a" 1M #(%) (proxy [Object] [])])
;; => ["a", 1M, #function[ch-12-java-next/eval13642/fn--13643],
;;     #object[ch_12_java_next.proxy$java.lang.Object$ff19274a 0x28cd90e8 "ch_12_java_next.proxy$java.lang.Object$ff19274a@28cd90e8"]]

(to-array [1 (int 2)])
;; => [1, 2]

;; Array mutability - because JVM arrays are mutable, you need to be
;; aware that their contents can change at any point
(def ary (into-array [1 2 3]))
(def sary (seq ary))
sary
;; => (1 2 3)

(aset ary 0 42)
sary
;; => (42 2 3)

(defn asum-sq [xs]
  (let [dbl (amap xs i ret
                  (* (aget xs i)
                     (aget xs i)))]
    (areduce dbl i ret 0
             (+ ret (aget dbl i)))))

(asum-sq (double-array [1 2 3 4 5]))
;; => 55.0

;; Array types
(defmulti what-is class)

(defmethod what-is
  (Class/forName "[Ljava.lang.String;")
  [_]
  "1d String")

(defmethod what-is
  (Class/forName "[Ljava.lang.Object;")
  [_]
  "2d Object")

(defmethod what-is
  (Class/forName "[[[[I")
  [_]
  "Primitive 4d int")

(defmethod what-is
  (Class/forName "[[D")
  [_]
  "Primitive 2d double")

(defmethod what-is
  (Class/forName "[Lclojure.lang.PersistentVector;")
  [_]
  "1d Persistent Vector")

(what-is (into-array ["a" "b"]))
;; => "1d String"

(what-is (to-array-2d [[1 2] [3 4]]))
;; => "2d Object"

(what-is (make-array Integer/TYPE 2 2 2 2))
;; => "Primitive 4d int"

(what-is (into-array [[1.0] [2.0]]))
;; => "2d Object"

(what-is (into-array (map double-array [[1.0] [2.0]])))
;; => "Primitive 2d double"

(String/format "An int %d and a String %s"
               (to-array [99, "luftballons"]))
;; => "An int 99 and a String luftballons"

(ancestors (class #()))
;; => #{java.lang.Runnable java.util.Comparator java.io.Serializable
;;      clojure.lang.AFn clojure.lang.Fn clojure.lang.IObj java.lang.Object
;;      clojure.lang.AFunction clojure.lang.IFn
;;      java.util.concurrent.Callable clojure.lang.IMeta}


;; The java.util.Comparator interface
(defn gimme [] (ArrayList. [1 3 4 8 2]))
(doto (gimme)
  (Collections/sort (Collections/reverseOrder)))
;; => [8 4 3 2 1]

(doto (gimme)
  (Collections/sort
   (reify Comparator
     (compare [this l r]
       (cond
         (> l r) -1
         (= l r) 0
         :else 1)))))
;; => [8 4 3 2 1]

(doto (gimme) (Collections/sort #(compare %2 %1)))
;; => [8 4 3 2 1]

(doto (gimme) (Collections/sort >))
;; => [8 4 3 2 1]

(doto (gimme) (Collections/sort <))
;; => [1 2 3 4 8]

(doto (gimme) (Collections/sort (complement <)))
;; => [8 4 3 2 1]

;; The java.lang.Runnable interface
(doto (Thread. #(do (Thread/sleep 5000)
                    (println "haikeeba!")))
  .start)
;; => #object[java.lang.Thread 0x50cea6f6 "Thread[Thread-287,5,main]"]
;; haikeeba!

;; The java.util.concurrent.Callable interface
(let [f (FutureTask. #(do (Thread/sleep 5000) 42))]
      (.start (Thread. #(.run f)))
      (.get f))
;; => 42

;; Using Clojure data structures in Java APIs
;; The java.util.List interface
(.get '[a b c] 1) ;; vectors
;; => b

(.get (repeat :a) 138) ;; lazy seqs
;; => :a

(.containsAll '[a b c] '[b c]) ;; vectors are collections
;; => true

(.add '[a b c] 'd) ;; sequences aren't mutable
;; Unhandled java.lang.UnsupportedOperationException

(java.util.Collections/sort [3 4 2 1])
;; Unhandled java.lang.UnsupportedOperationException

;; The java.lang.Comparable interface
;; Comparable is the cousin of the Comparator interface. Comparator
;; refers to objects that can compare two other objects, whereas
;; Comparable refers to an object tha tcan compare itself to another
;; object
(.compareTo [:a] [:a])
;; => 0

(.compareTo [:a :b] [:a])
;; => 1

(.compareTo [:a :b] [:a :b :c])
;; => -1

(sort [[:a :b :c] [:a] [:a :b]])
;; => ([:a] [:a :b] [:a :b :c])

(.compareTo [1 2 3] '(1 2 3))
;; Unhandled java.lang.ClassCastException
;; class clojure.lang.PersistentList cannot be cast to class
;; clojure.lang.IPersistentVector

(.get '[a b c] 2)
;; => c

;; The java.util.Collection interface
(defn shuffle [coll]
  (seq (doto (java.util.ArrayList. coll)
         java.util.Collections/shuffle)))

(shuffle (range 10))
;; => (6 4 0 7 9 2 8 3 5 1)

;; The Java.Util.Map interface
(java.util.Collections/unmodifiableMap
 (doto (java.util.HashMap.) (.put :a 1)))
;; => {:a 1}

(into {} (doto (java.util.HashMap.) (.put :a 1)))
;; => {:a 1}

;; The java.util.Set interface
;; In Java and Clojure sets the use of mutable objects as elements
;; is highly frowned upon

(def x (java.awt.Point. 0 0))
(def y (java.awt.Point. 0 42))
(def points #{x y})
points
;; => #{#object[java.awt.Point 0x4915a4b7 "java.awt.Point[x=0,y=0]"]
;;      #object[java.awt.Point 0x6b313289 "java.awt.Point[x=0,y=42]"]}

(.setLocation y 0 0)

points
;; => #{#object[java.awt.Point 0x4915a4b7 "java.awt.Point[x=0,y=0]"]
;;      #object[java.awt.Point 0x6b313289 "java.awt.Point[x=0,y=0]"]}

;; Interface definining a sliceable object
(definterface ISliceable
  (slice [^long s ^long e])
  (^long sliceCount []))
;; => ch_12_java_next.ISliceable

(def dumb
  (reify ISliceable
    (slice [_ s e] [:empty])
    (sliceCount [_] 42)))

(.sliceCount dumb)
;; => 42

(defprotocol Sliceable
  (slice [this s e])
  (sliceCount [this]))

(extend ISliceable
  Sliceable
  {:slice (fn [this s e] (.slice this s e))
   :sliceCount (fn [this] (.sliceCount this))})

(sliceCount dumb)
;; => 42

(slice dumb 0 0)
;; => [:empty]

;; Extending strings along the Sliceable protocol
(defn calc-slice-count [thing]
  "Calculates the number of possible slices using the formula:
    (n + r - 1)!
    -----------
    r!(n - 1)!
   where n is (count thing) and r is 2"
  (let [! #(reduce * (take % (iterate inc 1)))
        n (count thing)]
    (/ (! (- (+ n 2) 1))
       (* (! 2) (! (- n 1))))))

(extend-type String
  Sliceable
  (slice [this s e] (.substring this s (inc e)))
  (sliceCount [this] (calc-slice-count this)))

(slice "abc" 0 1)
;; => "ab"

(sliceCount "abc")
;; => 6

;; Be wary of exceptions
;; When writing Clojure code, use erros to mean can't continue and
;; exceptions to mean can or might continue

;; Runtime exceptions
(defn explode [] (explode))
(try (explode) (catch Exception e "Stack is blown"))
;; Unhandled java.lang.StackOverflowError
(try (explode) (catch StackOverflowError e "Stack is blown"))
;; => "Stack is blown"
(try (explode) (catch Error e "Stack is blown"))
;; => "Stack is blown"
(try (explode) (catch Throwable e "Stack is blown"))
;; => "Stack is blown"
(try (throw (RuntimeException.))
     (catch Throwable e "Catching Throwable is Bad"))
;; => "Catching Throwable is Bad"

;; Compile time exceptions
(defmacro do-something [x] `(~x))
(do-something 1)
;; Unhandled java.lang.ClassCastException
;; class java.lang.Long cannot be cast to class clojure.lang.IFn

(defmacro pairs [& args]
  (if (even? (count args))
    `(partition 2 '~args)
    (throw (Exception.
            (str "pairs requires an even number of args")))))

(pairs 1 2 3)
;; Unhandled clojure.lang.Compiler$CompilerException
;; Caused by java.lang.Exception
;; pairs requires an even number of args

(pairs 1 2 3 4)
;; => ((1 2) (3 4))

(fn [] (pairs 1 2 3))
;; Unhandled clojure.lang.Compiler$CompilerException
;; Caused by java.lang.Exception
;; pairs requires an even number of args

;; Handling exceptions
(defmacro -?> [& forms]
  `(try (-> ~@forms)
        (catch NullPointerException _# nil)))

(-?> 25 Math/sqrt (+ 100))
;; => 105.0

(-?> 25 Math/sqrt (and nil) (+ 100))
;; => nil

;; Custom exceptions
;; The idiom for Clojure is to throw derivatives of RunTimeException or
;; Error rather than its own custom exceptions

(defn perform-unclean-act [x y]
  (/ x y))

(try
  (perform-unclean-act 42 0)
  (catch RuntimeException ex
    (println (str "Something went wrong."))))
;; Something went wrong.

(defn perform-cleaner-act [x y]
  (try
    (/ x y)
    (catch ArithmeticException ex
      (throw (ex-info "You attempted an unclean act"
                      {:args [x y]})))))

(try
  (perform-cleaner-act 108 0)
  (catch RuntimeException ex
    (println (str "Received error: " (.getMessage ex)))
    (when-let [ctx (ex-data ex)]
      (println (str "More information: " ctx)))))
;; Received error: You attempted an unclean act
;; More information: {:args [108 0]}
