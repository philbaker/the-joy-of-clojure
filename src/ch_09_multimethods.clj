(ns ch-09-multimethods
  (:refer-clojure :exclude [get]))

;; Universal Design Pattern

;; Take a map and associate its prototype reference to another map,
;; returning a new map
(defn beget [this proto]
  (assoc this ::prototype proto))

(beget {:sub 0} {:super 1})
;; => {:sub 0, :ch-09-multimethods/prototype {:super 1}}

;; Whenever a value isn't found in a given map, the prototype chain
;; is followed to the end
(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))

(get (beget {:sub 0} {:super 1})
     :super)
;; => 1

;; The put function takes a key and an associated value and puts them
;; into the supplied map, overwriting any existing key of the same
;; name
(def put assoc)

;; Basic use of UDP
(def animal-cat {:likes-dogs true :ocd-bathing true})
(def morris (beget {:likes-9lives true} animal-cat))
(def post-traumatic-morris (beget {:likes-dogs nil} morris))

(get animal-cat :likes-dogs)
;; => true

(get morris :likes-dogs)
;; => nil

(get post-traumatic-morris :likes-dogs)
;; => nil

(get post-traumatic-morris :likes-9lives)
;; => true

;; Prototypal object system via multimethods
(defmulti compiler :os)
(defmethod compiler ::unix [m] (get m :c-compiler))
(defmethod compiler ::osx  [m] (get m :llvm-compiler))

(def clone (partial beget {}))
(def unix {:os ::unix :c-compiler "cc" :home "/home" :dev "/dev" })
(def osx (-> (clone unix)
             (put :os ::osx)
             (put :llvm-compiler "clang")
             (put :home "/Users")))

(compiler unix)
;; => "cc"

(compiler osx)
;; => "clang"

(defmulti home :os)
(defmethod home ::unix [m] (get m :home))

(home unix)
;; => "/home"

(home osx)
;; Unhandled java.lang.IllegalArgumentException
;; No method in multimethod 'home' for dispatch value:
;; :ch-09-multimethods/osx

(derive ::osx ::unix)

(home osx)
;; => "/Users"

(parents ::osx)
;; => #{:ch-09-multimethods/unix}

(ancestors ::osx)
;; => #{:ch-09-multimethods/unix}

(descendants ::unix)
;; => #{:ch-09-multimethods/osx}

(isa? ::osx ::unix)
;; => true

(isa? ::unix ::osx)
;; => false

;; Resolving conflict in hierarchies
(derive ::osx ::bsd)
(defmethod home ::bsd [m] "/home")

(home osx)
;; Unhandled java.lang.IllegalArgumentException
;; Multiple methods in multimethod 'home' match dispatch value:
;; :ch-09-multimethods/osx -> :ch-09-multimethods/unix and
;; :ch-09-multimethods/bsd, and neither is preferred

(prefer-method home ::unix ::bsd)
(home osx)
;; => "/Users"

(remove-method home ::bsd)
(home osx)
;; => "/Users"

(derive (make-hierarchy) ::osx ::unix)
;; => {:parents #:ch-09-multimethods{:osx #{:ch-09-multimethods/unix}},
;;     :ancestors #:ch-09-multimethods{:osx #{:ch-09-multimethods/unix}},
;;     :descendants #:ch-09-multimethods{:unix #{:ch-09-multimethods/osx}}}

;; Arbitrary dispatch
(defmulti compile-cmd (juxt :os compiler))

(defmethod compile-cmd [::osx "gcc"] [m]
  (str "/usr/bin" (get m :c-compiler)))

(defmethod compile-cmd :default [m]
  (str "Unsure where to locate " (get m :c-compiler)))

(compile-cmd osx)
;; => "Unsure where to locate cc"

(compile-cmd unix)
;; => "Unsure where to locate cc"

;; Juxt
;; juxt takes a bunch of functions and composes them into a function
;; returning a vector of its argument(s) applied to each given function
(def each-math (juxt + * - /))
(each-math 2 3)
;; => [5 6 -1 2/3]

((juxt take drop) 3 (range 9))
;; => [(0 1 2) (3 4 5 6 7 8)]

;; Records
(defrecord TreeNode [val l r])
(TreeNode. 5 nil nil)
;; => #ch_09_multimethods.TreeNode{:val 5, :l nil, :r nil}

;; Persistent binary tree built from records
(defn xconj [t v]
  (cond
    (nil? t)       (TreeNode. v nil nil)
    (< v (:val t)) (TreeNode. (:val t) (xconj (:l t) v) (:r t))
    :else          (TreeNode. (:val t) (:l t) (xconj (:r t) v))))

(defn xseq [t]
  (when t
    (concat (xseq (:l t)) [(:val t)] (xseq (:r t)))))

(def sample-tree (reduce xconj nil [3 5 2 4 6]))
(xseq sample-tree)
;; => (2 3 4 5 6)

(dissoc (TreeNode. 5 nil nil) :1)
;; => #ch_09_multimethods.TreeNode{:val 5, :l nil, :r nil}
