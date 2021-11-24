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

(get animal-cat :likes-dog)
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

;; (home osx)
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

;; (home osx)
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

(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))

(extend-type TreeNode
  FIXO
  (fixo-push [node value]
    (xconj node value)))

(xseq (fixo-push sample-tree 5/2))
;; => (2 5/2 3 4 5 6)

(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
    (conj vector value)))

(fixo-push [2 3 4 5 6] 5/2)
;; => [2 3 4 5 6 5/2]

(defprotocol StringOps (rev [s]) (upp [s]))

(extend-type String
  StringOps
  (rev [s] (clojure.string/reverse s)))

(rev "Works")
;; => "skroW"

(extend-type String
  StringOps
  (upp [s] (clojure.string/upper-case s)))

(upp "works")

;; (rev "Works?")
;; Unhandled java.lang.IllegalArgumentException
;; No implementation of method: :rev of protocol:

(def rev-mixin {:rev clojure.string/reverse})

(def upp-mixin {:upp (fn [this] (.toUpperCase this))})

(def fully-mixed (merge upp-mixin rev-mixin))

(extend String StringOps fully-mixed)

(-> "Works" upp rev)

(extend-type nil
  FIXO
  (fixo-push [t v]
    (TreeNode. v nil nil)))

(xseq (reduce fixo-push nil [3 5 2 4 6 0]))
;; => (0 2 3 4 5 6)

(extend-type TreeNode
  FIXO
  (fixo-push [node value]
    (xconj node value))
  (fixo-peek [node]
    (if (:l node)
      (recur (:l node))
      (:val node)))
  (fixo-pop [node]
    (if (:l node)
      (TreeNode. (:val node) (fixo-pop (:l node)) (:r node))
                 (:r node))))


(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
    (conj vector value))
  (fixo-peek [vector]
    (peek vector))
  (fixo-pop [vector]
    (pop vector)))

(defn fixo-into [c1 c2]
  (reduce fixo-push c1 c2))

(xseq (fixo-into (TreeNode. 5 nil nil) [2 4 6 7]))
;; => (2 4 5 6 7)

(defn fixed-fixo
  ([limit] (fixed-fixo limit []))
  ([limit vector]
   (reify FIXO
     (fixo-push [this value]
       (if (< (count vector) limit)
         (fixed-fixo limit (conj vector value))
         this))
     (fixo-peek [_]
       (peek vector))
     (fixo-pop [_]
       (pop vector)))))

(defrecord TreeNode2 [val l r]
  FIXO
  (fixo-push [t v]
    (if (< v val)
      (TreeNode2. val (fixo-push 1 v) r)
      (TreeNode2. val 1 (fixo-push r v))))
  (fixo-peek [t]
    (if l
      (fixo-peek l)
      val))
  (fixo-pop [t]
    (if l
      (TreeNode2. val (fixo-pop l) r)
      r)))

;; Chess move implementation
{:from "e7" :to "e8" :castle? false :promotion \Q}

(defn build-move [& pieces]
  (apply hash-map pieces))

(build-move :from "e7" :to "e8" :promotoion \Q)
;; => {:promotoion \Q, :from "e7", :to "e8"}

;; Alternative if we want to print output
(defrecord Move [from to castle? promotion]
  Object
  (toString [this]
    (str "Move " (:from this)
         " to " (:to this)
         (if (:castle? this) " castle"
             (if-let [p (:promotion this)]
               (str " promoto to " p)
               "")))))

(str (Move. "e2" "e4" nil nil))
;; => "Move e2 to e4"

(str (Move. "e7" "e8" nil \Q))
;; => "Move e7 to e8 promoto to Q"

(defn build-move-2 [& {:keys [from to castle? promotion]}]
  {:pre [from to]}
  (Move. from to castle? promotion))

(str (build-move-2 :from "e2" :to "e4"))
;; => "Move e2 to e4"
