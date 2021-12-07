(ns ch-13-clojurescript
  (:require [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.reader :as read]
            [clojure.walk :refer [prewalk]]
            [clojure.pprint :refer [pprint]]))

(def code-string "(defn hello [x] (js/alert (pr-str 'greetings x)))")
(def code-data (read/read-string code-string))
code-data
;; => (defn hello [x] (js/alert (pr-str 'greetings x)))

(first code-string)
;; => "("

(first code-data)
;; => defn

(def ast (ana/analyze (ana/empty-env) code-data))

(defn print-ast [ast]
  (pprint
   (prewalk
    (fn [x]
      (if (map? x)
        (select-keys x [:children :name :form :op])
        x))
    ast)))

(print-ast ast)
