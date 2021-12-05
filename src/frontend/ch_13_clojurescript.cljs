(ns ch-13-clojurescript
  (:require [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.reader :as read]))

(def code-string "(defn hello [x] (js/alert (pr-str 'greetings x)))")
(def code-data (read/read-string code-string))
code-data
;; => (defn hello [x] (js/alert (pr-str 'greetings x)))
