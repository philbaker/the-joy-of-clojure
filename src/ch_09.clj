(ns ch-09)

(in-ns 'ch-08)

(def authors ["Chouser"])

(in-ns 'ch-09)

(clojure.core/refer 'ch-08)

ch-08/authors
; => ["Chouser"]

(in-ns 'ch-08)
(def authors ["Chouser" "Fogus"])

(in-ns 'ch-09)
ch-08/authors
; => ["Chouser" "Fogus"]

(ns chimp)
(reduce + [1 2 (Integer. 3)])
; => 6

; create-ns
(def b (create-ns 'bonobo))
b
; => #namespace[bonobo]

((ns-map b) 'String)
; => java.lang.String

(intern b 'x 9)
;; => #'bonobo/x

(intern b 'reduce clojure.core/reduce)
; => #'bonobo/reduce

(intern b '+ clojure.core/+)
; => #'bonobo/+

(reduce + [1 2 3 4 5])
; => 15

(get (ns-map 'bonobo) 'reduce)
; => #'bonobo/reduce

(ns-unmap 'bonobo 'reduce)
; => nil

(get (ns-map 'bonobo) 'reduce)
; => nil

(remove-ns 'bonobo)
; => #namespace[bonobo]

(all-ns)
; => (#namespace[cider.nrepl.inlined-deps.toolsreader.v1v3v6.clojure.tools.reader.impl.commons]...
