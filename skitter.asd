;;;; skitter.asd

(asdf:defsystem #:skitter
  :description "An event system for games"
  :author "Chris Bagley Baggers <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:structy-defclass :rtg-math)
  :components ((:file "package")
               ;; internals
               (:file "hidden")
               (:file "utils")
               (:file "control")
               (:file "input-source")
               (:file "predicate-source")
               ;; (:file "combo")
               (:file "listener")
               ;; universal controls & sources
               (:file "common")))
