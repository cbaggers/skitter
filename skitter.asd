;;;; skitter.asd

(asdf:defsystem #:skitter
  :description "A GC'able event system for games"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:trivial-garbage)
  :components ((:file "package")
	       (:file "base-types")
	       (:file "event-nodes")
               (:file "named-event-nodes")))
