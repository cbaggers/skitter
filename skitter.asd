;;;; skitter.asd

(asdf:defsystem #:skitter
    :description "An event system for games"
    :author "Chris Bagley Baggers <techsnuffle@gmail.com>"
    :license "BSD 2 Clause"
    :serial t
    :depends-on (:structy-defclass :grab-bag :rtg-math :temporal-functions)
    :components ((:file "package")
		 (:file "helpers")
		 (:file "hidden")
		 (:file "core")
		 (:file "common")))
