(asdf:defsystem #:skitter.glop
    :description "An event system for games - backed by glop"
    :author "Chris Bagley Baggers <techsnuffle@gmail.com>"
    :license "BSD 2 Clause"
    :serial t
    :depends-on (:skitter :glop)
    :components ((:file "glop/package")
                 (:file "glop/glop")
                 (:file "glop/keys")
                 (:file "glop/mouse-buttons")))
