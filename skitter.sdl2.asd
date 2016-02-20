(asdf:defsystem #:skitter.sdl2
    :description "An event system for games - backed by sdl2"
    :author "Chris Bagley Baggers <techsnuffle@gmail.com>"
    :license "BSD 2 Clause"
    :serial t
    :depends-on (:skitter :sdl2)
    :components ((:file "sdl2/package")
		 (:file "sdl2/sdl2")
		 (:file "sdl2/keys")
		 (:file "sdl2/mouse-buttons")))
