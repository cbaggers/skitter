(in-package skitter.sdl2.mouse-buttons)

(defun mouse.button-id (name/event)
  (etypecase name/event
    (keyword
     (or (position name/event skitter.sdl2::*mouse-button-names*)
         (error "mouse.button-id: invalid name ~s" name/event)))

    (t (error "mouse.button-id: Must be given a keyword name or an instance of the button event.~%Recieved ~s"
              name/event))))

(defconstant mouse.left 1)
(defconstant mouse.middle 2)
(defconstant mouse.right 3)
(defconstant mouse.other0 4)
(defconstant mouse.other1 5)
(defconstant mouse.other2 6)
(defconstant mouse.other3 7)
(defconstant mouse.other4 8)
