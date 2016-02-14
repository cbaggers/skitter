(in-package :skitter)

;; to define a combo
(defcombo boom ((m mouse) (g gamepad) (k keyboard))
  (then (and (keyboard-button k :lctrl)
             (keyboard-button k :c))
        (before 200 (keboard-button k :a))
        (before 100 (gamepad-button g 0))))

;; to get the combo listener
(make-boom :m some-mouse :g gpad :k kbd)


(defun make-boom (m g k)
  (tlambda ()
    (then (until (and (keyboard-button k :lctrl)
		      (keyboard-button k :c)))
	  (before 200
	    (if (keboard-button k :a)
		(skip-step)
		???????????))
	  (before 100
	    (if (gamepad-button g 0)
		(boom!)
		???????????)))))

(defun boom! ()
  (print "BOOM!"))

;; ah so it turns out the problem is that the state machien for input need to be
;; based on the timestamps, but temporal-functions are based on real-time.
;; Lets just make a macro that is the syntactic dual of temporal functions

(defcombo boom (evt &source (m mouse) (g gamepad) (k keyboard))
  (and (keyboard-button k :lctrl)
       (keyboard-button k :c))
  (before 200 (keboard-button k :a))
  (before 100 (gamepad-button g 0)))

(defun make-boom (evt)
  (let ((step 0))
    (lambda (evt)
      (tagbody top
	 (case= step
	   (0 ())
	   (1 ())
	   (2 ()))))))

;; - ok so we need to know every event for every input
;; - but wait what about mouse button combos...we dont want to be disturbed by
;;   mouse movement...Ok os maybe we need to specify the input and the source.
;;   that means we need:
;; - the source..

;; Cant we just have:
;;
;; :step-predicate
;; :reset-predicate
;;
;; for each step?
;; then we need to see every event..thing is that we have 'apply' instead
;; of 'true' events

(defcombo boom (evt &source (m mouse) (g gamepad) (k keyboard))
  :step (and (keyboard-button k :lctrl)
	     (keyboard-button k :c))
  :reset (any other k g event or any m-button event)

  :step (before 200 (keboard-button k :a))
  :reset (expire or any other k g event or any m-button event )

  :step (before 100 (gamepad-button g 0))
  :reset (expire or any other k g event or any m-button event ))

;; remeber that each time an apply happens, the whole chain propagates.
;; So any depenmdent combo will see the event (with timestamp).
;; So how does the combo

(defmacro resetting-when (condition &body body))

(defcombo boom (evt &source (m mouse :button) (k keyboard :button))
  ((slots))
  (resetting-when ("any other" k g "event or any" m-button "event")
    (and (keyboard-button k :lctrl)
	 (keyboard-button k :c)))

  (resetting-when ("expire or any other" k g "event or any" m-button "event")
    (before 200 (keboard-button k :a)))

  (resetting-when ("expire or any other" k g "event or any" m-button "event")
    (before 100 (keyboard-button k 0))))
