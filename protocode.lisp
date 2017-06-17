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
;; So how does the combo..

;; trying again

(defcombo boom (evt &source (m mouse button) (key keyboard button)) ()
  (or (and (print 1)
           (or (print (key-p 0))
               (key-p x))
           (button-down-p evt))
      (progn (print "reset") (reset)))

  (or (and (or (key-p 0)
               (key-p key.rctrl))
           (not (button-down-p evt))
           (print 2))
      (reset))

  (or (and (before 200)
           (key-p key.a)
           (button-down-p evt))
      (reset))

  (or (and (before 200)
           (key-p key.a)
           (button-down-p evt))
      (reset))

  (or (and (before 100)
           (button-down-p evt))
      (reset)))


;; ok so now, how shall we use this?
;; most common will be basic input, keys bound to actions
;; need to check if a key is down

(defcombo alias (evt &source (key keyboard button))
    ((index (get-index-by-name 'keyboard 'button :a) fixnum))
  (when ())
  (button-down-p evt))
;; nah

(defun key-watcher (name &optional keyboard)
  (let ((index (get-index-by-name 'keyboard :button name)))
    (if keyboard
        (lambda () (button-down-p (keyboard-button keyboard index)))
        (lambda (keyboard) (button-down-p (keyboard-button keyboard index))))))
;; nah


(defun key-down-p (index &optional (keyboard (keyboard 0)))
  (button-down-p (keyboard-button keyboard index)))

(defun key-id (name)
  (get-index-by-name 'keyboard :button name))

;; well this is more sane

;; get-index-by-name is stupid, it still ends up backend specific so just use
;; constants
