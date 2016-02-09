(in-package :skitter)

;; to define a combo
(defcombo boom ((m mouse) (g gamepad) (k keyboard))
  (then (and (keyboard-button k :lctrl)
             (keyboard-button k :c))
        (before 200 (keboard-button k :a))
        (before 100 (gamepad-button g 0))))

;; to get the combo listener
(make-boom :m some-mouse :g gpad :k kbd)
