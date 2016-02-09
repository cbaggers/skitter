;;;; package.lisp

(defpackage #:skitter
  (:use #:cl #:structy-defclass #:rtg-math)
  (:export :defkind :defcombo :def-event-source
	   :button :apply-button
	   :xy-pos :apply-xy-pos
	   :wheel :apply-wheel
	   :xy-wheel :apply-xy-wheel
	   :mouse :mouse-pos :mouse-button :mouse-wheel
	   :keyboard :keyboard-button
	   :+mice+ :+keyboard+
	   :slow-mouse-button-by-name
	   :slow-keyboard-key-by-name
	   :slow-window-event-by-name))

(defpackage #:skitter-hidden
  (:use #:cl))
