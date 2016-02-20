;;;; package.lisp

(defpackage #:skitter
  (:use #:cl #:structy-defclass #:rtg-math #:temporal-functions)
  (:export :def-input-kind :def-event-source :def-combo-source :defcombo
	   :initialize-kind
	   :add
	   :listen-to :stop-listening :whilst-listening-to
	   ;;
	   :state :make-state :state-is :apply-state
           :button :make-button :apply-button :button-down-p
           :xy-pos :make-xy-pos :apply-xy-pos :xy-pos-vec :xy-pos-relative
           :wheel :make-wheel :apply-wheel :wheel-val
           :xy-wheel :make-xy-wheel :apply-xy-wheel :xy-wheel-vec
	   :pos-2d :make-pos-2d :pos-2d-vec :apply-pos-2d
	   :size-2d :make-size-2d :size-2d-vec :apply-size-2d
	   :layout :make-layout :layout-state :apply-layout
	   ;;
           :+mice+ :mouse :make-mouse :mouse-pos :mouse-button :mouse-wheel
           :+keyboard+ :keyboard :make-keyboard :keyboard-button
	   :+system+ :system :make-system :system-quitting
	   :+windows+ :window :make-window
	   :window-pos :window-size :window-closing :window-layout
	   ;;
	   :key-down-p :key-id
	   :mouse-down-p :mouse-button-id))

(defpackage #:skitter-hidden
  (:use #:cl))
