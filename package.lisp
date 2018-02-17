;;;; package.lisp

(uiop:define-package #:skitter-hidden
  (:use #:cl))

(uiop:define-package #:skitter.internals
    (:use #:cl #:structy-defclass #:rtg-math)
  (:export
   ;; controls
   :boolean-control :make-boolean-control
   :symbol-control :make-symbol-control

   :float-control :make-float-control
   :vec2-control :make-vec2-control
   :ivec2-control :make-ivec2-control
   :uvec2-control :make-uvec2-control

   :float-decaying-control :make-float-decaying-control
   :vec2-decaying-control :make-vec2-decaying-control
   :ivec2-decaying-control :make-ivec2-decaying-control
   :uvec2-decaying-control :make-uvec2-decaying-control

   ;; sources
   :define-input-source :define-control :initialize-kind :add))

(uiop:define-package #:skitter
    (:use #:cl #:structy-defclass #:rtg-math :skitter.internals)
  (:import-from :alexandria :with-gensyms)
  (:export :make-event-listener :listen-to :stop-listening :whilst-listening-to
           :define-logical-control :remove-control
           ;;
           :+mice+ :mouse :make-mouse
           :mouse-pos :set-mouse-pos
           :mouse-move :set-mouse-move
           :mouse-button :set-mouse-button
           :mouse-wheel :set-mouse-wheel

           :+gamepads+ :gamepad :make-gamepad
           :gamepad-button :set-gamepad-button
           :gamepad-1d :set-gamepad-1d
           :gamepad-2d :set-gamepad-2d

           :+keyboard+ :keyboard :make-keyboard
           :keyboard-button :set-keyboard-button

           :+window-manager+ :window-manager :make-window-manager
           :window-manager-quitting :set-window-manager-quitting

           :+windows+ :window :make-window
           :window-pos :set-window-pos
           :window-size :set-window-size
           :window-closing :set-window-closing
           :window-layout :set-window-layout
           ;;
           :key-down-p :key-id
           :mouse-down-p :mouse-button-id
           ;;
           :decay-events))
