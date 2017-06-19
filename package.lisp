;;;; package.lisp

(uiop:define-package #:skitter-hidden
  (:use #:cl))

(uiop:define-package #:skitter.internals
    (:use #:cl #:structy-defclass #:rtg-math #:%rtg-math)
  (:export
   ;; controls
   :position2 :make-position2
   :iposition2 :make-iposition2
   :uposition2 :make-uposition2
   :relative2 :make-relative2
   :irelative2 :make-irelative2
   :urelative2 :make-urelative2
   :size2 :make-size2
   :isize2 :make-isize2
   :usize2 :make-usize2
   :wheel :make-wheel
   :wheel2 :make-wheel2
   :boolean-state :make-boolean-state
   :layout :make-layout
   ;; sources
   :define-input-source :define-control :initialize-kind :add))

(uiop:define-package #:skitter
    (:use #:cl #:structy-defclass #:rtg-math #:%rtg-math
          :skitter.internals)
  (:import-from :alexandria :with-gensyms)
  (:export :make-event-listener :listen-to :stop-listening :whilst-listening-to
           :define-logical-control :remove-control
           ;;
           :+mice+ :mouse :make-mouse
           :mouse-pos :set-mouse-pos
           :mouse-move :set-mouse-move
           :mouse-button :set-mouse-button
           :mouse-wheel :set-mouse-wheel

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
           :next-skitter-frame))
