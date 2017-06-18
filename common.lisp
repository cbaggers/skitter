(in-package :skitter)

;;----------------------------------------------------------------------

(define-control position2 (:static t) rtg-math.types:vec2 (v! 0 0))
(define-control iposition2 (:static t) rtg-math.types:ivec2 (v!int 0 0))
(define-control uposition2 (:static t) rtg-math.types:uvec2 (v!uint 0 0))

(define-control relative2 (:static t) rtg-math.types:vec2 (v! 0 0))
(define-control irelative2 (:static t) rtg-math.types:vec2 (v! 0 0))
(define-control urelative2 (:static t) rtg-math.types:vec2 (v! 0 0))

(define-control size2 (:static t) rtg-math.types:vec2 (v! 0 0))
(define-control isize2 (:static t) rtg-math.types:ivec2 (v!int 0 0))
(define-control usize2 (:static t) rtg-math.types:uvec2 (v!uint 0 0))

(define-control wheel (:static t) single-float 0f0)
(define-control wheel2 (:static t) rtg-math.types:vec2 (v! 0 0))

(define-control boolean-state (:static t) boolean nil)

(define-control layout (:static t) symbol :unknown)

;;----------------------------------------------------------------------

(define-input-source mouse ()
  (pos position2)
  (move relative2)
  (wheel wheel2)
  (button boolean-state *))

(define-input-source keyboard ()
  (button boolean-state *))

(define-input-source window-manager ()
  (quitting boolean-state))

(define-input-source window ()
  (pos iposition2)
  (size usize2)
  (closing boolean-state)
  (layout layout))

;;----------------------------------------------------------------------

;; {TODO} better name
(defvar +window-manager+ (make-window-manager))

;;----------------------------------------------------------------------


(defvar +keyboards+ (make-array 1 :element-type '(or keyboard null)
                                :adjustable t :fill-pointer 0))

(defun keyboard (&optional (n 0))
  (when (> (1+ n) (length +keyboards+))
    (adjust-array +keyboards+ (1+ n) :fill-pointer (1+ n) :initial-element nil)
    (setf (aref +keyboards+ n) (make-keyboard)))
  (aref +keyboards+ n))

(defun key-down-p (index &optional (keyboard (keyboard 0)))
  (keyboard-button keyboard index))

;;----------------------------------------------------------------------


(defvar +mice+ (make-array 1 :element-type '(or null mouse)
                           :adjustable t :fill-pointer 0))

(defun mouse (&optional (n 0))
  (when (> (1+ n) (length +mice+))
    (adjust-array +mice+ (1+ n) :fill-pointer (1+ n) :initial-element nil)
    (setf (aref +mice+ n) (make-mouse)))
  (aref +mice+ n))

(defun mouse-down-p (index &optional (mouse (mouse 0)))
  (mouse-button mouse index))

;;----------------------------------------------------------------------

(defvar +windows+ (make-array 1 :element-type '(or null window)
                              :adjustable t :fill-pointer 0))

(defun window (n)
  (when (> (1+ n) (length +windows+))
    (adjust-array +windows+ (1+ n) :fill-pointer (1+ n) :initial-element nil)
    (setf (aref +windows+ n) (make-window)))
  (aref +windows+ n))

;;----------------------------------------------------------------------
