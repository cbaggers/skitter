(in-package :skitter)

;;----------------------------------------------------------------------

(define-control boolean-control (:static t) boolean nil)
(define-control symbol-control (:static t) symbol :unknown)

(define-control float-control (:static t) single-float 0f0)
(define-control vec2-control (:static t) rtg-math.types:vec2 (v! 0 0))
(define-control ivec2-control (:static t) rtg-math.types:ivec2 (v!int 0 0))
(define-control uvec2-control (:static t) rtg-math.types:uvec2 (v!uint 0 0))

(define-control float-decaying-control (:static t) single-float 0f0
                :decays t)
(define-control vec2-decaying-control (:static t) rtg-math.types:vec2 (v! 0 0)
                :decays t)
(define-control ivec2-decaying-control (:static t) rtg-math.types:vec2 (v! 0 0)
                :decays t)
(define-control uvec2-decaying-control (:static t) rtg-math.types:vec2 (v! 0 0)
                :decays t)

;;----------------------------------------------------------------------

(define-input-source mouse ()
  (pos vec2-control)
  (move vec2-decaying-control)
  (float-control vec2-control)
  (button boolean-control *))

(define-input-source gamepad ()
  (button boolean-control *)
  (1d vec2-control *)
  (2d vec2-control *))

(define-input-source keyboard ()
  (button boolean-control *))

(define-input-source window-manager ()
  (quitting boolean-control))

(define-input-source window ()
  (pos ivec2-control)
  (size uvec2-control)
  (closing boolean-control)
  (symbol-control symbol-control))

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
