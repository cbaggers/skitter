(in-package :skitter)

;;----------------------------------------------------------------------

(def-event-source button
  (down-p nil boolean))

(def-event-source xy-pos
  (vec (v! 0 0) rtg-math.types:vec2)
  (relative (v! 0 0) rtg-math.types:vec2))

(def-event-source wheel
  (val 0f0 single-float))

(def-event-source xy-wheel
  (vec (v! 0 0) rtg-math.types:vec2))

(def-event-source state
  (is nil boolean))

(def-event-source pos-2d
  (vec (v!int 100 100) rtg-math.types:ivec2))

(def-event-source size-2d
  (vec (v!int 100 100) rtg-math.types:ivec2))

(def-event-source layout
  (state :unknown symbol))

;;----------------------------------------------------------------------


(def-input-kind mouse
  (pos xy-pos)
  (wheel xy-wheel)
  (button button *))

(def-input-kind keyboard
  (button button *))

(def-input-kind system
  (quitting state))

(def-input-kind window
  (pos pos-2d)
  (size size-2d)
  (closing state)
  (layout layout))

;;----------------------------------------------------------------------

(defvar +system+ (make-system))

;;----------------------------------------------------------------------


(defvar +keyboards+ (make-array 1 :element-type '(or keyboard null)
				:adjustable t :fill-pointer 0))

(defun keyboard (n)
  (when (> (1+ n) (length +keyboards+))
    (adjust-array +keyboards+ (1+ n) :fill-pointer (1+ n) :initial-element nil)
    (setf (aref +keyboards+ n) (make-keyboard)))
  (aref +keyboards+ n))

(defun key-down-p (index &optional (keyboard (keyboard 0)))
  (button-down-p (keyboard-button keyboard index)))

;;----------------------------------------------------------------------


(defvar +mice+ (make-array 1 :element-type '(or null mouse)
			   :adjustable t :fill-pointer 0))

(defun mouse (n)
  (when (> (1+ n) (length +mice+))
    (adjust-array +mice+ (1+ n) :fill-pointer (1+ n) :initial-element nil)
    (setf (aref +mice+ n) (make-mouse)))
  (aref +mice+ n))

(defun mouse-down-p (index &optional (mouse (mouse 0)))
  (button-down-p (mouse-button mouse index)))

;;----------------------------------------------------------------------

(defvar +windows+ (make-array 1 :element-type '(or null window)
			      :adjustable t :fill-pointer 0))

(defun window (n)
  (when (> (1+ n) (length +windows+))
    (adjust-array +windows+ (1+ n) :fill-pointer (1+ n) :initial-element nil)
    (setf (aref +windows+ n) (make-window)))
  (aref +windows+ n))

;;----------------------------------------------------------------------
