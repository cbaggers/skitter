(in-package :skitter)

;;----------------------------------------------------------------------


(def-event-source button
  (down-p nil :type boolean))

(def-event-source key
  (name nil :type ))

(def-event-source xy-pos
  (vec (v! 0 0) :type rtg-math.types:vec2)
  (relative (v! 0 0) :type rtg-math.types:vec2))

(def-event-source wheel
  (val 0f0 :type single-float))

(def-event-source xy-wheel
  (vec (v! 0 0) :type rtg-math.types:vec2))


;;----------------------------------------------------------------------


(defkind mouse
  (pos xy-pos)
  (button button *)
  (wheel xy-wheel *))

(defkind keyboard
  (button button *))


;;----------------------------------------------------------------------

;; (apply-button x ts :down-p t)

(defvar +mice+ (make-array 1 :element-type 'mouse :adjustable t :fill-pointer 0
			   :initial-contents (list (make-mouse))))

(defvar +keyboards+ (make-array 1 :element-type 'keyboard
				:adjustable t :fill-pointer 0
				:initial-contents (list (make-keyboard))))

(defun mouse (n)
  (aref +mice+ n))

(defun keyboard (n)
  (aref +keyboards+ n))
