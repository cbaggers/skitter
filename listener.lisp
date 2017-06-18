(in-package :skitter)

(defgeneric listen-to (listener input-source slot-name &optional index))

;;----------------------------------------------------------------------

(defstruct (event-listener (:constructor %make-event-listener))
  (control nil :type t)
  (subject nil :type t)
  (callback (error "skitter: event-listener must be created with a callback")
            :type (function (t t t t t) t)))

(defun make-event-listener (callback)
  (labels ((adapter (data listener input-source timestamp tpref)
             (declare (ignore listener))
             (funcall callback data input-source timestamp tpref)))
    (%make-event-listener :callback #'adapter)))

;;----------------------------------------------------------------------

(defmethod listen-to ((listener function) input-source
                      slot-name &optional index)
  ;; listen-to for event-listeners is defined by define-input-source
  (let ((listener (make-event-listener listener)))
    (listen-to listener input-source slot-name index)))

;;----------------------------------------------------------------------

(defgeneric remove-listener (listener input))

(defun stop-listening (listener)
  (assert (typep listener 'event-listener))
  (remove-listener listener (event-listener-control listener)))

;;----------------------------------------------------------------------

(defmacro whilst-listening-to (mappings &body body)
  (let* ((callback-vars (loop :for m :in mappings
                           :do (identity m)
                           :collect (gensym)))
         (callback-attach-to (loop :for m :in mappings :collect
                                (remove nil m))))
    `(let (,@callback-vars)
       (unwind-protect
            (progn
              ,@(loop :for v :in callback-vars
                   :for a :in callback-attach-to :collect
                   `(setf ,v (listen-to ,@a)))
              ,@body)
         ,@(loop :for v :in callback-vars :collect
              `(when ,v (stop-listening ,v)))))))

;;----------------------------------------------------------------------

(defvar *null-listener*
  (make-event-listener
   (lambda (_ _1 _2 _3)
     (declare (ignore _ _1 _2 _3))
     (error "skitter bug: null listener fired"))))

;;----------------------------------------------------------------------
