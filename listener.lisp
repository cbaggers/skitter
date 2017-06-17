(in-package :skitter)

;;----------------------------------------------------------------------
;; Hmm this is odd. Predicate-source is a control so wouldnt it belong to
;; a input-source?

(deftclass (event-listener (:include predicate-source)
                           (:constructor %make-event-listener))
  (subject nil)
  (callback (error "skitter: event-listener must be created with a callback")
            :type function))

(defun make-event-listener (callback)
  (%make-event-listener :predicate #'%event-listener-body
                        :callback callback))

(defun %event-listener-body (this evt timestamp tpref)
  (funcall (event-listener-callback this) evt timestamp tpref))

(defmethod listen-to :after
    ((listener event-listener) input &optional timestamp)
  (declare (ignore timestamp))
  (setf (event-listener-subject listener) input))

(defmethod listen-to ((listener function) input &optional slot-name)
  (let ((listener (make-event-listener listener)))
    (listen-to listener input slot-name)))

(defmethod stop-listening ((listener event-listener))
  (remove-listener listener (event-listener-subject listener)))

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
