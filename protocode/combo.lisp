(in-package :skitter)

;;----------------------------------------------------------------------

(defmacro resetting-when (predicate-form &body body)
  (declare (ignore predicate-form body))
  (error "Skitter's resetting-when macro can only be used within defcombo"))

(defmacro defcombo (name (event-var &rest controls) slots &body body)
  (assert (string= (first controls) :&control))
  (let* ((slots (append `((%step 0 fixnum)
                          (%last-event-time 0 fixnum)
                          (%last-step-time 0 fixnum))
                        slots))
         (raw-controls controls)
         (controls (mapcar (lambda (x)
                            `(,(first x) ,(second x)
                               ,@(when (third x)
                                       (list (intern (symbol-name (third x))
                                                     :keyword)))))
                          (cdr controls)))
         (top (gensym "top"))
         (reset-harder (gensym "reset-harder")))
    `(define-predicate-control ,name (,event-var ,@raw-controls) ,slots
       (macrolet ((resetting-when (predicate-form &body body)
                    `(if ,predicate-form
                         (progn ,@body)
                         (reset)))
                  ,@(loop :for (name kind slot) :in controls :append
                       (let* ((acc (intern (format nil "~a-~a" kind slot)
                                           (symbol-package kind)))
                              (pred
                               `(eq (control-container-slot ,event-var)
                                    ',acc)))
                         `((,name (&optional index)
                                  (if index
                                      `(,',acc ,',kind index)
                                      `(,',acc ,',kind)))
                           (,(intern (format nil "~s-P" name)
                                     (symbol-package name))
                             (&optional index)
                             (if index
                                 (list 'and ',pred
                                       (list '= '(control-container-index
                                                  ,event-var)
                                             index))
                                 ',pred))))))
         (let ((%time (get-internal-real-time))
               (,reset-harder nil))
           (block ,top
             (labels ((reset ()
                        (if ,reset-harder
                            (reset-hard)
                            (progn
                              (setf %step 0)
                              (setf ,reset-harder t)
                              (return-from ,top (body ,event-var)))))
                      (reset-hard ()
                        (setf %step 0)
                        (return-from ,top))
                      (before (offset)
                        (< (- %time %last-step-time) offset))
                      (after (offset)
                        (> (- %time %last-step-time) offset))
                      (between (start-offset end-offset)
                        (and (after start-offset) (before end-offset)))
                      (body (,event-var)
                        (case= %step
                          ,@(loop for b in body for i from 0 collect
                                 `(,i ,b)))))
               (declare (ignorable #'reset #'reset-hard #'before
                                   #'after #'between))
               (let ((result (body ,event-var)))
                 (setf %last-event-time %time)
                 (when result
                   (setf %last-step-time %time)
                   (if (= %step ,(length body))
                       (progn (setf %step 0) t)
                       (progn (incf %step 1) nil)))))))))))
