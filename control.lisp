(in-package :skitter)

;; - We only hold the latest state in a control
;; - if you need a signal that is a composition of events then use a combo
;; -

;;----------------------------------------------------------------------

(deftclass control
  (container-slot :unknown-slot :type symbol)
  (container-index -1 :type fixnum)
  (listeners (make-array 0 :element-type 'predicate-source :adjustable t
                         :fill-pointer 0)
             :type (array predicate-source (*))))

(defun set-control-slots (control listeners-array slot-name index)
  "Set all the slots of an event source"
  (setf (control-listeners control) listeners-array
        (control-container-slot control) slot-name
        (control-container-index control) index))

(defgeneric add (inst control))

;;----------------------------------------------------------------------

(defmacro define-control (name type init-val)
  (let* ((apply (intern (format nil "APPLY-~a" name) (symbol-package name)))
         (accessor-name (intern (format nil "~a-DATA" name)
                                (symbol-package name)))
         (constructor (intern (format nil "%MAKE-~a" name)
                              (symbol-package name)))
         (make (intern (format nil "MAKE-~a" name)
                       (symbol-package name))))
    `(progn
       (deftclass (,name (:include control)
                         (:constructor ,constructor))
         (data ,init-val :type ,type))
       (defun ,make ()
         (,constructor))
       (defun ,apply (control timestamp data &optional tpref)
         (setf (,accessor-name control) data)
         (propagate control timestamp tpref)))))

;;----------------------------------------------------------------------
