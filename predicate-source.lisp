(in-package :skitter)

;;----------------------------------------------------------------------

(deftclass (predicate-control (:include control))
  (predicate (error "Bug found in skitter: predicate-controls must always be created with predicate logic")))

;;----------------------------------------------------------------------

(deftclass (null-listener (:include predicate-control)))

(defvar *null-listener*
  (make-null-listener
   :predicate (lambda (_ _1 _2)
                (declare (ignore _ _1 _2))
                (error "skitter bug: null listener fired"))))

;;----------------------------------------------------------------------

(defun propagate (control timestamp tpref)
  (loop :for predicate-control :across (control-listeners control) :do
     (funcall (predicate-control-predicate predicate-control)
              predicate-control control timestamp tpref)))

;;----------------------------------------------------------------------

(defmacro define-predicate-control
    (name type init-val (event-var &rest controls) internal-slots
     &body body)
  (assert (symbolp event-var))
  (assert (or (null controls) (string= (first controls) :&control)))
  (let* ((p (symbol-package name))
         (controls (mapcar (lambda (x)
                             `(,(first x) ,(second x)
                                ,@(when (third x)
                                        (list (intern (symbol-name (third x))
                                                      :keyword)))))
                           (cdr controls)))
         ;; funcs
         (make (symb p "MAKE-" name))
         (constructor (symb p "%MAKE-" name))
         (logic (symb p "%" name "-BODY" ))
         (control-types (remove-duplicates (mapcar #'second controls)))
         (control-slots (loop :for s :in control-types :collect
                           (symb p name "-" s)))
         ;;state-slots
         (original-slot-names (mapcar #'first internal-slots))
         (accessor-names (mapcar (lambda (x) (symb p name "-" x))
                                 original-slot-names))
         ;;-internal vars
         (this (gensym "this")))
    (assert (every (lambda (x)
                     (and (listp x)
                          (or (= (length x) 3)
                              (= (length x) 2))))
                   controls))
    `(progn
       (deftclass (,name (:include predicate-control) (:conc-name nil)
                         (:constructor ,constructor))
         (,(control-data-acc-name name) ,init-val :type ,type)
         ,@(mapcar (lambda (n) `(,n nil)) control-slots)
         ,@(mapcar (lambda (n s)
                     (destructuring-bind (_ init type) s
                       (declare (ignore _))
                       `(,n ,init :type ,type)))
                   accessor-names
                   internal-slots))

       (defun ,make (&key ,@control-types)
         (let ((result (,constructor :predicate #',logic)))
           ,@(loop :for (name kind slot) :in controls
                :do (identity name) :append
                `((assert (typep ,kind ',kind))
                  (listen-to result ,kind ,slot)))
           ,@(loop :for s-type :in control-types
                :for s-slot :in control-slots :collect
                `(setf (,s-slot result) ,s-type))
           result))

       (defun ,logic (,this ,event-var timestamp tpref)
         (declare (ignorable ,event-var tpref timestamp))
         (labels ((fire (new-val &optional tpref)
                    (setf (,(control-data-acc-name name) ,this) new-val)
                    (propagate ,this timestamp tpref)))
           (symbol-macrolet
               (,@(loop :for s-type :in control-types
                     :for s-slot :in control-slots :collect
                     `(,s-type (,s-slot ,this)))
                ,@(mapcar (lambda (n a) `(,n (,a ,this)))
                          original-slot-names
                          accessor-names))
             (locally
                 (declare (optimize (speed 1) (debug 1) (space 1)
                                    (safety 1) (compilation-speed 1)))
               ,@body
               (values)))))
       (defmethod stop-listening ((listener ,name))
         ,@(loop :for s :in control-slots :collect
              `(remove-listener listener (,s listener)))
         (setf (predicate-control-predicate listener) #'control-is-unregistered
               (control-listeners listener) nil
               ,@(loop :for s :in control-slots :append
                    `((,s listener) nil)))))))

(defun control-is-unregistered (_ _1 _2)
  (declare (ignore _ _1 _2))
  (error "Skitter: This predicate-control is not registered to anything"))

(defmethod listen-to ((listener predicate-control) (input predicate-control)
                      &optional slot-name)
  (assert (null slot-name))
  (vector-push-extend listener (control-listeners input))
  (values listener input))

(defmethod remove-listener ((listener predicate-control)
                            (input predicate-control))
  (shifting-remove (control-listeners input) listener *null-listener*)
  nil)

;;----------------------------------------------------------------------
