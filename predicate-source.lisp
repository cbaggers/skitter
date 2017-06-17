(in-package :skitter)

;;----------------------------------------------------------------------

(deftclass (predicate-source (:include control))
  (predicate (error "Bug found in skitter: predicate-sources must always be created with predicate logic")))

;;----------------------------------------------------------------------

(deftclass (null-listener (:include predicate-source)))

(defvar *null-listener*
  (make-null-listener
   :predicate (lambda (_ _1 _2)
                (declare (ignore _ _1 _2))
                (error "skitter bug: null listener fired"))))

;;----------------------------------------------------------------------

(defun propagate (control timestamp tpref)
  (labels ((fire-predicate-source (predicate-source)
             (funcall (predicate-source-predicate predicate-source)
                      predicate-source control timestamp tpref)))
    (loop :for c :across (control-listeners control) :do
       (fire-predicate-source c))))

;;----------------------------------------------------------------------

(defmacro def-predicate-source (name (event-var &rest controls) slots
                                &body body)
  (assert (symbolp event-var))
  (assert (or (null controls) (string= (first controls) :&control)))
  (let* ((controls (mapcar (lambda (x)
                            `(,(first x) ,(second x)
                               ,@(when (third x)
                                       (list (intern (symbol-name (third x))
                                                     :keyword)))))
                          (cdr controls)))
         ;; funcs
         (make (intern (format nil "MAKE-~a" name) (symbol-package name)))
         (constructor (intern (format nil "%MAKE-~a" name) (symbol-package name)))
         (logic (intern (format nil "%~a-BODY" name) (symbol-package name)))
         (active (intern (format nil "~s-ACTIVE-P" name)
                         (symbol-package name)))
         (control-types (remove-duplicates (mapcar #'second controls)))
         (control-slots (loop :for s :in control-types :collect
                          (intern (format nil "~s-~s" name s)
                                  (symbol-package name))))
         ;;state-slots
         (original-slot-names (mapcar #'first slots))
         (accessor-names (mapcar (lambda (x)
                                   (intern (format nil "~a-~a" name x)
                                           (symbol-package name)))
                                 original-slot-names))
         ;;-internal vars
         (this (gensym "this")))
    (assert (every (lambda (x)
                     (and (listp x)
                          (or (= (length x) 3)
                              (= (length x) 2))))
                   controls))
    `(progn
       (deftclass (,name (:include predicate-source) (:conc-name nil)
                         (:constructor ,constructor))
         ,@(mapcar (lambda (n) `(,n nil)) control-slots)
         ,@(mapcar (lambda (n s)
                     (destructuring-bind (_ init type) s
                       (declare (ignore _))
                       `(,n ,init :type ,type)))
                   accessor-names
                   slots)
         (,active nil :type boolean))
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
         (declare (ignorable ,event-var))
         (let* ((result
                 (symbol-macrolet
                     ((active-p (,active ,this))
                      ,@(loop :for s-type :in control-types
                           :for s-slot :in control-slots :collect
                           `(,s-type (,s-slot ,this)))
                      ,@(mapcar (lambda (n a) `(,n (,a ,this)))
                                original-slot-names
                                accessor-names))
                   (locally
                       (declare (optimize (speed 1) (debug 1) (space 1)
                                          (safety 1) (compilation-speed 1)))
                     ,@body)))
                (old-active (,active ,this))
                (new-active (not (null result))))
           (setf (,active ,this) new-active)
           (when (not (eq old-active new-active))
             (propagate ,this timestamp tpref))
           nil))
       (defmethod stop-listening ((listener ,name))
         ,@(loop :for s :in control-slots :collect
              `(remove-listener listener (,s listener)))
         (setf (predicate-source-predicate listener) #'control-is-unregistered
               (control-listeners listener) nil
               ,@(loop :for s :in control-slots :append
                    `((,s listener) nil)))))))

(defun control-is-unregistered (_ _1 _2)
  (declare (ignore _ _1 _2))
  (error "Skitter: This predicate-source is not registered to anything"))

(defmethod listen-to ((listener predicate-source) (input predicate-source)
                      &optional slot-name)
  (assert (null slot-name))
  (vector-push-extend listener (control-listeners input))
  (values listener input))

(defmethod remove-listener ((listener predicate-source)
                            (input predicate-source))
  (shifting-remove (control-listeners input) listener *null-listener*)
  nil)

;;----------------------------------------------------------------------
