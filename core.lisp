(in-package :skitter)

;;----------------------------------------------------------------------

(defun make-n-long (array n)
  (adjust-array array n :fill-pointer (1- n)))

(defmacro ensure-n-long (array n &optional init-form)
  (let ((arr (gensym "array"))
        (index (gensym "n")))
    `(let ((,arr ,array)
           (,index ,n))
       (when (< (length ,arr) ,index)
         ,(if init-form
              `(setf (aref (make-n-long ,arr ,index) ,index) ,init-form)
              `(make-n-long ,arr ,index)))
       ,arr)))

;;----------------------------------------------------------------------

(deftclass event-source
  (container nil :type (or null array)))

;; (combos (make-array 0 :element-type 'function
;; 		    :adjustable t :fill-pointer 0))

(defgeneric %set-container (source array))
(defmethod %set-container (source array)
  (setf (event-source-container source)
	array))

(deftclass (combo (:include event-source))
  (predicate (error "Bug found in skitter: combos must always be created with predicate logic")))

;;----------------------------------------------------------------------

(defmacro defkind (name &body slots)
  (labels ((parse-slot (s)
             (let* ((name (first s))
                    (array? (or (string= :* (third s))
                                (numberp (third s))))
                    (elem-type (when array? (second s)))
                    (len (when array? (third s)))
                    (type (if array?
                              `(array ,elem-type (,len))
                              (second s)))
                    (init (if array?
                              (if (numberp len)
                                  `(make-array ,len :element-type ',elem-type
                                               :fill-pointer 0)
                                  `(make-array 0 :element-type ',elem-type
                                               :adjustable t :fill-pointer 0))
                              `(,(intern (format nil "MAKE-~a" type)
                                         (symbol-package type))))))
               `(,name ,init :type ,type))))
    (let* ((original-slot-names
            (mapcar (lambda (x) (intern (format nil "~a-~a" name x)
                                        (symbol-package name)))
                    (mapcar #'first slots)))
           (hidden-slot-names (mapcar (lambda (x s) (if (third s) (hide x) x))
                                      original-slot-names
                                      slots))
	   (listener-slot-names
	    (mapcar (lambda (x) (intern (format nil "~s-LISTENERS" x)))
		    original-slot-names))
           (hidden-slots (mapcar #'cons
                                 hidden-slot-names
                                 (mapcar #'rest slots)))
           (types (mapcar #'second slots))
           (constructor (intern (format nil "%MAKE-~a" name)
                                (symbol-package name)))
           (make (intern (format nil "MAKE-~a" name)
                         (symbol-package name)))
           (lengths (mapcar #'third slots))
           (type-constructors (mapcar (lambda (n)
                                        (intern (format nil "MAKE-~a" n)
                                                (symbol-package n)))
                                      types)))
      (when (> (length types) (length (remove-duplicates types)))
        (error "multiple slots found with the same type, this is no allowed in defkind"))
      `(progn
         (deftclass (,name (:constructor ,constructor)
                           (:conc-name nil))
           ,@(mapcar #'parse-slot hidden-slots)
	   ,@(loop :for s :in listener-slot-names :collect
		`(,s (make-array 0 :element-type 'combo :adjustable t
				 :fill-pointer 0)
		     :type (array combo (*)))))
         (defun ,make ()
           (let ((result (,constructor)))
	     ,@(remove nil
		       (mapcar (lambda (h a l)
				 (when (not l)
				   `(%set-container (,h result)
						    (,a result))))
			       hidden-slot-names
			       listener-slot-names
			       lengths))
	     result))
         ,@(remove nil
                   (mapcar (lambda (x s tc)
                             (when (third s)
                               `(defun ,x (source index)
                                  (aref (ensure-n-long (,(first s) source) index
                                                       (,tc))
                                        index))))
                           original-slot-names
                           hidden-slots
                           type-constructors))
         ,@(remove nil
                   (mapcar (lambda (x a l ls)
                             (when l
                               (let ((push (if (numberp l)
                                               'vector-push
                                               'vector-push-extend)))
                                 `(defmethod add ((inst ,name) (source ,x))
				    (%set-container source (,ls inst))
				    (,push source (,a inst))))))
                           types
                           hidden-slot-names
                           lengths
			   listener-slot-names))
	 (defmethod %listen-to ((source ,name) slot-name listener)
	   (let ((arr
		  (ecase slot-name
		    ,@(loop :for s :in original-slot-names
			 :for l :in listener-slot-names :collect
			 `(,(intern (symbol-name s) :keyword)
			    (,l source))))))
	     (vector-push-extend listener arr)))))))

;;----------------------------------------------------------------------

(defmacro def-event-source (name &body slots)
  (let* ((apply (intern (format nil "APPLY-~a" name) (symbol-package name)))
         (slot-names (mapcar #'first slots))
         (accessor-names (mapcar (lambda (x)
                                   (intern (format nil "~a-~a" name x)
                                           (symbol-package name)))
                                 slot-names))
         (did-set (mapcar #'gensym (mapcar #'symbol-name slot-names)))
         (apply-args `(source timestamp
                              &key ,@(mapcar (lambda (x y) `(,x nil ,y))
                                             slot-names did-set)))
         (constructor (intern (format nil "%MAKE-~a" name)
                              (symbol-package name)))
         (make (intern (format nil "MAKE-~a" name)
                       (symbol-package name))))
    `(progn
       (deftclass (,name (:include event-source)
                         (:constructor ,constructor))
         ,@slots)
       (defun ,make ()
         (,constructor))
       (defun ,apply ,apply-args
         ,@(mapcar (lambda (x y z) `(when ,y (setf (,z source) ,x)))
                   slot-names did-set accessor-names)
         (propagate source timestamp))
       (define-compiler-macro ,apply ,apply-args
         (let ((src (gensym "source")))
           `(let ((,src ,source))
              ,,@(mapcar (lambda (x y z) `(when ,y (list 'setf (list ',z src) ,x)))
                         slot-names did-set accessor-names)
              ,(list 'propagate src timestamp)))))))

;;----------------------------------------------------------------------

(defun propagate (source timestamp)
  (labels ((fire-combo (combo source timestamp)
             (funcall (combo-predicate combo) combo source timestamp)))
    (loop :for c :across (event-source-container source) :do
       (fire-combo c source timestamp))))

(defmacro def-combo-source (name (event-var &rest sources) slots &body body)
  (assert (symbolp event-var))
  (assert (string= (first sources) :&source))
  (let* ((sources (cddr sources))

	 ;; funcs
         (make (intern (format nil "MAKE-~a" name) (symbol-package name)))
         (constructor (intern (format nil "%MAKE-~a" name) (symbol-package name)))
         (logic (intern (format nil "%~a-BODY" name) (symbol-package name)))
         (active (intern (format nil "~s-ACTIVE-P" name)
                         (symbol-package name)))
         ;;state-slots
         (original-slot-names (mapcar #'first slots))
         (accessor-names (mapcar (lambda (x)
                                   (intern (format nil "~a-~a" name x)
                                           (symbol-package name)))
                                 original-slot-names))
         ;;-internal vars
         (this (gensym "this")))
    (assert (every (lambda (x) (and (listp x) (= (length x) 3))) sources))
    `(progn
       (deftclass (,name (:include combo) (:conc-name nil)
                         (:constructor ,constructor))
         ,@(mapcar (lambda (n s) (cons n (rest s)))
		   accessor-names
		   slots)
         (,active nil :type boolean))
       (defun ,make ()
         (,constructor :predicate #',logic))
       (defun ,logic (,this ,event-var timestamp)
         (declare (ignorable ,event-var))
         (let* ((result
                 (symbol-macrolet
		     ((active-p (,active ,event-var))
		      ,@(mapcar (lambda (n a) `(,n (,a ,event-var)))
				original-slot-names
				accessor-names))
                   ,@body))
                (old-active (,active ,this))
                (new-active (not (null result))))
           (setf (,active ,this) new-active)
           (when (not (eq old-active new-active))
             (propagate ,this timestamp))
           nil)))))

(def-combo-source double-click (evt &source (m mouse :button))
    ((last-down 0 :type fix))
  1)


;;----------------------------------------------------------------------

(defgeneric slow-mouse-button-by-name (name))

(defgeneric slow-keyboard-key-by-name (name))

(defgeneric slow-window-event-by-name (name))
