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
  (container-slot :unknown-slot :type symbol)
  (container-index -1 :type fixnum)
  (listeners (make-array 0 :element-type 'predicate-source :adjustable t
			 :fill-pointer 0)
	     :type (array predicate-source (*))))

(defun %set-container (source listeners-array slot-name index)
  (setf (event-source-listeners source) listeners-array
	(event-source-container-slot source) slot-name
	(event-source-container-index source) index))

(deftclass (predicate-source (:include event-source))
  (predicate (error "Bug found in skitter: predicate-sources must always be created with predicate logic")))

(defgeneric listen-to (listener input &optional slot-name))
(defgeneric stop-listening (listener))
(defgeneric remove-listener (listener input))
(defgeneric add (inst source))
(defgeneric initialize-kind (obj))


(deftclass (null-listener (:include predicate-source)))

(defvar *null-listener*
  (make-null-listener
   :predicate (lambda (_ _1 _2)
		(declare (ignore _ _1 _2))
		(error "skitter bug: null listener fired"))))

;;----------------------------------------------------------------------

(defmacro def-input-kind (name &body slots)
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
      `(progn
         (deftclass (,name (:constructor ,constructor)
                           (:conc-name nil))
           ,@(mapcar #'parse-slot hidden-slots)
	   ,@(loop :for s :in listener-slot-names :collect
		`(,s (make-array 0 :element-type 'predicate-source :adjustable t
				 :fill-pointer 0)
		     :type (array predicate-source (*)))))
         (defun ,make ()
           (let ((result (,constructor)))
	     ,@(remove nil
		       (mapcar (lambda (h a l o)
				 (when (not l)
				   `(%set-container (,h result)
						    (,a result)
						    ',o
						    -1)))
			       hidden-slot-names
			       listener-slot-names
			       lengths
			       original-slot-names))
	     (initialize-kind result)
	     result))
	 (defmethod initialize-kind ((obj ,name)) obj)
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
                   (mapcar (lambda (x a l ls o)
                             (when l
                               (let ((push (if (numberp l)
                                               'vector-push
                                               'vector-push-extend)))
                                 `(defmethod add ((inst ,name) (source ,x))
				    (let ((arr (,a inst)))
				      (,push source arr)
				      (%set-container
				       source (,ls inst) ',o
				       (position source arr)))))))
                           types
                           hidden-slot-names
                           lengths
			   listener-slot-names
			   original-slot-names))
	 (defmethod remove-listener ((listener predicate-source) (input ,name))
	   ,@(loop :for l :in listener-slot-names :collect
		`(shifting-remove (,l input) listener *null-listener*))
	   nil)
	 (defmethod listen-to ((listener predicate-source) (input ,name)
			       &optional slot-name)
	   (let ((arr
		  (ecase slot-name
		    ,@(loop :for (s) :in slots
			 :for l :in listener-slot-names :collect
			 `(,(intern (symbol-name s) :keyword)
			    (,l input))))))
	     (vector-push-extend listener arr)
	     (values listener input)))))))

(defun shifting-remove (arr element &optional blank)
  (let ((count 0)
	(adjustable (adjustable-array-p arr)))
    (loop :for e :across arr :for i :from 0 :do
       (if (eq e element)
	   (progn
	     (setf (aref arr i) blank)
	     (incf count))
	   (when (> count 0)
	     (setf (aref arr (- i count)) (aref arr i))
	     (setf (aref arr i) blank))))
    (when adjustable
      (decf (fill-pointer arr) count))
    arr))

;;----------------------------------------------------------------------

(defmacro def-event-source (name &body slots)
  (let* ((apply (intern (format nil "APPLY-~a" name) (symbol-package name)))
         (slot-names (mapcar #'first slots))
         (accessor-names (mapcar (lambda (x)
                                   (intern (format nil "~a-~a" name x)
                                           (symbol-package name)))
                                 slot-names))
         (did-set (mapcar #'gensym (mapcar #'symbol-name slot-names)))
         (apply-args `(source timestamp tpref
                              &key ,@(mapcar (lambda (x y) `(,x nil ,y))
                                             slot-names did-set)))
         (constructor (intern (format nil "%MAKE-~a" name)
                              (symbol-package name)))
         (make (intern (format nil "MAKE-~a" name)
                       (symbol-package name))))
    `(progn
       (deftclass (,name (:include event-source)
                         (:constructor ,constructor))
	 ,@(mapcar (lambda (s)
		     (destructuring-bind (name init type) s
		       `(,name ,init :type ,type)))
		   slots))
       (defun ,make ()
         (,constructor))
       (defun ,apply ,apply-args
         ,@(mapcar (lambda (x y z) `(when ,y (setf (,z source) ,x)))
                   slot-names did-set accessor-names)
         (propagate source timestamp tpref)))))

;;----------------------------------------------------------------------

(defun propagate (source timestamp tpref)
  (labels ((fire-predicate-source (predicate-source)
             (funcall (predicate-source-predicate predicate-source)
		      predicate-source source timestamp tpref)))
    (loop :for c :across (event-source-listeners source) :do
       (fire-predicate-source c))))

(defmacro def-predicate-source (name (event-var &rest sources) slots &body body)
  (assert (symbolp event-var))
  (assert (or (null sources) (string= (first sources) :&source)))
  (let* ((sources (mapcar (lambda (x)
			    `(,(first x) ,(second x)
			       ,@(when (third x)
				       (list (intern (symbol-name (third x))
						     :keyword)))))
			  (cdr sources)))
	 ;; funcs
         (make (intern (format nil "MAKE-~a" name) (symbol-package name)))
         (constructor (intern (format nil "%MAKE-~a" name) (symbol-package name)))
         (logic (intern (format nil "%~a-BODY" name) (symbol-package name)))
         (active (intern (format nil "~s-ACTIVE-P" name)
                         (symbol-package name)))
	 (source-types (remove-duplicates (mapcar #'second sources)))
	 (source-slots (loop :for s :in source-types :collect
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
		   sources))
    `(progn
       (deftclass (,name (:include predicate-source) (:conc-name nil)
                         (:constructor ,constructor))
	 ,@(mapcar (lambda (n) `(,n nil)) source-slots)
         ,@(mapcar (lambda (n s)
		     (destructuring-bind (_ init type) s
		       (declare (ignore _))
		       `(,n ,init :type ,type)))
		   accessor-names
		   slots)
         (,active nil :type boolean))
       (defun ,make (&key ,@source-types)
         (let ((result (,constructor :predicate #',logic)))
	   ,@(loop :for (name kind slot) :in sources
		:do (identity name) :append
		`((assert (typep ,kind ',kind))
		  (listen-to result ,kind ,slot)))
	   ,@(loop :for s-type :in source-types
		:for s-slot :in source-slots :collect
		`(setf (,s-slot result) ,s-type))
	   result))
       (defun ,logic (,this ,event-var timestamp tpref)
         (declare (ignorable ,event-var))
         (let* ((result
                 (symbol-macrolet
		     ((active-p (,active ,this))
		      ,@(loop :for s-type :in source-types
			   :for s-slot :in source-slots :collect
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
	 ,@(loop :for s :in source-slots :collect
	      `(remove-listener listener (,s listener)))
	 (setf (predicate-source-predicate listener) #'source-is-unregistered
	       (event-source-listeners listener) nil
	       ,@(loop :for s :in source-slots :append
		    `((,s listener) nil)))))))

(defun source-is-unregistered (_ _1 _2)
  (declare (ignore _ _1 _2))
  (error "Skitter: This predicate-source is not registered to anything"))

(defmethod listen-to ((listener predicate-source) (input predicate-source)
		      &optional slot-name)
  (assert (null slot-name))
  (vector-push-extend listener (event-source-listeners input))
  (values listener input))

(defmethod remove-listener ((listener predicate-source)
			    (input predicate-source))
  (shifting-remove (event-source-listeners input) listener *null-listener*)
  nil)

;;----------------------------------------------------------------------

(defmacro resetting-when (predicate-form &body body)
  (declare (ignore predicate-form body))
  (error "Skitter's resetting-when macro can only be used within defcombo"))

(defmacro defcombo (name (event-var &rest sources) slots &body body)
  (assert (string= (first sources) :&source))
  (let* ((slots (append `((%step 0 fixnum)
			  (%last-event-time 0 fixnum)
			  (%last-step-time 0 fixnum))
			slots))
	 (raw-sources sources)
	 (sources (mapcar (lambda (x)
			    `(,(first x) ,(second x)
			       ,@(when (third x)
				       (list (intern (symbol-name (third x))
						     :keyword)))))
			  (cdr sources)))
	 (top (gensym "top"))
	 (reset-harder (gensym "reset-harder")))
    `(def-predicate-source ,name (,event-var ,@raw-sources) ,slots
       (macrolet ((resetting-when (predicate-form &body body)
		    `(if ,predicate-form
			 (progn ,@body)
			 (reset)))
		  ,@(loop :for (name kind slot) :in sources :append
		       (let* ((acc (intern (format nil "~a-~a" kind slot)
					  (symbol-package kind)))
			      (pred
			       `(eq (event-source-container-slot ,event-var)
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
				       (list '= '(event-source-container-index
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

;;----------------------------------------------------------------------

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
