(in-package :skitter)

;;----------------------------------------------------------------------

(defun make-n-long (array n)
  (if (< (length array) n)
      (adjust-array array n :fill-pointer (1- n))
      array))

;;----------------------------------------------------------------------

(deftclass event-source
  (combos (make-array 0 :element-type 'function
		      :adjustable t :fill-pointer 0)))

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
    (let* ((slot-names (mapcar #'first slots))
	   (types (mapcar #'second slots))
	   (accessor-names (mapcar (lambda (x)
				     (intern (format nil "~a-~a" name x)
					     (symbol-package name)))
				   slot-names))
	   (lengths (mapcar #'third slots)))
      (when (> (length types) (length (remove-duplicates types)))
	(error "multiple slots found with the same type, this is no allowed in defkind"))
      `(progn
	 (deftclass ,name
	   ,@(mapcar #'parse-slot slots))
	 ,@(remove nil
		   (mapcar (lambda (x a l)
			     (when l
			       (let ((push (if (numberp l)
					       'vector-push
					       'vector-push-extend)))
				 `(defmethod add ((inst ,name) (source ,x))
				    (,push source (,a inst))))))
			   types
			   accessor-names
			   lengths))))))

(defkind mouse
  (pos xy-pos)
  (button button *)
  (wheel wheel *))

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
					     slot-names did-set))))
    `(progn
       (deftclass (,name (:include event-source)) ,@slots)
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

(def-event-source button
  (down-p nil :type boolean))

(def-event-source xy-pos
  (vec (v! 0 0) :type rtg-math.types:vec2))

(def-event-source wheel
  (val 0f0 :type single-float))

;; (apply-button x ts :down-p t)

;;----------------------------------------------------------------------

(defun propagate (source timestamp)
  (labels ((fire-combo (combo source timestamp)
	     (funcall (combo-predicate combo) combo source timestamp)))
    (loop :for c :in (event-source-combos source) :do
       (fire-combo c source timestamp))))

(defmacro def-combo (name source-types (&key event-var (timestamp-var 'timestamp))

					  slots &body body)
  (assert (symbolp event-var))
  (assert (listp source-types))
  (let* (;; funcs
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
	 (hidden-slot-names (mapcar #'hide accessor-names))
	 (hidden-slots (mapcar (lambda (n s) (cons n (rest s)))
			       hidden-slot-names
			       slots))
	 ;;-internal vars
	 (this (gensym "this")))
    (assert (and (every #'symbolp source-types)
		 (> (length source-types) 0)))
    `(progn
       (deftclass (,name (:include combo) (:conc-name nil)
			 (:constructor ,constructor))
	 ,@hidden-slots
	 (,active nil :type boolean))
       (defun ,make ()
	 (,constructor :predicate #',logic))
       (defun ,logic (,this ,event-var ,timestamp-var)
	 (declare (ignorable ,event-var ,timestamp-var))
	 (let* ((result
		 (symbol-macrolet
		     ,(mapcar (lambda (n hn) `(,n (,hn ,event-var)))
			      original-slot-names hidden-slot-names)
		   ,@body))
		(old-active (,active ,this))
		(new-active (not (null result))))
	   (setf (,active ,this) new-active)
	   (when (not (eq old-active new-active))
	     (propagate ,this ,timestamp-var))
	   nil))
       (defmethod add ((inst ,name) source)
	 ,(when source-types
		`(assert (or ,@(mapcar (lambda (s) `(typep inst ',s))
				       source-types))))
	 (vector-push-extend inst (event-source-combos source))
	 source))))

(def-combo double-click (button) (:event-var evt)
    ((last-down 0 :type fix))
  1)


;;----------------------------------------------------------------------
