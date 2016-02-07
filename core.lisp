(in-package :skitter)

;;----------------------------------------------------------------------

(defun make-n-long (array n)
  (if (< (length array) n)
      (adjust-array array n :fill-pointer (1- n))
      array))

;;----------------------------------------------------------------------

(defparameter *ids* (make-hash-table))

(let ((id -1))
  (defun %next-id ()
    (incf id)))

;;----------------------------------------------------------------------

(deftclass event-source
  (local-combos (make-array 0 :element-type 'function
			    :adjustable t :fill-pointer 0)))

(deftclass (regular-source (:include event-source)))

(deftclass (combo-source (:include event-source))
  (body (error "Bug found in skitter: combo-source must always be created with a body")))

;;----------------------------------------------------------------------

(deftype source-array ()
  '(array (or null regular-source) (*)))

(deftclass source-group
  (sources (make-array 0 :element-type 'source-array
		       :adjustable t :fill-pointer 0)
	   :type (array source-array (*)))
  (global-combos (make-array 0 :element-type 'combo-source
			     :adjustable t :fill-pointer 0)
		 :type (array combo-source (*))))

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
				  `(make-array ,len :element-type ',elem-type)
				  `(make-array 0 :element-type ',elem-type
					       :adjustable t :fill-pointer 0))
			      `(,(intern (format nil "MAKE-~a" type)
					 (symbol-package type))))))
	       `(,name ,init :type ,type))))
    `(deftclass ,name
       ,@(mapcar #'parse-slot slots))))

(defkind mouse
  (pos xy-pos)
  (button button *)
  (wheel wheel *))

;;----------------------------------------------------------------------

(defmacro def-event-source (name &body slots)
  (let* ((id (or (gethash name *ids*) (%next-id)))
	 (make (intern (format nil "MAKE-~a" name) (symbol-package name)))
	 (apply (intern (format nil "APPLY-~a" name) (symbol-package name)))
	 (add (intern (format nil "ADD-~a" name) (symbol-package name)))
	 (add-n (intern (format nil "ADD-N-~a" name) (symbol-package name)))
	 (slot-names (mapcar #'first slots))
	 (accessor-names (mapcar (lambda (x)
				   (intern (format nil "~a-~a" name x)
					   (symbol-package name)))
				 slot-names))
	 (did-set (mapcar #'gensym (mapcar #'symbol-name slot-names))))
    `(progn
       (setf (gethash ',name *ids*) ,id)
       (deftclass (,name (:include regular-source)) ,@slots)
       (defun ,apply (group index &key ,@(mapcar (lambda (x y) `(,x nil ,y))
						 slot-names did-set))
	 (let ((it (aref (aref (source-group-sources group) ,id) index)))
	   ,@(mapcar (lambda (x y z) `(when ,y (setf (,z it) ,x)))
		     slot-names did-set accessor-names))
	 ;; new events :)
	 )
       (defun ,add (group)
	 (let ((g (source-group-sources group)))
	   (make-n-long g ,id)
	   (vector-push-extend (,make) (aref g ,id) )))
       (defun ,add-n (n group)
	 (loop :for i :below n :do (,add group))
	 group))))

(def-event-source button
  (down-p nil :type boolean))

(def-event-source xy-pos
  (vec (v! 0 0) :type rtg-math.types:vec2))

(def-event-source wheel
  (val 0f0 :type single-float))

;;----------------------------------------------------------------------

(defmacro def-combo-event-source (name slots (&key event-var) &body body)
  (assert (symbolp event-var))
  (let* (;; funcs
	 (make (intern (format nil "MAKE-~a" name) (symbol-package name)))
	 (logic (intern (format nil "%~a-BODY" name) (symbol-package name)))
	 (add (intern (format nil "ADD-~a" name) (symbol-package name)))
	 (active (intern (format nil "~s-ACTIVE-P" name)
			 (symbol-package name)))
	 (body-func (hide (format nil "~s-BODY" name)
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
    `(progn
       (deftclass (,name (:include regular-source) (:conc-name nil))
	 ,@hidden-slots
	 (,body-func #',logic :type function)
	 (,active nil :type boolean))
       (defun ,logic (,this ,event-var)
	 (let ((result
		(symbol-macrolet
		    ,(mapcar (lambda (n hn) `(,n (,hn ,event-var)))
			     original-slot-names hidden-slot-names)
		  ,@body))
	       (old-active (,active ,this))
	       (new-active (not (null result))))
	   (setf (,active ,this) new-active)
	   (when (not (eq old-active new-active))
	     ;; new events :)
	     )
	   nil))
       (defun ,add (group &rest listening-to)
	 (let* ((g (source-group-combos group)))
	   (vector-push-extend (,make) (source-group-combos group))
	   (loop :for (source-kind source-index) :across listening-to :do
	      (let ((source-kind-index (or (gethash ))))
		(vector-push-extend #',logic
				    (aref (aref (source-group-sources group)
						source-kind-index)
					  source-index))))
	   group)))))

(def-combo-event-source double-click
    ((last-down 0 :type fix))
    (:event-var evt)
  1)

;;----------------------------------------------------------------------

(defvar +mice+
  (make-array 0 :element-type 'source-group
	      :adjustable t :fill-pointer 0))

(defvar +keyboards+
  (make-array 0 :element-type 'source-group
	      :adjustable t :fill-pointer 0))
