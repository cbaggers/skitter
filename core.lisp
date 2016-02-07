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

(deftclass event-source) ;; still neccesary?

(deftclass (regular-source (:include event-source))
  (listeners (make-array 0 :element-type fixnum :adjustable t :fill-pointer 0)))

(deftclass (combo-source (:include event-source))
  (body (error "Bug found in skitter: combo-source must always be created with a body"))
  (listeners (make-array 0 :element-type fixnum :adjustable t :fill-pointer 0)))

;;----------------------------------------------------------------------

(deftype source-array ()
  '(array (or null regular-source) (*)))

(deftclass source-group
  (sources (make-array 0 :element-type 'source-array
		       :adjustable t :fill-pointer 0)
	   :type (array source-array (*)))
  (combos (make-array 0 :element-type 'combo-source
		      :adjustable t :fill-pointer 0)
	  :type (array combo-source (*))))

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
		     slot-names did-set accessor-names)))
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

;;----------------------------------------------------------------------

(defmacro def-combo-event-source (name slots (&key event-var) &body body)
  (assert (symbolp event-var))
  (let* ((make (intern (format nil "MAKE-~a" name) (symbol-package name)))
	 (logic (intern (format nil "%~a-BODY" name) (symbol-package name)))
	 (add (intern (format nil "ADD-~a" name) (symbol-package name)))
	 (slot-names (mapcar #'first slots))
	 (accessor-names (mapcar (lambda (x)
				   (intern (format nil "~a-~a" name x)
					   (symbol-package name)))
				 slot-names))
	 (active (intern (format nil "~s-ACTIVE-P" name)
			 (symbol-package name)))
	 (this (gensym "this")))
    `(progn
       (deftclass (,name (:include regular-source) (:conc nil))
	 ,@slots
	 (body #',logic :type function)
	 (active-p nil :type boolean))
       (defun ,logic (,this ,event-var)
	 (let ((result (progn ,@body))
	       (old-active (,active ,this))
	       (new-active (not (null result))))
	   (setf (,active ,this) new-active)
	   (when (not (eq old-active new-active))
	     ;; new events :)
	     )
	   nil))
       (defun ,add (group &rest listening-to)
	 (let* ((g (source-group-combos group))
		(i (length g)))
	   (vector-push-extend (,make) (source-group-combos group))
	   (loop :for (source-kind source-index) :across listening-to :do
	      (let ((source-kind-index (or (gethash ))))
		(vector-push-extend i (aref (aref (source-group-sources group)
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
