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
    (let* ((original-slot-names
            (mapcar (lambda (x) (intern (format nil "~a-~a" name x)
                                        (symbol-package name)))
                    (mapcar #'first slots)))
           (hidden-slot-names (mapcar (lambda (x s) (if (third s) (hide x) x))
                                      original-slot-names
                                      slots))
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
           ,@(mapcar #'parse-slot hidden-slots))
         (defun ,make ()
           (,constructor))
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
                   (mapcar (lambda (x a l)
                             (when l
                               (let ((push (if (numberp l)
                                               'vector-push
                                               'vector-push-extend)))
                                 `(defmethod add ((inst ,name) (source ,x))
                                    (,push source (,a inst))))))
                           types
                           hidden-slot-names
                           lengths))))))

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
    (loop :for c :in (event-source-combos source) :do
       (fire-combo c source timestamp))))

(defmacro def0combo-source (name source-types (&key event-var (timestamp-var 'timestamp))

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

;; (def-combo double-click (button) (:event-var evt)
;;     ((last-down 0 :type fix))
;;   1)


;;----------------------------------------------------------------------

(defgeneric slow-mouse-button-by-name (name))

(defgeneric slow-keyboard-key-by-name (name))

(defgeneric slow-window-event-by-name (name))
