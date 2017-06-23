(in-package :skitter)

(defgeneric initialize-kind (obj))

(defgeneric get-control
    (input-source &optional slot-name index allow-arr-result))

(defun isource-array-slot-p (slot)
  (or (string= :* (third slot))
      (numberp (third slot))))

;;----------------------------------------------------------------------

(defun gen-populate-control (control-type
                             hidden-slot-name
                             length
                             original-slot-name)
  ;; {TODO} needs better explanation
  "gens nil when there is a length as then you add the event controls using
   the add methods"
  (when (not length)
    `(set-control-slots
      ,control-type
      (,hidden-slot-name result)
      ',original-slot-name
      -1)))

(defun gen-add-methods (name
                        types
                        hidden-slot-names
                        lengths
                        original-slot-names)
  "This is a partner in crimer to #'gen-populate-control in that we
   on need add methods when there is a length"
  (denil
   (loop :for type :in types
      :for hidden-slot-name :in hidden-slot-names
      :for length :in lengths
      :for original-slot-name :in original-slot-names
      :when length :collect
      (when length
        (let ((push (if (numberp length)
                        'vector-push
                        'vector-push-extend)))
          `(defmethod add ((inst ,name) (control ,type))
             (let ((arr (,hidden-slot-name inst)))
               (,push control arr)
               (set-control-slots ,type
                                  control
                                  ',original-slot-name
                                  (position control arr)))))))))

(defun gen-input-source-slot-getter (original-slot-name
                                     hidden-slot
                                     control-type)
  (if (isource-array-slot-p hidden-slot)
      `(defun ,original-slot-name (input-source index)
         (ensure-n-long (,(first hidden-slot) input-source)
                        (+ index 1)
                        (,(control-constructor-name control-type)))
         (,(control-data-acc-name control-type)
           (aref (,(first hidden-slot) input-source) index)))
      `(defun ,original-slot-name (input-source)
         (,(control-data-acc-name control-type)
           (,(first hidden-slot) input-source)))))

(defun gen-input-source-slot-setter (original-slot-name
                                     hidden-slot
                                     control-type)
  (let* ((p (symbol-package original-slot-name))
         (func-name (symb p "SET-" original-slot-name)))
    (if (isource-array-slot-p hidden-slot)
        `(defun ,func-name (input-source index timestamp data &optional tpref)
           ;; {TODO} I still dont like this being in the aref
           (ensure-n-long
            (,(first hidden-slot) input-source)
            (+ index 1)
            (,(control-constructor-name control-type)))
           (let* ((control (aref (,(first hidden-slot) input-source)
                                 index)))
             (setf (,(control-data-acc-name control-type) control)
                   data)
             (propagate data control input-source index timestamp tpref)
             data))
        `(defun ,func-name (input-source timestamp data &optional tpref)
           (let* ((control (,(first hidden-slot) input-source)))
             (setf (,(control-data-acc-name control-type) control)
                   data)
             (propagate data control input-source -1 timestamp tpref)
             data)))))

(defun gen-struct-slot-from-input-source-slot (s)
  (let* ((name (first s))
         (array? (isource-array-slot-p s))
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
    `(,name ,init :type ,type)))

(defun gen-input-source-slot-name (type-name user-slot-name)
  (symb (symbol-package type-name) type-name :- user-slot-name))

(defun input-source-hidden-constructor-name (type-name)
  (intern-hidden "%MAKE-~a" type-name))

(defun input-source-constructor-name (type-name)
  (intern (format nil "MAKE-~a" type-name) (symbol-package type-name)))

(defmacro define-input-source (name (&key static) &body slots)
  (let* ((original-slot-names (mapcar (lambda (x) (gen-input-source-slot-name
                                                   name x))
                                      (mapcar #'first slots)))
         (types (mapcar #'second slots))
         (lengths (mapcar #'third slots))
         ;;
         (hidden-slot-names (mapcar #'hide original-slot-names))
         (hidden-slots (mapcar #'cons hidden-slot-names (mapcar #'rest slots)))
         ;;
         (constructor (input-source-hidden-constructor-name name))
         (def (if static 'defstruct 'deftclass)))
    `(progn
       ;; Type
       (,def (,name (:constructor ,constructor)
                    (:conc-name nil))
           ,@(mapcar #'gen-struct-slot-from-input-source-slot hidden-slots))

       (defmethod print-object ((obj ,name) stream)
         (print-unreadable-object (obj stream :type t :identity t)))

       ;; public constructor
       (defun ,(input-source-constructor-name name) ()
         (let ((result (,constructor)))
           ,@(denil
              (mapcar #'gen-populate-control
                      types
                      hidden-slot-names
                      lengths
                      original-slot-names))
           (initialize-kind result)
           result))

       ;; - Internal -
       ;; Exists so backends can hook onto this event and populate the device
       ;; elements. E.g. adding whatever keys or buttons the backend supports
       (defmethod initialize-kind ((obj ,name))
         obj)

       ;;
       ,@(denil
          (mapcar #'gen-input-source-slot-getter
                  original-slot-names
                  hidden-slots
                  types))
       ,@(denil
          (mapcar #'gen-input-source-slot-setter
                  original-slot-names
                  hidden-slots
                  types))

       ,@(gen-add-methods name types hidden-slot-names lengths
                          original-slot-names)

       (defmethod get-control ((input-source ,name)
                               &optional slot-name index allow-arr-result)
         (declare (ignorable allow-arr-result))
         (ecase slot-name
           ,@(loop :for (cname type length) :in slots
                :for slot-name :in hidden-slot-names :collect
                (let ((kwd (intern (symbol-name cname) :keyword))
                      (msg0 (format nil "SKITTER: ~a in ~a is a array of ~a"
                                    cname name type))
                      (msg1 (format nil "SKITTER: ~a in ~a is not an array of controls. No index is required"
                                    cname name)))
                  (if length
                      `(,kwd
                        (cond
                          (index (aref (,slot-name input-source) index))
                          (allow-arr-result (,slot-name input-source))
                          (t (error ,msg0))))
                      `(,kwd
                        (assert (null index) () ,msg1)
                        (,slot-name input-source)))))))

       (defmethod listen-to ((listener event-listener) (input-source ,name)
                             slot-name &optional index)
         (let ((control/s (get-control input-source slot-name index t)))
           (setf (event-listener-input-source-type listener)
                 (type-of input-source))
           (if (arrayp control/s)
               (loop :for control :across control/s :do
                  (listen-to-control control listener))
               (listen-to-control control/s listener))
           listener)))))

;;----------------------------------------------------------------------
