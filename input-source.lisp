(in-package :skitter)

(defgeneric listen-to (listener input &optional slot-name))
(defgeneric stop-listening (listener))
(defgeneric remove-listener (listener input))
(defgeneric initialize-kind (obj))

(defun isource-array-slot-p (slot)
  (or (string= :* (third slot))
      (numberp (third slot))))
;;----------------------------------------------------------------------

(defun gen-populate-control (hidden-slot-name
                             listener-slot-name
                             length
                             original-slot-name)
  ;; {TODO} needs better explanation
  "gens nil when there is a length as then you add the event controls using
   the add methods"
  (when (not length)
    `(set-control-slots
      (,hidden-slot-name result)
      (,listener-slot-name result)
      ',original-slot-name
      -1)))

(defun gen-add-methods (name
                        types
                        hidden-slot-names
                        lengths
                        listener-slot-names
                        original-slot-names)
  "This is a partner in crimer to #'gen-populate-control in that we
   on need add methods when there is a length"
  (denil
   (loop :for type :in types
      :for hidden-slot-name :in hidden-slot-names
      :for length :in lengths
      :for listener-slot-name :in listener-slot-names
      :for original-slot-name :in original-slot-names
      :when length :collect
      (when length
        (let ((push (if (numberp length)
                        'vector-push
                        'vector-push-extend)))
          `(defmethod add ((inst ,name) (control ,type))
             (let ((arr (,hidden-slot-name inst)))
               (,push control arr)
               (set-control-slots control
                                  (,listener-slot-name inst)
                                  ',original-slot-name
                                  (position control arr)))))))))

(defun gen-input-source-slot-getter (original-slot-name
                                     hidden-slot
                                     control-type)
  (if (isource-array-slot-p hidden-slot)
      `(defun ,original-slot-name (input-source index)
         (,(control-data-acc-name control-type)
           (aref (ensure-n-long (,(first hidden-slot) input-source)
                                index
                                (,(control-constructor-name control-type)))
                 index)))
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
           (let ((control (aref (ensure-n-long
                                 (,(first hidden-slot) input-source)
                                 index
                                 (,(control-constructor-name control-type)))
                                index)))
             (setf (,(control-data-acc-name control-type) control)
                   data)
             (propagate control timestamp tpref)
             data))
        `(defun ,func-name (input-source timestamp data &optional tpref)
           (let ((control (,(first hidden-slot) input-source)))
             (setf (,(control-data-acc-name control-type) control)
                   data)
             (propagate control timestamp tpref)
             data)))))

(defun intern-listener-name (name)
  (intern (format nil "~s-LISTENERS" name) (symbol-package name)))

(defun intern-listener-names (names)
  (loop :for n :in names :collect (intern-listener-name n)))

(defun parse-input-source-slot (s)
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
  (intern (format nil "%MAKE-~a" type-name) :skitter-hidden))

(defun input-source-constructor-name (type-name)
  (intern (format nil "MAKE-~a" type-name) (symbol-package type-name)))

(defmacro define-input-source (name (&key static) &body slots)
  (let* ((original-slot-names (mapcar (lambda (x) (gen-input-source-slot-name
                                                   name x))
                                      (mapcar #'first slots)))
         (hidden-slot-names (mapcar #'hide original-slot-names))
         (listener-slot-names (intern-listener-names original-slot-names))
         (hidden-slots (mapcar #'cons hidden-slot-names (mapcar #'rest slots)))
         (types (mapcar #'second slots))
         (constructor (input-source-hidden-constructor-name name))
         (lengths (mapcar #'third slots))
         (def (if static 'defstruct 'deftclass)))
    `(progn
       ;; Type
       (,def (,name (:constructor ,constructor)
                    (:conc-name nil))
         ,@(mapcar #'parse-input-source-slot hidden-slots)
         ,@(loop :for s :in listener-slot-names :collect
              `(,s (make-array 0 :element-type 'predicate-source
                               :adjustable t
                               :fill-pointer 0)
                   :type (array predicate-source (*)))))

       ;; public constructor
       (defun ,(input-source-constructor-name name) ()
         (let ((result (,constructor)))
           ,@(denil
              (mapcar #'gen-populate-control
                      hidden-slot-names
                      listener-slot-names
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
                          listener-slot-names original-slot-names)

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
           (values listener input))))))

;;----------------------------------------------------------------------
