(in-package :skitter)

(defgeneric listen-to (listener input &optional slot-name))
(defgeneric stop-listening (listener))
(defgeneric remove-listener (listener input))
(defgeneric initialize-kind (obj))

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

(defun gen-input-kind-accessor (original-slot-name
                                hidden-slot
                                type-constructor-name)
  (when (third hidden-slot)
    `(defun ,original-slot-name (control index)
       (aref (ensure-n-long (,(first hidden-slot) control)
                            index
                            (,type-constructor-name))
             index))))

(defun intern-listener-name (name)
  (intern (format nil "~s-LISTENERS" name) (symbol-package name)))

(defun intern-listener-names (names)
  (loop :for n :in names :collect (intern-listener-name n)))

(defun parse-input-source-slot (s)
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
    `(,name ,init :type ,type)))

(defmacro define-input-source (name &body slots)
  (let* ((original-slot-names
          (mapcar (lambda (x) (intern (format nil "~a-~a" name x)
                                      (symbol-package name)))
                  (mapcar #'first slots)))
         (hidden-slot-names (mapcar (lambda (x s) (if (third s) (hide x) x))
                                    original-slot-names
                                    slots))
         (listener-slot-names (intern-listener-names original-slot-names))
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
         ,@(mapcar #'parse-input-source-slot hidden-slots)
         ,@(loop :for s :in listener-slot-names :collect
              `(,s (make-array 0 :element-type 'predicate-source
                               :adjustable t
                               :fill-pointer 0)
                   :type (array predicate-source (*)))))

       (defun ,make ()
         (let ((result (,constructor)))
           ,@(remove nil
                     (mapcar #'gen-populate-control
                             hidden-slot-names
                             listener-slot-names
                             lengths
                             original-slot-names))
           (initialize-kind result)
           result))

       (defmethod initialize-kind ((obj ,name))
         "- Internal -
            Exists so backends can hook onto this event and populate the device
            elements. E.g. adding whatever keys or buttons the backend supports"
         obj)

       ,@(denil
          (mapcar #'gen-input-kind-accessor
                  original-slot-names
                  hidden-slots
                  type-constructors))

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
