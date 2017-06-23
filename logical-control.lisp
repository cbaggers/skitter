(in-package :skitter)

;;----------------------------------------------------------------------

(defun valid-control-form-p (control-form)
  (labels ((valid-control-arg (arg)
             (destructuring-bind (control-var-name control-type) arg
               (and (symbolp control-var-name) (symbolp control-type)))))
    (destructuring-bind (arg . body) control-form
      (declare (ignore body))
      (valid-control-arg arg))))

(defun gen-control-logic-func (logi-control-name
                               internal-slot-names
                               internal-acc-names
                               logic-func-name
                               control-form)

  (destructuring-bind (arg . body) control-form
    (destructuring-bind (var-name control-type) arg
      (declare (ignorable control-type))
      (let* ((this (gensym "THIS"))
             (listener (gensym "LISTENER")))
        `(defun ,logic-func-name
             (,var-name ,listener input-source index timestamp tpref)
           (declare (ignorable ,var-name tpref timestamp))
           (let ((,this (event-listener-subject ,listener)))
             (labels ((fire (new-val &optional tpref)
                        (setf (,(control-data-acc-name logi-control-name)
                                ,this)
                              new-val)
                        (propagate new-val ,this input-source index timestamp
                                   tpref)))
               (symbol-macrolet
                   (,@(mapcar (lambda (n a) `(,n (,a ,this)))
                              internal-slot-names
                              internal-acc-names))
                 (locally
                     (declare (optimize (speed 1) (debug 1) (space 1)
                                        (safety 1) (compilation-speed 1)))
                   ,@body
                   (values))))))))))

(defmacro define-logical-control
    ((name &key (type 'boolean) (initform nil) decays) internal-slots
     &body control-forms)
  (assert control-forms () "SKITTER: ~a must have at least one control form"
          name)
  (assert (every #'valid-control-form-p control-forms))
  (let* ((p (symbol-package name))
         ;; funcs
         (add (symb p "ADD-" name))
         (constructor (symb p "%MAKE-" name))

         ;; control slots
         (control-arg-forms (mapcar #'first control-forms))
         (control-slot-names (loop :for i :below (length control-arg-forms)
                                :collect (intern-hidden name "-CONTROL-" i)))
         (logic-func-names (loop :for name :in control-slot-names
                              :collect (intern-hidden name "-LOGIC" )))
         (control-types (mapcar #'second control-arg-forms))
         (add-arg-names (mapcar #'caar control-forms))
         ;; state-slots
         (internal-slot-names (mapcar #'first internal-slots))
         (internal-acc-names (mapcar (lambda (x) (symb p name "-" x))
                                 internal-slot-names)))
    `(progn
       (deftclass (,name (:conc-name nil)
                         (:constructor ,constructor))
         ;; the state for this control
         (,(control-hidden-data-name name) ,initform :type ,type)
         ;; state decay logic
         (,(control-decay-name name) ,decays :type boolean)
         (,(control-last-frame-name name) 0 :type (unsigned-byte 16))
         ;; the details for things listening to this control
         (,(control-container-slot-name name) :unknown-slot :type symbol)
         (,(control-container-index-name name) -1 :type fixnum)
         (,(control-listeners-name name)
           (make-array 0 :element-type 'event-listener :adjustable t
                       :fill-pointer 0)
           :type (array event-listener (*)))
         ;; the internal state for this control. Usable by the user defined
         ;; code, but doesnt get passed anywhere implicitly
         ,@(mapcar (lambda (n s)
                     (destructuring-bind (_ init type) s
                       (declare (ignore _))
                       `(,n ,init :type ,type)))
                   internal-acc-names
                   internal-slots)
         ;;
         ,@(mapcar (lambda (slot-name slot-type)
                     `(,slot-name nil :type (or null ,slot-type)))
                   control-slot-names
                   control-types))
       ;; set & get the for the data
       (declaim (type (function (,name (unsigned-byte 16)) ,type)
                      ,(control-data-acc-name name))
                (inline ,(control-data-acc-name name)))
       (defun ,(control-data-acc-name name) (control)
         (declare (optimize (speed 3) (safety 1) (debug 0))
                  (inline frame-id ,(control-decay-name name)
                          ,(control-last-frame-name name)
                          ,(control-hidden-data-name name))
                  (type ,name control))
         (let ((frame-id (frame-id)))
           (if (and (,(control-decay-name name) control)
                    (/= (the (unsigned-byte 16) (,(control-last-frame-name name) control))
                        frame-id))
               (progn
                 (setf (,(control-last-frame-name name) control) frame-id)
                 (setf (,(control-hidden-data-name name) control) ,initform))
               (,(control-hidden-data-name name) control))))
       (defun (setf ,(control-data-acc-name name)) (value control)
         (declare (optimize (speed 3) (safety 1) (debug 0))
                  (inline frame-id ,(control-last-frame-name name)
                          ,(control-hidden-data-name name))
                  (type ,name control)
                  (type ,type value))
         (let ((frame-id (frame-id)))
           (setf (,(control-last-frame-name name) control) frame-id)
           (setf (,(control-hidden-data-name name) control) value)))

       (defun ,add (input-source &key ,@add-arg-names)
         (let ((result (,constructor))
               ,@(loop :for arg-name :in add-arg-names :append
                    `((,arg-name (if (listp ,arg-name)
                                     ,arg-name
                                     (list ,arg-name))))))
           ,@(loop :for control-slot-name :in control-slot-names
                :for control-type :in control-types
                :for arg-name :in add-arg-names
                :for func :in logic-func-names :append
                `((assert (typep (get-control input-source
                                              (first ,arg-name)
                                              (second ,arg-name))
                                 ',control-type))
                  (setf (,control-slot-name result)
                        (listen-to (%make-event-listener
                                    :callback #',func
                                    :subject ,arg-name)
                                   input-source
                                   (first ,arg-name)
                                   (second ,arg-name)))))
           result))
       ,@(loop :for form :in control-forms :for func-name :in logic-func-names
            :collect (gen-control-logic-func
                      name
                      internal-slot-names
                      internal-acc-names
                      func-name
                      form))
       (defmethod remove-control ((control ,name))
         ;; remove this logical-control from the things it was listening to and
         ;; free all the slots, just in case someone holds onto anything
         ,@(loop :for s :in control-slot-names :append
              `((stop-listening (,s control))
                (setf (,s control) nil)))
         ;;    remove anything it was listening to
         (setf (,(control-listeners-name name) control)
               (make-array 0 :element-type 'event-listener :adjustable t
                           :fill-pointer 0)))
       (defmethod control-listeners ((control ,name))
         (,(control-listeners-name name) control)))))


;; (define-logical-control (double-click :decays t)
;;     ((last-press nil integer))
;;   ((button boolean-control)
;;    (when (< (- timestamp last-press) 10)
;;      (fire t))
;;    (setf last-press timestamp)))
