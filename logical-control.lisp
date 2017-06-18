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
      (let* ((this (gensym "THIS")))
        `(defun ,logic-func-name (,var-name ,this input-source timestamp tpref)
           (declare (ignorable ,var-name tpref timestamp))
           (labels ((fire (new-val &optional tpref)
                      (setf (,(control-data-acc-name logi-control-name) ,this)
                            new-val)
                      (propagate new-val ,this input-source timestamp tpref)))
             (symbol-macrolet
                 (,@(mapcar (lambda (n a) `(,n (,a ,this)))
                            internal-slot-names
                            internal-acc-names))
               (locally
                   (declare (optimize (speed 1) (debug 1) (space 1)
                                      (safety 1) (compilation-speed 1)))
                 ,@body
                 (values)))))))))

(defmacro define-logical-control
    ((name &key (type 'boolean) (initform nil)) internal-slots
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
         (control-slot-names (loop :for i :below (length control-arg-forms) :collect
                                (intern-hidden name "-CONTROL-" i)))
         (logic-func-names (loop :for name :in control-slot-names :collect
                              (intern-hidden name "-LOGIC" )))
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
         (,(control-data-acc-name name) ,initform :type ,type)
         ;; the details for things listening to this control
         (,(control-container-slot-name name) :unknown-slot :type symbol)
         (,(control-container-index-name name) -1 :type fixnum)
         (,(control-listeners-name name)
           (make-array 0 :element-type 'logical-control :adjustable t
                       :fill-pointer 0)
           :type (array logical-control (*)))
         ;; the internal state for this control. Usable by the user defined
         ;; code, but doesnt get passed anywhere implicitly
         ,@(mapcar (lambda (n s)
                     (destructuring-bind (_ init type) s
                       (declare (ignore _))
                       `(,n ,init :type ,type)))
                   internal-acc-names
                   internal-slots)
         ;;
         ,@(mapcar (lambda (cn ct) `(,cn nil ,ct))
                   control-slot-names
                   control-types))

       (defun ,add (source &key ,@add-arg-names)
         (let ((result (,constructor))
               ,@(loop :for name :in add-arg-names :collect
                    `(,name (%get-control source ,name))))
           ,@(loop :for (arg-name kind) :in control-arg-forms :append
                `((assert (typep ,arg-name ',kind))
                  (listen-to result ,arg-name)))
           ,@(loop :for control-slot-name :in control-slot-names
                :for arg-name :in add-arg-names
                :for func :in logic-func-names
                :collect
                `(setf (,control-slot-name result)
                       (%make-event-listener
                        :subject result
                        :callback #',func)
                       ,arg-name))
           (add-logical-control result)))
       ,@(loop :for form :in control-forms :for func-name :in logic-func-names
            :collect (gen-control-logic-func
                      name
                      internal-slot-names
                      internal-acc-names
                      func-name
                      form))
       (defmethod free-control ((control ,name))
         ;; remove this logical-control from the things it was listening to
         ,@(loop :for s :in control-slot-names :collect
              `(remove-listener listener (,s listener)))
         ;; remove the logic in case it was a closure an holding onto
         ;; something
         (setf (logical-control-logic listener) #'control-is-unregistered
               ;; remove anything it was listening to
               (,(control-listeners-name name) listener) nil
               ;; free all the slots, just in case someone holds onto the
               ;; logic func for some reason
               ,@(loop :for s :in control-slot-names :append
                    `((,s listener) nil))))
       (defmethod control-listeners ((control ,name))
         (,(control-listeners-name name) control)))))

(defun control-is-unregistered (_ _1 _2)
  (declare (ignore _ _1 _2))
  (error "Skitter: This logical-control is not registered to anything"))

;;----------------------------------------------------------------------
;; Messing around

;; (define-logical-control (double-click :type boolean :initform nil)
;;     ((last-timestamp 0 integer))
;;   ((button-a boolean-state)
;;    (setf last-timestamp timestamp)))

;; (define-input-source moose ()
;;   (button boolean-state *)
;;   (dub double-click *))
