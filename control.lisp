(in-package :skitter)

;; - We only hold the latest state in a control
;; - if you need a signal that is a composition of events then use a combo
;; -

;;----------------------------------------------------------------------

;; We used to have a supertype called control but has to remove it as we want
;; to be able to have some controls be dynamically redefinable (using classes)
;; and some to be static (using structs)

(defgeneric add (inst control))
(defgeneric control-listeners (control))
(defgeneric listen-to-control (control listener))
(defgeneric remove-listener (listener input))
(defgeneric remove-control (control)
  (:method ((control t)) nil))

;;----------------------------------------------------------------------

(defun control-hidden-constructor-name (control-type)
  (intern-hidden "%MAKE-" control-type))

(defun control-constructor-name (control-type)
  (symb (symbol-package control-type) "MAKE-" control-type))

(defun control-data-acc-name (control-type)
  (symb (symbol-package control-type) control-type "-DATA"))

(defun control-container-slot-name (control-type)
  (intern-hidden control-type "-CONTAINER-SLOT"))

(defun control-container-index-name (control-type)
  (intern-hidden control-type "-CONTAINER-INDEX"))

(defun control-listeners-name (control-type)
  (intern-hidden control-type "-LISTENERS"))

(defmacro define-control (name (&key static) type init-val)
  (let* ((constructor (control-hidden-constructor-name name))
         (def (if static 'defstruct 'deftclass)))
    `(progn
       (,def (,name (:conc-name nil)
                    (:constructor ,constructor))
           (,(control-data-acc-name name) ,init-val :type ,type)
         (,(control-container-slot-name name) :unknown-slot :type symbol)
         (,(control-container-index-name name) -1 :type fixnum)
         (,(control-listeners-name name)
           (make-array 0 :element-type 'event-listener :adjustable t
                       :fill-pointer 0)
           :type (array event-listener (*))))

       ;; This exists so people can't set values via the constructor
       (defun ,(control-constructor-name name) ()
         (,constructor))
       (defmethod listen-to-control ((control ,name) (listener event-listener))
         (let ((arr (,(control-listeners-name name) control)))
           (vector-push-extend listener arr)
           (setf (event-listener-control listener) control)
           listener))
       (defmethod control-listeners ((control ,name))
         (,(control-listeners-name name) control))
       (defmethod remove-listener ((listener event-listener) (control ,name))
         (shifting-remove (,(control-listeners-name name) control)
                          listener
                          *null-listener*)
         nil))))

;;----------------------------------------------------------------------

;; {TODO} make this control specific as it's the only one using
;;        control-listeners.
;;        Would also need adding to logical-control
(defun propagate (data control input-source timestamp tpref)
  (loop :for listener :across (control-listeners control) :do
     (funcall (event-listener-callback listener)
              data listener input-source timestamp tpref)))

;;----------------------------------------------------------------------

(defmacro set-control-slots (control-type control slot-name index)
  "Set all the slots of an event source"
  (with-gensyms (ctrl current s-name idx)
    `(let* ((,ctrl ,control)
            (,s-name ,slot-name)
            (,idx ,index)
            (,current
             (or (unless (eq (,(control-container-slot-name control-type)
                               ,ctrl)
                            :unknown-slot)
                   ,s-name)
                 (unless (= (,(control-container-index-name control-type)
                              ,ctrl)
                            -1)
                   ,idx))))
       (assert (null ,current) () "SKITTER: ~a is already used in ~a"
               ,current ,ctrl)
       (setf (,(control-container-slot-name control-type) ,ctrl) ,s-name
             (,(control-container-index-name control-type) ,ctrl) ,idx))))

;;----------------------------------------------------------------------
