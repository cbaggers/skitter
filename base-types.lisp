(in-package #:skitter)

;;----------------------------------------------------------------------
;; base event

(defstruct skitter-event
  (source-node (error "source-node is mandatory") :type skitter-event-node
	       :read-only t)
  (timestamp (get-internal-real-time) :type fixnum :read-only t))

;;----------------------------------------------------------------------
;; meta events

(defstruct
    (new-event-node-event
      (:include skitter-event
                (source-node event-system-meta-node)))
  (new-node (error "new-node must be provided")
	    :type skitter-event-node
	    :read-only t))

;;----------------------------------------------------------------------
;; helper macros

(defmacro %get (list)
  `(cdr ,list))

(defmacro %set (list value)
  `(setf (cdr ,list) ,value))

(defmacro %push (value list)
  `(%set ,list (cons ,value (%get ,list))))

(defmacro %delete (value list weak)
  (if weak
      `(%set ,list (delete ,value (%get ,list) :key #'subscriber-node))
      `(%set ,list (delete ,value (%get ,list)))))

;;----------------------------------------------------------------------
;; event nodes

(defconstant +default-node-name+ :no-human-name)

(defstruct (skitter-event-node (:constructor %make-skitter-event-node)
			    (:conc-name event-node-))
  (uid (gensym "EVENT-NODE-")
       :type symbol)
  (name +default-node-name+
        :type symbol)
  (tags nil
        :type list
        :read-only t)
  (%subscribers (cons nil nil) ;; weak refs to consumers
               :type list
               :read-only t)
  (%subscriptions (cons nil nil) ;; strong refs to nodes
                 :type list
                 :read-only t)
  (filter #'event-no-filter
          :type function
          :read-only t)
  (body #'event-no-body
	:type function
	:read-only t))

(defun event-node-subscribers (node)
  (%get (event-node-%subscribers node)))

(defun event-node-subscriptions (node)
  (%get (event-node-%subscriptions node)))

(defmethod print-object ((object skitter-event-node) stream)
  (format stream "#<SKITTER-EVENT-NODE ~@[:NAME ~S ~]:UID ~s>"
          (event-node-name object)
          (event-node-uid object)))

(defun event-node-eq (node-a node-b)
  (eq (event-node-uid node-a) (event-node-uid node-b)))

(defun event-no-filter (e)
  (declare (ignore e) (skitter-event e))
  t)

(defun event-no-body (e)
  (declare (ignore e) (skitter-event e))
  nil)

(defstruct (%event-node-subscriber (:conc-name %ens-))
  (weak t :type boolean))

(defstruct (weak-subscriber
	     (:include %event-node-subscriber)
	     (:conc-name weak-subscriber-))
  #+sbcl (node (error "Node must be provided to make subscriber")
	       :type sb-ext:weak-pointer)
  #-sbcl (node (error "Node must be provided to make subscriber")
	       :type t))

(defstruct (strong-subscriber
	     (:include %event-node-subscriber
		       (weak nil)))
  (node (error "Node must be provided to make subscriber")
	:type skitter-event-node))

(defun make-subscriber (node weak)
  (if weak
      (make-weak-subscriber :node (trivial-garbage:make-weak-pointer node))
      (make-strong-subscriber :node node)))

(defun subscriber-node (node)
  (declare ((or strong-subscriber weak-subscriber) node))
  (if (%ens-weak node)
      (locally (declare (weak-subscriber node))
	(trivial-garbage:weak-pointer-value (weak-subscriber-node node)))
      (locally (declare (strong-subscriber node))
	(strong-subscriber-node node))))

(defun make-event-node (&key name tags (filter #'event-no-filter)
                          (body #'event-no-body)
                          subscribe-to)
  (%make-node-asserts filter body subscribe-to)
  (%make-node-post-proc
   (%make-skitter-event-node
    :name (or name +default-node-name+)
    :tags (if (listp tags) tags (list tags))
    :filter filter
    :body body)
   subscribe-to))

(defun %make-node-asserts (filter body subscribe-to)
  (assert (typep filter 'function))
  (assert (typep body 'function))
  (assert (or (null subscribe-to) (typep subscribe-to 'skitter-event-node))))

(defun %make-node-post-proc (new-node subscribe-to)
  (when (boundp 'event-system-meta-node)
    (%push-event-to-event-node (symbol-value 'event-system-meta-node)
			       (make-new-event-node-event :new-node new-node)))
  (when subscribe-to (subscribe new-node subscribe-to))
  new-node)

(defmacro def-event-node-type (name &body slots)
  "This macro is used to define a new kind of event node. In most case this
   should not be neccesary as event nodes can carry any kind of event.
   See #'make-event-node to make sure that you definitely need to use
   def-event-node-type"
  (labels ((mkstr (&rest args)
	     (with-output-to-string (s)
	       (dolist (a args) (princ a s))))
	   (symb-package (package &rest args)
	     (values (intern (apply #'mkstr args) package)))
	   (flatten (x &optional acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (flatten (car x)
                           (flatten (cdr x) acc)))))
	   (kwd (&rest args)
	     (intern (apply #'mkstr args) "KEYWORD")))

    (let ((%make-name (symb-package (symbol-package name) '%make- name))
	  (make-name (symb-package (symbol-package name) 'make- name))
	  (slot-names (mapcar #'first slots)))
      `(progn
	 (defstruct (,name (:include skitter::skitter-event-node)
			   (:constructor ,%make-name))
	   ,@slots)
	 (defun ,make-name (&key (name +default-node-name+) tags (filter #'event-no-filter)
			      (body #'event-no-body) subscribe-to
			      ,@slot-names)
	   (%make-node-asserts filter body subscribe-to)
	   (%make-node-post-proc
	    (,%make-name
	     :name name
	     :tags (if (listp tags) tags (list tags))
	     :filter filter
	     :body body
	     ,@(flatten (mapcar #'(lambda (x) (list (kwd x) x)) slot-names)))
	    subscribe-to))))))

(defun subscribe (node source-node &key (weak t))
  (assert (typep node 'skitter-event-node))
  (assert (typep source-node 'skitter-event-node))
  (unless (event-node-already-subscribed node source-node)
    (%push (make-subscriber node weak) (event-node-%subscribers source-node))
    (%push source-node (event-node-%subscriptions node)))
  source-node)

(defun unsubscribe (node source-node)
  (assert (typep node 'skitter-event-node))
  (assert (typep source-node 'skitter-event-node))
  (%delete node (event-node-%subscribers source-node) t)
  (%delete node (event-node-%subscriptions source-node) nil)
  source-node)

(defun unsubscribe-from-all (node)
  (assert (typep node 'skitter-event-node))
  (mapcar #'(lambda (x) (unsubscribe node x))
	  (event-node-subscriptions node)))

(defun event-node-already-subscribed (node source-node)
  (member node (event-node-subscribers source-node)
	  :key #'subscriber-node :test #'event-node-eq))

(defun %move-subscriptions (from to)
  (assert (and from to))
  (let ((old-subscribers
	 (remove nil (mapcar #'subscriber-node (event-node-subscribers from))))
        (old-subscriptions (event-node-subscriptions from)))
    ;; recreate all the old subscriptions to and from new node
    (map nil (lambda (x) (subscribe x to)) old-subscribers)
    (map nil (lambda (x) (subscribe to x)) old-subscriptions)
    ;; remove subscriptions from old node
    (map nil (lambda (x) (unsubscribe x from)) old-subscribers)
    (map nil (lambda (x) (unsubscribe from x)) old-subscriptions))
  to)

;;----------------------------------------------------------------------
;; pushing events to nodes

(defun %push-event-to-event-node (node event)
  (labels ((push-to-subscriber (subscriber)
             (let ((destination-node (subscriber-node subscriber)))
               (when destination-node
		 (%push-event-to-event-node destination-node event)))))
    (when (funcall (event-node-filter node) event)
      (funcall (event-node-body node) event)
      (mapcar #'push-to-subscriber (event-node-subscribers node)))
    node))

(defun push-event (to-node event)
  (%push-event-to-event-node to-node event))
