;;;; package.lisp

(defpackage #:skitter
  (:use #:cl)
  (:export :skitter-event
	   :make-event-node
           :subscribe
           :unsubscribe
	   :unsubscribe-from-all
	   :push-event
	   :def-named-event-node
	   :skitter-event-source-node
	   :skitter-event-timestamp

	   :event-node-uid
	   :event-node-name
	   :event-node-tags
	   :event-node-filter
	   :event-node-body

	   :all-events
	   :event-system-meta-node))
