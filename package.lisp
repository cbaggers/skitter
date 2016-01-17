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

	   :all-events
	   :event-system-meta-node))
