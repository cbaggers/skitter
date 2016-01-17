(in-package #:skitter)

;;======================================================================
;; Event Nodes

;;----------------------------------------------------------------------
;; backend events

(defvar all-events
  (make-event-node
   :name :all-events
   :tags '(:everything :event-system-meta)))

;;----------------------------------------------------------------------
;; meta events

(defvar event-system-meta-node
  (make-event-node
   :name :event-system
   :tags :event-system-meta
   :subscribe-to all-events))
