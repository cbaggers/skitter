(in-package :skitter)

(declaim (type (unsigned-byte 16) *frame-id*))
(defvar *frame-id* 0)

(declaim (type (function () (unsigned-byte 16)) frame-id)
         (inline frame-id))
(defun frame-id ()
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  *frame-id*)

(declaim (type (function () (values)) decay-events)
         (inline decay-events))
(defun decay-events ()
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (inline mod expt values))
  (setf *frame-id* (mod (+ *frame-id* 1) #.(expt 2 16)))
  (values))
