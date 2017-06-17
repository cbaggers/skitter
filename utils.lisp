(in-package :skitter)

;;----------------------------------------------------------------------

(defun denil (x) (remove nil x))

;;----------------------------------------------------------------------

(defun make-n-long (array n)
  (adjust-array array n :fill-pointer (1- n)))

(defmacro ensure-n-long (array n &optional init-form)
  (let ((arr (gensym "array"))
        (index (gensym "n")))
    `(let ((,arr ,array)
           (,index ,n))
       (when (< (length ,arr) ,index)
         ,(if init-form
              `(setf (aref (make-n-long ,arr ,index) ,index) ,init-form)
              `(make-n-long ,arr ,index)))
       ,arr)))

;;----------------------------------------------------------------------

(defun shifting-remove (arr element &optional blank)
  ;; {TODO} why not move last listener to hole rather than
  ;;        shifting?
  (let ((count 0)
        (adjustable (adjustable-array-p arr)))
    (loop :for e :across arr :for i :from 0 :do
       (if (eq e element)
           (progn
             (setf (aref arr i) blank)
             (incf count))
           (when (> count 0)
             (setf (aref arr (- i count)) (aref arr i))
             (setf (aref arr i) blank))))
    (when adjustable
      (decf (fill-pointer arr) count))
    arr))

;;----------------------------------------------------------------------
