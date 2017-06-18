(in-package :skitter)

;;----------------------------------------------------------------------

(defun denil (x) (remove nil x))

;;----------------------------------------------------------------------

(defun make-n-long (array n)
  (adjust-array array n :fill-pointer n))

(defmacro ensure-n-long (array n &optional init-form)
  (let ((arr (gensym "array"))
        (len (gensym "n")))
    `(let ((,arr ,array)
           (,len ,n))
       (when (< (length ,arr) ,len)
         ,(if init-form
              `(setf (aref (make-n-long ,arr ,len) (- ,len 1)) ,init-form)
              `(make-n-long ,arr ,len)))
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

(defun symb (package name-part &rest name-parts)
  (intern (format nil "~{~a~}" (cons name-part name-parts))
          package))

(defun symb- (package name-part &rest name-parts)
  (intern (format nil "~{~a~^-~}" (cons name-part name-parts))
          package))

;;----------------------------------------------------------------------

(defun empty-p (x)
  ;; here because import-from alexandria fucked up for some package
  ;; lock reason
  (alexandria:emptyp x))

;;----------------------------------------------------------------------

(defun hide (symbol &optional package)
  (when (stringp symbol) (assert package))
  (let* ((symbol-package (or (and (symbolp symbol) (symbol-package symbol))
                             package))
         (symbol-name (if (stringp symbol)
                          symbol
                          (symbol-name symbol)))
         (name (format nil "~a-~a"
                       (package-name symbol-package)
                       symbol-name))
         (skitter-hidden (find-package :skitter-hidden)))

    (or (find-symbol name skitter-hidden)
        (intern (format nil "~a-~a"
                        (package-name symbol-package)
                        symbol-name)
                skitter-hidden))))

(defun intern-hidden (&rest parts)
  (intern (format nil "~{~a~}" parts) :skitter-hidden))

;;----------------------------------------------------------------------
