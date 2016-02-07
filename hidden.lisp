(in-package :skitter)

(defun hide (symbol)
  (let ((name (format nil "~s-~s"
		      (package-name (symbol-package symbol))
		      (symbol-name symbol)))
	(skitter-hidden (find-package :skitter-hidden)))

    (or (find-symbol name skitter-hidden)
	(intern (format nil "~s-~s"
			(package-name (symbol-package symbol))
			(symbol-name symbol))
		skitter-hidden))))


(defmacro with-hidden (names &body body)
  `(symbol-macrolet ,(mapcar (lambda (n) `(,n ,(hide n))) names)
     ,@body))
