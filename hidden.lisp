(in-package :skitter)

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


(defmacro with-hidden (names &body body)
  `(symbol-macrolet ,(mapcar (lambda (n) `(,n ,(hide n))) names)
     ,@body))
