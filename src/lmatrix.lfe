(defmodule lmatrix
  (export all))

(defun dim
  "Return a list of (m n) where m is the number of rows in the given matrix,
  and n is the number of columns."
  (('())
   '(0 0))
  (('(()))
   '(0 0))
  ((matrix)
    `(,(length matrix) ,(length (car matrix)))))
