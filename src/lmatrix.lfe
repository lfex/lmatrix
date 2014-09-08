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

(defun trans (matrix)
  "Transpose a given matrix."
  (case (dim matrix)
    ('(0 0)
      matrix)
    ('(1 1)
      matrix)
    (`(2 ,_)
      (list-comp ((<- elem (apply #'lists:zip/2 matrix)))
                 (tuple_to_list elem)))
    (`(3 ,_)
      (list-comp ((<- elem (apply #'lists:zip3/3 matrix)))
                 (tuple_to_list elem)))
    (`(,_ ,_)
       (lutil-type:zip matrix))))

(defun mult (matrix-1 matrix-2)
  "Multiply two matrices."
  (list-comp
    ((<- a matrix-1))
    (list-comp
      ((<- b (trans matrix-2)))
      (lists:foldl #'+/2 0
                   (lists:zipwith #'*/2 a b)))))

(defun identity
  "Provide an identify matrix.

  Takes either an integer or a list of integers (as returned by the dim/1
  function)."
  ((`(,m ,n))
   (identity m n))
  ((m)
   (identity m m)))

(defun identity (m n)
  "Provide an identify matrix."
  (lists:duplicate m (lists:duplicate n 1)))

(defun swap-rows
  "Swap two rows in a matrix, given a matrix (list of lists) and two integers
  representing the indices for the rows to be swapped."
  ((matrix index-1 index-2) (when (== index-1 index-2))
   matrix)
  ((matrix index-1 index-2) (when (< index-1 0))
   `#(error "Smallest index must be 0 or greater."))
  ((matrix index-1 index-2) (when (>= index-2 (length matrix)))
   `#(error "Largest index cannot be larger than max row index."))
  ((matrix index-1 index-2) (when (> index-1 index-2))
   (swap-rows matrix index-2 index-1))
  ((matrix index-1 index-2)
   (let* ((`#(,part-1 ,part-2) (lists:split index-1 matrix))
          (rel-index (- index-2 index-1 1))
          (`#(,part-3 ,part-4) (lists:split rel-index (cdr part-2))))
     (++ part-1
         (list (lists:nth (+ index-2 1) matrix))
         part-3
         (list (lists:nth (+ index-1 1) matrix))
         (cdr part-4)))))
