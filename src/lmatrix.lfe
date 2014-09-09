(defmodule lmatrix
  (export all))

(defun get (i matrix)
  "Get the ith matrix row.

  This uses 0-based counting."
  (get-nth (+ i 1) matrix))

(defun get (i j matrix)
  "Get the matrix element in the ith row and the jth column.

  This uses 0-based counting."
  (get-nth (+ i 1) (+ j 1) matrix))

(defun get-nth (i matrix)
  "Get the ith matrix row.

  This uses the same 1-based counting as Erlang's lists:nth/2."
  (lists:nth i matrix))

(defun get-nth (i j matrix)
  "Get the matrix element in the ith row and the jth column.

  This uses the same 1-based counting as Erlang's lists:nth/2."
  (lists:nth j (lists:nth i matrix)))

(defun set (i new-row matrix)
  "Set the ith matrix row.

  This uses 0-based counting."
  (set-nth (+ i 1) new-row matrix))

(defun set (i j new-elem matrix)
  "Set the matrix element in the ith row and the jth column.

  This uses 0-based counting."
  (set-nth (+ i 1) (+ j 1) new-elem matrix))

(defun set-nth
  "Set the ith matrix row.

  This uses the same 1-based counting as Erlang's lists:nth/2."
  ((1 new-row (cons _ rest))
   (cons new-row rest))
  ((i new-row (cons first rest))
   (cons first (set-nth (- i 1) new-row rest))))

(defun set-nth (i j new-elem matrix)
  "Set the matrix element in the ith row and the jth column.

  This uses the same 1-based counting as Erlang's lists:nth/2."
  (set-nth
    i
    (set-nth
      j
      new-elem
      (get-nth i matrix))
    matrix))

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

(defun swap-rows
  "Swap two rows in a matrix, given a matrix (list of lists) and two integers
  representing the indices for the rows to be swapped."
  ((index-1 index-2 matrix) (when (== index-1 index-2))
   matrix)
  ((index-1 index-2 matrix)
   (let ((first-swap (set index-1 (get index-2 matrix) matrix)))
     (set index-2 (get index-1 matrix) first-swap))))
