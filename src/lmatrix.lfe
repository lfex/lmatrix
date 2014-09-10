(defmodule lmatrix
  (export all))

(include-lib "lutil/include/compose-macros.lfe")

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
  ;; Original implementation, without the thrusing macro:
  ;; (set-nth
  ;;   i
  ;;   (set-nth
  ;;     j
  ;;     new-elem
  ;;     (get-nth i matrix))
  ;;   matrix))
  (set-nth
    i
    (->> matrix
         (get-nth i)
         (set-nth j new-elem))
    matrix))

(defun fill
  "Provide an fill matrix.

  Takes either an integer or a list of integers (as returned by the dim/1
  function)."
  ((`(,m ,n) val)
   (fill m n val))
  ((m val)
   (fill m m val)))

(defun fill (m n val)
  "Provide an fill matrix."
  (lists:duplicate m (lists:duplicate n val)))

(defun identity (m)
  (let ((matrix (fill m 0)))
    (identity m matrix)))

(defun identity
  ((0 matrix)
   matrix)
  ((i matrix)
   (identity (- i 1) (set-nth i i 1 matrix))))

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
  representing the indices for the rows to be swapped.

  This uses 0-based counting."
  ((index-1 index-2 matrix) (when (== index-1 index-2))
   matrix)
  ((index-1 index-2 matrix)
   (->> matrix
        (set index-1 (get index-2 matrix))
        (set index-2 (get index-1 matrix)))))
