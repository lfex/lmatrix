(defmodule unit-lmatrix-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun matrix-0a ()
  '())

(defun matrix-0b ()
  '(()))

(defun matrix-1 ()
  '((1)))

(defun matrix-2 ()
  '((1 2)
    (3 4)))

(defun matrix-3 ()
  '((1 2 3)
    (4 5 6)
    (7 8 9)))

(defun matrix-4 ()
  '((1 2)
    (2 4)
    (5 6)
    (7 8)))

(defun matrix-5 ()
  '((1 2 3 4 5)
    (6 7 8 9 10)))

(deftest dim
  (is-equal '(0 0) (lmatrix:dim (matrix-0a)))
  (is-equal '(0 0) (lmatrix:dim (matrix-0b)))
  (is-equal '(1 1) (lmatrix:dim (matrix-1)))
  (is-equal '(2 2) (lmatrix:dim (matrix-2)))
  (is-equal '(3 3) (lmatrix:dim (matrix-3)))
  (is-equal '(4 2) (lmatrix:dim (matrix-4)))
  (is-equal '(2 5) (lmatrix:dim (matrix-5))))
