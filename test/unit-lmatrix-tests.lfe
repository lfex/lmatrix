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

(defun matrix-6 ()
  '((1  2  3)
    (4  5  6)
    (7  8  9)
    (10 11 12)
    (13 14 15)
    (16 17 18)))

(defun matrix-7 ()
  '((1  2  3  4  5  6)
    (7  8  9  10 11 12)
    (13 14 15 16 17 18)))

(defun matrix-8 ()
  '((1  2  3  4)
    (5  6  7  8)
    (9  10 11 12)
    (13 14 15 16)))

(deftest dim
  (is-equal '(0 0) (lmatrix:dim (matrix-0a)))
  (is-equal '(0 0) (lmatrix:dim (matrix-0b)))
  (is-equal '(1 1) (lmatrix:dim (matrix-1)))
  (is-equal '(2 2) (lmatrix:dim (matrix-2)))
  (is-equal '(3 3) (lmatrix:dim (matrix-3)))
  (is-equal '(4 2) (lmatrix:dim (matrix-4)))
  (is-equal '(2 5) (lmatrix:dim (matrix-5))))

(deftest transpose-simple
  (is-equal '() (lmatrix:transpose (matrix-0a)))
  (is-equal '(()) (lmatrix:transpose (matrix-0b)))
  (is-equal '((1)) (lmatrix:transpose (matrix-1))))

(deftest transpose-2*
  (is-equal '((1 3)
              (2 4))
            (lmatrix:transpose (matrix-2)))
  (is-equal (matrix-2) (lmatrix:transpose (lmatrix:transpose (matrix-2))))
  (is-equal '((1 6)
              (2 7)
              (3 8)
              (4 9)
              (5 10))
            (lmatrix:transpose (matrix-5)))
  ;(is-equal (matrix-5) (lmatrix:transpose (lmatrix:transpose (matrix-5))))
  )

(deftest transpose-3*
  (is-equal '((1 4 7)
              (2 5 8)
              (3 6 9))
            (lmatrix:transpose (matrix-3)))
  (is-equal (matrix-3) (lmatrix:transpose (lmatrix:transpose (matrix-3))))
  (is-equal '((1 7  13)
              (2 8  14)
              (3 9  15)
              (4 10 16)
              (5 11 17)
              (6 12 18))
            (lmatrix:transpose (matrix-7)))
  ;(is-equal (matrix-7) (lmatrix:transpose (lmatrix:transpose (matrix-7))))
  )
