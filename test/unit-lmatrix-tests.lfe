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
    (3 4)
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

(deftest get-by-subscript
  (is-equal '(1 2 3 4) (lmatrix:get 0 (matrix-8)))
  (is-equal '(5 6 7 8) (lmatrix:get 1 (matrix-8)))
  (is-equal '(9 10 11 12) (lmatrix:get 2 (matrix-8)))
  (is-equal '(13 14 15 16) (lmatrix:get 3 (matrix-8))))

(deftest get-nth-by-subscript
  (is-equal '(1 2 3 4) (lmatrix:get-nth 1 (matrix-8)))
  (is-equal '(5 6 7 8) (lmatrix:get-nth 2 (matrix-8)))
  (is-equal '(9 10 11 12) (lmatrix:get-nth 3 (matrix-8)))
  (is-equal '(13 14 15 16) (lmatrix:get-nth 4(matrix-8))))

(deftest get-by-subscripts
  (is-equal 1 (lmatrix:get 0 0 (matrix-8)))
  (is-equal 5 (lmatrix:get 1 0 (matrix-8)))
  (is-equal 7 (lmatrix:get 1 2 (matrix-8)))
  (is-equal 12 (lmatrix:get 2 3 (matrix-8)))
  (is-equal 16 (lmatrix:get 3 3 (matrix-8))))

(deftest get-nth-by-subscripts
  (is-equal 1 (lmatrix:get-nth 1 1 (matrix-8)))
  (is-equal 5 (lmatrix:get-nth 2 1 (matrix-8)))
  (is-equal 7 (lmatrix:get-nth 2 3 (matrix-8)))
  (is-equal 12 (lmatrix:get-nth 3 4 (matrix-8)))
  (is-equal 16 (lmatrix:get-nth 4 4 (matrix-8))))

(deftest set-by-subscript
  (is-equal
    '((1  1  1  1)
      (5  6  7  8)
      (9  10 11 12)
      (13 14 15 16))
    (lmatrix:set 0 '(1 1 1 1) (matrix-8)))
  (is-equal
    '((1  2  3  4)
      (1  1  1  1)
      (9  10 11 12)
      (13 14 15 16))
    (lmatrix:set 1 '(1 1 1 1) (matrix-8)))
  (is-equal
    '((1  2  3  4)
      (5  6  7  8)
      (9  10 11 12)
      (1  1  1  1))
    (lmatrix:set 3 '(1 1 1 1) (matrix-8))))

(deftest set-nth-by-subscript
  (is-equal
    '((1  1  1  1)
      (5  6  7  8)
      (9  10 11 12)
      (13 14 15 16))
    (lmatrix:set-nth 1 '(1 1 1 1) (matrix-8)))
  (is-equal
    '((1  2  3  4)
      (1  1  1  1)
      (9  10 11 12)
      (13 14 15 16))
    (lmatrix:set-nth 2 '(1 1 1 1) (matrix-8)))
  (is-equal
    '((1  2  3  4)
      (5  6  7  8)
      (9  10 11 12)
      (1  1  1  1))
    (lmatrix:set-nth 4 '(1 1 1 1) (matrix-8))))

(deftest set-by-subscripts
  (is-equal
    '((99 2  3  4)
      (5  6  7  8)
      (9  10 11 12)
      (13 14 15 16))
    (lmatrix:set 0 0 99 (matrix-8)))
  (is-equal
    '((1  2  3  4)
      (5  6  99 8)
      (9  10 11 12)
      (13 14 15 16))
    (lmatrix:set 1 2 99 (matrix-8)))
  (is-equal
    '((1  2  3  4)
      (5  6  7  8)
      (9  10 11 99)
      (13 14 15 16))
    (lmatrix:set 2 3 99 (matrix-8))))

(deftest set-nth-by-subscripts
  (is-equal
    '((99 2  3  4)
      (5  6  7  8)
      (9  10 11 12)
      (13 14 15 16))
    (lmatrix:set-nth 1 1 99 (matrix-8)))
  (is-equal
    '((1  2  3  4)
      (5  6  99  8)
      (9  10 11 12)
      (13 14 15 16))
    (lmatrix:set-nth 2 3 99 (matrix-8)))
  (is-equal
    '((1  2  3  4)
      (5  6  7  8)
      (9  10 11 99)
      (13 14 15 16))
    (lmatrix:set-nth 3 4 99 (matrix-8))))

(deftest fill-square
  (is-equal '((1)) (lmatrix:fill 1 1))
  (is-equal '((1 1) (1 1)) (lmatrix:fill 2 1))
  (is-equal '((1 1 1) (1 1 1) (1 1 1)) (lmatrix:fill 3 1)))

(deftest fill
  (is-equal '((1 1 1) (1 1 1) (1 1 1)) (lmatrix:fill 3 3 1))
  (is-equal '((1 1 1 1 1 1)
              (1 1 1 1 1 1)
              (1 1 1 1 1 1))
            (lmatrix:fill (lmatrix:dim (matrix-7)) 1)))

(deftest identity
  (is-equal '((1)) (lmatrix:identity 1))
  (is-equal '((1 0)
              (0 1)) (lmatrix:identity 2))
  (is-equal '((1 0 0)
              (0 1 0)
              (0 0 1)) (lmatrix:identity 3))
  (is-equal '((1 0 0 0)
              (0 1 0 0)
              (0 0 1 0)
              (0 0 0 1)) (lmatrix:identity 4)))

(deftest dimension
  (is-equal '(0 0) (lmatrix:dim (matrix-0a)))
  (is-equal '(0 0) (lmatrix:dim (matrix-0b)))
  (is-equal '(1 1) (lmatrix:dim (matrix-1)))
  (is-equal '(2 2) (lmatrix:dim (matrix-2)))
  (is-equal '(3 3) (lmatrix:dim (matrix-3)))
  (is-equal '(4 2) (lmatrix:dim (matrix-4)))
  (is-equal '(2 5) (lmatrix:dim (matrix-5))))

(deftest transpose-simple
  (is-equal '() (lmatrix:trans (matrix-0a)))
  (is-equal '(()) (lmatrix:trans (matrix-0b)))
  (is-equal '((1)) (lmatrix:trans (matrix-1))))

(deftest transpose-2*
  (is-equal '((1 3)
              (2 4))
            (lmatrix:trans (matrix-2)))
  (is-equal (matrix-2) (lmatrix:trans (lmatrix:trans (matrix-2))))
  (is-equal '((1 6)
              (2 7)
              (3 8)
              (4 9)
              (5 10))
            (lmatrix:trans (matrix-5)))
  (is-equal (matrix-5) (lmatrix:trans (lmatrix:trans (matrix-5)))))

(deftest transpose-3*
  (is-equal '((1 4 7)
              (2 5 8)
              (3 6 9))
            (lmatrix:trans (matrix-3)))
  (is-equal (matrix-3) (lmatrix:trans (lmatrix:trans (matrix-3))))
  (is-equal '((1 7  13)
              (2 8  14)
              (3 9  15)
              (4 10 16)
              (5 11 17)
              (6 12 18))
            (lmatrix:trans (matrix-7)))
  (is-equal (matrix-7) (lmatrix:trans (lmatrix:trans (matrix-7)))))

(deftest transpose-any
  (is-equal '((1 3 5 7)
              (2 4 6 8))
            (lmatrix:trans (matrix-4)))
  (is-equal (matrix-4) (lmatrix:trans (lmatrix:trans (matrix-4))))
  (is-equal '((1 4 7 10 13 16)
              (2 5 8 11 14 17)
              (3 6 9 12 15 18))
            (lmatrix:trans (matrix-6)))
  (is-equal (matrix-6) (lmatrix:trans (lmatrix:trans (matrix-6))))
  (is-equal '((1 5 9  13)
              (2 6 10 14)
              (3 7 11 15)
              (4 8 12 16))
            (lmatrix:trans (matrix-8)))
  (is-equal (matrix-8) (lmatrix:trans (lmatrix:trans (matrix-8)))))

(deftest multiplication
  (is-equal '((20))
            (lmatrix:mult '((1 2 3 4)) '((4) (3) (2) (1))))
  (is-equal '((19) (43))
            (lmatrix:mult (matrix-2) '((5) (7))))
  (is-equal '((5 11 17 23)
              (11 25 39 53)
              (17 39 61 83)
              (23 53 83 113))
            (lmatrix:mult (matrix-4) (lmatrix:trans (matrix-4)))))

(deftest swap-rows-same-index
  (is-equal (matrix-6) (lmatrix:swap-rows 0 0 (matrix-6)))
  (is-equal (matrix-6) (lmatrix:swap-rows 1 1 (matrix-6)))
  (is-equal (matrix-6) (lmatrix:swap-rows 4 4 (matrix-6))))

(deftest swap-rows-inverted-indices
  (is-equal (lmatrix:swap-rows 0 1 (matrix-6))
            (lmatrix:swap-rows 1 0 (matrix-6)))
  (is-equal (lmatrix:swap-rows 2 4 (matrix-6))
            (lmatrix:swap-rows 4 2 (matrix-6))))

(deftest swap-rows
  (is-equal
    '((16 17 18)
      (4  5  6)
      (7  8  9)
      (10 11 12)
      (13 14 15)
      (1  2  3))
    (lmatrix:swap-rows 0 5 (matrix-6)))
  (is-equal
    '((1  2  3)
      (10 11 12)
      (7  8  9)
      (4  5  6)
      (13 14 15)
      (16 17 18))
    (lmatrix:swap-rows 3 1 (matrix-6))))
