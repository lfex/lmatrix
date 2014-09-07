(defmodule lmatrix-util
  (export all))

(defun get-lmatrix-version ()
  (lutil:get-app-src-version "src/lmatrix.app.src"))

(defun get-versions ()
  (++ (lutil:get-version)
      `(#(lmatrix ,(get-lmatrix-version)))))
