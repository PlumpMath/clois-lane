;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; ois-lib.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the clois-lane root directory for more info.

(in-package :clois-lane)


;;; Define and load the C wrapper.

(define-foreign-library libclois-lane
  (:windows "libclois-lane.dll")
  (:unix "libclois-lane.so")
  (t "libclois-lane"))


(defun load-foreign-libraries ()
  (use-foreign-library libclois-lane)
  (format t "~&[clois-lane] foreign library libclois-lane loaded~%"))


(load-foreign-libraries)
