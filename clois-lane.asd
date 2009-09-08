;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; clois-lane.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the clois-lane root directory for more info.

(in-package :cl-user)

(defpackage :clois-lane-system
  (:use :cl :asdf))

(in-package :clois-lane-system)

(asdf:defsystem :clois-lane
  :version "1.2.0.3"
  :components
    ((:module src
      :components
        ((:file "package")
         (:file "actions" :depends-on ("package" "keyboard-scancodes"))
         (:file "cffi" :depends-on ("package" "actions" "ois-lib"))
         (:file "keyboard-scancodes" :depends-on ("package"))
         (:file "ois-lib" :depends-on ("package")))))
  :depends-on (:cffi))
