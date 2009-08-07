;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; clois-lane-cegui.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the clois-lane root directory for more info.

(in-package :cl-user)

(defpackage :clois-lane-system
  (:use :cl :asdf))

(in-package :clois-lane-system)

(asdf:defsystem :clois-lane-cegui
  :version "1.2.0.2"
  :components
    ((:module src
      :components
        ((:file "actions-cegui"))))
  :depends-on (:clois-lane))
