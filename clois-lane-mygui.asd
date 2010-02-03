;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; clois-lane-cegui.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the clois-lane root directory for more info.

(in-package :cl-user)


(asdf:defsystem :clois-lane-mygui
  :version "1.2.0.4"
  :depends-on (:clois-lane :okra-mygui)
  :components ((:module src
                :components ((:file "actions-mygui")))))
