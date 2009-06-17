;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; package.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the clois-lane root directory for more info.

(in-package :cl-user)

(defpackage :clois-lane
  (:use :cl :cffi)
  (:export :capture :create-input-system :set-window-extents
           :add-action :get-actions :remove-action :set-actions))
