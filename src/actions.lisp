;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; actions.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the clois-lane root directory for more info.

(in-package :clois-lane)


;;; Variables

(defvar *actions* nil)


;;; Functions

(defun add-action (key action)
  (setf *actions* (append *actions* (list (cons key action)))))


(defun get-actions ()
  *actions*)


(defun handle-key-event (key state)
  (let* ((key-name (cdr (assoc key +scancodes+)))
         (action (cdr (assoc key-name *actions*)))
         (default (cdr (assoc :key-default *actions*))))
    (cond (action (funcall action key state))
          (default (funcall default key state)))))


(defun handle-mouse-button-event (button state)
  (let* ((mouse-button (intern (format nil "MOUSE-BUTTON-~A" button) :keyword))
         (action (cdr (assoc mouse-button *actions*)))
         (default (cdr (assoc :mouse-button-default *actions*))))
    (cond (action (funcall action mouse-button state))
          (default (funcall default mouse-button state)))))


(defun handle-mouse-move-event (x y)
  (let ((mouse-x (cdr (assoc :mouse-x *actions*)))
        (mouse-y (cdr (assoc :mouse-y *actions*)))
        (mouse-default (cdr (assoc :mouse-move-default *actions*))))
    (cond (mouse-x (funcall mouse-x :mouse-x x))
          (mouse-default (funcall mouse-default :mouse-x x)))
    (cond (mouse-y (funcall mouse-y :mouse-y y))
          (mouse-default (funcall mouse-default :mouse-y y)))))


(defun remove-action (key action)
  (setf *actions* (remove (cons key action) *actions* :test 'equal)))


(defun set-actions (actions)
  (setf *actions* actions))
