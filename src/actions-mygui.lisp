;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; actions-mygui.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the clois-lane root directory for more info.

(in-package :clois-lane)


;;; Functions

(defun handle-key-event (key text state)
  (let* ((key-name (cdr (assoc key +scancodes+)))
         (char (code-char text))
         (action (cdr (assoc key-name *actions*)))
         (default (cdr (assoc :key-default *actions*))))
    (cond ((equal state :pressed)  (okra-mygui::inject-key-press key text))
          ((equal state :released) (okra-mygui::inject-key-release key)))
    (cond (action (funcall action key char state))
          (default (funcall default key char state)))))


(defun handle-mouse-button-event (button state)
  (let* ((mouse-button (intern (format nil "MOUSE-BUTTON-~A" button) :keyword))
         (action (cdr (assoc mouse-button *actions*)))
         (default (cdr (assoc :mouse-button-default *actions*))))
    (cond ((equal state :pressed)
           ;; XXX: hack!  "0 0" is ignored in #'inject-mouse-press
           (okra-mygui::inject-mouse-press 0 0 button))
          ((equal state :released)
           ;; XXX: hack!  "0 0" is ignored in #'inject-mouse-release
           (okra-mygui::inject-mouse-release 0 0 button)))
    (cond (action (funcall action mouse-button state))
          (default (funcall default mouse-button state)))))


(defun handle-mouse-move-event (rel-x rel-y rel-z abs-x abs-y abs-z)
  (let ((mouse-x (cdr (assoc :mouse-x *actions*)))
        (mouse-y (cdr (assoc :mouse-y *actions*)))
        (mouse-z (cdr (assoc :mouse-z *actions*)))
        (mouse-default (cdr (assoc :mouse-move-default *actions*))))
    (okra-mygui::inject-mouse-move abs-x abs-y abs-z)
    (cond (mouse-x (funcall mouse-x :mouse-x rel-x abs-x))
          (mouse-default (funcall mouse-default :mouse-x rel-x abs-x)))
    (cond (mouse-y (funcall mouse-y :mouse-y rel-y abs-y))
          (mouse-default (funcall mouse-default :mouse-y rel-y abs-y)))
    (cond (mouse-z (funcall mouse-z :mouse-z rel-z abs-z))
          (mouse-default (funcall mouse-default :mouse-z rel-z abs-z)))))
