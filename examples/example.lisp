;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; example.lisp

(asdf:oos 'asdf:load-op :clois-lane)

(use-package :clois-lane)


;;; Variables

(defvar *actions* '((:kc-escape . stop-running)
                    (:key-default . echo-self)
                    (:mouse-move-default . echo-self)))

(defparameter *running* nil)


;;; Functions

(defun echo-self (key state)
  (format t "~&key: ~S (~S); state: ~S~%"
          key (cdr (assoc key clois-lane::+scancodes+)) state))


(defun run-example ()
  (set-actions *actions*)
  (setf *running* t)
  (format t "~&Press ESC to stop the loop...~%")
  (loop while *running*
        do (capture)
           (sleep .1)))


(defun stop-running (key state)
  (declare (ignore key))
  (when (equal state :released)
    (setf *running* nil)))


;;; Main Program

(format t (concatenate 'string "~&~%===~%"
  "Do a \"xwininfo -int | grep Window\\ id:\" in another xterm and click on~%"
  "the window you're reading this message is in.  Then do a:~%~%"
  "    (create-input-system \"window-id\")~%~%"
  "and finally:~%~%"
  "    (run-example)~%"
  "~%"
  "If you're running on Windows, you'll need a program that will tell you a~%"
  "window id f.e.: http://www.dennisbabkin.com/php/download.php?what=WinID~%"
  "Also, the window id passed to create-input-system needs to be a string~%"
  "and in base 10, not a hexadecimal value!  So if WinID shows 0x123abc as~%"
  "a window id, you'll need to type \"#x123abc\" on the repl and call:~%"
  "~%"
  "    (create-input-system \"1194684\")~%"
  "===~%"))
