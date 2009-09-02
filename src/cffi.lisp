;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cffi.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the clois-lane root directory for more info.

(in-package :clois-lane)


;;; Wrapper Functions

(defcfun ("ois_capture" capture)
    :void)


(defcfun ("ois_create_input_system" ois-create-input-system)
    :pointer
  (window :string)
  (hide-mouse :boolean))

(defun create-input-system (window &key (hide-mouse nil))
  (ois-create-input-system window hide-mouse))


(defcfun ("ois_set_window_extents" set-window-extents)
    :void
  (width :int)
  (height :int))


;;; OIS Callbacks

;;; Keyboard

(defcallback key-pressed
    :void
  ((key :int)
   (text :unsigned-int))
  (handle-key-event key text :pressed))

(defcvar "clfun_key_pressed" :pointer)


(defcallback key-released
    :void
  ((key :int)
   (text :unsigned-int))
  (handle-key-event key text :released))

(defcvar "clfun_key_released" :pointer)


;;; Mouse

(defcallback mouse-moved
    :void
  ((rel-x :int)  (rel-y :int)  (rel-z :int)
   (abs-x :int)  (abs-y :int)  (abs-z :int))
  (handle-mouse-move-event rel-x rel-y rel-z abs-x abs-y abs-z))

(defcvar "clfun_mouse_moved" :pointer)


(defcallback mouse-pressed :void ((mbtn :int))
  (handle-mouse-button-event mbtn :pressed))

(defcvar "clfun_mouse_pressed" :pointer)


(defcallback mouse-released :void ((mbtn :int))
  (handle-mouse-button-event mbtn :released))

(defcvar "clfun_mouse_released" :pointer)


;;; Initialising Callbacks

(defun initialise-callbacks ()
  (setf *clfun-key-pressed* (get-callback 'key-pressed))
  (setf *clfun-key-released* (get-callback 'key-released))
  (setf *clfun-mouse-moved* (get-callback 'mouse-moved))
  (setf *clfun-mouse-pressed* (get-callback 'mouse-pressed))
  (setf *clfun-mouse-released* (get-callback 'mouse-released)))

(initialise-callbacks)
