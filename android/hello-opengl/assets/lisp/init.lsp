(in-package :cl-user)
(use-package 'eclffi)

(defvar *width* 480)
(defvar *height* 724)
(defvar *x* 0)
(defvar *y* 0)
(defvar *length* 1)
(defvar *maxlength* 200)
(defvar *touch-x* 0)
(defvar *touch-y* 0)
(defvar *touch-p* nil)

(defun draw-cross (x y)
  (draw-test-line (- x 10) y        (+ x 10) y        1.0 0.0 0.0 1.0)
  (draw-test-line x        (- y 10) x        (+ y 10) 1.0 0.0 0.0 1.0))

(defun init ()
  (gl-init))

(defun main-loop-iteration ()
  (gl-prepare-frame *width* *height*)
  (if *touch-p* (draw-cross *touch-x* *touch-y*)))

(defun set-window-size (width height)
  (setf *width* width)
  (setf *height* height))

(defun register-touch (x y)
  (setf *touch-x* x)
  (setf *touch-y* y)
  (setf *touch-p* t))