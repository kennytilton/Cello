;;________________________________________________________
;;
;;;
;;; Copyright (c) 2004 by Kenneth William Tilton.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

;;; $Id: kt-opengl.lisp,v 1.13 2008/04/11 09:23:07 ktilton Exp $

(pushnew :kt-opengl *features*)

(in-package :kt-opengl)

(defvar *selecting*)

(defvar *opengl-dll* nil)

(defun kt-opengl-init ()
  (if *opengl-dll*
      (print :opengl-already-loaded)
    (progn
      (print :loading-opengl)
      (let* ((opengl-loaded-p
              (use-foreign-library OpenGL))
             (glu-loaded-p
              #+macosx
              t ;; on Mac OS X, no explicit loading of GLU needed.
              #-macosx 
              (use-foreign-library GLU)))
        (assert (and opengl-loaded-p glu-loaded-p))
        (print :opengl-successfully-loaded)
        (setf *opengl-dll* t)))))

(defun kt-opengl-reset ()
  (loop for ec = (glgeterror)
        for n below 10
        when (zerop ec) do (loop-finish)
        do (cells::trc "kt-opengl-reset sees error" ec)))

;; this breaks build of distro since that builds dll path differently
#+xxx
(eval-when (:load-toplevel :execute)
  (kt-opengl-init))


