;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cl-magick; -*-
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

;;; $Id: cl-magick.lisp,v 1.16 2008/04/11 09:23:01 ktilton Exp $


(defpackage :cl-magick
    (:nicknames :mgk)
    (:use
     #:common-lisp
     #:gui-geometry
     #-(or cormanlisp ccl sbcl openmcl) #:clos
     #:cffi
     #:cffi-extender
     #:utils-kt
     #+kt-opengl
     #:kt-opengl ;; wands as opengl textures
     )
  (:export #:cl-magick-init #:cl-magick-reset #:wand-manager #:wand-ensure-typed
    #:wands-clear #:wand-pixels #:wand-texture 
    #:wand-render
    #:image-size #:wand-texture-activate #:xim
    #:magick-get-image-width #:magick-get-image-height #:magick-get-image-pixels
    #:new-magick-wand #:magick-read-image #:magick-flip-image #:wand-get-image-pixels
    #:path-to-wand #:mgk-wand-images-write
    #:magick-wand-template))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cl-magick *features*))

(in-package :cl-magick)

(defvar *wand-template*
  nil
  "Path to wand graphics/templates")

(defun set-wand-template-path (wand-template-pathname)
  (setf *wand-template*
     wand-template-pathname))

(defun magick-wand-template ()
  (path-to-wand
   (or *wand-template*
       (make-pathname
#-macosx :directory '(:absolute "0dev" "user" "graphics" "templates")
#+macosx :directory '(:absolute "Users" "frgo" "Pictures")
         :name "metal003" :type "gif"))))

(defparameter *imagick-dll-loaded* nil)
(defparameter *wands-loaded* nil)

(defparameter *mgk-version* (fgn-alloc :unsigned-long 1))

(cffi:define-foreign-library Magick
;;; patches welcomes on this next bit
;;;  (:darwin #-(and)(:framework "GraphicsMagick")
;;;           "libGraphicsMagick.dylib"
;;;           "libGraphicsMagickWand.dylib")
  (:windows (:or "CORE_RL_wand_.dll" )))

#+test
(probe-file (cells:exe-dll "CORE_RL_wand_"))

(cffi:define-foreign-library Wand
  (:darwin (:or "/usr/local/lib/libWand.dylib")))

;; Order matters! First, load Wand then Magick on Darwin

#+macosx
(cffi:use-foreign-library Wand)
      

(defun cl-magick-reset ()
  (use-foreign-library Magick)
  (wands-clear)
  (progn
    (print `(magick-copyright ,(magick-get-copyright)))
    (print `(magick-version ,(magick-get-version *mgk-version*)))))

;;;(eval-now!
;;; (print `(GM load returns ,(cffi:use-foreign-library Magick))))

;-------------------------------------------------------------------

(defun cl-magick-init ()) ;; vestigial

;;;(defun cl-magick-reset ()
;;;  (wands-clear)
;;;  (progn
;;;    (print `(magick-copyright ,(magick-get-copyright)))
;;;    (print `(magick-version ,(magick-get-version *mgk-version*))))
;;;  )

(defun wands-loaded () *wands-loaded*)

(defun (setf wands-loaded) (new-value)
  (setf *wands-loaded* new-value))

(defun wands-clear ()
  (loop for wand in *wands-loaded*
        do (wand-release (cdr wand)))
  (setf (wands-loaded) nil))

#+doit
(wands-clear)

(defun wand-ensure-typed (wand-type path &rest iargs)
  (when path
    (cl-magick-init)
    (let ((key (list* wand-type (namestring path) iargs)))
      (or (let ((old (cdr (assoc key (wands-loaded) :test 'equal))))
             #+shhh (when old
               (format t "!&wand-ensure-typed re-using cached ~a ~a" path wand-type))
            old)
        (let ((wi (apply 'make-instance wand-type
                    :image-path path
                    iargs)))
          ;;(print `(wand-ensure-typed forced-to-load ,wand-type ,path))
          (push (cons key wi) (wands-loaded))
          wi)
        (error "Unable to load image file ~a" path)))))

#+allegro
(defun xim ()
  (wands-clear)
  (dolist (dll (ff:list-all-foreign-libraries))
    (when (search "wand" (pathname-name dll))
      (print `(unloading foreign library ,dll))
      (setf *imagick-dll-loaded* nil)
      (ff:unload-foreign-library dll))))

