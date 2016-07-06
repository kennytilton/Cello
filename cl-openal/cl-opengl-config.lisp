;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cello; -*-
;;;
;;; Copyright © 2004 by Kenneth William Tilton.
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

(in-package :cl-opengl)

(setq *gl-dynamic-lib*
    (make-pathname
    #+lispworks :host #-lispworks :device "c"
      :directory '(:absolute "windows" "system32")
      :name "opengl32"
      :type "dll"))

(setq *glu-dynamic-lib*
    (make-pathname
    #+lispworks :host #-lispworks :device "c"
      :directory '(:absolute "windows" "system32")
      :name "glu32"
      :type "dll"))

(setq *glut-dynamic-lib*
      (merge-pathnames
       (make-pathname :name "freeglut" :type "dll")
       cl-user::*user-dynlib-directory*))

