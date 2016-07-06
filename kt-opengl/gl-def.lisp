;; -*- mode: Lisp; Syntax: Common-Lisp; Package: kt-opengl; -*-
;;;
;;; Copyright (c) 1995,2003 by Kenneth William Tilton.
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

(in-package #:kt-opengl)

(defmacro defun-ogl (rtn module$ name$ (&rest type-args))
    `(defun-ffx ,rtn ,module$ ,name$ (,@type-args)
       (progn
         ;;(cells::count-it ,(intern (string-upcase name$) :keyword))
         ;;(format t "~&~(~a~) ~{ ~a~}" ,name$ (list ,@(loop for (nil arg) on type-args by #'cddr collecting arg)))
         #+nogoodinsideglbegin (glec ',(intern name$)))))

(defun aforef (o n)
  (cffi-uffi-compat:deref-array o '(:array :int) n))

(dft glenum #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)
(dft glbitfield #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)

(dft glint :int integer)
(dft glsizei :int integer)

(dft gluint #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)
(dft glushort #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)

(dft glfloat #+lispworks :float #-lispworks :float single-float)
(dft glclampf #+lispworks :float #-lispworks :float single-float)

(dft gldouble :double double-float)
(dft glclampd :double double-float)

;;;(dft glboolean :unsigned-byte #+allegro character #-allegro number)

(dft glbyte :unsigned-char  #+allegro character #-allegro number) ;; typedef signed char     GLbyte; 
(dft glvoid :void integer)

(dft glshort #-allegro-v5.0.1 :short #+allegro-v5.0.1 :int integer)
;;;(dft glubyte :unsigned-byte  #+allegro character #-allegro number)



;; next two were unsigned-byte, but I think ACL 8 has changed

(dft glubyte :int  #+allegro character #-allegro number)
(dft glboolean :int #+allegro unsigned-byte #-allegro number)
