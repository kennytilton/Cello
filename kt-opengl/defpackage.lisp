;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
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

;;; $Id: defpackage.lisp,v 1.4 2008/04/11 09:23:07 ktilton Exp $

(pushnew :kt-opengl *features*)

(defpackage #:kt-opengl
  (:nicknames #:ogl)
  (:use #:common-lisp #:cffi #:ffx #:utils-kt)
  (:export

    #:kt-opengl-init
    #:kt-opengl-reset
    #:glec

    #:*ogl-listing-p*
    #:*selecting*

    #:with-matrix
    #:with-attrib
    #:with-client-attrib
    #:with-gl-begun 
    #:with-gl-param
    #:with-bitmap-shifted
    #:with-gl-parami
    #:with-gl-paramf
    #:with-gl-paramd
    #:with-gl-integers
    #:with-gl-floats
    #:with-gl-doubles
    #:with-display-list
    #:with-gl-translation
    #:with-ogl-isolation

    #:gl-pushm 
    #:gl-popm
    
    #:closed-stream-p 

    #:ncalc-normalf
    #:ncalc-normalfv

    #:ogl-get-int
    #:ogl-get-boolean 

    #:v3i
    #:make-v3i
    #:mk-vertex3i
    #:v3i-x
    #:v3i-y
    #:v3i-z
    #:mkv3i

    #:v3f
    #:make-v3f
    #:mk-vertex3f
    #:v3f-x
    #:v3f-y
    #:v3f-z
    #:mkv3f
   #:mk-rgba

    #:v3d
    #:make-v3d
    #:mk-vertex3d
    #:v3d-x
    #:v3d-y
    #:v3d-z
    #:mkv3d
    
    #:xlin
    #:xlout
    #:farther
    #:nearer

    #:texture-name
    #:ogl-texture
    #:ogl-texture-gen
    #:ogl-texture-delete
    #:ogl-tex-gen-setup
    #:ogl-tex-activate

    #:ogl-current-color
    
    #:ogl-bounds
    #:ogl-scissor-box
    #:ogl-raster-pos-get

    #:ogl-pen-move

    #:ogl-list-cache
    #:ogl-lists-delete

    #:eltgli

    #:gl-name

    #:gl-get-integers
    #:gl-get-floats
    #:gl-get-doubles
    ))
