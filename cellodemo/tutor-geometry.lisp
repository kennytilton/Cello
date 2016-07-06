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


(in-package :cello)

(defun degree-radians (degrees)
  (/ degrees #.(/ 180 pi)))

(defun tu-geo ()
  (make-instance 'ix-zero-tl
    :md-name 'tu-geo
    :kids (c? (flet ((tu-box (name &rest deets)
                       (apply 'make-instance 'ix-view
                         :md-name name
                         :lr (c? (^lr-width 125))
                         :lb (c? (^lb-height (downs 125)))
                         :pre-layer (c? (with-layers
                                            (:disable gl_texture_2d)
                                          :off
                                          (:line-width 2)
                                          (:rgba +blue+)
                                          :line-frame))
                         deets)))
                (the-kids
                 (tu-box :ftgrow 
                   :px 50 :py -100
                   :skin +red+
                   :ll -25 :lt (ups 25))
                 (tu-box :ftgrow
                   :px 300 :py -100
                   :skin +green+
                   :ll 0 :lt 0)
                 (tu-box :ftgrow
                   :px 500 :py -100
                   :skin +yellow+
                   :ll 25 :lt (ups -25))
                 #+hhack
                 (tu-box :ftgrow
                   :px 300 :py -500
                   :skin +yellow+
                   :value (c? (degree-radians (mod (frame-ct .togl) 360)))
                   :ll (c? (+ -62.5 (* 62.5 (cos (^value)))))
                   :lt (c? (+ 62.5 (* -62.5 (sin (^value))))))
                 #+hhack
                 (mk-part :bye (ct-button)
                   :px (c? (/ (l-width .w.) 2))
                   :py (c? (downs (/ (l-height .w.) 2)))
                   :text$ "Close"
                   :ct-action (lambda (self event)
                                    (declare (ignorable self event))
                                    (ctk::tcl-eval-ex ctk::*tki* "{destroy .}"))))))))
  
  
  