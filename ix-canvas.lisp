;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cello; -*-
#|

Copyright (C) 2004 by Kenneth William Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :cello)



(defmodel ix-canvas (ix-family)
  (
   (target-res :initarg :target-res
              :initform (cs-logical-screen-resolution)
              :accessor target-res)
   
   (enclosing-res :initarg :enclosing-res
                 :initform nil
                 :accessor enclosing-res)
   
   (content :initarg :content
            :initform nil
            :accessor content))
  
  (:default-initargs
    :kids (c? (the-kids (^content)))))

(defmethod ncanvas-to-screen-point ((self ix-canvas) point)
  (if (enclosing-res self)
      (nres-to-res point (target-res self) (enclosing-res self))
    ;
    ; if no encloser we are at the top, time to convert
    ; to screen res...
    (logical-to-screen-point point (target-res self))))

(defmodel ix-canvas-nested (ix-canvas)
  (
   (target-res :initarg :target-res
              :initform (cs-logical-screen-resolution)
              :accessor target-res)
   
   (enclosing-res :initarg :enclosing-res
                 :initform (c? (bwhen (es (upper self ix-canvas))
                                   (target-res es)))
                 :accessor enclosing-res)
   
   (content :initarg :content
            :initform nil
            :accessor content)))
  
(defun at-enclosing-res (self n)
  (if (or (null (enclosing-res self))
          (eql (enclosing-res self) (target-res self)))
      n
    (progn
      (trc nil "targetres adj" self (target-res self) (enclosing-res self) n (round (* n (target-res self))
             (enclosing-res self)))
      (res-to-res n (target-res self) (enclosing-res self)))))

(defmodel ix-canvas-kid-sized (ix-canvas-nested)
  ()
  (:default-initargs
      :ll (c? (bif (root (car (^kids)))
                (at-enclosing-res self (ll root))
                0))
    :lt (c? (bif (root (car (^kids)))
                (at-enclosing-res self (lt root))
                0))
    :lr (c? (bif (root (car (^kids)))
                (at-enclosing-res self (lr root))
                0))
    :lb (c? (bif (root (car (^kids)))
                (at-enclosing-res self (lb root))
                0))
    :kids (c? (the-kids (mk-part :root (ix-root-kid-sized)
                           :kids (c? (the-kids (content .parent))))
                        ))))

(defmodel ix-canvas-parent-sized (ix-canvas-nested)
  ()
  (:default-initargs
      :ll 0
    :lt 0
    :lr  (c? (inset-width .parent))
    :lb  (c? (downs (inset-height .parent)))
    :kids (c? (the-kids (mk-part :root (ix-root)
                           :ll 0
                           :lt 0
                           :lr  (c? (round (* (enclosing-res .parent)
                                               (inset-width .parent))
                                            (target-res .parent)))
                           :lb  (c? (downs (round (* (enclosing-res .parent)
                                                   (inset-height .parent))
                                            (target-res .parent))))
                           :pre-layer (with-layers +white+)
                           :kids (c? (the-kids (content .parent))))
                        ))))

;-------------------------------------------

(defmacro canvas-res (&optional (self 'self))
  `(target-res (u-canvas ,self)))

(defmacro s-canvas ()
  `(nearest self ix-canvas))

(defun u-canvas (self)
  (upper self ix-canvas))

(defun at-canvas-res (self screen-amount)
  (scr2log screen-amount (target-res (nearest self ix-canvas))))

(defun in-canvas (self point)
  (mkv2 (in-canvas-vector self (v2-h point))
           (in-canvas-vector self (v2-v point))))

(defmethod in-canvas-vector ((self ix-canvas) amount)
  (if (enclosing-res self)
      (res-to-res amount (enclosing-res self) (target-res self))
    amount))

(defmethod in-canvas-vector (self amount)
  (declare (ignorable self))
  amount)

;-------------------------------------------

(defmethod g-offset ((self ix-canvas) &optional (accum-h 0) (accum-v 0) within)
  ;(trc "goffset self" self 'px (px self) 'py (py self) 'fm-parent (fm-parent self))
  (if (fm-parent self)
      (g-offset (fm-parent self)
               (+ (res-to-res accum-h (target-res self) (enclosing-res self))
                  (or (px self) 0))
               (+ (res-to-res accum-v (target-res self) (enclosing-res self))
                  (or (py self) 0))
        within)
    (mkv2 accum-h accum-v)))

(defmodel ix-root (ix-family)
  ()
  (:default-initargs
      :px (c? (px-maintain-pl 0))
      :py (c? (py-maintain-pt 0))))

(defmodel ix-root-kid-sized (ix-kid-sized ix-root)())

(defmethod gunscaled ((self ix-canvas) value) ;; fixcanvas
  (gunscaled (fm-parent self) (res-to-res value
                                         (target-res self)
                                         (cs-logical-screen-resolution))))

(eval-now!
  (export '(ix-canvas target-res)))


