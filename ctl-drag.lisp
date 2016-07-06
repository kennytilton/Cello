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

(define-symbol-macro dragged-parent (fm-parent (^ct-proxy)))
(define-symbol-macro dragged (^ct-proxy))

(export! ct-dragged drag-pct ^drag-pct)

(defmd ct-dragged ()
  (dragger (c? (fm-descendant-typed self 'ct-drag)))
  :px (c? (v2-h (drag-pos (^dragger))))
  :py (c? (v2-v (drag-pos (^dragger)))))

(defmd ct-drag (control ix-view)
  :ct-proxy (c? (or (u^ ct-dragged) self))
  (drag-pct (c-in (mkv2 0 0))
    :unchanged-if 'v2=)
  (drag-range (c? (count-it :drag-range)
                (eko (nil "new dragrange" .cause)
                  (let ((dl (- (ll dragged-parent) (ll dragged)))
                        (dt (- (lt dragged-parent) (lt dragged)))
                        (dr (- (lr dragged-parent) (lr dragged)))
                        (db (- (lb dragged-parent) (lb dragged))))
                    (mkr dl dt dr db)))))
  (drag-pos (mkv2 0 0) :unchanged-if 'v2=)
  :cursors '((:over . :fleur))
  :click-tolerance (mkv2 (u8ths 4)(u8ths 4))
  :px (c? (v2-h (^drag-pos)))
  :py (c? (v2-v (^drag-pos)))
  :drag-pos (c? (bif (e (^click-evt))
                  (prog1
                      (v2-in-rect (v2-add (v2-subtract (mouse-pos .og.)
                                            (evt-where (os-event e)))
                                    (clickee-pxy e))
                        (^drag-range))
                    (trc nil "drag pos rule sees click" e :where (evt-where (os-event e)) :mpos (mouse-pos .og.))
                    .retog.)
                  (mkv2 (+ (r-left (^drag-range))
                          (* (v2-h (drag-pct self)) (r-width (^drag-range))))
                    (ups (r-bottom (^drag-range))
                      (* (v2-v (drag-pct self)) (r-height (^drag-range))))))))

(defun div-safe (n d)
  (if (zerop d) 0 (/ n d)))

(defobserver drag-pos ((self ct-drag))
  (when (and new-value (^click-evt))
    (trc nil "drag-pos observer" self :deferring-new new-value old-value)
    (with-cc :drag-pos-2-pct
      (trc nil "drag-pos observer" self :processing-new new-value old-value)
      (bif (dragr (^drag-range))
        (let ((dh (- (v2-h new-value) (r-left dragr)))
              (rw (r-width dragr))
              (dv (- (v2-v new-value) (r-bottom dragr)))
              (rh (r-height dragr)))
          ;(trc  "echo dragpos" new-value old-value dragr dh rw dv rh)
          (setf (drag-pct self) (mkv2
                                 (div-safe dh rw)
                                 (div-safe dv rh))))
        (trc nil "no dragr for ctdrag?" self new-value)))))

(defmodel ct-poly-drag (ct-drag ix-polygon)())

(defmodel tab-bar-tracker ()
  ((tab-bar :reader tab-bar
     :initform (c? (let ((tracker (fm-other :htabs)))
                     (assert tracker)
                     tracker)))))

