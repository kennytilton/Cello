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

#| do list
rounded-rect for dialog (textured) and slider
hand tool grabs pane and scrolls 2d
slider trench prettied up
|#

(defmd ct-scroll-manager (focus control ix-zero-tl)
  (content-factory (c? (content-factory .pa)))
  (step-x (u96ths 12))
  (step-y (u96ths 12))
  (scroll-max (c-in nil)))

(defmodel ct-scroll-pane (ct-scroll-manager)
  ()
  (:default-initargs
     :clipped t
    :kid-slots (def-kid-slots
                   (mk-kid-slot (px) (c-in 0))
                   (mk-kid-slot (py) (c-in 0)))
    :kids (c? (the-kids (funcall (^content-factory) self)))
    :scroll-max (c? (mkv2
                     (min 0 (- (l-width self)(l-width (kid1 self))))
                     (max 0 (ups (- (l-height (kid1 self))(l-height self))))))))

(defobserver scroll-max ()
  (let ((c (kid1 self)))
    (when (> (py c) (v2-v new-value))
      (trc "smax y" (v2-v new-value) :vs (py c))
      (with-cc :scroll-max-adj-content-y
        (setf (py c) (v2-v new-value))))))

(defun scrolls-p (scroller dir)
  (member dir (scroll-bars scroller)))

(defconstant *sbar-thickness* 16)

(export! ix-scroller)

(defmd ix-scroller (ct-scroll-manager ix-zero-tl)
  mac-p
  (scroll-bars nil :cell nil)
  (resizeable nil :cell nil)
  resize-range
  (start-size nil :cell nil)
  content-factory
  :clipped nil
  ; these next two assume a resizer at bottom right of window actually controlling
  ; window resizing...
  ;:lr (c? (+ (outset self) (pr (fm-other :resizer))))
  ;:lb (c? (+ (downs (outset self))(pb (fm-other :resizer))))
  :scroll-max (c? (scroll-max (kid1 self)))
  :resize-range (c? (when (resizeable self)
                      (mkr 100 -100
                        (+ (outset self)
                          (l-width (kid1 self))
                          2 (floor *sbar-thickness* 2))
                        (downs (+ (outset self)
                                 (l-height (kid1 self))
                                 2 (floor *sbar-thickness* 2))))))
  :kids (c? (the-kids
             (mk-part :s-pane (ct-scroll-pane)
               :px 0 :py 0
               :ll 0 :lt 0
               :lr (c? (- (inset-lr .parent)
                         2 (if (scrolls-p .parent :vertical)
                               *sbar-thickness* 0)))
               :lb (c? (+ (inset-lb .parent)
                         (ups 2)
                         (if (scrolls-p .parent :horizontal)
                             (ups *sbar-thickness*) 0)))
               :step-x (c? (step-x .parent))
               :step-y (c? (step-y .parent)))
             (when (resizeable self)
               (let ((r (floor *sbar-thickness* 2)))
                 (mk-part :resizer (ct-resizer)
                   :ll (- r) :lt (ups r)
                   :lr r :lb (downs r)
                   :px (c-in (- (v2-h (start-size self)) r))
                   :py (c-in (+ (v2-v (start-size self)) (ups r)))
                   :drag-range (c? (resize-range .parent)))))
             (mapcar (lambda (bar-id)
                       (make-kid 'ct-scroll-bar
                         :md-name bar-id
                         :orientation bar-id))
               (scroll-bars self)))))

(defobserver lr ((self ct-scroll-pane))
  (when new-value
    (when (> new-value (pr (kid1 self)))
      (with-cc :obs-lr-scroll
        (setf (px (kid1 self)) (min 0 (+ (px (kid1 self))
                                        (- new-value (pr (kid1 self))))))))))

(defobserver lb ((self ct-scroll-pane))
  (when new-value
    (when (< new-value (pb (kid1 self)))
      (with-cc :obs-lb-scroll
        (setf (py (kid1 self)) (max 0 (+ (py (kid1 self))
                                        (downs (abs (- new-value (pb (kid1 self))))))))))))

(defmodel ct-resizer (ct-drag)
  ()
  (:default-initargs
      :drag-pos (c? (bwhen (e (^click-evt))
                     (eko (nil "resizer dragpos")
                       (v2-in-rect (v2-add (v2-subtract (mouse-pos .w.)
                                             (evt-where (os-event e)))
                                     (clickee-pxy e))
                         (^drag-range)))))
    :visible (c? (^enabled))
    :enabled (c? (not (null (^drag-range))))))

(defmethod ix-paint ((self ct-resizer)
                      &aux (r (l-box self)))
  (gl-line-width 2)
  (ix-render-rgba +gray+)
  (with-gl-begun (gl_lines)
    (loop repeat 4
          for x1 = (+ (r-left r) 0) then (+ x1 4)
          for y2 = (+ (r-top r) (downs 0)) then (+ y2 (downs 4))
          do (gl-vertex2f x1 (+ (r-bottom r) (ups 0)))
          (gl-vertex2f (- (r-right r) 0) y2))))

(defobserver drag-pos ((self ct-resizer))
  (when new-value
    (setf (px self) (v2-h new-value))
    (setf (py self) (v2-v new-value))))

(defmacro incf-max (place incr max)
  `(setf ,place (min ,max (+ ,place ,incr))))

(defmacro decf-min (place delta min)
  `(setf ,place (max ,min (- ,place ,delta))))

(defmethod focus-handle-keysym ((self ct-scroll-manager) k)
  (case-string-equal k
    (left (scroll-shift-left self))
    (right (scroll-shift-right self))
    (up (scroll-shift-up self))
    (down (scroll-shift-down self)) 
    (prior (scroll-shift-page-up self))
    (next (scroll-shift-page-down self))
    (home (scroll-shift-home self))
    (end (scroll-shift-end self))
    (otherwise
     (return-from focus-handle-keysym nil) ;;indicate unhandled
     ))
  :focus-handle-keysym-break)

(defun scroll-manager (descendant)
  (fm-ascendant-if descendant (lambda (node)
                                (and (enabled node)
                                  (typep node 'ct-scroll-manager)))))

(defun scroll-shift-left (self)
  (with-cc :scroll-shift-left
    (incf-max (px (content self)) (^step-x) 0)))
(defun scroll-shift-right (self)
  (with-cc :scroll-shift-right
    (decf-min (px (content self)) (^step-x) (v2-h (^scroll-max)))))
(defun scroll-shift-page-left (self &optional (limit 0))
  (with-cc :scroll-shift-page-left    
    (incf-max (px (content self)) (l-width self) limit)))
(defun scroll-shift-page-right (self &optional (limit (v2-h (^scroll-max))))
  (with-cc :scroll-shift-page-right    
    (decf-min (px (content self)) (l-width self) limit)))

(defun scroll-shift-up (self)
  (with-cc :scroll.shift-up    
    (decf-min (py (content self)) (^step-x) 0)))
(defun scroll-shift-down (self &optional (limit (v2-v (^scroll-max))))
  (with-cc :scroll.shift-down    
    (incf-max (py (content self)) (^step-x) limit)) )
(defun scroll-shift-page-up (self &optional (limit 0))
  (with-cc :scroll.shift-page-up    
    (decf-min (py (content self)) (l-height self) limit)))
(defun scroll-shift-page-down (self &optional (limit (v2-v (^scroll-max))))
  (with-cc :scroll.shift-page-down    
    (incf-max (py (content self)) (l-height self) limit)))


(defun scroll-shift-home (self)
  (with-cc :scroll-shift-home    
    (setf (py (content self)) 0)))
(defun scroll-shift-end (self)
  (with-cc :scroll-shift-end    
    (setf (py (content self)) (v2-v (^scroll-max)))))


;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------

;;;      (unless (and (zerop deltah) (zerop deltav)) ;; raw delta suggested by key
;;;        (setf deltah (if (minusp deltah)
;;;                         (min 0 (max deltah (- (lR self) (pR doc))))
;;;                       (max 0 (min deltah (- (lL self) (pL doc))))))
;;;        (setf deltav (if (minusp deltav)
;;;                         (min 0 (max deltav (- (lB self) (pB doc))))
;;;                       (max 0 (min deltav (- (lT self) (pT doc)))))))
;;;
;;;      (unless (and (zerop deltah) (zerop deltav)) ;; possible delta considering current scroll position
;;;        (setf (scrollDelta self)
;;;                  (make-2d deltah deltav))))))

;---------------------------------------------
#+(or)
(defun ct-scroll-pane-handscroll ()
  (c? (bwhen (click-evt (^click-evt))
          (when (^in-drag click-evt)
              (let ((delta-now  (echo-it "delta now" (make-2d (+ (2d-h (clickee-p-offset click-evt))
                                                               (at-canvas-res self
                                                                              (- (2d-h (mouse-pos (swdw)))
                                                                                 (evt-wherex (os-event click-evt)))))
                                                            (+ (2d-v (clickee-p-offset click-evt))
                                                               (at-canvas-res self
                                                                              (- (2d-v (mouse-pos (swdw)))
                                                                                 (evt-where-y (os-event click-evt)))))))))
                (if (cached-value)
                    (echo-it "delta net" (2d-subtract delta-now (cached-value)))
                  delta-now))))))

