;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cello; -*-
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

(in-package :cello)





 

(defmodel ct-scroll-bar (control ix-inline)
  ((overflow :accessor overflow 
     :initform (c? (trc nil "oflow sees" (content .parent) (content .parent) (l-height (content .parent)))
                 (ecase (orientation self)
                   (:horizontal (/ (l-width (content .parent))
                            (l-width (kid1 .parent))))
                   (:vertical (/ (l-height (content .parent)) 
                            (l-height (kid1 .parent)))))))
   (pct-scrolled :reader pct-scrolled
     :initform (c? (value (find :sbar-slider (^kids) :key 'md-name))))
   (scroll-handler :cell nil :initarg :scroll-handler :reader scroll-handler
     :initform (lambda (self scroll-pct)
                 (let ((mgr (scroll-manager self)))
                   (with-cc :scrollcc
                     (ecase (orientation self)
                       (:horizontal (setf (px (content mgr))
                                      (* scroll-pct (v2-h (scroll-max mgr)))))
                       (:vertical (setf (py (content mgr))
                                    (* scroll-pct (v2-v (scroll-max mgr))))))))))
   )
  (:default-initargs
      ;;:pre-layer (with-layers +white+ :fill)
      :justify :center
    :kids (c? (the-kids
               (funcall (if (mac-p (upper self ix-scroller))
                            'identity 'nreverse)
                 (list (scroll-bar-slider self (orientation self))
                   (scroll-bar-stepper self (md-name self) :home)))
               (scroll-bar-stepper self (md-name self) :end)))
    :visible (c? (eko (nil "viz? scrollbar-oflowww>" (^overflow))
                   (> (^overflow) 1)))
    ;;:collapsed (c? (not (^visible)))
    :px (c? (ecase (orientation self)
              (:horizontal 0)
              (:vertical (px-maintain-pr (inset-lr .parent)))))
    :py (c? (ecase (orientation self)
              (:vertical 0)
              (:horizontal (py-maintain-pb (inset-lb .parent)))))
    :ll 0 :lt 0
    
    :lr (c? (ecase (orientation self)
              (:horizontal (- (inset-lr .parent)
                             (if (or (resize-range .parent)
                                   (scrolls-p .parent :vertical))
                                 *sbar-thickness* 0)))
              (:vertical *sbar-thickness*)))
    :lb (c? (ecase (orientation self)
              (:vertical (+ (inset-lb .parent)
                           (if (or (resize-range .parent)
                                 (scrolls-p .parent :horizontal) )
                               (ups *sbar-thickness*) 0)))
              (:horizontal (downs *sbar-thickness*))))))

(defmethod content ((self ct-scroll-pane))
  (content (kid1 self)))

(defmethod content ((self ct-scroll-bar))
  (content .pa))

(defmethod content ((self ix-scroller))
  (kid1 (kid1 self)))

(defobserver pct-scrolled ()
  (bwhen (h (scroll-handler self))
    (funcall h self new-value)))


(defconstant *scroll-stepper-r* 6)

(defun scroll-bar-slider (self hz-vt-value)
  (macrolet ((hz-vt (hz-form vt-form)
               `(ecase hz-vt-value
                  (:horizontal ,hz-form)
                  (:vertical ,vt-form))))
    (make-kid 'ix-slider
      :md-name :sbar-slider
      :value-fn (lambda (pct)
                     (hz-vt (v2-h pct)(- 1 (v2-v pct))))
      :jumper-layers (with-layers :on +gray+
                       (:oblong -8 8 :slices 3 :stacks 3))
      :jumper-action (lambda (self e)
                       ;(trc "jumper-action in" (evt-where e))
                       (if (control-key-down (evt-buttons e))
                           (ix-slider-jumper-action self e)
                         (let ((mgr (scroll-manager self))
                               (where (v2-xlate .w. self (evt-where e))))
                           ;(trc "scroll jump" self mgr where (nsib))
                           (hz-vt
                            (cond
                             ((< (v2-h where)(pl (nsib)))
                              (if (< (v2-h where)(- (pl (nsib))
                                                   (l-width (nsib))))
                                  (scroll-shift-page-left mgr)
                                (ix-slider-jumper-action self e)))
                             ((> (v2-h where)(pr (nsib)))
                              (if (> (v2-h where)(+ (pr (nsib))
                                                   (l-width (nsib))))
                                  (scroll-shift-page-right mgr)
                                (ix-slider-jumper-action self e))))
                            (cond
                             ((ups-more (v2-v where)(pt (nsib)))
                              (if (ups-more (v2-v where)
                                    (+ (pt (nsib)) (ups (l-height (nsib)))))
                                  (scroll-shift-page-up mgr)
                                (ix-slider-jumper-action self e)))
                             ((ups-more (pb (nsib)) (v2-v where))
                              (if (ups-more (+ (pb (nsib))(downs (l-height (nsib))))
                                    (v2-v where))
                                  (scroll-shift-page-down mgr)
                                (ix-slider-jumper-action self e))))))))
      :skin (c? (skin .parent))
      :initial-pcts (list (hz-vt (mkv2 0.0 0.0)(mkv2 0.0 1.0)))
      :thumb-size (c? (mkv2
                       (hz-vt (max *sbar-thickness*
                                (if (zerop (overflow .parent))
                                    0
                                  (/ (l-width self)
                                    (overflow .parent))))
                         *sbar-thickness*)
                       (hz-vt *sbar-thickness*
                         (max *sbar-thickness*
                           (if (zerop (overflow .parent))
                               0
                             (/ (l-height self)
                               (overflow .parent)))))))
      :thumb-layers (with-layers (:out 24)
                      :on
                      (:texturing :off)
                      (:mat-shiny 0.05)
                      (:mat-emission (mk-rgba 0 64 255 255))
                      (:oblong -8 8 :slices 3 :stacks 3))
      :tracked-pct (c? (when (visible .parent)
                         (let* ((mgr (scroll-manager self))
                                (pct (hz-vt (/ (px (content mgr))
                                              (v2-h (scroll-max mgr)))
                                       (/ (py (content mgr))
                                         (v2-v (scroll-max mgr))))))
                           (hz-vt (mkv2 pct 0.0)(mkv2 0.0 (- 1 pct))))))
      :ll 0 :lt 0
      :lr (hz-vt (c? (eko (nil "lrsbar" .parent (inset-width .parent)
                           *scroll-stepper-r*)
                       (- (inset-width .parent)
                         ;(lwidth (fm-other :resizer))
                         (* 4 *scroll-stepper-r*))))
            *sbar-thickness*)
      :lb (hz-vt (downs *sbar-thickness*)
            (c? (downs (- (l-height .parent)
                         ;(lheight (fm-other :resizer))
                         (* 4 *scroll-stepper-r*))))))))


;;;(defmethod ix-paint ((self ix-slider))
;;;  (when (eql :vertical (md-name .parent))
;;;    (trc "slider px" (^px))
;;;    (trc "slider py" (^py))
;;;    (trc "slider ll" (^ll))
;;;    (trc "slider lt" (^lt))
;;;    (trc "slider lr" (^lr))
;;;    (trc "slider lb" (^lb))
;;;    )
;;;  (call-next-method))

(defmacro dumpns (places &rest vars)
  (declare (ignore places))
  (let ((f$ (gensym)))
    `(let ((,f$ "~a ~,1,,,f | "))
       ,@(mapcar (lambda (var)
                   `(format t ,f$ ',var ,var))
           vars))))

(defun radian-degrees (radians)
  (* radians (/ 180 pi)))


(defun scroll-bar-stepper (self hz-vt-value home-end-value)
  (macrolet
      ((home-end (home-form end-form)
         `(ecase home-end-value
            (:home ,home-form)(:end ,end-form)))
       (hz-vt (hz-form vt-form)
         `(ecase hz-vt-value
            (:horizontal ,hz-form)(:vertical ,vt-form))))
    (make-kid 'ix-control
      :md-name home-end-value
      :ll (- *scroll-stepper-r*) :lt (ups *scroll-stepper-r*)
      :lr *scroll-stepper-r* :lb (downs *scroll-stepper-r*)
      :skin nil
      :pre-layer (hz-vt
                  (home-end
                   (with-layers (:rgba +yellow+) (:tri-arrow :left :inset 2))
                   (with-layers (:rgba +yellow+) (:tri-arrow :right :inset 2)))
                  (home-end
                   (with-layers (:rgba +yellow+) (:tri-arrow :up :inset 2))
                   (with-layers (:rgba +yellow+) (:tri-arrow :down :inset 2))))
      :click-repeat-p t
      :ct-action (lambda (self os-event)
                       (declare (ignore os-event))
                       (trc nil "ehandler" self event-key)
                       (funcall (hz-vt
                                 (home-end 'scroll-shift-left 'scroll-shift-right)
                                 (home-end 'scroll-shift-up 'scroll-shift-down))
                         (scroll-manager self))))))

