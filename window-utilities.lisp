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

;-------------------- double click -----------------------------------

(export! do-double-click)
(defmethod do-double-click :around (self)
  (when self
    (or (when (^enabled) (call-next-method))
        (do-double-click (fm-parent self)))))

(defmethod do-double-click (self)
  (declare (ignorable self os-event))  
  ;;(trc "*** No special do-double-click for ix-view, event:" self osEvent)
  nil)

(defun geo-dump (i)
  (when (typep i 'ix-view)
    (print (list :pxy (cons (px i)(py i)) :lt (lt i) :lb (lb i)))
    (geo-dump (fm-parent i))))

(defmethod do-menu-right (self buttons wxwy)
  (declare (ignorable buttons self wxwy)))

(defmethod make-menu-right-items (self)
  (declare (ignorable self)))

(defmethod menu-right-select (self item)
  (declare (ignorable self item)))

(defmethod menu-shortc (self)
  (shortc self))

; --------------- geometry -------------------------------

(defun point-in-box (pt box)
  (and (<= (r-left box) (v2-h pt) (r-right box))
       (>= (r-top box) (v2-v pt) (r-bottom box))))


(defun screen-box (self)
  (g-box self)) ;; good luck

; ---------------------- finding parts ------------------------------

(defun mouseview-control (w)
  (fm-ascendant-if (mouse-view w)
                   (lambda (node)
                       (and (typep node 'control)
                            (fully-enabled node)))))

(defmacro dolistreversed ((item-var list-form &optional result-form) &body body)
  (let ((list (gensym))
        (looper (gensym))
        (l2 (gensym)))
    `(labels ((,looper (,l2)
                (when (cdr ,l2)
                  (,looper (cdr ,l2)))
                (let ((,item-var (car ,l2)))
                  ,@body)))
       (bwhen (,list ,list-form)
         (,looper ,list))
       ,result-form)))

(export! find-ix-under)

(defun find-ix-under (self os-pos &key (test #'true))
  (when (and (not (typep self 'tool-tip)) ;; <g>
          (visible self)
          (not (collapsed self)))
    (trc nil "find-ix-under" self os-pos (screen-box self))
    (let ((inself (point-in-box os-pos (screen-box self))))
      (or (when (or inself (not (clipped self)))
            (trc nil "inside self sbox" self os-pos (screen-box self))
            (dolistreversed (k (or (render-order self)(kids self))) ;; overlap goes to last kid displayed
              (unless (typep k 'window)
                (trc nil "fixunder kid!!!!!!!!" k)
                (bwhen (ix (find-ix-under k os-pos :test test))
                  (return-from find-ix-under ix)))))
          
          (when (and inself
                     (funcall test self)
                     (not (ix-click-transparent self)))
            (trc nil "no kid wants it" os-pos (screen-box self))
            self)))))


(defmethod ix-click-transparent (other)
  (declare (ignore other))
  t)

