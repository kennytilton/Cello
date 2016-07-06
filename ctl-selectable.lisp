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

;=========================================
    

(defmd ct-selector () ;; mixin at any node containing CTSelectable's
  (selection (c-in nil))
  selection-focus
  (initial-selection nil :cell nil)
  (multiple-choice? nil :cell nil)
  (match-test 'eql)
  toggle?)

(export! selected-key)

(defmd ct-selector-ex (ct-selector) ;; mixin at any node containing ct.selectable.ex's
  (selected-key (c-in nil))
  :selection (c? (let (sel)
                   (bwhen (skey (^selected-key))
                     (fm-traverse self
                       (lambda (node)
                         (when (typep node 'ct-selectable-ex)
                           (when (eql (selected-key node) skey)
                             (push node sel))))
                       :with-dependency t)
                     (if (multiple-choice? self)
                         (nreverse sel)
                       (car sel))))))

(defmethod sm-unchanged-p ((self ct-selector) (slotname (eql 'selection)) new-value old-value)
  (equal new-value old-value))

(defun initial-selection-first (self)
  (do-like-fm-parts (it (self ct-selectable))
    (when (enabled it)
      (return-from initial-selection-first (list it)))))

(defmethod md-awaken :after ((self ct-selector))
  (when (initial-selection self)
    (with-metrics (nil nil () "selector initialselct")
      (setf (selection self) (eko (nil "setting initial selection" self)
                               (funcall (initial-selection self) self))))))

(defmodel ct-selector-inline (ct-selector ix-inline)())

;----------

(export! selection-set)

(defmethod selection-set (self x)
   (setf (selection self) x))

(defmethod selection-set1 (self x)
  (selection-set self (list x)))

(defun selection1 (self)
  (car (selection self)))

;----------


(defmodel ct-details (ct-selector focus ix-details)
  ()
  (:default-initargs
      :outset (u8ths 1)))

(defmodel ct-details-exclusive (ct-exclusive ct-details)()) ;; go generic with CTSelectorNested?

(defmodel ct-selectable (control)
  ((selectedp :initarg :selectedp
     :initform (c? (bwhen (selector (ct-selector self))
                     ;(trc "selecteable-comps" (^value) (selection selector))
                     (selected-match (^value) (selection selector))))
     :reader selectedp))
  (:default-initargs
      :ct-action 'ct-selectable-act))

(defmd ct-selectable-view (ct-selectable)
  :selectedp (c? (bwhen (selector (ct-selector self))
                   (selected-match self (selection selector))))
  :ct-action 'ct-selectable-view-act)

(defmethod ct-selectable-view-act (self event &aux (buttons (evt-buttons event))
                                    (selector (ct-selector self))
                                    (selection (selection selector))
                                    (value self) ;; sneaky way to avoid refactoring
                                    (now-selected (selected-match value selection :test (match-test selector))))
  ;(trcx selectable-set value now-selected value (match-test selector))
  (with-cc :selectable-selected
    (if (multiple-choice? selector)
        (if now-selected
            (when (or (toggle? selector)
                    (shift-key-down buttons))
              (selection-set selector (remove value selection)))
          (selection-set selector (cons value selection)))
      (unless now-selected
        (selection-set selector value)))))

(export! ct-selectable-act ct-selector-ex ct-selectable-ex ct-selectable-view)

(defmethod ct-selectable-act (self event &aux (buttons (evt-buttons event))
                               (selector (ct-selector self))
                               (selection (selection selector))
                               (value (^value))
                               (now-selected (selected-match value selection :test (match-test selector))))
  ;(trcx selectable-set value now-selected value (match-test selector))
  (with-cc :selectable-selected
    (if (multiple-choice? selector)
        (if now-selected
            (when (or (toggle? selector)
                    (shift-key-down buttons))
              (selection-set selector (remove value selection)))
          (selection-set selector (cons value selection)))
      (unless now-selected
        (selection-set selector value)))))

(defmd ct-selectable-ex (control)
  (selected-key (c-in nil))
  (selectedp (c? (bif (selector (ct-selector self))
                   (progn
                     (trc nil "selectable-ex selectedp sees" self (^value) selector
                       :skey (selected-key selector) :sel (selection selector))
                     (bwhen (skey (selected-key selector))
                       (eql (^selected-key) skey)))
                   (trc nil "selectable no selector" self))))
  :ct-action 'ct-selectable-ex-act)

(defmethod ct-selectable-ex-act (self event &aux (buttons (evt-buttons event))
                                  (selector (ct-selector self))
                                  (now-selected (and (^selected-key)
                                                  (eql (^selected-key) (selected-key selector)))))
  ;(trcx ct-selectable-ex-act now-selected )
  (with-cc :selectable-selected-ex
    (if (and (multiple-choice? selector)(selected-key selector))
        (if now-selected
            (when (or (toggle? selector)
                    (shift-key-down buttons))
              (setf (^selected-key) nil))
          (setf (^selected-key) (selected-key selector)))
      (unless now-selected
        (let ((now (get-internal-real-time)))
          (setf (selected-key selector) now)
          (setf (^selected-key) now))))))

(defun selected-match (sought sel &key (test 'eql))
  (if (consp sel)
      (member sought sel :test test)
    (funcall test sought sel)))

(defun ct-selector (self)
  (upper self ct-selector))

