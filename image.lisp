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

(define-symbol-macro .focus  (focus .tkw))

(eval-when (compile load eval)
  (export '(mouse-over? ix-view ix-stack ix-row ix-stack-lazy ix-row-lazy 
             a-stack a-row a-stack-lazy a-row-lazy
             skin ^skin)))
; ------------------------------------------------------

(defmodel ogl-quadric-based (ogl-node)
  ((quadric :initform nil :initarg :quadric :reader quadric)))

; ---------------------------------------------

(export! ps3 ^ps3 font-size-adj ^3d-skinned bkg-color ^bkg-color)
(defmd ix-view (ogl-node geometer)
  ;
  ;   visibility
  (clipped :cell nil :initarg :clipped :initform nil :reader clipped)
  ;   appearance
  zoom rotation
  gui-styles sound lighting bkg-color fg-color skin pre-layer 
  cursors
  (mouse-over? (c-in nil))
  menu-right-items-factory menu-select-handler
  (font-size-adj 0 :cell nil)
  recording
  (snapshot-pathnamer nil :cell nil)
  (snapshot-release-id :initarg :snapshot-release-id
      :initform nil #+please (c-in nil) :accessor snapshot-release-id)
  ps3 ; persistence

  ; cached calculations
  (.window-cache nil :cell nil))

(defmethod skin :around (self)
  (or (call-next-method)
    (bwhen (p .parent)
      (when (typep p 'ix-view)
        (skin p)))))

(defmacro ^3d-skinned (3d-effect thickness &optional (text-color '+black+))
  `(with-layers :off +white+
     (:wand (^skin))
     (:frame-3d ,3d-effect
       :texturing (list (^skin))
       :thickness ,thickness)
     ,text-color))

(defobserver pre-layer ()
  .retog.)

(defobserver visible ()
  .retog.)

;;; --- S3 persistence -------------------------

(defvar *s3-bucket*)
(defvar *s3-key*)

(defstruct ps3 bucket key data)

(export! make-ps3 ps3-bucket-get ps3-key-get ps3-data-get ps3-group ps3-key-data ps3-make ps3key ps3keygeneric)

(defun ps3-bucket-get (ps3)
  (when ps3 (ps3-bucket ps3)))

(defun ps3-key-get (ps3)
  (when ps3 (ps3-key ps3)))

(defun ps3-data-get (ps3)
  (when ps3 (ps3-data ps3)))

(defun ps3-group (bucket &optional key)
  (make-ps3 :bucket bucket :key key))

(defun ps3-key-data (key &optional data)
  (make-ps3 :key key :data data))

(defun ps3-make (bucket &optional key data)
  (make-ps3 :bucket bucket :key key :data data))

(defun ps3key (&rest segs)
  (apply 'concatenate 'string
    (loop for (seg . rest) on segs
          collect (princ-to-string seg)
          when rest collect "/")))

(defun ps3keygeneric (&rest segs)
  (conc$ "/" (apply 'ps3key segs)))

(export! ix-form form-write error-fields)

(defmd ix-form ()
  (user-errors (c-in nil))
  (error-fields (c? (fm-collect-if self 'user-errors nil t) ))
  ps3-aux)

(defobserver visible ((self ix-form))
  (when new-value
    ;;(trc "visible ix-form" self)
    (bwhen (f (focus-find-first self))
      (with-cc :vis-form-focus
        ;;(trc "visible ix-form integ" self)
        (focus-on f)))))

(export! form-message-handle)


(defmethod form-message-handle (self category message)
  (case category
    (:keysym
     (case (keysym-sym message)
       (tab 
         (bwhen (new (funcall (if .shift-key-p 'focus-find-prior 'focus-find-next) .focus))
           (with-cc :tabbing-focus
             ;;(break ":tabbing-focus")
             (setf .focus new))))))))

#+rms-s3 
(defmethod form-write (self)
  (labels ((fw (self &optional aux-func)
             (let* ((*s3-bucket* (or (ps3-bucket-get (^ps3)) *s3-bucket*))
                    (*s3-key* (when *s3-bucket*
                                (bif (k (ps3-key-get (^ps3)))
                                  (cond 
                                   ((char= #\/ (schar k 0))
                                    (assert *s3-key*)
                                    (conc$ *s3-key* k))
                                   (t k))
                                  *s3-key*))))
               (unless (eq :ignore (^ps3))
                 (bwhen (data (ps3-data-get (^ps3)))
                   (rms-put *s3-bucket* *s3-key* data))
                 (when aux-func
                   (trcx formwrite self)
                   (funcall aux-func self))
                 (loop for k in (^kids) do
                       (fw k))))))
    (let ((*s3-bucket* nil))
      (trc "form write set cursor" self .tkw)
      (setf (cursor .tkw) :watch)
      (fw self (ps3-aux self)))))

;;------- IXFamily -----------------------------
;;

(export! render-order)

(defmd ix-family (ix-view family)
  styles
  (effective-styles nil #+(or) (ix-family-effective-styles))
  showkids
  render-order
  (kids-ever-shown (c? (or .cache (^showkids)))))

(defmethod render-order (other)
  (declare (ignore other)) nil)

(export! ix-zero-tl a-ztl)
(defmodel ix-zero-tl (geo-zero-tl ix-family)())
(defmacro a-ztl ((&rest stack-args) &body dd-kids)
  `(mk-part ,(copy-symbol 'ztl) (ix-zero-tl)
      ,@stack-args
     :fm-parent *parent*
     :kids (c? (the-kids ,@dd-kids))))

(export! ix-kid-sized)
(defmodel ix-kid-sized (geo-kid-sized ix-family)())
(defmodel ix-inline (geo-inline ix-view)())
(defobserver .kids ((self ix-inline))
  (when .togl .retog.))
(defmodel ix-inline-lazy (geo-inline-lazy ix-view)())
(defobserver .kids ((self ix-inline-lazy))
  (when .togl .retog.))

(defmodel ix-stack (ix-inline)
  ()
  (:default-initargs
      :orientation :vertical))

(defmodel ix-stack-lazy (ix-inline-lazy)
  ()
  (:default-initargs
      :orientation :vertical))

(defmodel ix-row (ix-inline)
  ()
  (:default-initargs
      :orientation :horizontal))


(defmd ix-row-flow (geo-row-flow ix-row))

(defmodel ix-row-lazy (ix-inline-lazy)
  ()
  (:default-initargs
      :orientation :horizontal))

(export! a-stack a-row ix-row-flow)

(defmacro a-stack ((&rest stack-args) &body dd-kids)
  `(mk-part ,(gensym "STAK") (ix-stack)
      ,@stack-args
     :fm-parent *parent*
     :kids (c? (the-kids ,@dd-kids))))

(defmacro a-stack-lazy ((&rest stack-args) &body dd-kids)
  `(mk-part ,(gensym "STAK") (ix-stack-lazy)
      ,@stack-args
     :fm-parent *parent*
     :kids (c? (the-kids ,@dd-kids))))

(defmacro a-row ((&rest stack-args) &body dd-kids)
  `(mk-part ,(copy-symbol 'row) (ix-row)
      ,@stack-args
     :fm-parent *parent*
     :kids (c? (the-kids ,@dd-kids))))

(defmacro a-row-lazy ((&rest stack-args) &body dd-kids)
  `(mk-part ,(copy-symbol 'row) (ix-row-lazy)
      ,@stack-args
     :fm-parent *parent*
     :kids (c? (the-kids ,@dd-kids))))

(defmethod focus-starting ((self ix-family))
  (some #'focus-find-first (kids self)))

(defmacro ^prior-sib (self)
   (let ((kid (gensym)))
      `(let* ((,kid ,self))
          (find-prior ,kid (kids (fm-parent ,kid))))))

(defmethod ogl-shared-resource-tender ((self ix-view))
  .w.)

(defmethod ogl-node-window ((self ix-view))
  .w.)

(defmethod path ((self ix-view))
  (path (fm-parent self)))

(defgeneric ogl-dsp-list-prep (self)
  (:method-combination progn)
  (:documentation "Do stuff needed before render but not needed/wanted in display list"))

(defmethod ogl-dsp-list-prep progn ((self ix-view))
  (ogl-dsp-list-prep (skin self)))

(defmethod ogl-dsp-list-prep progn ((self wand-texture))
  (texture-name self))

(defmacro uskin ()
  `(labels ((usk (self)
              (when (typep self 'ix-view)
                (or (skin self)
                  (usk .parent)))))
     (usk self)))

(defmethod ix-selectable ((self ix-view)) nil)

(defmethod ix-click-transparent ((self ix-view))
  nil)


(defun inset-h (v)
  (etypecase v
    (number v)
    (v2 (v2-h v))
    (ix-view (inset-h (inset v)))))

(defun inset-v (v)
  (etypecase v
    (number v)
    (v2 (v2-v v))
    (ix-view (inset-h (inset v)))))

(defmethod call-^fillright (self filled padding)
  (- (inset-lr filled)
     (or padding 0)
     (v2-h (^offset-within self filled))))

(defmethod (setf pxy) (new-offset self)
  (setf (px self) (v2-h new-offset))
  (setf (py self) (v2-v new-offset)))


(defmethod g-offset ((self ix-view) &optional (accum-h 0) (accum-v 0) within)
  (trc nil "goffset self" self 'px (px self) 'py (py self) 'fm-parent (fm-parent self))
  (let (
        (oh (+ accum-h (or (px self) 0)))
        (ov (+ accum-v (or (py self) 0)))
        )                               
     (if (eq within (fm-parent self)) ;; if within is nil we simply goto null parent
        (mkv2 oh ov)
        (g-offset (fm-parent self) oh ov))))

(defun w-bottom-left (self)
  (v2-add (g-offset self)
    (ll self)
    (+ (lb self) (l-height .w.))))

(defmethod gunscaled ((self null) value)
  value)

(defmethod gunscaled (self value)
  (gunscaled (fm-parent self) value))

(defmethod visible-fully ((self ix-view)) ;; this used to be an :around on visible, but then focus-first
   (and (visible self)                  ;; could not find focus on page it was /going to/ (not yet visi)
        (or (null (fm-parent self))      ;; ...not sure who need visible to go up all the way
            (visible (fm-parent self)))))

(defmethod visible (other)
   (declare (ignore other))
   t)

(defmethod visible ((other null))
   (c-break "visible called on NIL"))

(defmethod dbg-awake ((ap ix-view))
   (and (dbg-awake-num ap 'px)
        (dbg-awake-num ap 'py)
        (dbg-awake-num ap 'll)
        (dbg-awake-num ap 'lr)
        (dbg-awake-num ap 'lt)
        (dbg-awake-num ap 'lb)
        )
   #+nope (unless (>= (lb ap) (lt ap)) ;; this happens normally as structures get "collapsed" etc
            
            (error 'x-systemfatal :app-func 'dbg-awake :error-text "Bottom less than top: self, lT, height, lB"
              :other-data (list ap  (lt ap) (l-height ap) (lb ap))))
   (call-next-method))

(defun dbg-awake-num (model slotname)
   (when (slot-value model slotname)
      (c-assert (slot-value model slotname) nil
        "Error upon awakening: slot ~s of model ~s not specified; must be" slotname model)
      (c-assert (numberp (slot-value model slotname)) nil
        "Error upon awakening: slot ~s of model ~s not numeric" slotname model)))


; ------------------- right-click -------------------------

(defmethod make-menu-right-items ((self ix-view))
  (bwhen (f (menu-right-items-factory self))
     (funcall f self)))

(defmethod menu-right-select ((self ix-view) item)
  (when item
    (bwhen (h (menu-select-handler self))
       (funcall h self item))))

(defstruct ix-layers fn code)

(defmacro with-layers (&rest layers)
  (flet ((collect-output (layers)
           ;;(print (list "layers are" layers))
           (let (output)
             (dolist (layer layers)
               (typecase layer
                 (null)
                 (cons (push (apply #'ix-layer-expand layer) output))
                 (otherwise (push (ix-layer-expand layer) output))))
             (nreverse output))))
    `(make-ix-layers
      :code ',layers
      :fn (lambda (self l-box mode)
            (declare (ignorable self l-box))
            (trc nil "with-layers called!!!!!!!!!!!!!!!!" self mode)
            .retog.
            (ecase mode
              (:before ,@(collect-output
                          (subseq layers 0
                            (or (position :primary layers)
                              (length layers)))))
              (:after ,@(collect-output
                         (cdr (member :primary layers)))))))))

