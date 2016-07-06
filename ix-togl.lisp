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

(eval-now!
  (export '(ix-togl-event-handler)))

;------------- Window ---------------
;

(export! mouse-view-tracker mouse-view ^mouse-view mouse-pos ^mouse-pos 
  mouse-control ^mouse-control mouse-down-evt ^mouse-down-evt
  mouse-still ^mouse-still)

(defmd mouse-view-tracker ()
  (mouse-view :initarg :mouse-view :accessor mouse-view
    :initform (c? (let ((pos (mouse-pos .og.)))
                    (trc nil "mouseview sees pos" .w. pos)
                    (when pos
                      (eko (nil "ix-togl mouseview >" self pos)
                        (without-c-dependency
                            (find-ix-under self pos)))))))
  (mv (c? (bwhen (v (^mouse-view))
            (trc nil "new mouse view" v (visible v)))))
  (:documentation "Mixin to have mouse view tracked in a subtree of the window, mostly so other GUI layout can depend on
the sub-tree layout without creating a cyclic dependency, as would happen iof the whole window were watched."))

(defmd ix-togl (mouse-view-tracker #+not focuser ogl-lit-scene control ogl-shared-resource-tender togl ix-view)
  (redisplayp nil :cell nil)
  display-continuous
  (frame-ct :initarg :frame-ct :initform (c-in 0) :accessor frame-ct)
  activep
  (mouse-pos :initform (c-in nil))   ;logical coords.  Try to maintain for now.
  (mouse-still (let (last-pos last-pos-time)
                 (c? (let ((md (^mouse-down-evt))
                           (mp (^mouse-pos)))
                       (cond
                        (md 0)
                        (mp (if (and last-pos (v2= mp last-pos))
                                (- .time last-pos-time)
                              (progn
                                (setf last-pos mp last-pos-time .time)
                                0))))))))
  
  (mouse-control (c? (bwhen (node (^mouse-view))
                       (eko (nil " mousecontrol search starts with mouse-view" node)
                         (fm-ascendant-if node #'fully-enabled)))))
  
  (mouse-up-evt (c-in nil) :cell :ephemeral)
  (mouse-down-evt (c-in nil) :cell :ephemeral)
  (double-click-evt (c-in nil) :cell :ephemeral)
  
  (tick-count (c-in nil))
  (tick-fine (c-in nil))
  :px 0 :py 0
  :gl-name (c-in nil)
  :activep (c-in nil)
  :clear-rgba (list 0 0 0 1)
  
  :ll 0 :lt 0
  :lr (c-in (scr2log 1400))
  :lb (c-in (scr2log -800))
  :tick-count (c-in (os-tickcount))
  :clipped nil
  :event-handler 'ix-togl-event-handler
  :cb-destroy (lambda (self)
                ;(trc "IX-TOGL being destoyed!!!!!!!!!!" self)
                (setf (togl-ptr self) nil) ;; new 2007-04-13 to avoid togl.c line 1039 crash closing window
                ;; bad idea to do it this way, gotta get *istack* bound first: (setf cells::*c-debug* t)
                ))

(defmethod ctk::do-on-double-click-1 :before ((self ix-togl) &rest args)
  ;
  ; we do these as :before so primary does the right thing talking to Tk (like return 0 on success)
  ;
  ;(trc "NEW! IX-togl do-on-double-click-1 sets dc evt" self)
  ;;;  (bif (mi (mouse-control self))
  ;;;    (do-double-click mi )
  ;;;    (do-double-click self ))
  (setf (double-click-evt self) (eko (nil "mouse up!!!")
                                  (make-os-event
                                   :modifiers (keyboard-modifiers .tkw)
                                   :where (mouse-pos self)
                                   :realtime (now)
                                   :c-event :sorry-no-event))))

;;;(defobserver mouse-pos ((self ix-togl))
;;;  #+nah (when new-value
;;;    (let ((x (min (floor (v2-h new-value) 10)
;;;               (1- (length *cursors*)))))
;;;      (trc "new cursor" x (aref *cursors* x))
;;;      (setf (cursor .tkw)
;;;        (aref *cursors* x)))))
    
(defmethod ctk::togl-display-using-class :around ((self ix-togl))
  (if (not (togl-ptr self))
      (print :not-togl-displaying!!)
    (call-next-method)))

(defmethod focus ((self ix-togl)) .focus) ;; 2007-04 ugliness occasioned by ix-togl pretending to be window incompletely (not a focuser as you see above)

(defmethod ctk::togl-create-using-class :around ((self ix-togl))
  (setf cl-ftgl:*ftgl-ogl* (togl-ptr self)) ;; help debug failure to use lazy cells/classes to defer FTGL till Ogl ready
  (kt-opengl:kt-opengl-reset)
  (call-next-method))

(defmethod ctk::togl-display-using-class ((self ix-togl))
  (unless (or  *ogl-listing-p* ;; re-entrance happens if a DLL puts up a MessageBox
	       (c-stopped))
    (with-metrics (nil nil "ctk::togl-display-using-class")
	(bif (dl (dsp-list self))
	     (progn
	       (trc "togl display using disp list !!!!" self)
	       (gl-call-list (dsp-list self)))
	     (ix-paint self)))))

(defmethod ctk::togl-timer-using-class ((self ix-togl))
  (unless (or  *ogl-listing-p* ;; re-entrance happens if a DLL puts up a MessageBox
            (c-stopped))
    (with-metrics (nil nil "ctk::ctk::togl-timer-using-class")
      (when (display-continuous self)
          (trc nil "window-display > continuous specified so posting redisplay" self)
          .retog.))))

(defmethod ix-togl-event-handler (self xe)
  "Tk does not go inside Togl OpenGL-land, so Cello Classic effectively begins here"
  (TRC nil "ix-togl-event-handler" self (ctk::tk-event-type (ctk::xsv type xe)) )
  (case (ctk::tk-event-type (ctk::xsv type xe))
    (:virtualevent      )
    (:KeyPress          )
    (:KeyRelease        )
    (:ButtonPress
     (case (xbe-button xe)
       (1 ;(trc ":ButtonPress mouse-down")
        (setf (mouse-pos self) (mkv2 (xbe-x xe)
                                   (- (xbe-y xe)))) ; trigger mouseview recalc
        (setf (mouse-down-evt self) (eko (nil "mousedown!!!" (ctk::xbe button xe))
                                      (make-os-event
                                       :modifiers (keyboard-modifiers .tkw)
                                       :where (mouse-pos self)
                                       :realtime (now)
                                       :c-event xe))))
        (3 (when (^mouse-view)
             (inspect (^mouse-view))))))

    (:ButtonRelease
     (case (xbe-button xe)
       (1 ;(trc ":ButtonRelease mouse-up")
          (setf (mouse-pos self) (mkv2 (ctk::xbe-x xe)
                                   (- (ctk::xbe-y xe)))) ; trigger mouseview recalc
        (progn ;; with-metrics (t t "mouse up evt")
          (progn ;;prof:with-profiling (:type :space)
            (setf (mouse-up-evt self) (eko (nil "mouse up!!!")
                                        (make-os-event
                                         :modifiers (keyboard-modifiers .tkw)
                                         :where (mouse-pos self)
                                         :realtime (now)
                                         :c-event xe))))
          ;;(prof:show-flat-profile)
          ))))
    
    (:MotionNotify
     (trc nil "setting mouse pos!!!!" (ctk::xbe-x xe) (- (ctk::xbe-y xe)))
     (setf (mouse-pos self) (mkv2 (ctk::xbe-x xe)
                                (- (ctk::xbe-y xe)))))
    (:EnterNotify		)
    (:LeaveNotify		)
    (:FocusIn		)
    (:FocusOut		)
    (:KeymapNotify	)
    (:Expose		)
    (:GraphicsExpose	)
    (:NoExpose		)
    (:VisibilityNotify	)
    (:CreateNotify	)
    (:DestroyNotify	)
    (:UnmapNotify		)
    (:MapNotify		)
    (:MapRequest		)
    (:ReparentNotify	)
    (:ConfigureNotify	)
    (:ConfigureRequest	)
    (:GravityNotify	)
    (:ResizeRequest	)
    (:CirculateNotify	)
    (:CirculateRequest	)
    (:PropertyNotify	)
    (:SelectionClear	)
    (:SelectionRequest	)
    (:SelectionNotify	)
    (:ColormapNotify	)
    (:ClientMessage	)
    (:MappingNotify	)
    (:ActivateNotify    )
    (:DeactivateNotify  )
    (:MouseWheelEvent)))

(defobserver lights ()
  (dolist (light new-value)
    (md-awaken light)))

(defmethod ogl-node-window ((self ix-togl))
  self)

(defmethod ogl-shared-resource-tender ((self ix-togl))
  self)
                      
(defmethod ctl-notify-mouse-click ((self ix-togl) clickee click)
  (declare (ignore clickee click))
  t)

(defmethod ctl-notify-keydown ((self ix-togl) target key-char event)
  (declare (ignore target event key-char))
  t)

(defun buttons-shifted (buttons)
  #+glut (logtest buttons glut_active_shift)
  (find :shift-key buttons))

(defun shift-key-down (buttons)
  #+glut (logtest buttons glut_active_shift)
  (find :shift-key buttons))

(defun control-key-down (buttons)
  #+glut (logtest buttons glut_active_ctrl)
  (find :control-key buttons))

(defun alt-key-down (buttons)
  #+glut (logtest buttons glut_active_alt)
  (find :alt-key buttons))

(defun control-shift-key-down (buttons)
  (and (shift-key-down buttons)
       (control-key-down buttons)))

(defun shift-key-only? (buttons)
  #+glut (eql glut_active_shift buttons)
  (equal '(:shift-key) buttons))

;------------------------------------------

(defun v2-log-to-scr (xy)
  (mkv2 (log2scr (v2-h xy)) (log2scr (v2-v xy))))

(defobserver mouse-view ()
  (unless .tkw
    (trc "no tkw early" self new-value old-value)
    (break "early no tkw"))
  (when old-value
    (with-integrity (:change 'mview-lost)
      (trc nil "mouseover lost by" old-value (.window-cache old-value))
      (setf (mouse-over? old-value) nil)))
  (when new-value
    (with-integrity (:change 'mview-gained)
      (trc nil "mouseover gained by" new-value )
      (setf (mouse-over? new-value) t)))
  
  (with-cc :mouse-view-set-cursor
    (when .tkw
      (setf (cursor .tkw)
        (or (when new-value
              (cdr (assoc :over (cursors new-value))))
          :arrow)))))

(defobserver mouse-down-evt (self m-down)
  .retog.
  (when m-down
    #+x (trcx mousedown self m-down (mouse-control self))
    (bwhen (clickee (mouse-control self))
      (trc nil "mousedown clickee, clickw" clickee self)
      (mk-part :click (mouse-click) ;; wow, a free-floating part
        :click-window self
        :clickee clickee
        :os-event m-down
        :clickee-pxy (mkv2 (px (ct-proxy clickee)) (py (ct-proxy clickee)))))))

(defobserver mouse-up-evt (self up)
  .retog.
  (when up ;; should be since this is ephemeral, but still..
    (trc nil "mouseup" self up (mouse-control self))
    (bwhen (clickee (mouse-control self))
      (trc nil "mouseup clickee" self up (mouse-up-handler clickee))
      (bwhen (upper (mouse-up-handler clickee))
        (trc nil "with metrics" self up (mouse-up-handler clickee))
        (progn ;; with-metrics (t nil "mouseup clickee, clickw" clickee self)
          (funcall upper clickee up))))))

(defparameter *gw* nil)
(defparameter *mgw-near* 1500)
(defparameter *mgw-far* -1500)

(defmethod ctk:togl-create-using-class ((self ix-togl))    
  (setf (gl-name self) (gl-gen-lists 1))
  (cello-gl-init)
  (gl-disable gl_texture_2d)
  (gl-shade-model gl_smooth)     ;; Enable Smooth Shading
  (gl-clear-depth 1.0f0)     ;; Depth Buffer Setup
  (gl-enable gl_depth_test)     ;; Enables Depth Testing
  (gl-depth-func gl_lequal)     ;; The Type Of Depth Testing To Do
  (gl-hint gl_perspective_correction_hint gl_nicest))

(defun cello-gl-init ()
  (trc nil "clearing gl errors....")
  (loop for ct upfrom 0
      until (zerop (eko (nil "cleared gl errorr")
                     (glGetError)))
      when (> ct 10) 
      do #-lispworks (c-break "gl-init")
        #+lispworks (return-from cello-gl-init))
  
  #+shhh (macrolet ((glm (param num)
               (declare (ignore num))
               `(trc ,(symbol-name param) (ogl-get-int ,param))))
    (glm gl_max_list_nesting 0)
    (glm gl_max_eval_order    #X0000)  
    (glm gl_max_lights   #x3377 )  
    (glm gl_max_clip_planes  #x3378 )  
    (glm gl_max_texture_size   #x3379 )  
    (glm gl_max_pixel_map_table  #x3380 )  
    (glm gl_max_attrib_stack_depth #x3381 )  
    (glm gl_max_model-view_stack_depth #x3382 )  
    (glm gl_max_name_stack_depth  #x3383 )  
    (glm gl_max_projection_stack_depth #x3384 )  
    (glm gl_max_texture_stack_depth  #x3385 )  
    (glm gl_max_viewport_dims   #x3386 )))

(defmethod ix-selectable ((self ix-togl)) t)

(defmethod togl-reshape-using-class ((self ix-togl) &aux (width (ctk::togl-width (ctk::togl-ptr self)))
                                     (height (ctk::togl-height (ctk::togl-ptr self))))
  (let ((ctk::*tki* (ctk::togl-interp (ctk::togl-ptr self))))
    (trc "mg-window-reshape" self width height)
    (gl-viewport 0 0 width height)

    (gl-matrix-mode gl_projection)
    (gl-load-identity)
  
    (trc "mg-window-reshape ortho"  0 width (- height) 0 *mgw-near* *mgw-far*)
    (gl-ortho 0 width (- height) 0 *mgw-near* *mgw-far*)
    (trc nil "mg-window-reshape > new window wid,hei:" self width height)

    ;;;  (gl-load-identity)
    (setf (lr self) (+ (ll self) (scr2log width)))
    (setf (lb self) (- (lt self) (scr2log height)))))

(defun run-cello-window (new-window-class &optional run-init-func)
  (assert (symbolp new-window-class))
  (when run-init-func
    (funcall run-init-func))
  (ctk::run-window new-window-class))

(defun w-quadric-ensure (ogl-resource-tender key)
  (or (cdr (assoc key (quadrics ogl-resource-tender)))
    (cdar (push (cons key (glu-new-quadric))
            (quadrics ogl-resource-tender)))))
