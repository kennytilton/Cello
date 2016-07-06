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

;------------- Window ---------------
;

(defmodel cello-window (celtk:window focuser)
  (
   (gl-name-highest :cell nil :initarg :gl-name-highest
     :initform 0
     :accessor gl-name-highest))
  (:default-initargs
      :px 0 :py 0
    ;;:gl-name (c-in nil)
    ;;:focus (c-in nil)
    :ll 0 :lt 0
    :lr (c-in (scr2log 1400))
    :lb (c-in (scr2log -800))
    ;; :tick-count (c-in (os-tickcount))
    :event-handler 'cello-window-event-handler
    :registry? t
    ))

(defmethod path ((self cello-window))  ".")
(defmethod parent-path ((self cello-window)) "")

(defmethod g-offset ((self cello-window) &optional (accum-h 0) (accum-v 0) within)
  (declare (ignorable self within))
  (mkv2 accum-h accum-v))

(defmethod cello-window-event-handler (self xe)
  (declare (ignorable self))
  (TRC nil "cello-window-event-handler" self (ctk::tk-event-type (ctk::xsv type xe)) )
  ;
  ; this next bit is actually offered as a template. suggest users subclass cello-window,
  ; specialize cello-window-event-handler on that subclass, handle what you want else
  ; call-next-method. eventually some generic stuff will be landing in here.
  ;
  (case (ctk::tk-event-type (ctk::xsv type xe))
    (:virtualevent      )
    (:KeyPress          ) ;; this and next handled as app virtual events because Tcl events useless
    (:KeyRelease        )
    (:ButtonPress       )
    (:ButtonRelease	)
    (:MotionNotify	(trc "we got motion!!!!"))
    (:EnterNotify		)
    (:LeaveNotify		)
    (:FocusIn		(TRC "cello-window-event-handler" self :FocusIn ))
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

(defmethod context-cursor (other kbd-modifiers)
   (if (and other (fm-parent other))
       (context-cursor (fm-parent other) kbd-modifiers)
     (cello-cursor :arrow)))

(defun cello-cursor (cursor-id)
  (ecase cursor-id
    (:crosshair #+celtk 'crosshair #+glut GLUT_CURSOR_CROSSHAIR)
    (:arrow #+celtk 'arrow #+glut GLUT_CURSOR_LEFT_ARROW)
    (:i-beam #+celtk 'ibeam #+glut (break))
    (:watch #+celtk 'watch #+glut (break))))


;------------------------------------------

(defmethod ix-selectable ((self cello-window)) t)

