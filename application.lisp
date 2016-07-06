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

(export! *sys*)

(defparameter *sys* nil)

(defun cello-system-get ()
  *sys*)

(defparameter *first-kill-all-the-windows* nil)

(export! cello-reset cello-system cello-system-get)

#+test
(cello-reset nil)

(defun cello-reset (&optional (system-type 'cello-system))
  
  ;; Reset CFFI, CFFI Extender
  (ffx-reset)
  
  ;; Reset CELLS
  (cells-reset 'tk-user-queue-handler) ; :debug t)
  (kt-opengl-init)
  
  ;; Reset OpenGL special vars
  (makunbound 'ogl::*gl-stop*)
  
  (setf *ctk-dbg* nil)
  
  #+hhack (ft:initialize-ft)
  
  #+hhack  (cl-ftgl-reset) ;; 2006-09-27 back in temporarily ...
  ;; new 2006-08-28: in face of weird OGL 1282 when
  ;; new chars hit in ratios
  
  
  #+hhack (mgk:cl-magick-reset)
  
  ;; Init global *sys* ... needed for Cello context ops
  (when system-type
    (setf *sys* (make-instance system-type))
    
    #+rms-s3 (rms-reset))
  (values))

(defmd cello-system (family)
  (main-window (c-in nil))
  ;(mouse nil :cell nil)
  (mouse (cells::make-instance 'mouse) :owning t)
  (sys-time (c-in (now)))
  (user-preferences (c-in nil))
  :kids (c-in nil))

(defun sys-now ()
  (sys-time *sys*))

#+not
(defmethod initialize-instance :after ((self cello-system) &key)
  (setf (mouse self) (cells::make-instance 'mouse))) ;; 2006-06-01 was make-be

(defmethod sys-close (other)
  (declare (ignore other)))

(defun user-pref-set (key value)
  ;; weird sequence necessary to trigger cell ///
  (setf (user-preferences *sys*)
          (cons (cons key value)
                (delete (assoc key (user-preferences *sys*))
                        (user-preferences *sys*)))))

(defun user-pref-toggle (key)
  (user-pref-set key (not (user-pref key))))

(defun user-pref (key)
  (cdr (assoc key (user-preferences *sys*))))
    
(defmacro ^user-pref (key)
  `(bwhen (ups (^user-preferences *sys*))
      (user-pref ,key)))

;---------------------------------------------------

(defun current-application-time ()
  (ymdhmsh (current-app-universal-time  *sys*)))

(defmethod current-app-universal-time (system)
  (declare (ignore system))
  (get-universal-time))

;may write globals to file when set, then read in at startup time for continuity

(defun fm-find-system (md)
   (upper md cello-system))
