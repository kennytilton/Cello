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

(defstruct sound paths (gain 1) callback loopingp start (source :default) buffer sources)

(export! make-sound ix-sound-install ix-play-start)

(defun ix-sound-install (self sound)
  (when (and sound (cl-openal-init))
    (ix-play-start self sound)
    (bwhen (mgr (nearest self sound-manager))
      (push sound (sounds mgr)))))

(defun ix-play-start (self s)
  (when (cl-openal-init)
    (let ((source (typecase (sound-source s)
                    (keyword (bwhen (mgr (nearest self sound-manager))
                               (cdr (assoc (sound-source s) (sources mgr)))))
                    (null (bwhen (mgr (nearest self sound-manager))
                            (cdr (assoc :default (sources mgr)))))
                    (otherwise (sound-source s)))))
      (assert source)
      (sound-play-start s source))))
  
(defun sound-play-start (s srcs)
  (flet ((start-w-src (w src)
           (al-sourcef src al_gain (sound-gain s))
           (al-chk "al-Sourcef AL_GAIN")
        
           (al-sourcei src al_looping (if (sound-loopingp s) 1 0))
           (al-chk "al-Sourcei AL_LOOPING")

           (source-wav-play-start src w)))
    (if (listp srcs)
        (progn
          (assert (eql (length (sound-paths s)) (length srcs)))
          (setf (sound-sources s) srcs)
          (loop for wp in (sound-paths s)
              and src in srcs
              do (start-w-src wp src)))
      (progn
        (assert (and (atom srcs) (eql 1 (length (sound-paths s)))))
        (setf (sound-sources s) (list srcs))
        (start-w-src (car (sound-paths s)) srcs)))

    (setf (sound-start s) (/ (get-internal-real-time)
                            internal-time-units-per-second))))
        
(defun ix-sound-find (self sound-key)
  (bwhen (sound-spec (ix-sound-spec-find self sound-key))
    (ix-sound-spec-sound self sound-spec)))

(defun ix-sound-spec-sound (self sound-spec)
  (etypecase sound-spec
    (sound sound-spec)
    (function (funcall sound-spec self))
    (string (make-sound :paths (list (merge-pathnames sound-spec
                                                  oal::*audio-files*))))
    (pathname (make-sound :paths (list (merge-pathnames sound-spec
                                                  oal::*audio-files*))))))

(merge-pathnames (make-pathname :directory '(:relative "mistakes"))
                                                  oal::*audio-files*)

(defun ix-sound-spec-find (self key)
  (when (typep self 'ix-view)
    (or (cdr (assoc key (sound self)))
      (ix-sound-spec-find .parent key))))

(export! sound-manager sounds sources)

(defmodel sound-manager ()
  ((sources :initarg :sources :accessor sources
     :initform (list (cons :default (car (al-source-gen 1)))))
   (sounds :cell nil :initform nil :accessor sounds)))

(defmethod ctl-notify-mouse-click ((self sound-manager) clickee click)
  (declare (ignore click))
  (bwhen (s (ix-sound-find clickee :click))
    (ix-sound-install self s))
  t)

(defun sound-completed (sound)
  (notany 'al-source-playing-p (sound-sources sound)))

(defmethod ix-paint :after ((self sound-manager))
  (sounds-tend self)
  )

(defun sounds-tend (self)
  (when (cl-openal-init)
    (let ((sounds (sounds self)))
      (loop for sound in sounds
          do ;;(trc "soundtendiung" self sound)
            (if (sound-completed sound)
                (progn
                  (setf (sounds self) (delete sound (sounds self)))
                  (when (listp (sound-source sound))
                    (loop for src in (sound-sources sound)
                        do (al-source-free src))))
              (when (sound-callback sound)
                (funcall (sound-callback sound) 
                  (- (/ (get-internal-real-time) internal-time-units-per-second)
                    (sound-start sound)) (sound-sources sound))))))))

