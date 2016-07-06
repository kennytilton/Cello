;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cl-openal; -*-
;;________________________________________________________
;;
;;;
;;; Copyright (c) 2004 by Kenneth William Tilton.
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

(in-package :cl-openal)

(defparameter *openal-initialized-p* nil)
(defparameter *openal-library-loaded-p* nil)

#+force
(cl-openal-init t)

(defun cl-openal-init (&optional force)
  (return-from cl-openal-init nil)
  (when (and *openal-initialized-p* (not force))
    (return-from cl-openal-init t))

#-cffi-features:darwin (xoa)

  (when (or (not *openal-library-loaded-p*)
            force)
    (progn
      (assert (use-foreign-library OpenAL)
              () "Failed to load OpenAL dynamic lib")
      (setf *openal-library-loaded-p* t)))

#-cffi-features:darwin
  (assert (use-foreign-library ALut)
    () "Failed to load alut dynamic lib")
  
  (format t "~&Open AL loaded")
  
  #+shakyatbest  (print `(alut init ,(alut-init 0 0)))
  (let ((device (loop for device-name in '("DirectSound3D" "DirectSound" "MMSYSTEM")
                      for alc-device = (alc-open-device device-name)
                      unless (null-pointer-p alc-device)
                      do (format t "~&OpenAL will be using device named ~a" device-name)
                      (return alc-device))))
    (unless device
      (break "~&Failed to Initialize Open AL"))

    (format t "got openal device ~a" device)
      
    (let* ((context (alc-create-context device (null-pointer))))
      (when (null-pointer-p context)
        (break "~&Failed to create Open AL context"))
      (format t "~&created openal context ~a" context)
      (format t "~&making context current ~a"
        (alc-make-context-current context))))
    
  (format t "~&clear AL error code ~a"
    (al-get-error))

  (let ((l-zip (make-ff-array al-float 0 0 10))
        (l-ori (make-ff-array al-float 0 0 -1 0 1 0)))
    
    (al-listenerfv al_position l-zip)
    (al-chk "alListenerfv POSITION : ")
    
    (al-listenerfv al_velocity l-zip)
    (al-chk "alListenerfv VELOCITY : ")
    
    (al-listenerfv al_orientation l-ori)
    (al-chk "alListenerfv ORIENTATION : ")
    (fgn-free l-zip l-ori))

  (setf *openal-initialized-p* t))

(defun cl-openal-shutdown ()
  #+shakyatbest (alut-exit)
  (when *openal-initialized-p*
    (let ((context (alc-get-current-context)))
      (unless (null-pointer-p context)
        (let ((device (alc-get-contexts-device context)))
          (alc-make-context-current (null-pointer))
          (alc-destroy-context context)
          (alc-close-device device)
          (setf *openal-initialized-p* nil))))))

(defun al-chk (error$)
  (let ((status (al-get-error)))
    (if (eql status al_no_error)
        (progn
          #+shh (print (list "al-chk OK:" error$)))
      (break "~&Error< ~d > on ~a" status error$))))
