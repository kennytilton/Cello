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

(defun al-source-gen (&optional (count 1))
  (when (cl-openal-init)
    (let ((sources (fgn-alloc 'al-uint count)))
      (al-gen-sources count sources)
      (al-chk "al-Gen-Sources")
      (prog1
          (loop for n below count
              collecting (progn (assert (plusp (elti sources n)))
                           (elti sources n)))
        (fgn-free sources)))))

(defun al-source-free (sources)
  (let* ((sct (if (listp sources)
                  (length sources) 1))
         (sv (fgn-alloc 'al-uint sct)))
    (if (listp sources)
        (loop for s in sources
           and n below sct
           do (setf (elti sv n) s))
        (setf (elti sv 0) sources))
    (al-delete-sources sct sv)
    (fgn-free sv)))

(defun wav-play-start (wav-path)
  (assert (probe-file wav-path))
  (source-wav-play-start (car (al-source-gen 1)) wav-path))

(defun wav-play-till-end (callback &rest wav-names)
  (when (cl-openal-init)
    (let ((sources (al-source-gen (length wav-names))))
      (loop for wp in wav-names
          and src in sources
          do (source-wav-play-start src wp))
      (unwind-protect
          (loop with start = (get-internal-real-time)
              while (find-if 'al-source-playing-p sources)
              when callback
              do (funcall callback (/ (- (get-internal-real-time) start)
                                     internal-time-units-per-second)
                   sources))
        (al-source-free sources)))))

(let (ss)
  (defun al-source-playing-p (source)
    (unless ss (setf ss (fgn-alloc 'al-int 1)))
    (al-get-sourcei source al_source_state ss)
    (eql al_playing (elti ss 0))))

(defun source-wav-play-start (source wav-path)
  (let ((wav-path (merge-pathnames
                   wav-path
                   *audio-files*)))
    (assert (probe-file wav-path)() "WAV ~a not found" wav-path)
    (bwhen (buffer (wav-to-buffer wav-path)) ;; not if OAL does not like the wav file
      (source-buffer-load source buffer)
      (al-source-play source)
      (al-chk "al-Source-Play")
      source)))

#+test
(go-round)

(defun go-round ()
  (loop ;;for wav in (directory (make-pathname :directory '(:absolute  "sounds")))
    with wav = (make-pathname :directory '(:absolute "0dev" "user"  "sounds") :name "galloping" :type "wav")
    with start = (get-internal-real-time)
    repeat 4
    do (wav-play-till-end 
        (lambda (time srcs)
          (declare (ignore time srcs))
          (let* ((elapsed (coerce (/ (- (get-internal-real-time) start) internal-time-units-per-second) 'float))
                 (angle (* elapsed (/ pi 2)))
                 (dist 5)
                 (x (* dist (cos angle)))
                 (z (* dist (sin angle)))
                 )
            
            ;(cells:trc "time" elapsed srcs)
            (let ((l-zip (make-ff-array al-float x 0 z ))
                  (l-vel (make-ff-array al-float 1 0 0))
                  (l-ori (make-ff-array al-float 0 0 -1 0 1 0)))
              (declare (ignore l-vel))
              ;(al-listenerfv al_position l-zip)
              (al-listenerfv al_position l-zip)
              (al-chk "alListenerfv POSITION : ")
              
              #+noo
              (progn
                (al-listenerfv al_velocity l-zip)
                (al-chk "alListenerfv VELOCITY : "))
              
              ;(al-listenerfv al_orientation l-ori)
              ;(al-chk "alListenerfv ORIENTATION : ")
              (fgn-free l-zip l-ori))))
        wav)
    finally (cells:trc "time" (coerce (/ (- (get-internal-real-time) start) internal-time-units-per-second) 'float))))

#+test
(source-wav-play-start (car (al-source-gen 1) )
  (make-pathname :directory '(:absolute "0dev" "user" "sounds")
          :name "galloping"
          :type "wav"))

(defun wav-to-buffer (wav-path)
  (when (cl-openal-init)
    (let ((buffer (fgn-alloc 'al-uint 1)) ;; was '(* :void) 1)) ;; was 'aluint
          (format (fgn-alloc 'al-enum 1))
          (datahandle (fgn-alloc :pointer 1)) ;; was 4
          (size (fgn-alloc 'al-sizei 1))
          (freq (fgn-alloc 'al-sizei 1))
          (loop (fgn-alloc 'al-boolean 1))
          )
      (al-gen-buffers 1 buffer)
      (al-chk "wav-to-buffer al-gen-buffer")
      
      (unwind-protect
          (progn
            (alut-load-wav-file (namestring wav-path) format datahandle size freq loop)
            (al-chk " wav-to-buffer alut-load-wav-File")
            
            #+shhhh (print (list "wav loaded!" wav-path
                             :format (elti format 0)
                             :datahandle (fgn-pa datahandle 0)
                             :size (fgn-pa size 0)
                             :freq (fgn-pa freq 0)
                             :loop (fgn-pa loop 0)))
            
             (when (null-pointer-p (fgn-pa datahandle 0)) ;; 04-11-14 was elti, bad for OpenMCL
               (format t "~&Cannot handle WAV ~a null-pointer-p datahandle ~a" (namestring wav-path) datahandle (fgn-pa datahandle 0))
               (return-from wav-to-buffer nil))
            
             #+shh (print (list :buffering-data (elti buffer 0) (elti format 0) (fgn-pa datahandle 0)
                          (elti size 0)(elti freq 0)))
             (al-buffer-data (elti buffer 0) (elti format 0) (fgn-pa datahandle 0)
                             (elti size 0)(elti freq 0))
            (al-chk "al-buffer-data")
            
            (alut-unload-wav (elti format 0)(fgn-pa datahandle 0)
              (elti size 0)(elti freq 0))
            (al-chk "alut-unload-wav")
            ;;(format t "~&buffer is ~a" (elti buffer 0))
            (elti buffer 0))
        (fgn-free buffer)
        (fgn-free format)
        (fgn-free datahandle)
        (fgn-free size)
        (fgn-free freq)
        (fgn-free loop)))))

(defun source-buffer-load (source buffer)
 ; (assert (plusp source))
 ; (assert (plusp buffer))

  (al-source-stop source)
  (al-chk "al-Source-Stop")
                  
  (al-sourcei source al_buffer buffer)
  (al-chk "al-Sourcei AL_BUFFER")

  source)
