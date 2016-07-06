(in-package :cl-openal)

(defconstant num_buffers 7)
(defparameter g-buffers (fgn-alloc 'al-uint num_buffers))
    
(defun cl-openal-test-many ()
  (let ((w$ (list "/0dev/user/sounds/jshootme.wav" )))
    (cl-openal-init)
     (apply 'wav-play-till-end
       (lambda (dur sources)
         (loop for source in sources
             for gain = (max 0 (- 1 (/ dur 3)))
             do (al-sourcef source al_gain gain)
               (al-chk "openal test GAIN set")))
       w$))
  (sleep 1)
  (cl-openal-shutdown))

(defun cl-openal-test ()
  (wav-play-till-end nil "/0dev/user/sounds/jshootme.wav"))



