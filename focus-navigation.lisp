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

;_____________________ N a v i g a t i o n  ____________________
;
(defun focus-navigate (old new &optional leave-old)
  ;; (trc "focus-navigate > old, new" old new)
  ;; (c-assert new) ;; 990810kt i don't remember if we navigate to nil (should tho) ///

  (when (eql old new)
    (return-from focus-navigate new))
  
  (when (or (null old)(mdead old))
    (focus-navi-enter new)
    (focus-on new)
    (return-from focus-navigate new))
  
  (let ((resting
         (catch :focus-navigate
            (let (in-range)
               (labels ((step-stone (stone)
                                    ;;(trc "step-stone" stone in-range)
                                    (when (eql stone new)
                                      (when (and leave-old old)
                                        (setf leave-old nil)
                                        (focus-navi-leave old))
                                      (when in-range ; if not in range, navving backwards
                                        (focus-navi-enter stone))
                                      (throw :focus-navigate new))
                                    (when (kids stone)
                                      (dolist (kid (kids stone))
                                        (step-stone kid)))
                                    (when (eql stone old)
                                      (setf in-range t))
                                    (when in-range
                                      (setf leave-old nil)
                                      (focus-navi-leave stone))))
                 ;
                 (step-stone (fm-ascendant-common old new))
                 ;; (step-stone (focus-navi-cares (fm-ascendant-common old new) old))
                 ;
                 )))))
     #+nah (trc "focus-navigate > BINGO: root, old, new, newtracker"
          (find-focus-root resting) old resting (focuser resting))
     (with-metrics (nil nil "Actual set focus")
       (focus-on resting))
     ))

(defun focus-navi-cares (top-focus start-focus)
  (cond ((eql top-focus start-focus) start-focus)
        ((focus-cares top-focus) top-focus)
        (t (focus-navi-cares (fm-kid-containing top-focus start-focus) start-focus))))

(defmethod focus-cares (wk)
  (declare (ignore wk))
  t)

(defmethod focus-navi-enter (work)
   (declare (ignorable work))
  #+navi (trc "focus-navi-enter >" work)
   )

(defmethod focus-navi-leave (work)
  (declare (ignore work)))

