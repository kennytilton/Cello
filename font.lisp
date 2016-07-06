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

(defstruct font)


;---------------------------------------------------------

(defmethod font-string-length :around (font string &optional start end)
  (declare (ignorable font start end))
  (if string
      (call-next-method)
    0))



;------------------------------------------------------------
(defun font-string-width
     (target-resolution
      font string
      &optional (start 0 start-supplied) (end (length string)))
  (if (or (null string)
        (eql start end))
      0
    (floor (* (cs-logical-dpi) (if start-supplied
                                   (font-string-length font string start end)
                                 (font-string-length font string)))
      target-resolution)))


