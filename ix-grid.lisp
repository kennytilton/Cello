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

(defmacro u-grid () `(fm-parent self))

(export! ix-grid)

(defmodel ix-grid (ix-zero-tl)
  ((col-ct :initarg :col-ct :initform nil :accessor col-ct)
   (row-ct :initarg :row-ct :initform nil :accessor row-ct)
   ;
   (colpadding :cell nil :initarg :colpadding :initform 0 :reader colpadding)
   (row-padding :cell nil :initarg :row-padding :initform 0 :reader row-padding)
   ;
   (all-cell-width :initarg :all-cell-width :initform nil :reader all-cell-width)
   (all-cell-height :initarg :all-cell-height :initform nil :reader all-cell-height)
   (all-cell-frame :cell nil :initarg :all-cell-frame :initform nil :reader all-cell-frame)
   ;
   (row-offsets :initarg :row-offsets :initform (ix-grid-row-offsets) :reader row-offsets)
   (col-offsets :initarg :col-offsets :initform (ixgrid-coloffsets) :reader col-offsets)
   ;
   (row-justifys :initarg :row-justifys :initform :center :reader row-justifys)
   (col-justifys :initarg :col-justifys :initform :left :accessor col-justifys)
   )
  (:default-initargs
      :ll (c? (- (outset self)))
    :lt (c? (outset self))
    :lb (ix-grid-lb)
    :lr (ix-grid-lr)
    :clipped nil #+not t
    :col-ct (c? (ceiling (length (^kids)) (^row-ct)))
    :row-ct (c? (ceiling (length (^kids)) (^col-ct)))
    :kid-slots (lambda (self)
                 (declare (ignore self))
                 (list
                  (mk-kid-slot (px)
                    (make-ix-grid-px))

                   (mk-kid-slot (py)
                     (c? (let ((justify (row-justifys (u-grid))))
                           (case (if (listp justify)
                                     (elt justify (cell-row self))
                                   justify)
                             (:top (py-maintain-pt (cell-pt self)))
                             (:center (py-maintain-pt
                                       (- (cell-pt self)
                                         (floor (- (cell-height self) (l-height self)) 2))))
                             (:bottom (py-maintain-pb (cell-pb self)))
                             (otherwise 0)))))))))

(defun ix-grid-lr ()
  (c? (col-pr self (1- (^col-ct)))))

(defun ix-grid-lb ()
  (c? (row-pb self (cell-row (last1 (^kids))))))

(defun row-pb (grid rown)
  (- (elt (row-offsets grid) (1+ rown))
      (row-padding grid)))

(defun col-pr (grid coln)
  (- (elt (col-offsets grid) (1+ coln))
           (colpadding grid)))

(defun cell-pl (self)
  (elt (col-offsets (fm-parent self)) (cell-col self)))

(defun cell-pr (self)
  (col-pr (u-grid) (cell-col self)))

(defun cell-width (self)
  (- (cell-pr self) (cell-pl self)))

(defun cell-pt (self)
  (elt (row-offsets (fm-parent self)) (cell-row self)))

(defun cell-pb (self)
  (row-pb (u-grid) (cell-row self)))

(defun cell-height (self)
  (- (cell-pb self) (cell-pt self)))

(defmodel ix-grid-uniform (ix-grid)
  ()
  (:default-initargs
      :all-cell-width (c? (round (l-width self) (col-ct self)))
    :all-cell-height (c? (round (l-height self) (row-ct self)))
    :col-justifys :left
    :row-justifys :top))
                   
(defmodel ix-grid-fv (family-values ix-grid)())
(defmodel ix-grid-uniform-fv (ix-grid-uniform ix-grid-fv)())

;
;---------- painting --------------------------

;;;(defmethod ix-paint :after ((self IXGrid) gR w)
;;;  (bWhen (frame (allCellFrame self))
;;;     (let ((cellR #.(mkR 0 0 0 0)))
;;;       (dotimes (rowNo (1- (length (rowOffsets self))))
;;;         (doTimes (colNo (1- (length (colOffsets self))))
;;;           (let ((left (elt (colOffsets self) colNo))
;;;                 (top (elt (rowOffsets self) rowNo))
;;;                 (right (elt (colOffsets self) (1+ colNo)))
;;;                 (bottom (elt (rowOffsets self) (1+ rowNo)))
;;;                 )
;;;             (nr-make cellR left top right bottom)
;;;             (nr-move cellR (r-top-left gR))
;;;             (trc nil "grid R" self rowno colno cellR)
;;;             (ix-paint frame cellR w)))))))

;
;---------- geometry ----------------------------



(defun ixgrid-coloffsets ()
  (c? (declare (optimize (safety 3)))
    (let* ((col-ct (col-ct self))
           (offsets (make-array (1+ col-ct)
                      :initial-element (or (^all-cell-width) 0))))
      (assert (null (^all-cell-width)))
      
      (unless (^all-cell-width)
        (trcx :setcellw2)
        (loop for kid in (kids self)
            for kn upfrom 0
            for ox = (mod kn col-ct)
            do (assert (numberp ox) () "ox??? ~a ~a ~a" (kid-no kid) col-ct (mod (kid-no kid) col-ct))
              (trcx :stats ox (kid-no kid) (mod (kid-no kid) col-ct))
              (assert (numberp (elt offsets ox)) () "offs?? ~a ~a ~a" offsets (type-of offsets) (elt offsets ox))
              (setf (elt offsets ox)
                (max (elt offsets ox)(l-width kid)))))
      
      (let ((offset 0) this-size)
        (trcx :setcoloffs)
        (dotimes (col-no (1+ col-ct))
          (setf this-size (elt offsets col-no))
          (setf (elt offsets col-no) offset)
          (let ((p (+ (colpadding self) this-size)))
            (trcx :gotp p)
            (incf offset p))))
      
      (trcx :colofffini)
      
      offsets)))


(defun ix-grid-row-offsets ()
  (c? (declare (optimize (safety 3)(speed 0)))
    (let* ((col-ct (col-ct self))
              (row-ct (ceiling (length (kids self)) col-ct))
              (offsets (make-array (1+ row-ct)
                                 :initial-element (or (^all-cell-height) 0))))

         (unless (^all-cell-height)
           (loop for kid in (kids self)
                 for ox = (floor (kid-no kid) col-ct)
                 do (setf (elt offsets ox)
                      (max (elt offsets ox)(l-height kid)))))

         (let ((offset 0) this-size)
            (dotimes (row-no (1+ row-ct))
              (setf this-size (elt offsets row-no))
              (setf (elt offsets row-no) offset)
              (decf offset (+ (row-padding self) this-size))))
         offsets)))

(defun cell-row (self)
  (floor (kid-no self) (col-ct (u-grid))))

(defun cell-col (self)
  (mod (kid-no self) (col-ct (u-grid))))

(defun cell-sib (self row-no col-no)
  (elt (kids .parent) (+ (* row-no (col-ct (u-grid))) col-no)))

(defun col-head (self)
  (cell-sib self 0 (cell-col self)))

(defun row-head (self)
  (cell-sib self (cell-row self) 0))


(defun make-ix-grid-px ()
  (c? (let ((justify (col-justifys (u-grid))))
        (case (if (listp justify)
                  (elt justify (cell-col self))
                justify)
          (:left (px-maintain-pl (cell-pl self)))
          (:center (px-maintain-pl
                    (+ (cell-pl self)
                      (floor (- (cell-width self) (l-width self)) 2))))
          (:right (px-maintain-pr (cell-pr self)))
          (otherwise (trc "woohoo bad justify" (if (listp justify)
                                                   (elt justify (cell-col self))
                                                 justify)(cell-col self)justify)
            0)))))



(defun elt1 (list-or-atom n)
   (trc "elt1 > la, latyp" list-or-atom (type-of list-or-atom))
   (if (listp list-or-atom)
      (elt list-or-atom n) list-or-atom))

(defun grid-cell-ref (grid row-no col-no)
  (elt (kids grid) (+ (* row-no (col-ct grid)) col-no)))


;;; --- ix dot grid ----------------------------------------------------------

(export! ix-dot-grid dot-color ^dot-color dot-size ^dot-size)

(defmd ix-dot-grid (ix-view)
  dot-color
  (back-color nil #+ewww +gray+)
  (dot-size 6)
  (rows (c? (when (numberp (^value))
              (floor (sqrt (abs (^value)))))))
  (columns (c? (when (and (numberp (^value))
                       (numberp (^rows))
                       (plusp (^rows)))
                 (ceiling (abs (^value)) (^rows)))))
  :ll (c? (if (^collapsed)
              0 (- (v2-h (^inset)))))
  :lt (c? (if (^collapsed)
              0 (ups (v2-v (^inset)))))
  :lb (c? (if (^collapsed)
              0 (+ (downs (* 2 (v2-v (^inset))))
                  (* (^rows) (- (+ 2 (^dot-size))))
                  -2)))
  :lr (c? (if (^collapsed)
              0 (+ (* 2 (v2-h (^inset)))
                  (* (+ 2 (^dot-size)) (^columns))
                  -2)))
  :pre-layer (c? (bif (bkc (^back-color))
                   (with-layers :off (:rgba bkc) :fill
                     (:poly-mode gl_front_and_back gl_fill)
                     (:rgba (^dot-color)))
                   (with-layers :off (:poly-mode gl_front_and_back gl_fill)
                     (:rgba (^dot-color))))))

(defmethod ix-paint ((self ix-dot-grid))
  (let ((spacing 2)
        (offset (ceiling (^dot-size) 2)))
    (gl-point-size (^dot-size))
    (gl-enable gl_point_smooth)
    (with-gl-translation ((+ offset (v2-h (^inset))) (downs (+ offset (v2-v (^inset)))))
      (with-gl-begun (gl_points)
        (loop for pn below (abs (^value))
            for row = (mod pn (^rows))
            for col = (floor pn (^rows))
            do (gl-vertex2f (* col (+ spacing (^dot-size)))(* row (- (+ spacing (^dot-size))))))))))
