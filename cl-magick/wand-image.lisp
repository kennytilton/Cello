;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cl-magick; -*-
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

(in-package :cl-magick)

(export! wand-direction image-path image-size tilep)

(defclass wand-image ()
  ((wand-direction  :initarg :wand-direction  :initform :input :accessor wand-direction)
   (image-path :initarg :image-path :initform nil    :accessor image-path)
   (mgk-wand   :initarg :mgk-wand   :initform nil    :accessor mgk-wand)
   (image-size :initarg :image-size :initform nil    :accessor image-size)
   (storage :initarg :storage :initform GL_RGB :accessor storage)
   (tilep     :initarg :tilep     :initform t      :accessor tilep)))

(defmethod initialize-instance :after ((self wand-image) &key)
  (ecase (wand-direction self)
    (:output (progn
               (assert (pixels self))
               (assert (image-size self))
               (setf (mgk-wand self) (new-magick-wand))
               (destructuring-bind (columns . rows) (image-size self)
                 (progn ;; assert (zerop ... well, the doc says zero=sucess, but not the GM.c code (or flop writes)
                   (magick-set-image-pixels
                    (setf (mgk-wand self) (new-magick-wand))
                    0 0 columns rows "CRGB" 3 (pixels self))))
               (magick-set-image-type (mgk-wand self) 3)))
    (:input
     (assert (probe-file (image-path self)) ()
       "Image file ~a not found initializing wand" (image-path self))
     (assert (not (mgk-wand self))) ;; make sure not leaking
     (setf (mgk-wand self) (path-to-wand (image-path self)))
     ;;(mgk-wand-dump (mgk-wand self) (image-path self))
     (when (and (mgk-wand self) (not (image-size self)))
       (setf (image-size self)
         (cons (magick-get-image-width (mgk-wand self))
           (magick-get-image-height (mgk-wand self))))
       (when (zerop (* (car (image-size self)) (cdr (image-size self))))
           (setf (image-size self) (cons 64 64)))))))

(defmethod wand-release ((wand wand-image))
  (when (mgk-wand wand)
    ;(print (list "destroying magick wand" wand))
    ;(describe wand)
    (destroy-magick-wand (mgk-wand wand))))

(defun path-to-wand (path)
  (let ((wand (new-magick-wand))
        (p (namestring path)))
    (assert (probe-file p))
    (let ((stat (magick-read-image wand p)))
      (if (zerop stat)
        (format t "~&magick-read-image failed on ~a" p)
        (format nil "~&magick-read-OK ~a" p))
      wand)))

(defun wand-get-image-pixels (self &optional (first-col 0) (first-row 0)
                               (last-col (magick-get-image-width (mgk-wand self)))
                               (last-row (magick-get-image-height (mgk-wand self)))
                               &aux (wand (mgk-wand self))
                               (bytes-per-pixel (ecase (storage self) (#.gl_rgb 3)(#.gl_rgba 4))))
  (declare (fixnum bytes-per-pixel))
  (if (zerop (* last-col last-row))
      (let* ((columns 64)(rows 64)
             (pixels (fgn-alloc :unsigned-char (* bytes-per-pixel columns rows) :wand-image)))
        (print "wand-get-image-pixels > wand has zero pixels; did the load fail?")
        (dotimes (pn (* columns rows))
          (setf (elti pixels pn) -1))
        (values pixels columns rows))
    
    (let* ((columns (- last-col first-col))
           (rows (- last-row first-row))
           (fmt (intern (string-upcase (magick-get-image-format wand)) :mgk))
           (storage$ (ecase (storage self) (#.gl_rgb "RGB")(#.gl_rgba "RGBA")))
           (pixels (fgn-alloc :unsigned-char (* bytes-per-pixel columns rows) :wand-image)))
      (declare (ignorable fmt))
      (assert (not (null-pointer-p pixels))() "wand-get-image-pixels > fgn-alloc of ~a bytes failed" (* bytes-per-pixel columns rows))
      #+shhh (cells:trc nil "cols, rows, image format" last-col last-row wand fmt bytes-per-pixel storage$)
      
      
      (magick-get-image-pixels wand first-col first-row columns rows storage$ 0 pixels )
      
      #+shhh (cells:trc "doing cols rows image!!!!!!!!!!!!!" rows columns (* columns rows)
               :img-type (magick-get-image-type (mgk-wand self)))
      
      
      (when (find fmt '(gif png))
        ;
        ; fix alpha channel which gets written out inverted for some strange reason I forget
        ;
        (unless #+not nil (block detect-converted
                  (loop for pixel-col fixnum below (floor columns 4)
                      for pixel-offset fixnum = (the fixnum (+ 3 (*  pixel-col bytes-per-pixel)))
                      for pixel-value = (eltuc pixels (the fixnum pixel-offset))
                      ;collecting pixel-value into checked
                      when (> 96 ;; rough guess at how to detect: can't always get perfect alpha w eraser: /= 255
                             pixel-value)
                      do (cells:trc nil "image alpha already converted. I see non-255"
                           pixel-value
                           :at-col pixel-col :checked checked :file (image-path self))
                        (return-from detect-converted t)))
          ;; (cells:trc "converting alpha channel" (image-path self) fmt)
          
          (loop with pix1
              for row fixnum below rows
              do (loop for pixel-col fixnum below columns
                     for pixel-offset fixnum = (the fixnum (+ 3 (the fixnum (* (+ (* row columns) pixel-col) bytes-per-pixel))))
                     do (let ((alpha (eltuc pixels pixel-offset)))
                          (unless pix1
                            (when (zerop alpha)
                              ;;(cells::trcx binogo-pix1 pixel-col row)
                              (setf pix1 (cons pixel-col row))))
                          (setf (eltuc pixels (the fixnum pixel-offset)) (- 255 alpha))))
                ;;when (zerop (eltuc pixels (the fixnum pixel-offset)))
                
              finally
                ;
                ; in place...
                ;
                ;(cells:trc "finally image" self fmt)
                (magick-set-image-pixels wand 0 0 columns rows storage$ 0 pixels)
                
                #+no(let ((reduction (max 1 (sqrt (/ (* columns rows) 200000)))))
                      (unless (= reduction 1)
                        (cells:trc "reduction factor!!!!!!!" reduction)
                        (setf columns (round columns reduction) rows (round rows reduction))
                        (setf (image-size self) (cons columns rows))
                        (magick-resize-image wand columns rows cubic-filter 0)
                        (wand-images-write wand (image-path self))))
                ;
                ; flopped...
                ;
                #+nahh (unless (eq fmt 'gif)
                  (let ((cw (clone-magick-wand wand)))
                    (magick-set-image-type cw (magick-get-image-type wand))
                    (magick-get-image-pixels wand 0 0 columns rows storage$ 0 pixels ) ;; get resized pixels
                    (magick-set-image-pixels cw 0 0 columns rows storage$ 0 pixels)
                    
                    (magick-flop-image cw)
                    (wand-images-write cw (merge-pathnames (conc$ (pathname-name (image-path self)) "-flop")
                                            (image-path self)))
                    (cells:trc nil "local magick" (list columns rows)
                      (list (magick-get-image-width wand)
                        (magick-get-image-height wand))))))))
      
      (values pixels columns rows))))

