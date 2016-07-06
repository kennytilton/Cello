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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(pixels-to-file wand-image-pixels-set 
             make-recording record-frame recording-write recording-destroy)))

(defstruct recording
  wand
  splice-wand
  pathname
  (frame-no -1))

(defun recording-write (recording)
  (wand-images-write
   (recording-wand recording)
   (namestring (recording-pathname recording))
   t))

(defun recording-destroy (recording)
  (when (recording-wand recording)
    (destroy-magick-wand (recording-wand recording)))
  (when (recording-splice-wand recording)
    (destroy-magick-wand (recording-splice-wand recording))))

(defun record-frame (recording pixels width height
                            &aux (wand (recording-wand recording)))
  (print `(wand-image-pixels-set ,recording ,pixels ,width ,height))
  (when (recording-splice-wand recording)
    (incf (recording-frame-no recording)))
  (if (> (recording-frame-no recording) 0)
      (progn
        (print `(adding-frame ,(recording-frame-no recording)
                  index ,(magick-get-image-index wand)
                  apicedex ,(magick-get-image-index (recording-splice-wand recording))))
        (magick-add-image wand
          (wand-image-pixels-set (recording-splice-wand recording) pixels width height))
        (print `(post-add-index ,(magick-get-image-index wand))))
    (wand-image-pixels-set wand pixels width height)))

(defun wand-image-pixels-set (wand pixels width height)
  (unless (and (eql width (magick-get-image-width wand))
            (eql height (magick-get-image-height wand)))
    (magick-scale-image wand width height))
;;;      gaussian-filter ;; /// any faster? mode doesn't matter, about to stomp pix
;;;      0))

  (if (zerop ;; the GM doc seems in error when it says zero is success
       (magick-set-image-pixels wand 0 0 
         width height "RGB" short-pixel pixels))
      (error "MagickSetImagePixels failed: ~a" wand)
    (magick-flip-image wand) ;; /// necessary?
    )
  wand)


(defun pixels-to-file (pixels width height path$)
  (print `(pixels-to-file ,pixels ,width ,height ,path$))
  (time
   (let* ((wand (magick-wand-template)))
     ;;;          (pixels (wand-get-image-pixels moon)))
     ;;;      (print `(old-format ,(magick-get-image-format old)))
     ;;;      (print `(old-type ,(magick-get-image-type old)))
     ;;;      (print `(dims-old ,(cons (magick-get-image-width old)(magick-get-image-height old))))
     ;;;      (print `(dims-moon ,(cons (magick-get-image-width moon)(magick-get-image-height moon))))
     ;;(magick-set-filename wand path$) ;; pointless?
     (magick-scale-image wand
        width height
        ;; undefined-filter 0;; \\\all the same?
        )
     
     (if (zerop (magick-set-image-pixels wand 0 0 
                  width height "RGB" short-pixel pixels))
         (error "MagickSetImagePixels failed preparing ~a" (namestring path$))
       (magick-flip-image wand)))))

(defun wand-images-write (mgk-wand path$ &optional adjoin)
  ;(print `(wand-images-write ,(magick-get-image-index mgk-wand)))
  (when (zerop (magick-write-images mgk-wand (namestring path$) (if adjoin 1 0)))
    (break "MagickWriteImage failed writing ~a" (namestring path$))))