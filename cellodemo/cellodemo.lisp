;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cello; -*-
;;;
;;; Copyright © 2004 by Kenneth William Tilton.
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

(in-package :cello)

#+(or)
(list
 (demo-image-subdir "shapers")
 (demo-image-subdir))

(defun demo-image-subdir (&optional subdir)
  (make-pathname :directory
    (append '(:absolute "0dev" "user" "graphics")
      (when subdir (list subdir)))))

(defun demo-image-file (subdir file)
  (merge-pathnames file
    (demo-image-subdir subdir)))

(defun ft-jpg (self)
  (mk-part :ft-jpg (ix-zero-tl)
    :px 0 :py 0
    :kids (c? (the-kids
               (a-row (:px 96 :py (downs 96))
                 (mk-part :imk-jpg (ix-image-file)
                   :pre-layer (c? (with-layers +red+ :fill (:wand (^value))))
                   :value (c? (demo-image-file "shapers" "grace.jpg")))

                 (a-stack ()
                   (loop for face in '(antquabi bookosb
                                        georgiai framd times
                                        gothic impact
                                        lucon micross
                                        palab)
                       collect (mk-part :xxx (ix-text)
                                 :pre-layer (with-layers (:rgba +white+))
                                 :text-font (let ((myface face))
                                             (c? (font-ftgl-ensure :texture myface  24)))
                                 :text$ "Hello, world!"))))

               (mk-part :zee (ix-text)
                 :value (c? (if (visible (fm-other :ft-jpg))
                                   (without-c-dependency (frame-ct .togl)) 0))
                 :px (c? (px-maintain-pl (pl (psib))))
                 :justify-hz :center
                 :py (c? (py-maintain-pt (pb (psib))))
                 :pre-layer (with-layers (:out 1500) +blue+)
                 :zoom (c? (let ((start (^value)))
                             (if (without-c-dependency (< 200 (- (frame-ct .togl) start)))
                               .cache
                             (make-list 3 :initial-element (min 2.0 (/ (- (frame-ct .togl) start)
                                                                        100.0))))))
                 
                 :rotation (c? (let ((start (^value)))
                             (if (without-c-dependency (< 200 (- (frame-ct .togl) start)))
                                   .cache
                                 (list (* 360 (/ (min 200 (- (frame-ct .togl) start)) 100.0))
                                     1 1 1))))
                 
                 :text-font (c? (font-ftgl-ensure :texture *gui-style-default-face* 24 ))
                 :text$ "hello, world!")))))
