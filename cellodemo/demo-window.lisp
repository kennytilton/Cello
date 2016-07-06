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

(defun cello-test ()
  (cello-reset)
  (let ((cells::*c-debug* (get-internal-real-time)))
    (run-stylish-demos '(#+No light-panel
                          ;;ft-jpg
                          tu-geo
                          ;;ftgl-test
                          #+no demo-scroller)
      'tu-geo
      :skin nil #+hhack (c? (wand-ensure-typed 'wand-texture
                  (car (value (fm-other :texture-picker)))))
      :lb (c-in (downs 1000)))))

(defun demo-scroller (self)
  (mk-part :demo-scroller (ix-zero-tl)
    :kids (c? (the-kids
               (mk-part :dialog (ix-zero-tl)
                 :px 48 :py -48
                 :outset (u8ths 2)
                 :skin (c? (wand-ensure-typed 'wand-texture
                             (merge-pathnames
                              (make-pathname
                               :name "brushdmtl"
                               :type "jpg")
                              (demo-image-subdir))))
                 :pre-layer (c? (let ((tx-name (texture-name (^skin)))
                                      (tx-size (image-size (^skin))))
                                  (with-layers :on +white+
                                    (:mat-emission (let ((b 64))
                                                     (mk-rgba b b b 255)))
                                    (:texturing :on :name tx-name
                                      :tx-size tx-size)
                                    (:oblong 16 18 :slices 9 :stacks 6)
                                    (:out 16))))
                 :kids (demo-scroller-beef))))))

(defun demo-scroller-beef ()
  (c? (the-kids
       (mk-part :scroller (ix-scroller)
         :px 0 :py 0
         :mac-p t
         :scroll-bars '(:horizontal :vertical)
         :start-size (mkv2 (u96ths 150)(u96ths (downs 250)))
         :resizeable t
         :content (c? (mk-part :gview (ix-image-file)
                         :wand-type 'wand-pixels
                         :value (demo-image-file "shapers" "mandelbrot3.gif")))))))

(defun run-demos (demo-names start-at &rest iargs)
  (declare (ignorable start-at))
  (apply 'run-window 'demo-window 'application t
     (lambda ()
                   ;;; -- not sure how much of this new reset stuff is necessary ---
                   ;;; (cl-ftgl-reset)
                   ;;; (cl-ftgl-init)
                   (wands-clear))
    :value (c-in (list start-at))
    :content demo-names
    iargs))

(defun run-stylish-demos (demo-names start-at &rest iargs)
  (apply 'run-demos demo-names start-at iargs)
  #+hhack
  (with-styles (
                (make-instance 'gui-style-ftgl
                  :id :button 
                  :face *gui-style-button-face*
                  :sizes '(14 14 14 14 14)
                  :text-color +white+)
                (make-instance 'gui-style-ftgl
                  :id :label 
                  :face *gui-style-button-face*
                  :sizes '(12 12 12 12 12)
                  :text-color +white+)
                (make-instance 'gui-style-ftgl
                  :id :default 
                  :mode :texture
                  :face *gui-style-button-face*
                  :sizes '(14 9 14 14 14)
                  :text-color +green+))
    (apply 'run-demos demo-names start-at iargs)))

(defmodel demo-window (sound-manager cello-window)
  ()
  (:default-initargs
      :sound nil #+hhack   `((:open .
                 ,(lambda (self)
                    (declare (ignorable self))
                    (make-sound :paths '("open-window")
                      :source (al-source-gen 1)
                      :callback (lambda (dur sources)
                                  (loop for source in sources
                                      for gain = (max 0 (- 1 (/ dur 3)))
                                      do (al-sourcef source al_gain gain)
                                        (al-chk "openal test GAIN set"))))))
                (:keydown . "key-down")
                (:close . "close-window"))
    :ll 0 :lt 0
    :lr (c-in (scr2log 1000))
    :lb (c-in (scr2log -1500))
    
;;     :fixed-lighting (list (make-instance 'light
;;                             :id gl_light6
;;                             :enabled t
;;                             :pos (make-ff-array :float 200 (downs 300) (farther 500) 1)
;;                             :ambient *dusk*
;;                             :diffuse *dim*
;;                             :specular *bright*))
    
    :recording nil #+(or) (c? (when (value (fm-other :record))
                     (make-recording
                      :wand (magick-wand-template)
                      :splice-wand (magick-wand-template)
                      :pathname (merge-pathnames
                                 (make-pathname :name "bingo" :type "mpg")
                                 cl-user::*user-output-directory*))))
    
;;    :display-continuous nil
    :md-name :demo-w
    :title$ "Hello there, world"
    :skin nil 
    :lighting :on
;;    :clear-rgba (list 0 0 0 1)
;;     :light-model (c? (bwhen (lm (fm-other? :light-model))
;;                        (list (value lm))))
    
    :snapshot-pathnamer (lambda (self)
                          (make-pathname
                           :directory '(:absolute "odev" "user" "output")
                           :name (format nil "snap-me-~3,,,'0@A"
                                   (snapshot-release-id self))
                           :type "jpg"))
    
    :pre-layer (c? (with-layers
                       +red+
                     :off
                     +blue+
                     (:in 500)
                     :normal-out
                     ;; hhack (:wand (^skin))
                     (:out 500)))
    :clipped nil
    :kids (c? (the-kids
               ;; (demo-window-beef self)
               #+nicetry
               (mk-part :wintop (ix-zero-tl)
                 :px 0 :py 0
                 :ll 0 :lt 0 :lr (c? (l-width .parent))
                 :lb (c? (downs (l-height .parent)))
                 :kids (c? (the-kids
                            (demo-window-beef)
                            (mk-part :cursor (ix-view)
                              :px (c? (bif (mpos (mouse-pos .w.))
                                        (v2-h mpos) 100))
                              :py (c? (bif (mpos (mouse-pos .w.))
                                        (v2-v mpos) -100))
                              :ll -8 :lt 8 :lr 8 :lb -8
                              :pre-layer (with-layers :off (:out 800) +red+ (:x-mark t)))
                            )))))))
  
(defmethod window-display :after ((self demo-window))
  (when (snapshot-release-id self)
    (ix-snapshot self)
    (setf (snapshot-release-id self) nil))
        
  (when (recording self)
    (ix-record-frame self (recording self)))

  #+someday (fm-traverse self (lambda (node)
                                (when (recording node)
                                  (ix-snapshot node (recordingp node))))))

(defmethod not-to-be :after ((self window))
  (unless (kids *sys*)
    (cl-openal-shutdown))
  (wands-clear))

(defmethod do-keydown :before ((self demo-window) key-char event)
  (declare (ignorable key-char event))
  (bwhen (s (and (eql key-char #\escape) ;; lame
              (ix-sound-find self :close)))
    (wav-play-till-end nil (car (sound-paths s)))))

(defun demo-window-beef (self)
  (mk-part :beef (ix-inline)
    :orientation :vertical
    :px 0 :py (u8ths (downs 1))
    :spacing (u8ths 1)
    :lb (c? (^fill-parent-down))
    :kids (c? (the-kids
               (demo-control-panel self)
               (mk-part :demos (ix-zero-tl)
                 ;;:py (u8ths 4)
                 :lb (c? (^fill-parent-down))
                 :kid-slots (lambda (self)
                              (declare (ignore self))
                              (list
                               (mk-kid-slot (visible)
                                 (c? (string-equal (md-name self)
                                       (car (value .w.)))))
                               (mk-kid-slot (px)
                                 (c? (px-maintain-pl 0)))
                               (mk-kid-slot (py)
                                 (c? (py-maintain-pt 0)))))
                 :kids (let (demos-built)
                         (c? (bwhen (demo-factory (car (value .w.)))
                               (unless (assoc demo-factory demos-built)
                                 (pushnew (cons demo-factory (funcall demo-factory))
                                   demos-built)))
                           (mapcar 'cdr demos-built))))))))

(defun demo-control-panel (self)
  (a-row (:spacing (u8ths 2) :justify :center)
    ;;(mk-part :rate (frame-rate-text))
    (a-stack (:spacing (u16ths 1))
      (texture-picker self)
      (demo-picker self))
    (a-stack (:spacing (u96ths 6) 
               :justify :center
               :outset (u96ths 6)
               :visible (c? (not (snapshot-release-id .w.)))
               :pre-layer (c? (with-layers +gray+ :on
                                (:frame-3d :edge-sunken
                                  :thickness (u96ths 4))
                                ;;+black+
                                +yellow+
                                )))
      
      (a-label "just shoot me!"
        :text-font (c? (ftgl-font-ensure
                        :texture 'stacc222 14 96))
        :pre-layer (c? (with-layers +yellow+ :fill +gray+)))
      (mk-part :record (ct-push-toggle)
        :value (c-in nil)
        :title$ "record")
      (mk-part :snapshot (ct-button)
        :title$ "snapshot"
        :ct-action (let ((snap-count -1))
                     (lambda (self event)
                       (declare (ignorable event))
                       (ix-sound-install self
                         (make-sound :paths '("jshootme.wav")
                           :gain .5 :source :default))
                       (setf (snapshot-release-id .w.)
                         (incf snap-count))))))))


(defun texture-picker (self &aux (backdrops 
                             (directory
                              (demo-image-subdir "window-bkgs"))))
  (a-row (:spacing (u8ths 1))
    (a-label "Skins")
    (mk-part :texture-picker (ct-radio-row)
      :spacing (upts 4)
      :value (c-in (let ((jpegs backdrops))
                      (list (or (find-if (lambda (jpeg)
                                          (search "concrete" (pathname-name jpeg)))
                                  jpegs)
                              (car jpegs)))))
      :clipped nil
      :kids (c? (mapcar (lambda (p)
                          (mk-part :rb (ct-radio-push-button)
                            :radio self
                            :associated-value p
                            :lr (u8ths 6)
                            ;:inset (mkv2 4 2)
                            :title$ (pathname-name p)))
                  backdrops)))))

(defun demo-picker (self)
  (a-row (:spacing (u8ths 1) :justify :center)
    (a-label "Demos")
    (mk-part :demo (ix-row)
      :spacing (upts 4)
      :clipped nil
      :kids (c? (mapcar (lambda (s)
                          (mk-part :rb (ct-radio-push-button)
                            :radio .w.
                            :associated-value s
                            :title$ (string-downcase
                                     (format nil "~d" s))))
                  (content .w.))))))

(defun nested-windows (self)
  (a-row (:md-name 'nested-windows :px 0 :py 0 :spacing (upts 10))
    (a-stack ()
      (starter-toolbar self)
      (starter-hedron self))
    
    (mk-part :socket (window-socket)
      :px (uin 2)
      :window-factory (lambda (socket glut-xy)
                        (declare (ignorable socket))
                        (make-instance 'demo-window
                          :value (c-in (list (car (content .w.))))
                          :content (content .w.)
                          :glut-xy glut-xy))
      :gen-window-p (c? (value (cells::fm-find-one (upper self window)
                                     :nested
                                     :must-find t
                                     :skip-tree self))))))
  
(defparameter *starter-font* nil)

(defparameter *rot* 0)


(defparameter *idle-angle* 0)

(defun starter-toolbar (self)
  (a-row (:spacing (upts 10))
    (mk-part :hw (ct-button)
      ;:inset (mkv2 (uPts 4)(uPts 2))
      ;:lr (uin 1)
      :text$ "Close"
      :ct-action (lambda (self event)
                   (declare (ignorable self event))
                   (ctk::tcl-eval-ex ctk::*tki* "{destroy .}")))
    
    (mk-part :neww (ct-button)
      ;:inset (mkv2 (uPts 4)(uPts 2))
      ;:lR (uIn 1)
      :text$ "New"
      :ct-action (lambda (self event)
                       (declare (ignorable self event))
                       (push (run-demos (content .w.)
                               :start-at (car (content .w.))
                               :skin (skin .w.))
                         (kids *sys*))))
    
    (mk-part :nested (ct-check-text)
      :value (c-in nil)
      :title$ "Nested")))






