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

#|
  ImageMagick Pixel Wand API.
|#

(in-package :cl-magick)

(defun-ffx :string "imagick"
  "PixelGetColorAsString" (:void *pxwand))

(ffx::defun-ffx-multi :double "imagick"
  "PixelGetBlack" (:void *pxwand) 
  "PixelGetBlue" (:void *pxwand) 
  "PixelGetCyan" (:void *pxwand) 
  "PixelGetGreen" (:void *pxwand) 
  "PixelGetMagenta" (:void *pxwand) 
  "PixelGetOpacity" (:void *pxwand) 
  "PixelGetRed" (:void *pxwand) 
  "PixelGetYellow" (:void *pxwand))

(defun-ffx (* :void) "imagick"
  "NewPixelWand" ())
;;  **NewPixelWands" ( unsigned long);

;;; defun-ffx :Quantum
;;;  "PixelGetBlackQuantum" ( :void *pxwand) 
;;;  "PixelGetBlueQuantum" ( :void *pxwand) 
;;;  "PixelGetCyanQuantum" ( :void *pxwand) 
;;;  "PixelGetGreenQuantum" ( :void *pxwand) 
;;;  "PixelGetMagentaQuantum" ( :void *pxwand) 
;;;  "PixelGetOpacityQuantum" ( :void *pxwand) 
;;;  "PixelGetRedQuantum" ( :void *pxwand) 
;;;  "PixelGetYellowQuantum" ( :void *pxwand);

(defun-ffx :unsigned-int "imagick"
  "PixelSetColor" ( :void *wand  :string hex-rgb))

(defun-ffx :unsigned-long "imagick"
  "PixelGetColorCount" ( :void *pxwand))

(ffx::defun-ffx-multi :void "imagick"
  "DestroyPixelWand" (:void *pxwand) 
  "PixelGetQuantumColor" (:void *pxwand :void *pxpacket) 
  "PixelSetBlack" ( :void *pxwand :double color) 
;  "PixelSetBlackQuantum" ( :void *pxwand Quantum) 
  "PixelSetBlue" ( :void *pxwand :double color) 
;  "PixelSetBlueQuantum" ( :void *pxwand Quantum) 
  "PixelSetColorCount" ( :void *pxwand :unsigned-long count) 
  "PixelSetCyan" ( :void *pxwand :double color) 
;  "PixelSetCyanQuantum" ( :void *pxwand Quantum) 
  "PixelSetGreen" ( :void *pxwand :double color) 
;  "PixelSetGreenQuantum" ( :void *pxwand Quantum) 
  "PixelSetMagenta" ( :void *pxwand :double color) 
;  "PixelSetMagentaQuantum" ( :void *pxwand Quantum) 
  "PixelSetOpacity" ( :void *pxwand :double opacity) 
;  "PixelSetOpacityQuantum" ( :void *pxwand Quantum) 
;  "PixelSetQuantumColor" ( :void *pxwand PixelPacket *) 
  "PixelSetRed" ( :void *pxwand :double color) 
;  "PixelSetRedQuantum" ( :void *pxwand Quantum) 
  "PixelSetYellow" ( :void *pxwand :double color) 
;  "PixelSetYellowQuantum" ( :void *pxwand Quantum)
  )

(defun rgb$ (r g b)
   (string-upcase (format nil "#~6,'0x" (+ (* r #x10000) (* g #x100) b))))

(let (pxw)
  (defun pxw-color (&optional (r 0) (g 0) (b 0))
    (setq pxw (or pxw (new-pixel-wand)))
    (pixel-set-color pxw (rgb$ r g b))
    pxw))

(let (pxw)
  (defun pxw-opacity (&optional (opacity 1.0))
    (setq pxw (or pxw (new-pixel-wand)))
    (pixel-set-opacity pxw opacity)
    pxw))
