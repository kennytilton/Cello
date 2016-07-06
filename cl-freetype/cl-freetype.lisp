;;;  cl-freetype
;;;

(defpackage #:cl-freetype
  (:nicknames #:ft)
  (:use #:common-lisp #:ffx #:cffi-uffi-compat #:cl-rsrc)
  (:export #:freetype-dynamic-lib-path
	   #:*default-face*
	   #:*face-registry*

	   ;; these should be moved to somewhere else.
	   #:get-lisp-object
	   #:with-lisp-pointer
	   
	   #:initialize-ft
	   #:done-ft
	   #:with-ft-face
	   #:get-new-face
	   #:with-ft-memory-face
	   #:get-new-memory-face
	   #:done-face
	   #:set-char-size
	   #:set-pixel-sizes
	   #:get-char-index
	   #:get-kerning
	   #:load-glyph
	   #:load-char
	   #:render-glyph
	   #:get-first-char
	   #:get-next-char
	   #:get-postscript-name

	   #:get-face-spec
	   #:get-face-by-spec
	   #:get-face
	   
	   #:ft-glyph-format/none
	   #:ft-glyph-format/composite
	   #:ft-glyph-format/bitmap
	   #:ft-glyph-format/outline
	   #:ft-glyph-format/plotter
	   
	   #:ft-encoding/none
	   #:ft-encoding/ms-symbol
	   #:ft-encoding/unicode
	   #:ft-encoding/sjis
	   #:ft-encoding/gb2312
	   #:ft-encoding/big5
	   #:ft-encoding/wansung
	   #:ft-encoding/johab
	   #:ft-encoding/ms-sjis
	   #:ft-encoding/ms-gb2312
	   #:ft-encoding/ms-big5
	   #:ft-encoding/ms-wansung
	   #:ft-encoding/ms-johab
	   #:ft-encoding/adobe-standard
	   #:ft-encoding/adobe-expert
	   #:ft-encoding/adobe-custom
	   #:ft-encoding/adobe-latin-1
	   #:ft-encoding/old-latin-2
	   #:ft-encoding/apple-roman

	   #:ft-render-mode/normal
	   #:ft-render-mode/mono
	   #:ft-render-mode/light
	   #:ft-render-mode/lcd
	   #:ft-render-mode/lcd-v
	   #:ft-render-mode/max

	   #:ft-pixel-mode/none
	   #:ft-pixel-mode/mono
	   #:ft-pixel-mode/grays
	   #:ft-pixel-mode/gray2
	   #:ft-pixel-mode/gray4
	   #:ft-pixel-mode/lcd
	   #:ft-pixel-mode/lcd-v

	   #:ft-outline/none
	   #:ft-outline/owner
	   #:ft-outline/even-odd-fill
	   #:ft-outline/reverse-fill
	   #:ft-outline/ignore-dropouts

	   #:ft-get-tag
	   #:ft-curve-tag/on
	   #:ft-curve-tag/conic
	   #:ft-curve-tag/cubic

	   #:ft-kerning/default
	   #:ft-kerning/unfitted
	   #:ft-kerning/unscaled

	   #:ft-face-flag/scalable
	   #:ft-face-flag/fixed-sizes
	   #:ft-face-flag/fixed-width
	   #:ft-face-flag/sfnt
	   #:ft-face-flag/horizontal
	   #:ft-face-flag/vertical
	   #:ft-face-flag/kerning
	   #:ft-face-flag/fast-glyphs
	   #:ft-face-flag/multiple-masters
	   #:ft-face-flag/glyph-names
	   #:ft-face-flag/external-stream

	   #:ft-load/default
	   #:ft-load/no-scale
	   #:ft-load/no-hinting
	   #:ft-load/render
	   #:ft-load/no-bitmap
	   #:ft-load/vertical-layout
	   #:ft-load/force-autohint
	   #:ft-load/crop-bitmap
	   #:ft-load/pedantic
	   #:ft-load/ignore-global-advance-width
	   #:ft-load/no-recurse
	   #:ft-load/ignore-transform
	   #:ft-load/monochrome
	   #:ft-load/linear-design
	   #:ft-load/sbits-only
	   #:ft-load/no-autohint

	   #:ft-has-kerning
	   #:ft-is-scalable
	   #:from-ft
	   #:to-ft

	   #:ft-vector
	   #:ft-vector/y
	   #:ft-vector/x

	   #:ft-bbox
	   #:ft-bbox/xmin
	   #:ft-bbox/ymin
	   #:ft-bbox/xmax
	   #:ft-bbox/ymax

	   #:ft-generic
	   #:ft-generic/data
	   #:ft-generic/finalizer

	   #:ft-bitmap-size
	   #:ft-bitmap-size/width
	   #:ft-bitmap-size/height
	   #:ft-bitmap-size/size
	   #:ft-bitmap-size/x-ppem
	   #:ft-bitmap-size/y-ppem

	   #:ft-glyph-metrics
	   #:ft-glyph-metrics/width
	   #:ft-glyph-metrics/height
	   #:ft-glyph-metrics/vert-advance
	   #:ft-glyph-metrics/vert-bearing/y
	   #:ft-glyph-metrics/vert-bearing/x
	   #:ft-glyph-metrics/hori-advance
	   #:ft-glyph-metrics/hori-bearing/y
	   #:ft-glyph-metrics/hori-bearing/x

	   #:ft-sizerec
	   #:ft-sizerec/metrics/x-scale
	   #:ft-sizerec/metrics/y-scale
	   #:ft-sizerec/metrics/x-ppem
	   #:ft-sizerec/metrics/y-ppem
	   #:ft-sizerec/metrics/ascender
	   #:ft-sizerec/metrics/descender
	   #:ft-sizerec/metrics/height
	   #:ft-sizerec/metrics/max-advance
	   
	   #:ft-charmaprec
	   #:ft-charmaprec/face
	   #:ft-charmaprec/encoding
	   #:ft-charmaprec/platform-id
	   #:ft-charmaprec/encoding-id

	   #:ft-bitmap
	   #:ft-bitmap/n-contours
	   #:ft-bitmap/contours
	   #:ft-bitmap/n-points
	   #:ft-bitmap/tags
	   #:ft-bitmap/points
	   #:ft-bitmap/flags

	   #:ft-facerec/num-faces
	   #:ft-facerec/face-index
	   #:ft-facerec/face-flags
	   #:ft-facerec/style-flags
	   #:ft-facerec/num-glyphs
	   #:ft-facerec/family-name
	   #:ft-facerec/style-name
	   #:ft-facerec/num-charmaps
	   #:ft-facerec/charmaps
	   #:ft-facerec/units-per-em
	   #:ft-facerec/ascender
	   #:ft-facerec/descender
	   #:ft-facerec/height
	   #:ft-facerec/max-advance-width
	   #:ft-facerec/max-advance-height
	   #:ft-facerec/bbox/ymin
	   #:ft-facerec/bbox/ymax
	   #:ft-facerec/bbox/xmin
	   #:ft-facerec/bbox/xmax
	   #:ft-facerec/size
	   #:ft-facerec/charmap

	   #:ft-glyphslotrec/metrics/width
	   #:ft-glyphslotrec/metrics/height
	   #:ft-glyphslotrec/metrics/vert-advance
	   #:ft-glyphslotrec/metrics/vert-bearing/y
	   #:ft-glyphslotrec/metrics/vert-bearing/x
	   #:ft-glyphslotrec/metrics/hori-advance
	   #:ft-glyphslotrec/metrics/hori-bearing/y
	   #:ft-glyphslotrec/metrics/hori-bearing/x
	   #:ft-glyphslotrec/advance/x
	   #:ft-glyphslotrec/advance/y
	   #:ft-glyphslotrec/format
	   #:ft-glyphslotrec/bitmap-top
	   #:ft-glyphslotrec/bitmap-left
	   #:ft-glyphslotrec/bitmap/rows
	   #:ft-glyphslotrec/bitmap/width
	   #:ft-glyphslotrec/bitmap/pitch
	   #:ft-glyphslotrec/bitmap/pixel-mode
	   #:ft-glyphslotrec/bitmap/buffer
	   #:ft-glyphslotrec/outline/flags
	   #:ft-glyphslotrec/outline/tags
	   #:ft-glyphslotrec/outline/n-points
	   #:ft-glyphslotrec/outline/points
	   #:ft-glyphslotrec/outline/n-contours
	   #:ft-glyphslotrec/outline/contours

	   ))

(in-package :cl-freetype)


(defun freetype-dynamic-lib-path ()
  (or #+(or mswindows win32 win64) #p"libfreetype-6.dll"
    #+(or darwin macosx) #p"/usr/X11R6/lib/libfreetype.dylib"))

#+xxx
(load "libfreetype-6.dll")

;;;(define-foreign-library FreeType
;;;    (:darwin (:framework "FreeType"))
;;;  (:windows (:or "freetype6.dll"))
;;;  (:unix "libtcl.so")
;;;  (t (:default "libtcl")))

(defparameter *default-face* :courier)

(defparameter *face-registry*
  #+(or win32 mswindows)
  '((:helvetica :file #p"/windows/fonts/arial.ttf")
    (:helvetica-bold :file #p"/windows/fonts/arialbd.ttf")
    (:helvetica-italic :file #p"/windows/fonts/ariali.ttf")
    (:helvetica-bold-italic :file #p"/windows/fonts/arialbi.ttf")
    (:courier :file #p"/windows/fonts/cour.ttf")
    (:courier-bold :file #p"/windows/fonts/courbd.ttf")
    (:courier-italic :file #p"/windows/fonts/couri.ttf")
    (:courier-bold-italic :file #p"/windows/fonts/courbi.ttf")
    (:times :file #p"/windows/fonts/times.ttf")
    (:times-bold :file #p"/windows/fonts/timesbd.ttf")
    (:times-italic :file #p"/windows/fonts/timesi.ttf")
    (:times-bold-italic :file #p"/windows/fonts/timesbi.ttf")
    (:symbol :file #p"/windows/fonts/symbol.ttf")
    )

  #+(or darwin macosx)
  '((:helvetica :rsrc #p"/System/Library/Fonts/Helvetica.dfont" "Helvetica")
    (:helvetica-bold :rsrc #p"/System/Library/Fonts/Helvetica.dfont" "Helvetica Bold")
    (:courier :rsrc #p"/System/Library/Fonts/Courier.dfont" "Courier")
    (:courier-bold :rsrc #p"/System/Library/Fonts/Courier.dfont" "Courier Bold")
    (:times :rsrc #p"/System/Library/Fonts/Times.dfont" "Times")
    (:times-bold :rsrc #p"/System/Library/Fonts/Times.dfont" "Times Bold")
    (:times-italic :rsrc #p"/System/Library/Fonts/Times.dfont" "Times Italic")
    (:times-bold-italic :rsrc #p"/System/Library/Fonts/Times.dfont" "Times Bold Italic")
    (:symbol :rsrc #p"/System/Library/Fonts/Symbol.dfont" "Symbol")
    (:zapf-dingbats :rsrc #P"/System/Library/Fonts/ZapfDingbats.dfont" "Zapf Dingbats")
    )
  )

; strint32:
;   Converts 4-char string into 32bit int.
;   ex. (strint32 "abcd") -> ((('a'*256)+'b')*256)+'c')*256+'d'
(defmacro strint32 (str)
  (assert (= (length str) 4))
  (reduce (lambda (r c) (logior (* 256 r) (char-code c)))
	  str :initial-value 0))

; def-struct-rec:
;
;   This allows you to define a struct within another struct:
;     (def-struct-rec :point (x :int) (y :int))
;     (def-struct-rec :rect (topleft :point) (bottomright :point))
;
;   You can access fields as follows:
;     (point/x point1)
;     (rect/topleft/x rect1)
;
;   NOTICE: you should use this macro for *all* structs
;           which can be recursively included in other structs.
;
(eval-when (compile load eval)
  (defvar *recursive-structs* nil))
(defmacro def-struct-rec (typename &rest decls)
  (declare (special *recursive-structs*))
  (labels ((expand1 (prefix decl)
	     (let* ((fname (if prefix 
			       (intern (concatenate 'string 
						    (symbol-name prefix) "/"
						    (symbol-name (car decl))))
			       (car decl)))
		    (ftype (cadr decl))
		    (struct1 (assoc ftype *recursive-structs*)))
	       (if struct1
		   (apply 'append (mapcar (lambda (d) (expand1 fname d)) (cdr struct1)))
		   (list (list fname ftype))))))
    (let* ((expanded 
	    (apply 'append
		   (mapcar (lambda (d) (expand1 nil d)) decls)))
	   (accessors
	    (apply 'append
		   (mapcar (lambda (d)
			     (let* ((slotname (car d))
				    (funcname (intern
					       (concatenate 'string
							    (string-left-trim ":" (symbol-name typename)) "/" 
							    (symbol-name slotname))))
				    )
			       `((defun ,funcname (struct) 
				   (get-slot-value struct (quote ,typename) (quote ,slotname)))
				 (defun (setf ,funcname) (value struct)
				   (setf (get-slot-value struct (quote ,typename) (quote ,slotname)) value))
				 )
			       ))
			   expanded)))
	   )
    (push (cons typename expanded) *recursive-structs*)
    `(progn (def-struct ,typename ,@expanded)
	    ,@accessors))))


;  Utility to carry lisp objects within callbacks.
;
;  usage:
;    (ff-defun-callable :cdecl :void mycallback ((* :void) mydata)
;      (get-lisp-object mydata))
;
;    (with-lisp-pointer (mydata (make-my-lisp-object))
;      (register-callback (ff-register-callable 'mycallback))
;      (some-foreign-function mydata)
;    )
;
(defvar *working-objects* (make-hash-table))

(defun get-lisp-object (objid)
  (gethash (pointer-address objid) *working-objects*))

(defun deregister-lisp-object (objid)
  (remhash (pointer-address objid) *working-objects*))

(defun register-lisp-object (objid object)
  (assert (not (gethash (pointer-address objid) *working-objects*)))
  (setf (gethash (pointer-address objid) *working-objects*) object))

(defmacro with-lisp-pointer ((var form) &body body)
  `(with-foreign-object (,var :int)
     (register-lisp-object ,var ,form)
     ,@body
     (deregister-lisp-object ,var))
  )


;;  Face manager
;;
(defun get-face-spec (face-name)
  (let ((p (or (assoc face-name *face-registry*)
	       (assoc *default-face* *face-registry*))))
    (format t "get-face-spec: ~a~%" p)
    (when (not p)
      (error "Face not found: ~a" face-name))
    (case (cadr p)
      (:file
       (list :face-file (caddr p)))
      (:rsrc
       (destructuring-bind (rsrc-path rsrc-name) (cddr p)
	 (list :face-data (with-rsrc-fork (resfork rsrc-path)
			    (or (get-resource-by-name resfork "sfnt" rsrc-name)
				(error "Not found: ~a in ~a" rsrc-name rsrc-path))))))
      (else
       (error "Illegal face-spec: ~a" p))
      )))

(defun get-face-by-spec (&key face-file face-data (face-index 0))
  (assert (or face-file face-data))
  (if face-file
      (get-new-face face-file face-index)
      (get-new-memory-face face-data face-index)))

(defun get-face (face-name)
  (apply #'get-face-by-spec (get-face-spec face-name)))


;;  utility macros
;;
(defmacro with-ft-face ((var face-file) &body body)
  `(let ((,var (get-new-face ,face-file)))
     (prog1 (progn ,@body)
       (done-face ,var)))
  )

(defmacro with-ft-memory-face ((var face-data) &body body)
  (let ((buf (gensym)))
    `(multiple-value-bind (,var ,buf) (get-new-memory-face ,face-data)
       (prog1 (progn ,@body)
	 (done-face ,var)
	 (free-foreign-object ,buf)))
    ))
