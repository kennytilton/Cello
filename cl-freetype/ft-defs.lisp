(in-package :cl-freetype)

;;;  Definitions from Freetype headers
;;;

;;  Basic data types
;;
(def-foreign-type ft-int :int)		; FT_Int
(def-foreign-type ft-int32 :int)	; FT_Int32 (32bits)
(def-foreign-type ft-uint :unsigned-int) ; FT_UInt
(def-foreign-type ft-short :short)	 ; FT_UShort
(def-foreign-type ft-ushort :unsigned-short) ; FT_UShort
(def-foreign-type ft-long :long)	     ; FT_Long
(def-foreign-type ft-ulong :unsigned-long)   ; FT_ULong

(def-foreign-type ft-fixed :long)	; FT_Fixed
(def-foreign-type ft-pos :long)		; FT_Pos
(def-foreign-type ft-f26dot6 :long)	; FT_F26Dot6

; actually enum type, but define as int here.
(def-foreign-type ft-glyph-format :int) ; FT_Glyph_Format
(def-foreign-type ft-encoding :int)	 ; FT_Encoding
(def-foreign-type ft-render-mode :int)	 ; FT_Render_Mode

(def-foreign-type ft-error :int)	; FT_Error
(def-foreign-type ft-string :cstring)	; FT_String

(def-foreign-type ft-library :pointer-void) ; FT_Library (freetype.h)
(def-foreign-type ft-subglyph :pointer-void) ; FT_Subglyph (freetype.h)
(def-foreign-type ft-size-internal :pointer-void) ; FT_Size_Internal (freetype.h)
(def-foreign-type ft-generic-finalizer :pointer-void) ; FT_Generic_Finalizer
(def-foreign-type ft-face (* ft-facerec)) ; FT_Face (freetype.h)
(def-foreign-type ft-size (* ft-sizerec)) ; FT_Size (freetype.h)
(def-foreign-type ft-charmap (* ft-charmaprec)) ; FT_Charmap (freetype.h)
(def-foreign-type ft-glyphslot (* ft-glyphslotrec)) ; FT_GlyphSlot (freetype.h)
(def-foreign-type ft-array-vector (* ft-vector))
(def-foreign-type ft-array-bitmap-size (* ft-bitmap-size))
(def-foreign-type ft-array-charmap (* ft-charmap))
;(def-foreign-type ft-outline-p (* ft-outline)) ; FT_Outline*

; enum FT_Glyph_Format
(defconstant ft-glyph-format/none 0)
(defconstant ft-glyph-format/composite (strint32 "comp"))
(defconstant ft-glyph-format/bitmap (strint32 "bits"))
(defconstant ft-glyph-format/outline (strint32 "outl"))
(defconstant ft-glyph-format/plotter (strint32 "plot"))

; enum FT_Encoding
(defconstant ft-encoding/none 0)
(defconstant ft-encoding/ms-symbol (strint32 "symb"))
(defconstant ft-encoding/unicode (strint32 "unic"))
(defconstant ft-encoding/sjis (strint32 "sjis"))
(defconstant ft-encoding/gb2312 (strint32 "gb  "))
(defconstant ft-encoding/big5 (strint32 "big5"))
(defconstant ft-encoding/wansung (strint32 "wans"))
(defconstant ft-encoding/johab (strint32 "joha"))
(defconstant ft-encoding/ms-sjis ft-encoding/sjis)
(defconstant ft-encoding/ms-gb2312 ft-encoding/gb2312)
(defconstant ft-encoding/ms-big5 ft-encoding/big5)
(defconstant ft-encoding/ms-wansung ft-encoding/wansung)
(defconstant ft-encoding/ms-johab ft-encoding/johab)
(defconstant ft-encoding/adobe-standard (strint32 "ADOB"))
(defconstant ft-encoding/adobe-expert (strint32 "ADBE"))
(defconstant ft-encoding/adobe-custom (strint32 "ADBC"))
(defconstant ft-encoding/adobe-latin-1 (strint32 "lat1"))
(defconstant ft-encoding/old-latin-2 (strint32 "lat2"))
(defconstant ft-encoding/apple-roman (strint32 "armn"))

; enum FT_Render_Mode
; FreeType 2.1
(defconstant ft-render-mode/normal 0)
;;;(defconstant ft-render-mode/mono 1)
; FreeType 2.3
; OMG, these constants are different in different FT versions!
(defconstant ft-render-mode/light 1)
(defconstant ft-render-mode/mono 2)
(defconstant ft-render-mode/lcd 3)
(defconstant ft-render-mode/lcd-v 4)
(defconstant ft-render-mode/max 5)

; const FT_Pixel_Mode
; FreeType 2.1
(defconstant ft-pixel-mode/none 0)
(defconstant ft-pixel-mode/mono 1)
(defconstant ft-pixel-mode/grays 2)
; FreeType 2.3
(defconstant ft-pixel-mode/gray2 3)
(defconstant ft-pixel-mode/gray4 4)
(defconstant ft-pixel-mode/lcd 5)
(defconstant ft-pixel-mode/lcd-v 6)

; const FT_OUTLINE_*
(defconstant ft-outline/none 0)
(defconstant ft-outline/owner #x01)
(defconstant ft-outline/even-odd-fill #x02)
(defconstant ft-outline/reverse-fill #x04)
(defconstant ft-outline/ignore-dropouts #x08)

; const FT_CURVE_TAG_*
(defun ft-get-tag (x) (logand x 3))
(defconstant ft-curve-tag/on 1)
(defconstant ft-curve-tag/conic 0)
(defconstant ft-curve-tag/cubic 2)

; const FT_KERNING_*
(defconstant ft-kerning/default 0)
(defconstant ft-kerning/unfitted 1)
(defconstant ft-kerning/unscaled 2)

; const FT_FACE_FLAG_*
(defconstant ft-face-flag/scalable #x01)
(defconstant ft-face-flag/fixed-sizes #x02)
(defconstant ft-face-flag/fixed-width #x04)
(defconstant ft-face-flag/sfnt #x08)
(defconstant ft-face-flag/horizontal #x10)
(defconstant ft-face-flag/vertical #x20)
(defconstant ft-face-flag/kerning #x40)
(defconstant ft-face-flag/fast-glyphs #x80)
(defconstant ft-face-flag/multiple-masters #x100)
(defconstant ft-face-flag/glyph-names #x200)
(defconstant ft-face-flag/external-stream #x400)

; const FT_LOAD_*
(defconstant ft-load/default 0)
(defconstant ft-load/no-scale #x1)
(defconstant ft-load/no-hinting #x2)
(defconstant ft-load/render #x4)
(defconstant ft-load/no-bitmap #x8)
(defconstant ft-load/vertical-layout #x10)
(defconstant ft-load/force-autohint #x20)
(defconstant ft-load/crop-bitmap #x40)
(defconstant ft-load/pedantic #x80)
(defconstant ft-load/ignore-global-advance-width #x200)
(defconstant ft-load/no-recurse #x400)
(defconstant ft-load/ignore-transform #x800)
(defconstant ft-load/monochrome #x1000)
(defconstant ft-load/linear-design #x2000)
(defconstant ft-load/sbits-only #x4000)
(defconstant ft-load/no-autohint #x8000)


;;  Structures
;;

; FT_Vector (ftimage.h)
(def-struct-rec ft-vector
    (x ft-pos)
    (y ft-pos)
)

; FT_BBox (ftimage.h)
(def-struct-rec ft-bbox
    (xmin ft-pos)
    (ymin ft-pos)
    (xmax ft-pos)
    (ymax ft-pos)
)

; FT_Generic (fttypes.h)
(def-struct-rec ft-generic
    (data :pointer-void)
    (finalizer ft-generic-finalizer)
)

; FT_Bitmap_Size (freetype.h)
(def-struct-rec ft-bitmap-size
    (height ft-short)
    (width ft-short)
    (size ft-pos)
    (x-ppem ft-pos)
    (y-ppem ft-pos)
)

; FT_Glyph_Metrics (freetype.h)
(def-struct-rec ft-glyph-metrics
    (width ft-pos)
    (height ft-pos)
    (hori-bearing ft-vector)
    (hori-advance ft-pos)
    (vert-bearing ft-vector)
    (vert-advance ft-pos)
)

; FT_Size_Metrics (freetype.h)
(def-struct-rec ft-size-metrics
    (x-ppem ft-ushort)
    (y-ppem ft-ushort)
    
    (x-scale ft-fixed)
    (y-scale ft-fixed)
    
    (ascender ft-pos)
    (descender ft-pos)
    (height ft-pos)
    (max-advance ft-pos)
)

; FT_SizeRec (freetype.h)
(def-struct-rec ft-sizerec
    (face ft-face)
    (generic ft-generic)
    (metrics ft-size-metrics)
    (internal ft-size-internal)
)

; FT_CharMapRec (freetype.h)
(def-struct-rec ft-charmaprec
    (face ft-face)
    (encoding ft-encoding)
    (platform-id ft-ushort)
    (encoding-id ft-ushort)
)

; FT_Bitmap
(def-struct-rec ft-bitmap
    (rows :int)
    (width :int)
    (pitch :int)
    (buffer :pointer-void)
    (num-grays :short)
    (pixel-mode :byte)
    (palette-mode :byte)
    (palette :pointer-void)
)

; FT_Outline (ftimage.h)
(def-struct-rec ft-outline
    (n-contours :short)
    (n-points :short)
    (points ft-array-vector)
    (tags (* :byte))
    (contours (* :short))
    (flags :int)
)

; FT_GlyphShotRec (freetype.h)
(def-struct-rec ft-glyphslotrec
    (library ft-library)
    (face ft-face)
    (next ft-glyphslot)
    (reserved ft-uint)
    (generic ft-generic)

    (metrics ft-glyph-metrics)
    (linear-hori-advance ft-fixed)
    (linear-vert-advance ft-fixed)
    (advance ft-vector)
    
    (format ft-glyph-format)
    (bitmap ft-bitmap)
    (bitmap-left ft-int)
    (bitmap-top ft-int)
    (outline ft-outline)
    
    (num-subglyphs ft-uint)
    (subglyphs ft-subglyph)

    ; some private data ...
)

; FT_Face (freetype.h)
(def-struct-rec ft-facerec
    (num-faces ft-long)
    (face-index ft-long)
    (face-flags ft-long)
    (style-flags ft-long)
    (num-glyphs ft-long)

    (family-name ft-string)
    (style-name ft-string)

    (num-fixed-sizes ft-int)
    (available-sizes ft-array-bitmap-size)

    (num-charmaps ft-int)
    (charmaps ft-array-charmap)
    (generic ft-generic)

    ; the following are only relevant to scalable outlines.
    (bbox ft-bbox)
    (units-per-em ft-ushort)
    (ascender ft-short)
    (descender ft-short)
    (height ft-short)

    (max-advance-width ft-short)
    (max-advance-height ft-short)
    (underline-position ft-short)
    (underline-thickness ft-short)

    (glyph ft-glyphslot)
    (size ft-size)
    (charmap ft-charmap)

    ; some private data ...
)

(defun ft-is-scalable (face) (/= 0 (logand ft-face-flag/scalable (ft-facerec/face-flags face))))
(defun ft-has-horizontal (face) (/= 0 (logand ft-face-flag/horizontal (ft-facerec/face-flags face))))
(defun ft-has-vertical (face) (/= 0 (logand ft-face-flag/vertical (ft-facerec/face-flags face))))
(defun ft-has-kerning (face) (/= 0 (logand ft-face-flag/kerning (ft-facerec/face-flags face))))
(defun from-ft (x) (/ x 64.0))
(defun to-ft (x) (round (* x 64)))

;;;  Lower-level routines
;;;

;;  APIs and wrapper functions
;;
(def-function "FT_Init_FreeType"
    ((alibraryp :pointer-void))
  :returning ft-error)

(def-function "FT_Done_FreeType"
    ((library ft-library))
  :returning ft-error)

(def-function "FT_Library_Version"
    ((library ft-library) 
     (amajor :pointer-void)
     (aminor :pointer-void)
     (apatch :pointer-void))
 :returning :void)

(def-function "FT_New_Face" 
    ((library ft-library) 
     (pathname :cstring)
     (face_index ft-long) 
     (aface :pointer-void))
  :returning ft-error)

(def-function "FT_New_Memory_Face"
    ((library ft-library) 
     (file_base :pointer-void)
     (file_size ft-long)
     (face_index ft-long) 
     (aface :pointer-void))
  :returning ft-error)

(def-function "FT_Done_Face" 
    ((face ft-face))
  :returning ft-error)

(def-function "FT_Set_Char_Size"
    ((face ft-face)
     (char_width ft-f26dot6)
     (char_height ft-f26dot6)
     (horz_resolution ft-uint)
     (vert_resolution ft-uint))
  :returning ft-error)

(def-function "FT_Set_Pixel_Sizes"
    ((face ft-face)
     (width ft-uint)
     (height ft-uint))
  :returning ft-error)

(def-function "FT_Set_Charmap"
    ((face ft-face)
     (charmap ft-charmap))
  :returning ft-error)
  
(def-function "FT_Select_Charmap"
    ((face ft-face)
     (encoding ft-encoding))
  :returning ft-error)

(def-function "FT_Get_Char_Index"
    ((face ft-face)
     (charcode ft-ulong))
  :returning ft-uint)

(def-function "FT_Get_Kerning"
    ((face ft-face)
     (left_glyph ft-uint)
     (right_glyph ft-uint)
     (kern_mode ft-uint)
     (akerning :pointer-void))
  :returning ft-uint)

(def-function "FT_Load_Glyph" 
    ((face ft-face) 
     (glyph_index ft-ulong)
     (load_flags ft-int32))
  :returning ft-error)

(def-function "FT_Load_Char" 
    ((face ft-face) 
     (char_code ft-ulong)
     (load_flags ft-int32))
  :returning ft-error)

;(def-function "FT_Outline_Get_CBox"
;    ((outline ft-outline-p)
;     (acbox :pointer-void))
;  :returning :void)

(def-function "FT_Render_Glyph"
    ((slot ft-glyphslot)
     (render_mode ft-render-mode))
  :returning ft-error)

(def-function "FT_Get_First_Char"
    ((face ft-face)
     (agindex :pointer-void))
  :returning ft-ulong)

(def-function "FT_Get_Next_Char"
    ((face ft-face)
     (char_code ft-ulong)
     (agindex :pointer-void))
  :returning ft-ulong)

(def-function "FT_Get_Postscript_Name"
    ((face ft-face))
  :returning :pointer-void)

