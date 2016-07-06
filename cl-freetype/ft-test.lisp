(in-package :cl-freetype)

;;
;; I moved the other test stuff into a separate area so that Glut was not assumed. kt 5/18/06
;;

(defparameter *myfont* :times)


(defun ft-test ()
  (initialize-ft)
  (let ((face (get-new-face "\\StuckOnAlgebra\\font\\math2___.ttf"
                #+not "\\windows\\fonts\\math2___.ttf" #+not "Times New Roman")))
    (set-char-size face (to-ft 24) 96)
    (loop ; for x below 4
      for c across "_|"
          do (multiple-value-bind (w h hbx hby adv vbx vby v-adv)
                 (glyph-metrics face c #+not (code-char (+ x 160)) 3)
               (print (list :w (from-ft w ) :h (from-ft h) :hbx (from-ft hbx) :hby (from-ft hby)
                        :wid (from-ft w) :ht (from-ft h) :adv (from-ft adv)
                        :vbx (from-ft vbx) :vby (from-ft vby) :v-adv (from-ft v-adv)))))
    #+not
    (let ((font1 (ftgl-get-font :outline *myfont* :font-size 36)))
      (format t "font=~a~%" font1)
      (format t "advance=~a~%" (font-advance font1 str))
      (format t "bbox=~a~%" (font-bbox font1 str))
      (dolist (c (coerce str 'list))
        (format t "glyph-index(~a)=~a~%" c (get-index-by-char font1 c)))
      )))

#+test-ft
(ft::ft-test "pix")


#+test
(defparameter *ftface*
  (get-new-face "\\windows\\fonts\\math2___.ttf"))

;;; kenny stuff

(eval-when (compile load eval)
  (EXPORT '(make-face-size glyph-metrics)))

#+test
(get-char-index (get-new-face "\\windows\\fonts\\Times.ttf") (char-code #\m))

#+test
(ft-has-kerning (get-new-face "\\windows\\fonts\\Times.ttf"))

#+test
(progn
  (initialize-ft)
  (let ((face #+not *ftface* (get-new-face "\\windows\\fonts\\L_10646.ttf"
                               #+not "\\StuckOnAlgebra\\application\\font\\math2___.ttf"
                               #+not "\\windows\\fonts\\Times.ttf")))
    (set-char-size face (to-ft 24) 96)
    (loop for acc in '(ft-facerec/units-per-em
                       ft-facerec/ascender
                       ft-facerec/descender
                       ft-facerec/height
                       ft-facerec/max-advance-width
                       ft-facerec/max-advance-height
                       ft-facerec/bbox/ymin
                       ft-facerec/bbox/ymax
                       ft-facerec/bbox/xmin
                       ft-facerec/bbox/xmax)
        do (print (list acc (from-ft (funcall acc face)))))
    (loop for c in (mapcar 'code-char (list 34 35 36 37 38 39 40 41 42 43 44 45 46)) ; #\") ;;across "10mi"
        do
          (multiple-value-bind (w h hbx hby adv vbx vby v-adv)
              (glyph-metrics face c 3)
            (print (list :w (from-ft w ) :h (from-ft h) :hbx (from-ft hbx) :hby (from-ft hby)
                     :wid (from-ft w) :ht (from-ft h) :adv (from-ft adv)
                     :vbx (from-ft vbx) :vby (from-ft vby) :v-adv (from-ft v-adv)))))))

#+weird
(progn
  (initialize-ft)
  (let ((face #+bzzt  (get-new-face "\\windows\\fonts\\Times.ttf")
          (get-new-face "\\StuckOnAlgebra\\application\\font\\math2___.ttf")))
    (set-char-size face (to-ft 24) 96)
    (print `(numfac ,(ft::ft-facerec/num-faces face)))
    (print `( "gcis!!!!!!!!!!!" ,(delete-duplicates
                       (loop for cc below 1024
                           collecting (ft:get-char-index face cc)))))))

;;;(defun make-face-size (font-file-path font-size)
;;;  (let ((face (get-new-face "\\windows\\fonts\\Times.ttf")))
;;;    (set-char-size face (to-ft 24) 96)
;;;    face))

;;ft-load/no-bitmap

(defun glyph-metrics (face char flags)
  "(GLYPH-METRICS face glyph-index flags) -> (values width height h-bearing-x h-bearing-y h-advance v-bearing-x v-bearing-y v-advance)
where: face is returned by (ft:get-new-face <font path>)
       char is a Lisp character"
  (let* ((gci (get-char-index face (char-code char)))
         (glyph (load-glyph face gci flags)))
    ;(print `(gci ,gci char ,char cc ,(char-code char)))
    (values (ft-glyphslotrec/metrics/width glyph)
      (ft-glyphslotrec/metrics/height glyph)
      (ft-glyphslotrec/metrics/hori-bearing/x glyph)
      (ft-glyphslotrec/metrics/hori-bearing/y glyph)
      (ft-glyphslotrec/metrics/hori-advance glyph)
      (ft-glyphslotrec/metrics/vert-bearing/x glyph)
      (ft-glyphslotrec/metrics/vert-bearing/y glyph)
      (ft-glyphslotrec/metrics/vert-advance glyph))))

(defun glyph-bearing-v-y (face char-code flags)
  (let ((glyph (load-glyph face (get-char-index face char-code) flags)))
    (ft-glyphslotrec/metrics/vert-bearing/y glyph)))


(defparameter *mfroot* nil #+test(get-new-face "/0Algebra/TY Extender/font/math2___.ttf"))

(defun pft (n)
  (loop repeat n do
        (glyph-bearing-v-y *mfroot* 44 3)))

#+test
(time (pft 10000))

#+test
(progn
  ;(initialize-ft)
  (let* ((face (get-new-face "\\windows\\fonts\\Times.ttf"))
         (size (ft-facerec/size face)))
    (print `(pre-ascender ,(ft-facerec/ascender face) ,(ft-facerec/units-per-em face)))
    (loop for acc in '(ft-facerec/units-per-em
                       ft-facerec/ascender
                       ft-facerec/descender
                       ft-facerec/height
                       ft-facerec/max-advance-width
                       ft-facerec/max-advance-height
                       ft-facerec/bbox/ymin
                       ft-facerec/bbox/ymax
                       ft-facerec/bbox/xmin
                       ft-facerec/bbox/xmax)
        do (print (list acc (from-ft (funcall acc face)))))
    (loop for sz in '(24)
        do (set-char-size face (to-ft sz) 96)
          (print `(sized-ascender ,sz , (to-ft sz) ,(from-ft (ft-facerec/ascender face))))
          (loop for (k m) on (list :x-ppem (ft-sizerec/metrics/x-ppem size)
                               :y-ppem (ft-sizerec/metrics/y-ppem size)
                               :x-scale (ft-sizerec/metrics/x-scale size)
                               :y-scale (ft-sizerec/metrics/y-scale size)
                               :ascender (ft-sizerec/metrics/ascender size)
                               :descender (ft-sizerec/metrics/descender size)
                               :height (ft-sizerec/metrics/height size)
                               :max-advance (ft-sizerec/metrics/max-advance size)) by #'cddr
              do (print (list k (from-ft m))))
          (ft-facerec/ascender face))))

#+test
(progn
  (initialize-ft)
  (let ((face (get-new-face "\\windows\\fonts\\Times.ttf")))
    (glyph-bbox face #\h 3)))

(defstruct ft-font
  path
  (size 12)
  res
  face
  ascender
  descender
  leading
  char-metrics)

;;;(defun load-ft-font (path &optional (size 12) (res 96))
;;;  (let ((f (make-ft-font :path path :size size)))
;;;    (set-char-size f (to-ft

(defun glyph-bbox (face char flags)
  "(GLYPH-METRICS face glyph-index flags) -> (values width height h-bearing-x h-bearing-y h-advance v-bearing-x v-bearing-y :v-advance)
where: face is returned by (ft:get-new-face <font path>)
       char is a Lisp character"
  (let ((glyph (load-glyph face (get-char-index face (char-code char)) flags)))
    (values (- (ft-glyphslotrec/metrics/hori-bearing/x glyph))
      (ft-glyphslotrec/metrics/height glyph)
      (ft-glyphslotrec/metrics/width glyph)
      (ft-glyphslotrec/metrics/vert-bearing/y glyph))))

