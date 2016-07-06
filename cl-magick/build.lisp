(in-package :cl-user)


#-allegro-ide
(let ((drive "C")
      (d-force nil))
  (build-sys d-force drive "dvx" "uffi")
  (build-sys d-force drive "dvx" "ffi-extender")
  (build-sys d-force drive "dvx" "cl-opengl")
  (load (dev-root "cl-ftgl" "cl-ftgl.lisp"))
  (build-sys d-force drive "dvx" "cl-magick")
  ; (cl-magick::cl-magick-test)
  )

#+test
(cl-magick::cl-magick-test)

(in-package :cl-user)


#-allegro-ide
(let ((drive "C")
      (d-force nil))
  (build-sys d-force drive "dvx" "uffi")
  (build-sys d-force drive "dvx" "ffi-extender")
  (build-sys d-force drive "dvx" "cl-opengl")
  (load (dev-root "cl-ftgl" "cl-ftgl.lisp"))
  (build-sys d-force drive "dvx" "cl-magick")
  ; (cl-magick::cl-magick-test)
  )

#+test
(cl-magick::cl-magick-test)
