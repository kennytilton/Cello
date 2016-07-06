(defpackage #:cffi-extender
  (:nicknames #:ffx)
  #+hunh? (:shadowing-import-from #:cffi #:with-foreign-object
    #:load-foreign-library #:with-foreign-string)
  (:use #:common-lisp #:cffi))

