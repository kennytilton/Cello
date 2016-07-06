(push (make-pathname :directory '(:absolute "1-devtools" "parse-number"))
  asdf:*central-registry*)

(asdf:operate 'asdf:load-op 'parse-number)

(push (make-pathname :directory '(:absolute "1-devtools" "cffi"))
  asdf:*central-registry*)

(asdf:operate 'asdf:load-op 'cffi)

(push (make-pathname :directory '(:absolute "1-devtools" "split-sequence"))
  asdf:*central-registry*)

(push (make-pathname :directory '(:absolute "1-devtools" "s-xml"))
  asdf:*central-registry*)

(push (make-pathname :directory '(:absolute "1-devtools" "verrazano"))
  asdf:*central-registry*)

(push (make-pathname :directory '(:absolute "1-devtools" "verrazano-support"))
  asdf:*central-registry*)

;;; (pushnew "c:/1-devtools/verrazano/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op 'verrazano)