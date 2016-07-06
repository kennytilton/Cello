(use-package :mop)

(defclass my-generic-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defmethod compute-applicable-methods-using-classes ((gf my-generic-function) classes)
  (warn "hala")
  (values nil nil))

(defmethod compute-discriminating-function ((gf my-generic-function))
  (let* ((default-discriminating-function (call-next-method)))
    (lambda (a)
      (let ((*test* 15))
        (declare (special *test*))
        (funcall default-discriminating-function a)))))

(defgeneric test (a)
  (:generic-function-class my-generic-function))

(defmethod test (a)
  (declare (special *test*))
  (+ a *test*)) 

(test 42)