;;;
;;; Copyright (c) 1995,2003 by Kenneth William Tilton.
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


(in-package :ffx)


#+precffi
(defun ff-register-callable (callback-name)
  #+allegro
  (ff:register-foreign-callable callback-name)
  #+lispworks
  (let ((cb (progn ;; fli:pointer-address
              (fli:make-pointer :symbol-name (symbol-name callback-name) ;; leak?
                :functionp t))))
    (print (list :ff-register-callable-returns cb))
    cb))

(defun ff-register-callable (callback-name)
  (let ((known-callback (cffi:get-callback callback-name)))
    (assert known-callback)
    known-callback))

(defmacro ff-defun-callable (call-convention result-type name args &body body)
  (declare (ignorable call-convention))
  `(defcallback ,name ,result-type ,args ,@body))

#+precffi
(defmacro ff-defun-callable (call-convention result-type name args &body body)
  (declare (ignorable call-convention result-type))
  (let ((native-args (when args ;; without this p-f-a returns '(:void) as if for declare
                       (process-function-args args))))
    #+lispworks
    `(fli:define-foreign-callable
      (,(symbol-name name) :result-type ,result-type :calling-convention ,call-convention)
      (,@native-args)
      ,@body)
    #+allegro
    `(ff:defun-foreign-callable ,name ,native-args
       (declare (:convention ,(ecase call-convention
                                (:cdecl :c)
                                (:stdcall :stdcall))))
       ,@body)))


#+(or)
(ff-defun-callable :cdecl :int square ((arg-1 :int)(data :pointer))
  (list data (* arg-1 arg-1)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(ff-register-callable
            ff-defun-callable
            ff-pointer-address)))