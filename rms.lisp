
;;;
;;; Copyright (c) 2007 by Kenneth William Tilton.
;;;
;;;       All rights reserved.
;;;


(in-package :cello)

(export! rms-system rms-clear rms-dump rms-get rms-get-maybe rms-get-all rms-put rms-update rms-get-generic with-rms-dependency
  rms-bucket db-available rms-shutdown)

(pushnew :rms-s3 *features*)

;;; --- runtime initialization -----------------

(defmd rms-bucket ()
  (s3-bucket nil :cell nil)
  (db-watch (c-in nil) :cell :ephemeral)
  (db-pulse (c-in 0)))

(defun rms-shutdown ()
  (bwhen (sf (rms-store *sys*))
    (trc "closing store-forward!!!!!!!")
    (close sf)))

(defmd rms-system () ;; mixin, do not inherit form cello-system
  (rms-store nil :cell nil :documentation "file pointer for store-forward when net unavailable")
  (rms-buckets nil :owning t))

(defun db-available (&optional (system *sys*))
  (not (rms-store system)))

#+adhoc
(rms-reset)

(define-symbol-macro .rms-down (rms-store *sys*))
(export! .rms-down)

(define-condition trial-period-expired ()
  ())

#+testofff
(cl-s3:put-object "tyl.version" "1" "ok" "text/plain")

#+testoff
(cl-s3:put-object "tyl.version" "1" "expired" "text/plain")

(defun rms-reset ()
  (assert *sys* () "The RMS System requires first an instance of rms-system be bound to *sys*")
  (assert (typep *sys* 'rms-system))
  (handler-case
      (progn
        ;(signal 'cl-s3:amazon-s3-api-error)
        
        (trc "rms-reset attempts version get")
        (let ((d (cl-s3:get-object "tyl.version" "1")))
          (trc "tylversion" d)
          
          (if (string= d "expired")
              (error 'trial-period-expired)
            (unless (string= "ok" d)
              (print :S3-not-available-x)
              (setf (rms-store *sys*) (open (conc$ "tya-nos3" #+not (get-universal-time) ".txt")
                                        :if-does-not-exist :create
                                        :if-exists :append ;; :supersede
                                        :direction :io))))))
    (cl-s3:amazon-s3-api-error (c)
      (declare (ignorable c))
      (print :S3-not-available-a)
      (setf (rms-store *sys*) (open (conc$ "tya-nos3" #+not (get-universal-time) ".txt")
                                :if-does-not-exist :create
                                :if-exists :append ;; :supersede
                                :direction :io))
      (return-from rms-reset))
    (trial-period-expired (c)
      (declare (ignorable c))
      (trc "rms-reset trial-period-expired  ")
      (alert-ok-ex "Free Trial Ended" "The free trial period for Stuck On Algebra
has ended. Please download a new version from
http://www.stuckonalgbra.com" "Ouch!")
      (throw 'trial-period-expired "1"))
    (t (c)
      (declare (ignorable c))
      (print :S3-not-available)
      (describe c)
      (setf (rms-store *sys*) (open (conc$ "tya-nos3" #+not (get-universal-time) ".txt")
                                :if-does-not-exist :create
                                :if-exists :append ;; :supersede
                                :direction :io)))))

(defun tya ()
  (with-open-file (tya "tya.txt" :direction :input
                    :if-does-not-exist :create)
    (loop for line = (read-line tya nil :eof)
          while (not (eq line :eof))
          do (print line))))

#+test
(tya)

;;; --- macros ------------------------------------------

#+adhoc
(bucket-s3 :user)

(defun bucket-key-bucket (b-key)
  (cdr (assoc b-key (rms-buckets *sys*))))

(defun bucket-s3 (b-key)
  (s3-bucket (bucket-key-bucket b-key)))

(defmacro with-rms-dependency (b-key &body body)
  (let ((b (gensym)))
    `(let ((,b (bucket-key-bucket ,b-key)))
       (assert ,b () "Bucket ~a unknown among ~a" ,b-key (rms-buckets *sys*))
       (db-pulse ,b)
       ,@body)))

(defmacro with-rms-read ((s3-var b-key) &body body)
  `(let ((,s3-var (bucket-s3 ,b-key)))
     ,@body))

(defmacro with-rms-write ((s3-var b-key) &body  body)
  (let ((bucket (gensym)))
    `(let* ((,bucket (bucket-key-bucket ,b-key))
            (,s3-var (s3-bucket ,bucket)))
       (prog1
           (progn ,@body)
         (incf (db-pulse ,bucket))))))

;;; --- reads -------------------------------------------------------------
  
#+test
(progn
 (trace cl-s3:get-object
   cl-s3::do-s3-request
   cl-s3::make-authorization
   s-http-client::do-http-request
   s-http-client::do-http-request-internal
   s-http-client::get-open-connection
   s-http-client::do-one-request-response)
 (cl-s3:get-object "tyl.users" "kz4/pwd")
 (untrace))

(defun rms-get (bucket key)
  (when (keywordp bucket)
    (setf bucket (bucket-s3 bucket)))
  (prog1
      (bif (sf nil #+nahhh (rms-store *sys*))
        nil #+nahh (progn
          (file-position sf :start)
          (loop with result
              for line = (read-line sf nil :eof)
              if (eq line :eof) do (loop-finish)
              else if (plusp (length line))
              do (destructuring-bind (sf-b-key sf-key content ctype)
                     (read-from-string line)
                   (declare (ignorable ctype))
                   (when (string= b-key sf-b-key)
                     #+xxx (trc "rms-in-bucket" b-key sf-key :seeking key)
                     (when (string= key sf-key)
                       #+xxx (trc "bingo!!! rms-get" key content)
                       (setf result content)))) 
              finally (return result)))
        (progn ;with-rms-read (s3 b-key)
          (handler-case
              (cl-s3:get-object bucket key)
            (cl-s3:amazon-s3-api-error (c)
              (declare (ignore c))))))
    (clock :rms-got bucket key)))
  
(defun rms-get-all (bucket &key key max (delim #\/))
  (when (keywordp bucket)
      (setf bucket (bucket-s3 bucket)))
  (bif (sf nil #+broke (rms-store *sys*))
        nil 
    #+nahh (progn
             (file-position sf :start)
             (loop with result
                 for line = (read-line sf nil :eof)
                 if (eq line :eof) do (loop-finish)
                 else if (plusp (length line))
                 do (destructuring-bind (sf-b-key sf-key c ct)
                        (let ((words (read-from-string line)))
                          ;(trc "rms-get-all> got line" line)
                          ;(trc "rms-get-all> got words" words)
                          words)
                      (declare (ignorable c ct))
                      (when (string= b-key sf-b-key)
                        (when (eql 0 (search key sf-key))
                          (trc nil "match" sf-key (length key) (position #\/ sf-key :start (length key)) key)
                          (bwhen (dpos (position #\/ sf-key :start (length key)))
                            (pushnew (subseq sf-key 0 (1+ dpos)) result :test 'string=)))))
                 finally (return (nreverse result))))
    (progn ;; with-rms-read (s3 b-key)
      (trc nil "rms-get-all listing" b-key key max delim)
      (cl-s3::list-objects-delimited bucket :prefix key :max-keys max :delimiter delim))))

(defun rms-get-generic (b-key &rest ksegs &aux (k (when ksegs (apply 'ps3key ksegs))))
  (multiple-value-bind (p c)
      (rms-get-all b-key :key (when k (conc$ k "/")))
    ;(print (list :p p :c c))
    (if p
        (loop for s in p collecting (subseq s 0 (1- (length s))))
      c)))

#|
(rms-get-all :user)

(rms-get-generic :user "kkk2")

(rms-get-generic :mission "kenny6")

(rms-get-generic2 "tyl.users" "lisa")

|#

(export! rms-get-generic2 rms-peek-generic2)

(defun rms-get-generic2 (b-key &rest ksegs &aux (k (when ksegs (apply 'ps3key ksegs))))
  (multiple-value-bind (p c)
      (rms-get-all b-key :key k)
    (if p
        (loop for s in p collecting (subseq s 0 (1- (length s))))
      c)))

(defun rms-peek-generic2 (b-key &rest ksegs &aux (k (when ksegs (apply 'ps3key ksegs))))
  (multiple-value-bind (p c)
      (rms-get-all b-key :key k :max 1)
    (or p c)))

;;; --- writes --------------------------------


(defun rms-put (b-key key content &optional (content-type "text/plain") update?
                 &aux (c-type (or content-type "text/plain")))
  (if (and (not update?) (rms-get-maybe b-key key))
      (error "rms-put: Record already exists ~a ~a" (bucket-s3 b-key) key)
    (with-rms-write (s3 b-key)
      (bif (sf (rms-store *sys*))
        (progn
          (trc "no ps3, writing here")
          (file-position sf :end)
          (format sf "~&(~s ~s ~s ~s)" b-key key content c-type))
        (cl-s3:put-object s3 key content c-type))
      #-its-alive! (trc "rms-put> " content key s3)
      content)))

#+test
(cl-s3:put-object "tyl.version" "1" "ok" "text/plain")

#+quicklogin
(cl-s3:put-object "tyl.users" "kkk" "trial" "text/plain")

#+test
(rms-put :login "announce" "t" #+not
  "(progn (tk-format-now \"tk_dialog .byebye {No Mas} {asta la vista} {} {Cool} {Cool}\")
          t)" nil t)

#+test
(eval (read-from-string (rms-get :login "announce" )))

(defun rms-get-maybe (b-key key)
  (unless .rms-down
    (handler-case
        (rms-get b-key key)
      (cl-s3:amazon-s3-api-error (c)
        (declare (ignorable c))))))

(defun rms-update (b-key key content &optional (content-type "text/plain") )
  (if #+its-alive! t #-its-alive! (rms-get b-key key)
    (rms-put b-key key content content-type t)
    (break "rms-put: Record does not exist ~a ~a" b-key key)))


;;; --- maintenance ----------------------------------


(defun rms-clear (&optional b)
  (break "too dangerous, clears everything, production and test. Use rms-clear-bucket")
  ;;;
  ;;;  (if b
  ;;;      (rms-clear-bucket (bucket-s3 b))
  ;;;  (dolist (b (cl-s3:list-buckets))
  ;;;    (rms-clear-bucket b)))
  b)

#+xx
(cl-s3:list-buckets)

(defun rms-clear-bucket (s3)
  (dolist (k (cl-s3:list-objects s3))
    (handler-case 
        ;;(cl-s3:delete-object b k)
        (progn (trcx rms-clearing s3 k)
          (cl-s3::do-s3-request :delete (concatenate 'string "/" s3 "/" k) :content "x" :content-type "text/plain"))
      (t (c) (trc "error deleting" k c)
        (describe c)))))

(defun rms-dump (&optional b (ct 20))
  
  (flet ((dump-b (b)
           (loop for k in (cl-s3:list-objects b)
                 for n below ct
                 do (handler-case 
                        (trc "DB> " b k (cl-s3:get-object b k))
                      (t (c) (trc "error reading" k (type-of c))))
                 )))
    (if b (dump-b b)
      (dolist (b (cl-s3:list-buckets))
        (dump-b b)
        ))))

#|
(rms-dump "tyl.users" 100)

(rms-get-all :user :key "kkk2/")
(cl-s3:list-objects "tyl.users")
(rms-clear-bucket "tyl.flares")

(rms-dump "tyl.crash")

(rms-dump "tyl.crash" 266)


|#

(defun save-crash-log ()
  (let ((log (conc$ "/0algebra/crashes/" (substitute #\- #\: (ymdhmsh)) ".txt")))
    (print `(:log-name ,log))
    (dribble log)
    (rms-dump "tyl.crash")
    (dribble)))

#+test
(save-crash-log)

#+mopup
(rms-clear-bucket "tyl.crash")

#|
(cl-s3:delete-bucket "tyl.mxxxxxxissions")
(cl-s3:get-service)
(cl-s3:put-bucket "tyl.users")
(cl-s3:put-bucket "tyl.version")
(cl-s3:put-bucket "tyl.faq")
(cl-s3:put-bucket "tyl.missions")
(cl-s3:put-bucket "tyl.crash")
(cl-s3:put-bucket "tyl.failed")
(cl-s3:put-bucket "tyl.practiced")

(cl-s3:delete-bucket *user-db*)
(cl-s3:put-bucket *login-db*)
(rms-put :user "kenny.pwd" "tao")
(rms-put :user "kenny7" "tilton")
(rms-get :user "kenny.pwd")
(rms-get :user "kenny")

(cello-reset 'a1-system)

(rms-put :user "kkk/pwd" "kkkkkk")
(rms-put :user "kkk" "trial")

(dolist (k (cl-s3:list-objects *user-db* :prefix "p"))
  (trc "userdata" k (rms-get :user k)))

(list-objects-delimited *login-db* :prefix "kenny6" :max-keys 1)

(dolist (k (cl-s3:list-objects *login-db* :prefix "kenny6"))
  (trc "userdata" k (rms-get *login-db* k)))

(rms-clear)
(rms-dump "tyl.flares")

(get-bucket-delimited :flare :prefix "kenny6" :delimiter "/")

(cl-s3:list-objects "tyl.users")

(rms-dump "tyl.missions")

(rms-dump "tyl.logins")

(rms-tree-do "tyl.missions" (lambda (x y)
                              (print (list x y))) "kz5")

(decode-universal-time 3426546334)

(rms-get-all "tyl.missions" :key "spirozh/")

|#



(defun rms-tree-do (b func &optional k)
  (trc "seeking key---------" k)
  (funcall func k (rms-get b k))
  (multiple-value-bind (p c)
      (rms-get-all b :key (when k (conc$ k "/")))
    (trcx raw-p p)(trcx raw-c c)
    (loop for cx in c
          do (funcall func cx (rms-get b cx)))
    (loop for sub-k in p 
          do (rms-tree-do b func (subseq sub-k 0 (1- (length sub-k)))))))

(defun rms-tree-two (b func &optional k)
  ;(trc "seeking key---------" k)
  (multiple-value-bind (p c)
      (rms-get-all b :key k)
    ;(trcx raw-p p)(trcx raw-c c)
    (loop for cx in c
        do (funcall func b cx (rms-get b cx)))
    (loop for sub-k in p 
        do (rms-tree-two b func sub-k))))

#+test
(rms-tree-two "tyl.users" (lambda (b key content)
                           (print (list b key :-> content))) "kkk2")

#+purge
(rms-tree-two "tyl.users" (lambda (bucket key content)
                            (declare (ignore content))
                            (print (list :deleting key))
                            (cl-s3:delete-object bucket key)) "jjj")

#+test
(rms-get-all "tyl.users" :key "lisa")

#+everything
(rms-dump "tyl.users" 100)