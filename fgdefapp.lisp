(in-package :cello)

;;;  ------------------------------------------------------------------------ ----
;;; defapp - Define an applicatin window to be run as a Cello  app.        MACRO
;;;  ------------------------------------------------------------------------ ----
;;; Arguments are normal defclass arguments. Creates a class as a  subclass of
;;; gt.app using Cells' defmodel.
;;;
;;; Side effects:
;;; Two other functions %%RUN-... and RUN-... are defun'd (all %%...  named fns
;;; are purely internal functions not to be called outside of this  package!!!)
;;; A (defapp my-app ... ) defines the user callable fn RUN-MY-APP.
;;; This function is to be called to run the application my-app.
;;;
;;; STATUS: Released.

(defmacro defapp (class directsupers slotspecs &rest options)
  `(prog1
     (defmodel ,class ,(or directsupers '(gt.app)) ,slotspecs ,(car  options))
     (mop:finalize-inheritance (find-class ',class))
     (defun ,(intern (conc$ "%%RUN-" (symbol-name class))) (w-t-f)
       (declare (ignore w-t-f))
       (cl-user::gc t) ;; Oddity in ACL: without gc-ing here ACL errors
       (cells-reset 'ctk:tk-user-queue-handler)
       (wands-clear)
       (ctk::test-window ',class))
     (defun ,(intern (conc$ "RUN-" (symbol-name class))) ()
       (mk-thread ,(with-output-to-string (stream)
                  (format stream "APPLICATION-THREAD-~A"
                (symbol-name class)))
              ',(intern (conc$ "%%RUN-" (symbol-name class)))))
     (export ',(intern (conc$ "RUN-" (symbol-name class))))
     (export ',(intern (symbol-name class)))
     ))

(defun mk-thread (name run-this)
  (print (list name run-this)))
;;;The class gt.app is defined as:

;;;  ------------------------------------------------------------------------ ----
;;; gt.app - Application Base  Class                                       CLASS
;;;  ------------------------------------------------------------------------ ----
;;; STATUS: Released.

(defmodel gt.app ( cello-window )
  ((.md-name            :cell t :accessor id
                :initform (c-in nil)
            :initarg :id
            :documentation
             "The model ID of the instance of the application.")
   (init-fn             :cell nil :accessor init-fn
            :initform nil
            :initarg :initfn
            :documentation
             "Function to be called before running the application.")
   (status              :cell t :accessor status
                :initform (c-in nil) :initarg :status
            :documentation
             "Status := { :CREATED | :INITIALIZING | :RUNNING | :SHUTTDING- DOWN | :HALTED | :BLOCKED }")
   (opcode              :cell t :accessor opcode
                :initform (c-in nil)
            :initarg :opcode
            :documentation
             "Operation Code := { :INIT | :RUN | :SHUTDOWN }")
   (current-opcode-task :cell t :accessor current-opcode-task
                :initform nil ;;; (application-current-opcode-task-cell-rule)
            :initarg :current-opcode-task
            :documentation
             "Holds the Cell Rule to execute a task depending on the opcode  slot.")
   (main-thread         :cell t :accessor main-thread
            :initform (c-in nil)
            :initarg :main-thread
            :documentation
             "Holds the thread object created by RUN-... (which calls mk- thread)"))
  (:documentation
   "gt.app - Application Base  Class                                       CLASS"))



(defapp my-app ()
  ()
  (:default-initargs
     :id :my-app
     :kids (c? (the-kids
         (mk-stack (:packing (c?pack-self))
            (mk-row ()
               (mk-label :text "Status : "
                 :width 15)
               (mk-label :text (c_? (status (fm-other :my-app)))))
            (mk-row ()
               (mk-label :text "Opcode : "
                 :width 15)
               (mk-label :text (c_? (opcode (fm-other :my-app))))))))))



(PROG1 (DEFMODEL MY-APP (GT.APP) NIL
                 (:DEFAULT-INITARGS :ID :MY-APP :KIDS
                  (C? (THE-KIDS (MENUBAR)
                                (MK-STACK
                                 (:PACKING (C?PACK-SELF))
                                 (MK-ROW
                                  NIL
                                  (MK-LABEL
                                   :TEXT
                                   "Status : "
                                   :WIDTH
                                   15)
                                  (MK-LABEL
                                   :TEXT
                                   (C_? (STATUS (FM-OTHER :MY-APP)))))
                                 (MK-ROW
                                  NIL
                                  (MK-LABEL
                                   :TEXT
                                   "Opcode : "
                                   :WIDTH
                                   15)
                                  (MK-LABEL
                                   :TEXT
                                   (C_?
                                    (OPCODE (FM-OTHER :MY-APP))))))))))
       (FINALIZE-INHERITANCE (FIND-CLASS 'MY-APP))
       (DEFUN %%RUN-MY-APP (GT.APP.BASE::W-T-F)
         (DECLARE (IGNORE GT.APP.BASE::W-T-F))
         (EXCL:GC T)
         (CELLS-RESET 'TK-USER-QUEUE-HANDLER)
         (MGK:WANDS-CLEAR)
         (TEST-WINDOW 'MY-APP))
       (DEFUN RUN-MY-APP ()
         (GT.APP.BASE::MK-THREAD "APPLICATION-THREAD-MY-APP"
           '%%RUN-MY-APP))
       (EXPORT 'RUN-MY-APP) (EXPORT 'MY-APP))

;;;I can  see no error in this. Yet there are undefined functions status  and opcode:
;;;
;;;;;; Compiling file /tmp/tempa24242105201
;;;;;; Writing fasl file /tmp/tempa24242105201.fasl
;;;;;; Fasl write complete
;;;Warning: While compiling these undefined functions were referenced:
;;;         STATUS from position 336 in #1=test.lisp;483
;;;         OPCODE from position 336 in #1# 