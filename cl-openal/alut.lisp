(in-package :cl-openal)

(defun-ffx :void "alut" "alutInit" (:void *argc :void *argv))
(defun-ffx :void "alut" "alutExit" ())

;;;(defun-ffx :void "alut" "alutLoadWAVFile"
;;;  (:void *file :void *format :void *data
;;;    :void *size :void *freq :void *loop))

(defun-ffx :void "alut" "alutLoadWAVFile"
    (:string file :pointer *format :pointer *data 
      :pointer *size :pointer freq :pointer loop))

(defun-ffx :void "alut" "alutLoadWAVMemory"
  (:void *memory :void *format :void *data :void *size
    :void *freq :void *loop))

(defun-ffx :void "alut" "alutUnloadWAV" 
  (al-enum format :void *data al-sizei size al-sizei freq))
