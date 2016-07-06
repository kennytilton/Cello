(in-package :cl-openal)

; typedef struct ALCdevice_struct ALCdevice;
; typedef struct ALCcontext_struct ALCcontext;


;;;(defun-ffx ALCubyte*  "openal" "alcGetString" (ALCdevice *device ALCenum param))
;;;(defun-ffx ALCvoid    "openal" "alcGetIntegerv" (ALCdevice *device ALCenum param ALCsizei size ALCint *data))
;;;

(defun-ffx (* :void) "openal" "alcOpenDevice" (:string device-name))
(defun-ffx :void     "openal" "alcCloseDevice" (:void *device))
(defun-ffx (* :void) "openal" "alcCreateContext" (:void *device alc-int *attr-list))
(defun-ffx alc-enum "openal" "alcMakeContextCurrent" (:void *context))

(defun-ffx :void   "openal" "alcProcessContext" (:void *context))
(defun-ffx (* :void) "openal" "alcGetCurrentContext" ())
(defun-ffx (* :void) "openal" "alcGetContextsDevice" (:void *context))
(defun-ffx :void   "openal" "alcSuspendContext" (:void *context))
(defun-ffx alc-enum    "openal" "alcDestroyContext" (:void *context))
;;;
(defun-ffx alc-enum "openal" "alcGetError" (:void *device))
;;;
(defun-ffx alc-boolean "openal" "alcIsExtensionPresent" (:void *device alc-ubyte *ext-name))
(defun-ffx :void  "openal" "alcGetProcAddress" (:void *device alc-ubyte *func-name))
(defun-ffx alc-enum "openal" "alcGetEnumValue" (:void *device alc-ubyte *enum-name))
