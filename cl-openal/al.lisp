(in-package :cl-openal)

#|*
 * OpenAL cross platform audio library
 * Copyright (C) 1999-2000 by authors.
 * This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the
 *  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *  Boston, MA  02111-1307, USA.
 * Or go to http://www.gnu.org/copyleft/lgpl.html
 |#

#|*
 * OpenAL Maintenance Functions
 * Initialization and exiting.
 * State Management and Query.
 * Error Handling.
 * Extension Support.
 |#

#|* State management. |#
(defun-ffx al-void  "openal" "alEnable" ( al-enum capability ))
(defun-ffx al-void  "openal" "alDisable" ( al-enum capability ))
(defun-ffx al-boolean  "openal" "alIsEnabled" ( al-enum capability ))

#|* Application preferences for driver performance choices. |#
(defun-ffx al-void  "openal" "alHint" ( al-enum target al-enum mode ))

#|* State retrieval. |#
(defun-ffx al-boolean  "openal" "alGetBoolean" ( al-enum param ))
(defun-ffx al-int     "openal" "alGetInteger" ( al-enum param ))
(defun-ffx al-float    "openal" "alGetFloat" ( al-enum param ))
(defun-ffx al-double   "openal" "alGetDouble" ( al-enum param ))
(defun-ffx al-void  "openal" "alGetBooleanv" ( al-enum param :void *data ))
(defun-ffx al-void  "openal" "alGetIntegerv" ( al-enum param :void *data ))
(defun-ffx al-void  "openal" "alGetFloatv" ( al-enum param :void *data ))
(defun-ffx al-void  "openal" "alGetDoublev" ( al-enum param :void *data ))
(defun-ffx (* :void) "openal" "alGetString" ( al-enum param ))

#|*
 * Error support.
 * Obtain the most recent error generated in the AL state machine.
 |#

(defun-ffx al-enum "openal" "alGetError" ( ))


#|* 
 * Extension support.
 * Obtain the address of a function (usually an extension)
 *  with the name fname. All addresses are context-independent. 
 |#
(defun-ffx al-boolean  "openal" "alIsExtensionPresent" ( :void *fname ))


#|* 
 * Extension support.
 * Obtain the address of a function (usually an extension)
 *  with the name fname. All addresses are context-independent. 
 |#
(defun-ffx (* :void)  "openal" "alGetProcAddress" ( :void *fname ))


#|*
 * Extension support.
 * Obtain the integer value of an enumeration (usually an extension) with the name ename. 
 |#
(defun-ffx al-enum  "openal" "alGetEnumValue" ( :void *ename ))

#|*
 * LISTENER
 * Listener is the sample position for a given context.
 * The multi-channel (usually stereo) output stream generated
 *  by the mixer is parametrized by this Listener object:
 *  its position and velocity relative to Sources, within
 *  occluder and reflector geometry.
 |#


#|*
 *
 * Listener Environment:  default 0.
 |#

(defun-ffx al-void  "openal" "alListeneri" ( al-enum param al-int value ))

#|*
 *
 * Listener Gain:  default 1.0f.
 |#

(defun-ffx al-void  "openal" "alListenerf" ( al-enum param al-float value ))

#|*  
 *
 * Listener Position.
 * Listener Velocity.
 |#
(defun-ffx al-void  "openal" "alListener3f" ( al-enum param al-float v1 al-float v2 al-float v3 )) 


#|*
 *
 * Listener Position:        ALfloat[3]
 * Listener Velocity:        ALfloat[3]
 * Listener Orientation:     ALfloat[6]  (forward and up vector).
 |#
(defun-ffx al-void  "openal" "alListenerfv" ( al-enum param :void *values )) 

(defun-ffx al-void  "openal" "alGetListeneri" ( al-enum param :void *value ))
(defun-ffx al-void  "openal" "alGetListenerf" ( al-enum param :void *value ))
(defun-ffx al-void  "openal" "alGetListener3f" ( al-enum param :void *v1 :void *v2 :void *v3 )) 
(defun-ffx al-void  "openal" "alGetListenerfv" ( al-enum param :void *values )) 


#|*
 * SOURCE
 * Source objects are by default localized. Sources
 *  take the PCM data provided in the specified Buffer,
 *  apply Source-specific modifications, and then
 *  submit them to be mixed according to spatial 
 *  arrangement etc.
 |#



#|* Create Source objects. |#
(defun-ffx al-void  "openal" "alGenSources" ( al-sizei n :void *sources )) 

#|* Delete Source objects. |#
(defun-ffx al-void  "openal" "alDeleteSources" ( al-sizei n :void *sources ))

#|* Verify a handle is a valid Source. |# 
(defun-ffx al-boolean  "openal" "alIsSource" ( al-uint id )) 

#|* Set an integer parameter for a Source object. |#
(defun-ffx al-void  "openal" "alSourcei" ( al-uint source al-enum param al-int value )) 
(defun-ffx al-void  "openal" "alSourcef" ( al-uint source al-enum param al-float value )) 
(defun-ffx al-void  "openal" "alSource3f" ( al-uint source al-enum param al-float v1 al-float v2 al-float v3 ))
(defun-ffx al-void  "openal" "alSourcefv" ( al-uint source al-enum param :void *values )) 

#|* Get an integer parameter for a Source object. |#
(defun-ffx al-void  "openal" "alGetSourcei" ( al-uint source  al-enum param :void *value ))
(defun-ffx al-void  "openal" "alGetSourcef" ( al-uint source  al-enum param :void *value ))
(defun-ffx al-void  "openal" "alGetSource3f" ( al-uint source  al-enum param :void *v1 :void *v2 :void *v3 ))
(defun-ffx al-void  "openal" "alGetSourcefv" ( al-uint source al-enum param :void *values ))

(defun-ffx al-void  "openal" "alSourcePlayv" ( al-sizei n al-uint *sources ))
(defun-ffx al-void  "openal" "alSourcePausev" ( al-sizei n al-uint *sources ))
(defun-ffx al-void  "openal" "alSourceStopv" ( al-sizei n al-uint *sources ))
(defun-ffx al-void  "openal" "alSourceRewindv" (al-sizei n al-uint *sources))

#|* Activate a source, start replay. |#
(defun-ffx al-void  "openal" "alSourcePlay" ( al-uint source ))

#|*
 * Pause a source, 
 *  temporarily remove it from the mixer list.
 |#
(defun-ffx al-void  "openal" "alSourcePause" ( al-uint source ))

#|*
 * Stop a source,
 *  temporarily remove it from the mixer list,
 *  and reset its internal state to pre-Play.
 * To remove a Source completely, it has to be
 *  deleted following Stop, or before Play.
 |#
(defun-ffx al-void  "openal" "alSourceStop" ( al-uint source ))

#|
 *
 * Rewinds a source, 
 *  temporarily remove it from the mixer list,
 *  and reset its internal state to pre-Play.
 |#
(defun-ffx al-void  "openal" "alSourceRewind" ( al-uint source ))

#|
 *
 * BUFFER
 * Buffer objects are storage space for sample data.
 * Buffers are referred to by Sources. There can be more than
 *  one Source using the same Buffer data. If Buffers have
 *  to be duplicated on a per-Source basis, the driver has to
 *  take care of allocation, copying, and deallocation as well
 *  as propagating buffer data changes.
 |#

#|* Buffer object generation. |#

(defun-ffx al-void "openal" "alGenBuffers" ( al-sizei n :void *buffer-uints ))
(defun-ffx al-void "openal" "alDeleteBuffers" ( al-sizei n :void *buffers ))
(defun-ffx al-boolean  "openal" "alIsBuffer" ( al-uint buffer ))

#|
 *
 * Specify the data to be filled into a buffer.
 *
|#
(defun-ffx al-void  "openal" "alBufferData" ( al-uint buffer
                                             al-enum format
                                             :void  *data
                                             al-sizei size
                                             al-sizei freq ))

(defun-ffx al-void  "openal" "alGetBufferi" ( al-uint buffer al-enum param :void *value ))
(defun-ffx al-void  "openal" "alGetBufferf" ( al-uint buffer al-enum param :void *value ))

#|
 *
 * Queue stuff
 *
 |#

(defun-ffx al-void  "openal" "alSourceQueueBuffers" ( al-uint source al-sizei n :void *buffers ))
(defun-ffx al-void  "openal" "alSourceUnqueueBuffers" ( al-uint source al-sizei n :void *buffers ))

#|
 *
 * Knobs and dials
 *
 |#
(defun-ffx al-void  "openal" "alDistanceModel" ( al-enum value ))
(defun-ffx al-void  "openal" "alDopplerFactor" ( al-float value ))
(defun-ffx al-void  "openal" "alDopplerVelocity" ( al-float value ))
