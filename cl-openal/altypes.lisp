(in-package :cl-openal)
#|
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


(dft al-enum #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)
(dft al-bitfield #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)

(dft al-int :int integer)
(dft al-sizei :int integer)

(dft al-uint #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)
(dft al-ushort #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)

(dft al-float #+lispworks :lisp-single-float #-lispworks :float single-float)
(dft al-clampf #+lispworks :lisp-single-float #-lispworks :float single-float)

(dft al-double :double double-float)
(dft al-clampd :double double-float)

(dft al-boolean ::unsigned-char #+allegro character #-allegro number)
(dft al-byte :unsigned-char  #+allegro character #-allegro number) ;; typedef signed char     GLbyte; 
(dft al-void :void integer)

(dft al-short #-allegro-v5.0.1 :short #+allegro-v5.0.1 :int integer)
(dft al-ubyte ::unsigned-char  #+allegro character #-allegro number)


(dft al-sizei #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)

(dfc al_invalid                    -1)
(dfc al_none                      0)
(dfc al_false                      0)
(dfc al_true                       1)


#|*
  * Indicate the type of AL_SOURCE.
  * Sources can be spatialized 
  |#
(dfc al_source_type                           #x200)

#|* Indicate source has absolute coordinates. |#
(dfc al_source_absolute                       #x201)

#|* Indicate Source has listener relative coordinates. |#
(dfc al_source_relative                       #x202)

#|*
 * Directional source, inner cone angle, in degrees.
 * Range:    [0-360] 
 * Default:  360
 |#
(dfc al_cone_inner_angle                      #x1001)

#|*
 * Directional source, outer cone angle, in degrees.
 * Range:    [0-360] 
 * Default:  360
 |#
(dfc al_cone_outer_angle                      #x1002)

#|*
 * Specify the pitch to be applied, either at source,
 *  or on mixer results, at listener.
 * Range:  [0.5-2.0]
 * Default:  1.0
 |#
(dfc al_pitch                                 #x1003)

#|* 
 * Specify the current location in three dimensional space.
 * OpenAL, like OpenGL, uses a right handed coordinate system,
 *  where in a frontal default view X (thumb) points right, 
 *  Y points up (index finger), and Z points towards the
 *  viewer/camera (middle finger). 
 * To switch from a left handed coordinate system, flip the
 *  sign on the Z coordinate.
 * Listener position is always in the world coordinate system.
 |# 
(dfc al_position                              #x1004)
  
#|* Specify the current direction as forward vector. |#
(dfc al_direction                             #x1005)
  
#|* Specify the current velocity in three dimensional space. |#
(dfc al_velocity                              #x1006)

#|*
 * Indicate whether source has to loop infinite.
 * Type: ALboolean
 * Range:    [AL_TRUE, AL_FALSE]
 * Default:  AL_FALSE
 |#
(dfc al_looping                               #x1007)

#|*
 * Indicate the buffer to provide sound samples. 
 * Type: ALuint.
 * Range: any valid Buffer id.
 |#
(dfc al_buffer                                #x1009)

#|*
 * Indicate the gain (volume amplification) applied. 
 * Type:     ALfloat.
 * Range:    ]0.0-  ]
 * A value of 1.0 means un-attenuated/unchanged.
 * Each division by 2 equals an attenuation of -6dB.
 * Each multiplicaton with 2 equals an amplification of +6dB.
 * A value of 0.0 is meaningless with respect to a logarithmic
 *  scale; it is interpreted as zero volume - the channel
 *  is effectively disabled.
 |#
(dfc al_gain                                  #x100a)

#|*
 * Indicate minimum source attenuation.
 * Type:     ALfloat
 * Range:  [0.0 - 1.0]
 |#
(dfc al_min_gain                              #x100d)

#|*
 * Indicate maximum source attenuation.
 * Type:  ALfloat
 * Range:  [0.0 - 1.0]
 |#
(dfc al_max_gain                              #x100e)

#|* 
 * Specify the current orientation.
 * Type:  ALfv6 (at/up)
 * Range:  N/A
 |#
(dfc al_orientation                           #x100f)

#| byte offset into source (in canon format).  -1 if source
 * is not playing.  Don't set this, get this.
 *
 * Type:     ALfloat
 * Range:    [0.0 - ]
 * Default:  1.0
 |#
(dfc al_reference_distance                    #x1020)

 #|*
 * Indicate the rolloff factor for the source.
 * Type: ALfloat
 * Range:    [0.0 - ]
 * Default:  1.0
 |#
(dfc al_rolloff_factor                        #x1021)

#|*
 * Indicate the gain (volume amplification) applied. 
 * Type:     ALfloat.
 * Range:    ]0.0-  ]
 * A value of 1.0 means un-attenuated/unchanged.
 * Each division by 2 equals an attenuation of -6dB.
 * Each multiplicaton with 2 equals an amplification of +6dB.
 * A value of 0.0 is meaningless with respect to a logarithmic
 *  scale; it is interpreted as zero volume - the channel
 *  is effectively disabled.
 |#
(dfc al_cone_outer_gain                       #x1022)

#|* 
 * Specify the maximum distance.
 * Type:  ALfloat
 * Range:  [0.0 - ]
 |#
(dfc al_max_distance                          #x1023)

#|*
 * Specify the channel mask. (Creative)
 * Type:  ALuint
 * Range:  [0 - 255]
 |#
(dfc al_channel_mask        #x3000)

#|*
 * Source state information
 |#
(dfc al_source_state                          #x1010)
(dfc al_initial                               #x1011)
(dfc al_playing                               #x1012)
(dfc al_paused                                #x1013)
(dfc al_stopped                               #x1014)

#|*
 * Buffer Queue params
 |#
(dfc al_buffers_queued                        #x1015)
(dfc al_buffers_processed                     #x1016)

#|* Sound buffers: format specifier. |#
(dfc al_format_mono8                          #x1100)
(dfc al_format_mono16                         #x1101)
(dfc al_format_stereo8                        #x1102)
(dfc al_format_stereo16                       #x1103)

#|* 
 * Sound buffers: frequency, in units of Hertz [Hz].
 * This is the number of samples per second. Half of the
 *  sample frequency marks the maximum significant
 *  frequency component.
 |#
(dfc al_frequency                             #x2001)
(dfc al_bits                                  #x2002)
(dfc al_channels                              #x2003)
(dfc al_size                                  #x2004)
(dfc al_data                                  #x2005)

#|*
 * Buffer state.
 *
 * Not supported for public use (yet).
 |#
(dfc al_unused                                #x2010)
(dfc al_pending                               #x2011)
(dfc al_processed                             #x2012)

#|* Errors: No Error. |#
(dfc al_no_error                              al_false)

#|* 
 * Illegal name passed as an argument to an AL call.
 |#
(dfc al_invalid_name                          #xa001)

#|* 
 * Illegal enum passed as an argument to an AL call.
 |#
(dfc al_invalid_enum                          #xa002) 
#|* 
 * Illegal value passed as an argument to an AL call.
 * Applies to parameter values, but not to enumerations.
 |#
(dfc al_invalid_value                         #xa003)
  
#|*
 * A function was called at inappropriate time,
 *  or in an inappropriate way, causing an illegal state.
 * This can be an incompatible ALenum, object ID,
 *  and/or function.
 |#
(dfc al_invalid_operation                     #xa004)

#|*
 * A function could not be completed,
 * because there is not enough memory available.
 |#
(dfc al_out_of_memory                         #xa005)

#|* Context strings: Vendor Name. |#
(dfc al_vendor                                #xb001)
(dfc al_version                               #xb002)
(dfc al_renderer                              #xb003)
(dfc al_extensions                            #xb004)

#|* Global tweakage. |#

#|*
 * Doppler scale.  Default 1.0
 |#
(dfc al_doppler_factor                        #xc000)
 
#|*
 * Doppler velocity.  Default 1.0
 |#
(dfc al_doppler_velocity                      #xc001)

#|*
 * Distance model.  Default AL_INVERSE_DISTANCE_CLAMPED
 |#
(dfc al_distance_model                        #xd000)

#|* Distance models. |#

(dfc al_inverse_distance                      #xd001)
(dfc al_inverse_distance_clamped              #xd002)
 
