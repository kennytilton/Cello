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

(dft alc-boolean ::unsigned-char #+allegro character #-allegro number)
(dft alc-byte :unsigned-char  #+allegro character #-allegro number)
(dft alc-ubyte ::unsigned-char  #+allegro character #-allegro number)
(dft alc-short #-allegro-v5.0.1 :short #+allegro-v5.0.1 :int integer)
(dft alc-ushort #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)
(dft alc-uint #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)
(dft alc-int :int integer)
(dft alc-float #+lispworks :lisp-single-float #-lispworks :float single-float)
(dft alc-double :double double-float)
(dft alc-sizei #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)
(dft alc-void :void integer)
(dft alc-enum #-allegro-v5.0.1 :unsigned-int #+allegro-v5.0.1 :int integer)

(dfc alc_invalid                    -1)
(dfc alc_false                      0)
(dfc alc_true                       1)
(dfc alc_no_error                   alc_false)

(dfc alc_major_version              #x1000)
(dfc alc_minor_version              #x1001)
(dfc alc_attributes_size            #x1002)
(dfc alc_all_attributes             #x1003)

(dfc alc_default_device_specifier   #x1004)
(dfc alc_device_specifier           #x1005)
(dfc alc_extensions                 #x1006)

(dfc alc_frequency                  #x1007)
(dfc alc_refresh    #x1008)
(dfc alc_sync    #x1009)
(dfc alc_invalid_device             #xa001)
(dfc alc_invalid_context            #xa002) 
(dfc alc_invalid_enum   #xa003)
(dfc alc_invalid_value              #xa004)
(dfc alc_out_of_memory              #xa005)
