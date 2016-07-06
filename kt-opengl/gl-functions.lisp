;; -*- mode: Lisp; Syntax: Common-Lisp; Package: kt-opengl; -*-
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

(in-package #:kt-opengl)

(defparameter *ogl-listing-p* nil)


(defun-ogl :void "open-gl" "glFlush" ())

(defun-ogl :void "open-gl" "glMaterialfv" (glenum face glenum pname glfloat *params))

#| drawing functions |#

(defun-ogl :void "open-gl" "glBegin" (glenum mode ))
(defun-ogl :void "open-gl" "glEnd" ( ))

(defun-ogl :void "open-gl" "glVertex2d" (gldouble x gldouble y ))
(defun-ogl :void "open-gl" "glVertex2f" (glfloat x glfloat y ))
(defun-ogl :void "open-gl" "glVertex2i" (glint x glint y ))
(defun-ogl :void "open-gl" "glVertex2s" (glshort x glshort y ))
(defun-ogl :void "open-gl" "glVertex3d" (gldouble x gldouble y gldouble z ))
(defun-ogl :void "open-gl" "glVertex3f" (glfloat x glfloat y glfloat z ))

(defun-ogl :void "open-gl" "glVertex3i" (glint x glint y glint z ))
(defun-ogl :void "open-gl" "glVertex3s" (glshort x glshort y glshort z ))
(defun-ogl :void "open-gl" "glVertex4d" (gldouble x gldouble y gldouble z gldouble w ))
(defun-ogl :void "open-gl" "glVertex4f" (glfloat x glfloat y glfloat z glfloat w ))
(defun-ogl :void "open-gl" "glVertex4i" (glint x glint y glint z glint w ))
(defun-ogl :void "open-gl" "glVertex4s" (glshort x glshort y glshort z glshort w ))

(defun-ogl :void "open-gl" "glVertex2dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glVertex2fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glVertex2iv" (glint *v ))
(defun-ogl :void "open-gl" "glVertex2sv" (glshort *v ))
(defun-ogl :void "open-gl" "glVertex3dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glVertex3fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glVertex3iv" (glint *v ))
(defun-ogl :void "open-gl" "glVertex3sv" (glshort *v ))
(defun-ogl :void "open-gl" "glVertex4dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glVertex4fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glVertex4iv" (glint *v ))
(defun-ogl :void "open-gl" "glVertex4sv" (glshort *v ))
(defun-ogl :void "open-gl" "glNormal3b" (glbyte nx glbyte ny glbyte nz ))
(defun-ogl :void "open-gl" "glNormal3d" (gldouble nx gldouble ny gldouble nz ))
(defun-ogl :void "open-gl" "glNormal3f" (glfloat nx glfloat ny glfloat nz ))
(defun-ogl :void "open-gl" "glNormal3i" (glint nx glint ny glint nz ))
(defun-ogl :void "open-gl" "glNormal3s" (glshort nx glshort ny glshort nz ))
;;;(defun-ogl :void "open-gl" "glNormal3bv" (GLbyte *v ))
(defun-ogl :void "open-gl" "glNormal3dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glNormal3fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glNormal3iv" (glint *v ))
(defun-ogl :void "open-gl" "glNormal3sv" (glshort *v ))
(defun-ogl :void "open-gl" "glIndexd" (gldouble c ))
(defun-ogl :void "open-gl" "glIndexf" (glfloat c ))
(defun-ogl :void "open-gl" "glIndexi" (glint c ))
(defun-ogl :void "open-gl" "glIndexs" (glshort c ))
(defun-ogl :void "open-gl" "glIndexub" (glubyte c ))
(defun-ogl :void "open-gl" "glIndexdv" (gldouble *c ))
(defun-ogl :void "open-gl" "glIndexfv" (glfloat *c ))
(defun-ogl :void "open-gl" "glIndexiv" (glint *c ))
(defun-ogl :void "open-gl" "glIndexsv" (glshort *c ))
(defun-ogl :void "open-gl" "glIndexubv" (glubyte *c ))
(defun-ogl :void "open-gl" "glColor3b" (glbyte red glbyte green glbyte blue ))
(defun-ogl :void "open-gl" "glColor3d" (gldouble red gldouble green gldouble blue ))
(defun-ogl :void "open-gl" "glColor3f" (glfloat red glfloat green glfloat blue ))
(defun-ogl :void "open-gl" "glColor3i" (glint red glint green glint blue ))
(defun-ogl :void "open-gl" "glColor3s" (glshort red glshort green glshort blue ))
(defun-ogl :void "open-gl" "glColor3ub" (glubyte red glubyte green glubyte blue ))
(defun-ogl :void "open-gl" "glColor3ui" (gluint red gluint green gluint blue ))
(defun-ogl :void "open-gl" "glColor3us" (glushort red glushort green glushort blue ))
(defun-ogl :void "open-gl" "glColor4b" (glbyte red glbyte green glbyte blue glbyte alpha ))
(defun-ogl :void "open-gl" "glColor4d" (gldouble red gldouble green gldouble blue gldouble alpha ))
(defun-ogl :void "open-gl" "glColor4f" (glfloat red glfloat green glfloat blue glfloat alpha ))
(defun-ogl :void "open-gl" "glColor4i" (glint red glint green glint blue glint alpha ))
(defun-ogl :void "open-gl" "glColor4s" (glshort red glshort green glshort blue glshort alpha ))
(defun-ogl :void "open-gl" "glColor4ub" (glubyte red glubyte green glubyte blue glubyte alpha ))
(defun-ogl :void "open-gl" "glColor4ui" (gluint red gluint green gluint blue gluint alpha ))
(defun-ogl :void "open-gl" "glColor4us" (glushort red glushort green glushort blue glushort alpha ))
;;;(defun-ogl :void "open-gl" "glColor3bv" (GLbyte *v ))
(defun-ogl :void "open-gl" "glColor3dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glColor3fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glColor3iv" (glint *v ))
(defun-ogl :void "open-gl" "glColor3sv" (glshort *v ))
(defun-ogl :void "open-gl" "glColor3ubv" (glubyte *v ))
(defun-ogl :void "open-gl" "glColor3uiv" (gluint *v ))
(defun-ogl :void "open-gl" "glColor3usv" (glushort *v ))
;;;(defun-ogl :void "open-gl" "glColor4bv" (GLbyte *v ))
(defun-ogl :void "open-gl" "glColor4dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glColor4fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glColor4iv" (glint *v ))
(defun-ogl :void "open-gl" "glColor4sv" (glshort *v ))
(defun-ogl :void "open-gl" "glColor4ubv" (glubyte *v ))
(defun-ogl :void "open-gl" "glColor4uiv" (gluint *v ))
(defun-ogl :void "open-gl" "glColor4usv" (glushort *v ))
(defun-ogl :void "open-gl" "glTexCoord1d" (gldouble s ))
(defun-ogl :void "open-gl" "glTexCoord1f" (glfloat s ))
(defun-ogl :void "open-gl" "glTexCoord1i" (glint s ))
(defun-ogl :void "open-gl" "glTexCoord1s" (glshort s ))
(defun-ogl :void "open-gl" "glTexCoord2d" (gldouble s gldouble tt ))
(defun-ogl :void "open-gl" "glTexCoord2f" (glfloat s glfloat tt ))
(defun-ogl :void "open-gl" "glTexCoord2i" (glint s glint tt ))
(defun-ogl :void "open-gl" "glTexCoord2s" (glshort s glshort tt ))
(defun-ogl :void "open-gl" "glTexCoord3d" (gldouble s gldouble tt gldouble r ))
(defun-ogl :void "open-gl" "glTexCoord3f" (glfloat s glfloat tt glfloat r ))
(defun-ogl :void "open-gl" "glTexCoord3i" (glint s glint tt glint r ))
(defun-ogl :void "open-gl" "glTexCoord3s" (glshort s glshort tt glshort r ))
(defun-ogl :void "open-gl" "glTexCoord4d" (gldouble s gldouble tt gldouble r gldouble q ))
(defun-ogl :void "open-gl" "glTexCoord4f" (glfloat s glfloat tt glfloat r glfloat q ))
(defun-ogl :void "open-gl" "glTexCoord4i" (glint s glint tt glint r glint q ))
(defun-ogl :void "open-gl" "glTexCoord4s" (glshort s glshort tt glshort r glshort q ))
(defun-ogl :void "open-gl" "glTexCoord1dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glTexCoord1fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glTexCoord1iv" (glint *v ))
(defun-ogl :void "open-gl" "glTexCoord1sv" (glshort *v ))
(defun-ogl :void "open-gl" "glTexCoord2dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glTexCoord2fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glTexCoord2iv" (glint *v ))
(defun-ogl :void "open-gl" "glTexCoord2sv" (glshort *v ))
(defun-ogl :void "open-gl" "glTexCoord3dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glTexCoord3fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glTexCoord3iv" (glint *v ))
(defun-ogl :void "open-gl" "glTexCoord3sv" (glshort *v ))
(defun-ogl :void "open-gl" "glTexCoord4dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glTexCoord4fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glTexCoord4iv" (glint *v ))
(defun-ogl :void "open-gl" "glTexCoord4sv" (glshort *v ))
(defun-ffx :void "open-gl" "glTexCoordPointer" (glint size glenum type glsizei stride glvoid *pointer))
(defun-ffx :void "open-gl" "glTexEnvf" (glenum target glenum pname glfloat param))
(defun-ffx :void "open-gl" "glTexEnvfv" (glenum target glenum pname glfloat *params))
(defun-ffx :void "open-gl" "glTexEnvi" (glenum target glenum pname glint param))
(defun-ffx :void "open-gl" "glTexEnviv" (glenum target glenum pname glint *params))
(defun-ffx :void "open-gl" "glTexGend" (glenum coord glenum pname gldouble param))
(defun-ffx :void "open-gl" "glTexGendv" (glenum coord glenum pname gldouble *params))
(defun-ffx :void "open-gl" "glTexGenf" (glenum coord glenum pname glfloat param))
(defun-ffx :void "open-gl" "glTexGenfv" (glenum coord glenum pname glfloat *params))
(defun-ffx :void "open-gl" "glTexGeni" (glenum coord glenum pname glint param))
(defun-ffx :void "open-gl" "glTexGeniv" (glenum coord glenum pname glint *params))
;;;(defun-ffx :void "open-gl" "glTexImage1D" (GLenum target GLint level GLint internalformat 
;;;                                             GLsizei width GLint border GLenum format 
;;;                                             GLenum type GLvoid *pixels))
(defun-ffx :void "open-gl" "glTexImage2D" (glenum target glint level glint internalformat 
                                             glsizei width glsizei height glint border 
                                             glenum format glenum type glvoid *pixels))
(defun-ffx :void "open-gl" "glTexParameterf" (glenum target glenum pname glfloat param))
(defun-ffx :void "open-gl" "glTexParameterfv" (glenum target glenum pname glfloat *params))
(defun-ffx :void "open-gl" "glTexParameteri" (glenum target glenum pname glint param))
(defun-ffx :void "open-gl" "glTexParameteriv" (glenum target glenum pname glint *params))
(defun-ffx :void "open-gl" "glTexSubImage1D" (GLenum target GLint level GLint xoffset GLsizei width GLenum format GLenum type GLvoid *pixels))
(defun-ffx :void "open-gl" "glTexSubImage2D" (GLenum target GLint level GLint xoffset GLint yoffset GLsizei width GLsizei height GLenum format GLenum type GLvoid *pixels))

(defun-ffx :void "open-gl" "glGenTextures" (glsizei n gluint *textures))
(defun-ffx :void "open-gl" "glBindTexture" (glenum target gluint texture))
(defun-ffx :void "open-gl" "glDeleteTextures" (glsizei n gluint *textures))
(defun-ffx :int "open-gl" "glIsTexture" (gluint textureName))

;;;/* TextureGenParameter */
;;;#define GL_TEXTURE_GEN_MODE               0x2500
;;;#define GL_OBJECT_PLANE                   0x2501
;;;#define GL_EYE_PLANE                      0x2502
;;;
;;;#define GL_NEAREST_MIPMAP_NEAREST         0x2700
;;;#define GL_LINEAR_MIPMAP_NEAREST          0x2701
;;;#define GL_NEAREST_MIPMAP_LINEAR          0x2702
;;;#define GL_LINEAR_MIPMAP_LINEAR           0x2703
;;;
;;;/* TextureParameterName */
;;;#define GL_TEXTURE_MAG_FILTER             0x2800
;;;#define GL_TEXTURE_MIN_FILTER             0x2801
;;;#define GL_TEXTURE_WRAP_S                 0x2802
;;;#define GL_TEXTURE_WRAP_T                 0x2803
;;;/*      GL_TEXTURE_BORDER_COLOR */
;;;/*      GL_TEXTURE_PRIORITY */
;;;
;;;/* TextureTarget */
;;;/*      GL_TEXTURE_1D */
;;;/*      GL_TEXTURE_2D */
;;;/*      GL_PROXY_TEXTURE_1D */
;;;/*      GL_PROXY_TEXTURE_2D */

(defun-ogl :void "open-gl" "glRasterPos2d" (gldouble x gldouble y ))
(defun-ogl :void "open-gl" "glRasterPos2f" (glfloat x glfloat y ))
(defun-ogl :void "open-gl" "glRasterPos2i" (glint x glint y ))
(defun-ogl :void "open-gl" "glRasterPos2s" (glshort x glshort y ))
(defun-ogl :void "open-gl" "glRasterPos3d" (gldouble x gldouble y gldouble z ))
(defun-ogl :void "open-gl" "glRasterPos3f" (glfloat x glfloat y glfloat z ))
(defun-ogl :void "open-gl" "glRasterPos3i" (glint x glint y glint z ))
(defun-ogl :void "open-gl" "glRasterPos3s" (glshort x glshort y glshort z ))
(defun-ogl :void "open-gl" "glRasterPos4d" (gldouble x gldouble y gldouble z gldouble w ))
(defun-ogl :void "open-gl" "glRasterPos4f" (glfloat x glfloat y glfloat z glfloat w ))
(defun-ogl :void "open-gl" "glRasterPos4i" (glint x glint y glint z glint w ))
(defun-ogl :void "open-gl" "glRasterPos4s" (glshort x glshort y glshort z glshort w ))
(defun-ogl :void "open-gl" "glRasterPos2dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glRasterPos2fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glRasterPos2iv" (glint *v ))
(defun-ogl :void "open-gl" "glRasterPos2sv" (glshort *v ))
(defun-ogl :void "open-gl" "glRasterPos3dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glRasterPos3fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glRasterPos3iv" (glint *v ))
(defun-ogl :void "open-gl" "glRasterPos3sv" (glshort *v ))
(defun-ogl :void "open-gl" "glRasterPos4dv" (gldouble *v ))
(defun-ogl :void "open-gl" "glRasterPos4fv" (glfloat *v ))
(defun-ogl :void "open-gl" "glRasterPos4iv" (glint *v ))
(defun-ogl :void "open-gl" "glRasterPos4sv" (glshort *v ))
(defun-ogl :void "open-gl" "glRectd" (gldouble x1 gldouble y1 gldouble x2 gldouble y2 ))
(defun-ogl :void "open-gl" "glRectf" (glfloat x1 glfloat y1 glfloat x2 glfloat y2 ))
(defun-ogl :void "open-gl" "glRecti" (glint x1 glint y1 glint x2 glint y2 ))
(defun-ogl :void "open-gl" "glRects" (glshort x1 glshort y1 glshort x2 glshort y2 ))
(defun-ogl :void "open-gl" "glRectdv" (gldouble *v1 gldouble *v2 ))
(defun-ogl :void "open-gl" "glRectfv" (glfloat *v1 glfloat *v2 ))
(defun-ogl :void "open-gl" "glRectiv" (glint *v1 glint *v2 ))
(defun-ogl :void "open-gl" "glRectsv" (glshort *v1 glshort *v2 ))

#| lighting |#
(defun-ogl :void "open-gl" "glShadeModel" (glenum mode ))
(defun-ogl :void "open-gl" "glLightf" (glenum light glenum pname glfloat param ))
(defun-ogl :void "open-gl" "glLighti" (glenum light glenum pname glint param ))
(defun-ogl :void "open-gl" "glLightfv" (glenum light glenum pname glfloat *params ))
(defun-ogl :void "open-gl" "glLightiv" (glenum light glenum pname glint *params ))
(defun-ogl :void "open-gl" "glGetLightfv" (glenum light glenum pname glfloat *params ))
(defun-ogl :void "open-gl" "glGetLightiv" (glenum light glenum pname glint *params ))
(defun-ogl :void "open-gl" "glLightModelf" (glenum pname glfloat param ))
(defun-ogl :void "open-gl" "glLightModeli" (glenum pname glint param ))
(defun-ogl :void "open-gl" "glLightModelfv" (glenum pname glfloat *params ))
(defun-ogl :void "open-gl" "glLightModeliv" (glenum pname glint *params ))
(defun-ogl :void "open-gl" "glMaterialf" (glenum face glenum pname glfloat param ))
(defun-ogl :void "open-gl" "glMateriali" (glenum face glenum pname glint param ))

(defun-ogl :void "open-gl" "glMaterialiv" (glenum face glenum pname glint *params ))
(defun-ogl :void "open-gl" "glGetMaterialfv" (glenum face glenum pname glfloat *params ))
(defun-ogl :void "open-gl" "glGetMaterialiv" (glenum face glenum pname glint *params ))
(defun-ogl :void "open-gl" "glColorMaterial" (glenum face glenum mode ))

#| fog |#
(defun-ogl :void "open-gl" "glFogf" (glenum pname glfloat param))
(defun-ogl :void "open-gl" "glFogi" (glenum pname glint param))
(defun-ogl :void "open-gl" "glFogfv" (glenum pname glfloat *params))
(defun-ogl :void "open-gl" "glFogiv" (glenum pname glint *params))

#| selection and feedback |#
(defun-ogl :void "open-gl" "glFeedbackBuffer" (glsizei size glenum type glfloat *buffer))
(defun-ogl :void "open-gl" "glPassThrough" (glfloat token))
(defun-ogl :void "open-gl" "glSelectBuffer" (glsizei size gluint *buffer ))
(defun-ogl :void "open-gl" "glInitNames" ())
(defun-ogl :void "open-gl" "glLoadName" (gluint name))
(defun-ogl :void "open-gl" "glPushName" (gluint name))
(defun-ogl :void "open-gl" "glPopName" ())

#| miscellaneous |#
(defun-ogl :void "open-gl" "glClearIndex" (glfloat c ))
(defun-ogl :void "open-gl" "glClearColor" (glclampf red glclampf green glclampf blue glclampf alpha ))
(defun-ogl :void "open-gl" "glClear" (glbitfield mask ))
(defun-ogl :void "open-gl" "glIndexMask" (gluint mask ))
(defun-ogl :void "open-gl" "glColorMask" (glboolean red glboolean green glboolean blue glboolean alpha ))
(defun-ogl :void "open-gl" "glAlphaFunc" (glenum func glclampf ref ))
(defun-ogl :void "open-gl" "glBlendFunc" (glenum sfactor glenum dfactor ))
(defun-ogl :void "open-gl" "glLogicOp" (glenum opcode ))
(defun-ogl :void "open-gl" "glCullFace" (glenum mode ))
(defun-ogl :void "open-gl" "glFrontFace" (glenum mode ))
(defun-ogl :void "open-gl" "glPointSize" (glfloat size ))
(defun-ogl :void "open-gl" "glLineWidth" (glfloat width ))
(defun-ogl :void "open-gl" "glLineStipple" (glint factor glushort pattern ))
(defun-ogl :void "open-gl" "glPolygonMode" (glenum face glenum mode ))
(defun-ogl :void "open-gl" "glPolygonOffset" (glfloat factor glfloat units ))
(defun-ogl :void "open-gl" "glPolygonStipple" (glubyte *mask ))
(defun-ogl :void "open-gl" "glGetPolygonStipple" (glubyte *mask ))
(defun-ogl :void "open-gl" "glEdgeFlag" (glboolean flag ))
(defun-ogl :void "open-gl" "glEdgeFlagv" (glboolean *flag ))
(defun-ogl :void "open-gl" "glScissor" (glint x glint y glsizei width glsizei height))
(defun-ogl :void "open-gl" "glClipPlane" (glenum plane gldouble *equation ))
(defun-ogl :void "open-gl" "glGetClipPlane" (glenum plane gldouble *equation ))

(defun-ogl :void "open-gl" "glDrawArrays" (glenum mode glint first glsizei count))
(defun-ogl :void "open-gl" "glDrawBuffer" (glenum mode))


(defun-ogl :void "open-gl" "glDrawElements"
  (glenum mode glsizei count glenum type glvoid *indices))

(defun-ogl :void "open-gl" "glReadBuffer" (glenum mode ))
(defun-ogl :void "open-gl" "glEnable" (glenum cap ))
(defun-ogl :void "open-gl" "glDisable" (glenum cap ))
(defun-ogl :int "open-gl" "glIsEnabled" (glenum cap ))
(defun-ogl :void "open-gl" "glEnableClientState" (glenum cap ))  #| 1.1 |#
(defun-ogl :void "open-gl" "glDisableClientState" (glenum cap ))  #| 1.1 |#
(defun-ogl :void "open-gl" "glGetBooleanv" (glenum pname glboolean *params ))
(defun-ogl :void "open-gl" "glGetDoublev" (glenum pname gldouble *params ))
(defun-ogl :void "open-gl" "glGetFloatv" (glenum pname glfloat *params ))
(defun-ogl :void "open-gl" "glGetIntegerv" (glenum pname glint *params ))
(defun-ogl :void "open-gl" "glPushAttrib" (glbitfield mask ))
(defun-ogl :void "open-gl" "glPopAttrib"  ())
(defun-ogl :void "open-gl" "glPushClientAttrib" (glbitfield mask ))  #| 1.1 |#
(defun-ogl :void "open-gl" "glPopClientAttrib"  ())  #| 1.1 |#
(defun-ogl glint "open-gl" "glRenderMode" (glenum mode ))
(defun-ogl glenum "open-gl" "glGetError"  ())
(defun-ogl (* glubyte) "open-gl" "glGetString" (glenum name ))
(defun-ogl :void "open-gl" "glFinish"  ())
(defun-ogl :void "open-gl" "glHint" (glenum target glenum mode ))

#| depth buffer |#
(defun-ogl :void "open-gl" "glClearDepth" (glclampd depth ))
(defun-ogl :void "open-gl" "glDepthFunc" (glenum func ))
(defun-ogl :void "open-gl" "glDepthMask" (glboolean flag ))
(defun-ogl :void "open-gl" "glDepthRange" (glclampd near_val glclampd far_val ))

#| accumulation buffer |#
(defun-ogl :void "open-gl" "glClearAccum" (glfloat red glfloat green glfloat blue glfloat alpha ))
(defun-ogl :void "open-gl" "glAccum" (glenum op glfloat value ))

#| transformation |#
(defun-ogl :void "open-gl" "glMatrixMode" (glenum mode ))
(defun-ogl :void "open-gl" "glOrtho" (gldouble left gldouble right gldouble bottom gldouble top gldouble near_val gldouble far_val ))
(defun-ogl :void "open-gl" "glFrustum" (gldouble left gldouble right gldouble bottom gldouble top gldouble near_val gldouble far_val ))
(defun-ogl :void "open-gl" "glViewport" (glint x glint y glsizei width glsizei height ))
(defun-ogl :void "open-gl" "glPushMatrix" ( ))
(defun-ogl :void "open-gl" "glPopMatrix" ( ))
(defun-ogl :void "open-gl" "glLoadIdentity" ( ))
(defun-ogl :void "open-gl" "glLoadMatrixd" (gldouble *m ))
(defun-ogl :void "open-gl" "glLoadMatrixf" (glfloat *m ))
(defun-ogl :void "open-gl" "glMultMatrixd" (gldouble *m ))
(defun-ogl :void "open-gl" "glMultMatrixf" (glfloat *m ))
(defun-ogl :void "open-gl" "glRotated" (gldouble angle gldouble x gldouble y gldouble z ))
(defun-ogl :void "open-gl" "glRotatef" (glfloat angle glfloat x glfloat y glfloat z ))
(defun-ogl :void "open-gl" "glScaled" (gldouble x gldouble y gldouble z ))
(defun-ogl :void "open-gl" "glScalef" (glfloat x glfloat y glfloat z ))
(defun-ogl :void "open-gl" "glTranslated" (gldouble x gldouble y gldouble z ))
(defun-ogl :void "open-gl" "glTranslatef" (glfloat x glfloat y glfloat z ))
#+diehard (DEFUN-FFX :VOID "open-gl" "glTranslatef" (GLFLOAT X GLFLOAT Y GLFLOAT Z)
            (PROGN (GLEC '|glTranslatef|)
              (trc (or (not (zerop x))(not (zerop y))) "TRANSLATED" x y z)))
(defun-ogl :void "open-gl" "glBitmap" (glsizei width glsizei height
                                         glfloat xorig glfloat yorig
                                         glfloat xmove glfloat ymove
					 glvoid *bitmap))

(defun-ogl :void "open-gl" "glReadPixels" ( glint x glint y glsizei width glsizei height glenum format glenum type glvoid *pixels ))

(defun-ogl :void "open-gl" "glDrawPixels"
  (glsizei width glsizei height glenum format glenum type glvoid *pixels))
(defun-ogl :void "open-gl" "glCopyPixels" ( glint x glint y glsizei width glsizei height glenum type ))

#| stenciling |#

(defun-ogl :void "open-gl" "glStencilFunc" ( glenum func glint ref gluint mask ))
(defun-ogl :void "open-gl" "glStencilMask" ( gluint mask ))
(defun-ogl :void "open-gl" "glStencilOp" ( glenum fail glenum zfail glenum zpass ))
(defun-ogl :void "open-gl" "glClearStencil" ( glint s ))

(defun-ogl :void "open-gl" "glPixelMapfv" (glenum map glsizei mapsize glfloat *values))
(defun-ogl :void "open-gl" "glPixelMapuiv" (glenum map glsizei mapsize gluint *values))
(defun-ogl :void "open-gl" "glPixelMapusv" (glenum map glsizei mapsize glushort *values))
(defun-ogl :void "open-gl" "glPixelStoref" (glenum pname glfloat param))
(defun-ogl :void "open-gl" "glPixelStorei" (glenum pname glint param))
(defun-ogl :void "open-gl" "glPixelTransferf" (glenum pname glfloat param))
(defun-ogl :void "open-gl" "glPixelTransferi" (glenum pname glint param))
(defun-ogl :void "open-gl" "glPixelZoom" (glfloat xfactor glfloat yfactor))

#| display lists |#
(defun-ogl :int "open-gl" "glIsList" (gluint list))
(defun-ogl :void "open-gl" "glDeleteLists" (gluint list glsizei range ))
(defun-ogl gluint "open-gl" "glGenLists" (glsizei range ))
(defun-ogl :void "open-gl" "glNewList" (gluint list glenum mode ))
(defun-ogl :void "open-gl" "glEndList" ())
(defun-ogl :void "open-gl" "glCallList" (gluint list ))
(defun-ogl :void "open-gl" "glCallLists" (glsizei n glenum type glvoid *lists))
(defun-ogl :void "open-gl" "glListBase" (gluint base))
