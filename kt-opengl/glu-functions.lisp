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

(in-package :kt-opengl)

;;; ***           Generic constants               ****/

;;;  Errors: (return value 0 = no error) */
(dfc GLU_INVALID_ENUM        100900)
(dfc GLU_INVALID_VALUE       100901)
(dfc GLU_OUT_OF_MEMORY       100902)
(dfc GLU_INCOMPATIBLE_GL_VERSION     100903)

;;;  StringName */
(dfc GLU_VERSION             100800)
(dfc GLU_EXTENSIONS          100801)

;;;  Boolean */
(dfc GLU_TRUE                1)
(dfc GLU_FALSE               0)


;;; ***           Quadric constants               ****/

;;;  QuadricNormal */
(dfc GLU_SMOOTH              100000)
(dfc GLU_FLAT                100001)
(dfc GLU_NONE                100002)

;;;  QuadricDrawStyle */
(dfc GLU_POINT               100010)
(dfc GLU_LINE                100011)
(dfc GLU_FILL                100012)
(dfc GLU_SILHOUETTE          100013)

;;;  QuadricOrientation */
(dfc GLU_OUTSIDE             100020)
(dfc GLU_INSIDE              100021)

;;;  Callback types: */
;;;       GLU_ERROR               100103 */


;;; ***           Tesselation constants           ****/

;;(dfc GLU_TESS_MAX_COORD              1.0e150)

;;;  TessProperty */
(dfc GLU_TESS_WINDING_RULE           100140)
(dfc GLU_TESS_BOUNDARY_ONLY          100141)
(dfc GLU_TESS_TOLERANCE              100142)

;;;  TessWinding */
(dfc GLU_TESS_WINDING_ODD            100130)
(dfc GLU_TESS_WINDING_NONZERO        100131)
(dfc GLU_TESS_WINDING_POSITIVE       100132)
(dfc GLU_TESS_WINDING_NEGATIVE       100133)
(dfc GLU_TESS_WINDING_ABS_GEQ_TWO    100134)

;;;  TessCallback */
(dfc GLU_TESS_BEGIN          100100)  ;;;  void (CALLBACK*)(GLenum    type)  */
(dfc GLU_TESS_VERTEX         100101)  ;;;  void (CALLBACK*)(void      *data) */
(dfc GLU_TESS_END            100102)  ;;;  void (CALLBACK*)(void)            */
(dfc GLU_TESS_ERROR          100103)  ;;;  void (CALLBACK*)(GLenum    errno) */
(dfc GLU_TESS_EDGE_FLAG      100104)  ;;;  void (CALLBACK*)(GLboolean boundaryEdge)  */
(dfc GLU_TESS_COMBINE        100105)  ;;;  void (CALLBACK*)(GLdouble  coords[3],
                                      ;;;                      void      *data[4],
                                      ;;;                      GLfloat   weight[4],
                                      ;;;                      void      **dataOut)     */
(dfc GLU_TESS_BEGIN_DATA     100106)  ;;;  void (CALLBACK*)(GLenum    type,  
                                     ;;;                       void      *polygon_data) */
(dfc GLU_TESS_VERTEX_DATA    100107)  ;;;  void (CALLBACK*)(void      *data, 
                                     ;;;                       void      *polygon_data) */
(dfc GLU_TESS_END_DATA       100108)  ;;;  void (CALLBACK*)(void      *polygon_data) */
(dfc GLU_TESS_ERROR_DATA     100109)  ;;;  void (CALLBACK*)(GLenum    errno, 
                                     ;;;                       void      *polygon_data) */
(dfc GLU_TESS_EDGE_FLAG_DATA 100110)  ;;;  void (CALLBACK*)(GLboolean boundaryEdge,
                                     ;;;                       void      *polygon_data) */
(dfc GLU_TESS_COMBINE_DATA   100111)  ;;;  void (CALLBACK*)(GLdouble  coords[3],
                                     ;;;                       void      *data[4],
                                     ;;;                       GLfloat   weight[4],
                                     ;;;                       void      **dataOut,
                                     ;;;                       void      *polygon_data) */

;;;  TessError */
(dfc GLU_TESS_ERROR1     100151)
(dfc GLU_TESS_ERROR2     100152)
(dfc GLU_TESS_ERROR3     100153)
(dfc GLU_TESS_ERROR4     100154)
(dfc GLU_TESS_ERROR5     100155)
(dfc GLU_TESS_ERROR6     100156)
(dfc GLU_TESS_ERROR7     100157)
(dfc GLU_TESS_ERROR8     100158)

(dfc GLU_TESS_MISSING_BEGIN_POLYGON  GLU_TESS_ERROR1)
(dfc GLU_TESS_MISSING_BEGIN_CONTOUR  GLU_TESS_ERROR2)
(dfc GLU_TESS_MISSING_END_POLYGON    GLU_TESS_ERROR3)
(dfc GLU_TESS_MISSING_END_CONTOUR    GLU_TESS_ERROR4)
(dfc GLU_TESS_COORD_TOO_LARGE        GLU_TESS_ERROR5)
(dfc GLU_TESS_NEED_COMBINE_CALLBACK  GLU_TESS_ERROR6)

;;; ****           NURBS constants                 ****/

;;; NurbsProperty */
(dfc  GLU_AUTO_LOAD_MATRIX    100200)
(dfc  GLU_CULLING             100201)
(dfc  GLU_SAMPLING_TOLERANCE  100203)
(dfc  GLU_DISPLAY_MODE        100204)
(dfc  GLU_PARAMETRIC_TOLERANCE        100202)
(dfc  GLU_SAMPLING_METHOD             100205)
(dfc  GLU_U_STEP                      100206)
(dfc  GLU_V_STEP                      100207)


;;; NurbsSampling */
(dfc  GLU_PATH_LENGTH                 100215)
(dfc  GLU_PARAMETRIC_ERROR            100216)
(dfc  GLU_DOMAIN_DISTANCE             100217)

;;; NurbsTrim */
(dfc  GLU_MAP1_TRIM_2         100210)
(dfc  GLU_MAP1_TRIM_3         100211)

;;; NurbsDisplay */

(dfc  GLU_OUTLINE_POLYGON     100240)
(dfc  GLU_OUTLINE_PATCH       100241)

;;; NurbsCallback */
;;;      GLU_ERROR               100103 */

;;; NurbsErrors */
(dfc  GLU_NURBS_ERROR1        100251)
(dfc  GLU_NURBS_ERROR37       100287)

(defun-ogl (* glubyte)  "gl-util" "gluErrorString" (glenum error))
;;;(defun-ogl GLubyte  *"gl-util" "gluGetString" (GLenum name))
;;;(defun-ogl void "gl-util" "gluLoadSamplingMatrices" (GLUnurbs *nurb GLfloat *model GLfloat *perspective GLint *view))

(defun-ogl :int "gl-util" "gluBuild2DMipmaps" (glenum  target
                                               glint  components
                                               glint  width
                                               glint  height
                                               glenum format
                                               glenum type
                                               :void  *data))

(defun-ogl :void "gl-util" "gluCylinder" (:void *qobj gldouble base-radius
                                         gldouble top-radius
                                         gldouble height
                                         glint slices
                                         glint stacks))

(defun-ogl :void "gl-util" "gluDisk" (:void *qobj gldouble inner-radius
                                           gldouble outer-radius
                                           glint slices
                                           glint loops))

(defun-ogl :void "gl-util" "gluLookAt" (gldouble eye-x gldouble eye-y gldouble eye-z 
                                       gldouble center-x gldouble center-y gldouble center-z 
                                       gldouble upx gldouble upy gldouble upz))

(defun-ogl (* :void) "gl-util" "gluNewQuadric" ())

(defun-ogl :void "gl-util" "gluDeleteQuadric" (:void *quadric))

(defun-ogl (* :void) "gl-util" "gluNewNurbsRenderer" ())
(defun-ogl :void "gl-util" "gluDeleteNurbsRenderer" (:void *nurb))
(defun-ogl :void "gl-util" "gluBeginSurface" (:void *nurb))
(defun-ogl :void "gl-util" "gluEndSurface" (:void *nurb))
(defun-ogl :void "gl-util" "gluBeginCurve" (:void *nurb))
(defun-ogl :void "gl-util" "gluEndCurve" (:void *nurb))
(defun-ogl :void "gl-util" "gluBeginTrim" (:void *nurb))
(defun-ogl :void "gl-util" "gluEndTrim" (:void *nurb))

(defun-ogl :void "gl-util" "gluGetNurbsProperty" (:void *nurb GLenum property GLfloat *data))
(defun-ogl :void "gl-util" "gluNurbsCurve" (:void *nurb GLint knotCount GLfloat *knots GLint stride GLfloat *control GLint order GLenum type))
(defun-ogl :void "gl-util" "gluNurbsProperty" (:void *nurb GLenum property GLfloat value))
(defun-ogl :void "gl-util" "gluNurbsSurface" (:void *nurb GLint sKnotCount GLfloat *sKnots GLint tKnotCount GLfloat *tKnots GLint sStride GLint tStride GLfloat *control GLint sOrder GLint tOrder GLenum type))

(defun-ogl :void "gl-util" "gluOrtho2D" (GLdouble left GLdouble right 
                                        GLdouble bottom GLdouble top))
;;;(defun-ogl void "gl-util" "gluPartialDisk" (GLUquadric *quad GLdouble inner GLdouble outer GLint slices GLint loops GLdouble start GLdouble sweep))

(defun-ogl :void "gl-util" "gluPerspective" (gldouble fovy gldouble aspect gldouble z-near gldouble z-far))
(defun-ogl :void "gl-util" "gluPickMatrix" (gldouble x gldouble y gldouble del-x gldouble del-y glint *viewport))
(defun-ogl glint "gl-util" "gluProject" (gldouble obj-x gldouble obj-y gldouble obj-z 
                                        gldouble *model gldouble *proj 
                                        glint *view gldouble *winx gldouble *winy gldouble *winz))

;;;(defun-ogl void "gl-util" "gluPwlCurve" (GLUnurbs *nurb GLint count GLfloat *data GLint stride GLenum type))
;;;(defun-ogl void "gl-util" "gluQuadricDrawStyle" (GLUquadric *quad GLenum draw))
;;;(defun-ogl void "gl-util" "gluQuadricNormals" (GLUquadric *quad GLenum normal))
;;;(defun-ogl void "gl-util" "gluQuadricOrientation" (GLUquadric *quad GLenum orientation))
(defun-ogl :void "gl-util" "gluQuadricTexture" (:void *quad glboolean texture))
;;;(defun-ogl GLint "gl-util" "gluScaleImage" (GLenum format GLsizei wIn GLsizei hIn GLenum typeIn void *dataIn GLsizei wOut GLsizei hOut GLenum typeOut GLvoid *dataOut))
;;;(defun-ogl void "gl-util" "gluSphere" (GLUquadric *quad GLdouble radius GLint slices GLint stacks))

(defun-ogl glint "gl-util" "gluUnProject" (gldouble winx gldouble winy gldouble winz 
                                            gldouble *model gldouble *proj 
                                            glint *view gldouble *obj-x gldouble *obj-y gldouble *obj-z))

; yusuke

(defun-ogl (* :void) "gl-util" "gluNewTess" ())
(defun-ogl :void "gl-util" "gluDeleteTess" (:void *tess))
(defun-ogl :void "gl-util" "gluNextContour" (:void *tess GLenum type))
(defun-ogl :void "gl-util" "gluGetTessProperty" (:void *tess GLenum which GLdouble *data))
(defun-ogl :void "gl-util" "gluTessBeginContour" (:void *tess))
(defun-ogl :void "gl-util" "gluTessBeginPolygon" (:void *tess GLvoid *data))
(defun-ogl :void "gl-util" "gluTessEndContour" (:void *tess))
(defun-ogl :void "gl-util" "gluTessEndPolygon" (:void *tess))
(defun-ogl :void "gl-util" "gluTessNormal" (:void *tess GLdouble valueX GLdouble valueY GLdouble valueZ))
(defun-ogl :void "gl-util" "gluTessProperty" (:void *tess GLenum which GLdouble data))
(defun-ogl :void "gl-util" "gluTessVertex" (:void *tess GLdouble *location GLvoid *data))
(defun-ogl :void "gl-util" "gluTessCallback" (:void *tess GLenum which :void *callback))
