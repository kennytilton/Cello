;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cl-magick; -*-
;;;
;;; Copyright (c) 2004 by Kenneth William Tilton.
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

(in-package :cl-magick)

;;;/*
;;;  ImageMagick Drawing Wand API.
;;;*/
;;;#ifndef _MAGICK_DRAWING_WAND_H
;;;#define _MAGICK_DRAWING_WAND_H
;;;
;;;#if defined(__cplusplus) || defined(c_plusplus)
;;;extern "C" {
;;;#endif
;;;
;;;#include "wand/pixel_wand.h"
;;;
;;;typedef struct _DrawingWand
;;;  *DrawContext
;;;  DrawingWand;
;;;
;;;extern WandExport char
;;;  *DrawGetClipPath( :void *DrawingWand)
;;;  *DrawGetFont( :void *DrawingWand)
;;;  *DrawGetFontFamily( :void *DrawingWand)
;;;  *DrawGetTextEncoding( :void *DrawingWand);
;;;
;;;extern WandExport ClipPathUnits
;;;  DrawGetClipUnits( :void *DrawingWand);
;;;
;;;extern WandExport DecorationType
;;;  DrawGetTextDecoration( :void *DrawingWand);
;;;
;;;extern WandExport double
;;;  DrawGetFillOpacity( :void *DrawingWand)
;;;  DrawGetFontSize( :void *DrawingWand)
;;;  *DrawGetStrokeDashArray( :void *DrawingWandunsigned long *)
;;;  DrawGetStrokeDashOffset( :void *DrawingWand)
;;;  DrawGetStrokeOpacity( :void *DrawingWand)
;;;  DrawGetStrokeWidth( :void *DrawingWand);
;;;
;;;extern WandExport DrawInfo
;;;  *DrawPeekGraphicContext( :void *DrawingWand);
;;;
(defun-ffx (* :void) "imagick" "NewDrawingWand" ())
;;;extern WandExport DrawingWand
;;;  *DrawAllocateWand( DrawInfo *Image *)
;;;  *NewDrawingWand(void);
;;;
;;;extern WandExport FillRule
;;;  DrawGetClipRule( :void *DrawingWand)
;;;  DrawGetFillRule( :void *DrawingWand);
;;;
;;;extern WandExport GravityType
;;;  DrawGetGravity( :void *DrawingWand);
;;;
;;;extern WandExport LineCap
;;;  DrawGetStrokeLineCap( :void *DrawingWand);
;;;
;;;extern WandExport LineJoin
;;;  DrawGetStrokeLineJoin( :void *DrawingWand);
;;;
;;;extern WandExport StretchType
;;;  DrawGetFontStretch( :void *DrawingWand);
;;;
;;;extern WandExport StyleType
;;;  DrawGetFontStyle( :void *DrawingWand);
;;;
;;;extern WandExport :unsigned-int
;;;  DrawGetStrokeAntialias( :void *DrawingWand)
;;;  DrawGetTextAntialias( :void *DrawingWand)
;;;  DrawRender( :void *DrawingWand);
;;;
;;;extern WandExport :unsigned-long
;;;  DrawGetFontWeight( :void *DrawingWand)
;;;  DrawGetStrokeMiterLimit( :void *DrawingWand);
;;;
(ffx::defun-ffx-multi :void "imagick"
;;;  DrawAffine(:void *DrawingWand AffineMatrix *)
;;;  DrawAnnotation(:void *DrawingWand double double :unsigned-char *)
;;;  DrawArc(:void *DrawingWand double double double double
;;;     double double)
;;;  DrawBezier(:void *DrawingWand :unsigned-long PointInfo *)
;;;  DrawCircle(:void *DrawingWand double double double double)
;;;  DrawColor(:void *DrawingWand double double PaintMethod)
;;;  DrawComment(:void *DrawingWand char *)
;;;  DestroyDrawingWand(:void *DrawingWand)
  "DrawEllipse" (:void *drawingwand :double ox :double oy :double rx :double ry
                  :double start-angle :double end-angle)
;;;  DrawComposite(:void *DrawingWand CompositeOperator double double
;;;     double double Image *)
;;;  DrawGetFillColor( :void *DrawingWandPixelWand *)
;;;  DrawGetStrokeColor( :void *DrawingWandPixelWand *)
;;;  DrawGetTextUnderColor( :void *DrawingWandPixelWand *)
;;;  DrawLine(:void *DrawingWand double  double double double)
;;;  DrawMatte(:void *DrawingWand double double PaintMethod)
;;;  DrawPathClose(:void *DrawingWand)
;;;  DrawPathCurveToAbsolute(:void *DrawingWand double double double
;;;     double double double)
;;;  DrawPathCurveToRelative(:void *DrawingWand double double double
;;;     double double  double)
;;;  DrawPathCurveToQuadraticBezierAbsolute(:void *DrawingWand double
;;;     double double double)
;;;  DrawPathCurveToQuadraticBezierRelative(:void *DrawingWand double
;;;     double double double)
;;;  DrawPathCurveToQuadraticBezierSmoothAbsolute(:void *DrawingWand double
;;;     double)
;;;  DrawPathCurveToQuadraticBezierSmoothRelative(:void *DrawingWand double
;;;     double)
;;;  DrawPathCurveToSmoothAbsolute(:void *DrawingWand double double
;;;     double double)
;;;  DrawPathCurveToSmoothRelative(:void *DrawingWand double double
;;;     double double)
;;;  DrawPathEllipticArcAbsolute(:void *DrawingWand double double
;;;     double:unsigned-int:unsigned-int double double)
;;;  DrawPathEllipticArcRelative(:void *DrawingWand double double
;;;     double:unsigned-int:unsigned-int double double)
;;;  DrawPathFinish(:void *DrawingWand)
;;;  DrawPathLineToAbsolute(:void *DrawingWand double double)
;;;  DrawPathLineToRelative(:void *DrawingWand double double)
;;;  DrawPathLineToHorizontalAbsolute(:void *DrawingWand double)
;;;  DrawPathLineToHorizontalRelative(:void *DrawingWand double)
;;;  DrawPathLineToVerticalAbsolute(:void *DrawingWand double)
;;;  DrawPathLineToVerticalRelative(:void *DrawingWand double)
;;;  DrawPathMoveToAbsolute(:void *DrawingWand double double)
;;;  DrawPathMoveToRelative(:void *DrawingWand double double)
;;;  DrawPathStart(:void *DrawingWand)
;;;  DrawPoint(:void *DrawingWand double double)
;;;  DrawPolygon(:void *DrawingWand :unsigned-long PointInfo *)
;;;  DrawPolyline(:void *DrawingWand :unsigned-long PointInfo *)
;;;  DrawPopClipPath(:void *DrawingWand)
;;;  DrawPopDefs(:void *DrawingWand)
;;;  DrawPopGraphicContext(:void *DrawingWand)
;;;  DrawPopPattern(:void *DrawingWand)
;;;  DrawPushClipPath(:void *DrawingWand char *)
;;;  DrawPushDefs(:void *DrawingWand)
;;;  DrawPushGraphicContext(:void *DrawingWand)
;;;  DrawPushPattern(:void *DrawingWand char * double double
;;;     double double)
;;;  DrawRectangle(:void *DrawingWand double double double
;;;     double)
;;;  DrawRotate(:void *DrawingWand double)
;;;  DrawRoundRectangle(:void *DrawingWanddoubledoubledoubledoubledoubledouble)
;;;  DrawScale(:void *DrawingWand double double)
;;;  DrawSetClipPath(:void *DrawingWand char *)
;;;  DrawSetClipRule(:void *DrawingWand FillRule)
;;;  DrawSetClipUnits(:void *DrawingWand ClipPathUnits)
;;;  DrawSetFillColor(:void *DrawingWand PixelWand *)
;;;  DrawSetFillOpacity(:void *DrawingWand double)
;;;  DrawSetFillRule(:void *DrawingWand FillRule)
;;;  DrawSetFillPatternURL(:void *DrawingWand char *)
;;;  DrawSetFont(:void *DrawingWand char *)
;;;  DrawSetFontFamily(:void *DrawingWand char *)
;;;  DrawSetFontSize(:void *DrawingWand double)
;;;  DrawSetFontStretch(:void *DrawingWand StretchType)
;;;  DrawSetFontStyle(:void *DrawingWand StyleType)
;;;  DrawSetFontWeight(:void *DrawingWand :unsigned-long)
;;;  DrawSetGravity(:void *DrawingWand GravityType)
;;;  DrawSkewX(:void *DrawingWand double)
;;;  DrawSkewY(:void *DrawingWand double)
;;;  DrawSetStrokeAntialias(:void *DrawingWand :unsigned-int)
;;;  DrawSetStrokeColor(:void *DrawingWand PixelWand *)
;;;  DrawSetStrokeDashArray(:void *DrawingWand :unsigned-long double *)
;;;  DrawSetStrokeDashOffset(:void *DrawingWand double dashoffset)
;;;  DrawSetStrokeLineCap(:void *DrawingWand LineCap)
;;;  DrawSetStrokeLineJoin(:void *DrawingWand LineJoin)
;;;  DrawSetStrokeMiterLimit(:void *DrawingWand :unsigned-long)
;;;  DrawSetStrokeOpacity(:void *DrawingWand  double)
;;;  DrawSetStrokePatternURL(:void *DrawingWand char *)
;;;  DrawSetStrokeWidth(:void *DrawingWand double)
;;;  DrawSetTextAntialias(:void *DrawingWand :unsigned-int)
;;;  DrawSetTextDecoration(:void *DrawingWand DecorationType)
;;;  DrawSetTextEncoding(:void *DrawingWand char *)
;;;  DrawSetTextUnderColor(:void *DrawingWand PixelWand *)
;;;  DrawSetViewbox(:void *DrawingWand:unsigned-long:unsigned-long:unsigned-long
;;;    :unsigned-long)
;;;  DrawTranslate(:void *DrawingWand double double);
  )

(let (drw)
  (defun drw-ellipse (ox oy rx ry start-a end-a)
    (setq drw (or (new-drawing-wand)))
    (draw-ellipse drw ox oy rx ry start-a end-a)
    drw))