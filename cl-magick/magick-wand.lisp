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

#|
  ImageMagick MagickWand interface.
|#

(in-package :cl-magick)

;;; these could all be moved into magick-wand.lisp
;;;


(defun-ffx :unsigned-int "imagick" "MagickReadImage"
  (:void *wand :string filename))

(defun-ffx :unsigned-int "imagick" "MagickNextImage" (:void *wand))
(defun-ffx :unsigned-int "imagick" "MagickPreviousImage" (:void *wand))
(defun-ffx :unsigned-long "imagick" "MagickGetNumberOfImages" (:void *wand ))
(defun-ffx :unsigned-int "imagick" "MagickRotateImage" ( :void *wand :double degrees ))
(defun-ffx :unsigned-int "imagick" "MagickGetImagePixels"
  (:void *wand :long x_offset :long y_offset :unsigned-long columns
    :unsigned-long rows :string map :unsigned-int storage :void *pixels ))
(defun-ffx :void "open-gl" "MagickGetSize" (:void *wand :unsigned-long *columns :unsigned-long *rows ))
(defun-ffx :void "open-gl" "MagickSetSize" (:void *wand :unsigned-long columns :unsigned-long rows ))


;;;typedef struct _MagickWand
;;;  MagickWand;
;;;
;;;extern WandExport char

(ffx::defun-ffx-multi :string "imagick"
  "MagickDescribeImage" (:void *wand)
  ;;;  *MagickGetConfigureInfo(:void *,const char *),
  ;;;  *MagickGetException(const :void *,ExceptionType *),
  ;;;  *MagickGetFilename(const :void *),
  "MagickGetImageFilename" (:void *wand)
  "MagickGetImageFormat" (:void *wand)
  ;;;  *MagickGetImageSignature(:void *),
  ;;;  **MagickQueryFonts(const char *,unsigned long *),
  ;;;  **MagickQueryFormats(const char *,unsigned long *);
  )

;;;extern WandExport CompositeOperator
;;;  MagickGetImageCompose(:void *);
;;;
;;;extern WandExport ColorspaceType
;;;  MagickGetImageColorspace(:void *);
;;;
;;;extern WandExport CompressionType
;;;  MagickGetImageCompression(:void *);
;;;
;;;extern WandExport const char
(defun-ffx-multi :string "imagick"
  "MagickGetCopyright" ()
;;;  *MagickGetHomeURL(void),
;;;  *MagickGetPackageName(void),
;;;  *MagickGetQuantumDepth(unsigned long *),
;;;  *MagickGetReleaseDate(void),
  "MagickGetVersion" (:unsigned-long *version)
  )
;;;
;;;extern WandExport DisposeType
;;;  MagickGetImageDispose(:void *);
;;;
;;;extern WandExport double
;;;  MagickGetImageGamma(:void *),
;;;  *MagickGetSamplingFactors(:void *,unsigned long *),
;;;  *MagickQueryFontMetrics(:void *,const DrawingWand *,const char *);
;;;
(defun-ffx :unsigned-int "imagick"
  "MagickGetImageType" (:void *wand))
;;;
;;;extern WandExport InterlaceType
;;;  MagickGetImageInterlaceScheme(:void *);
;;;

(defun-ffx :unsigned-long "imagick" "MagickGetImageIndex" (:void *wand))
(defun-ffx (* :void) "imagick" "NewMagickWand" ())

;;;extern WandExport MagickSizeType
;;;  MagickGetImageSize(:void *);
;;;
(defun-ffx-multi (* :void) "imagick"
;;;  "NewMagickWand" ()
;;;extern WandExport MagickWand
  "CloneMagickWand" (:void *wand)
;;;  *MagickAppendImages(:void *const :unsigned-int)
;;;  *MagickAverageImages(:void *)
;;;  *MagickCoalesceImages(:void *)
;;;  *MagickCompareImageChannels(:void *const :void *const ChannelType
;;;    const MetricTypedouble *)
;;;  *MagickCompareImages(:void *const :void *const MetricType
;;;    double *)
;;;  *MagickDeconstructImages(:void *)
;;;  *MagickFlattenImages(:void *)
  "MagickGetImage" (:void *wand)
;;;  *MagickMorphImages(:void *const :unsigned-long)
;;;  *MagickMosaicImages(:void *)
;;;  *MagickMontageImage(:void *const DrawingWand *const char *
;;;    const char *const MontageModeconst char *)
;;;  *MagickPreviewImages(:void *wandconst PreviewType)
;;;  *MagickSteganoImage(:void *const :void *const long)
;;;  *MagickStereoImage(:void *const :void *)
;;;  *MagickTextureImage(:void *const :void *)
;;;  *MagickTransformImage(:void *const char *const char *)
  )
;;;
;;;extern WandExport PixelWand
;;;  **MagickGetImageHistogram(:void *:unsigned-long *);
;;;
;;;extern WandExport RenderingIntent
;;;  MagickGetImageRenderingIntent(:void *);
;;;
;;;extern WandExport ResolutionType
;;;  MagickGetImageUnits(:void *);
;;;

;;;
;;;extern WandExport VirtualPixelMethod
;;;  MagickGetImageVirtualPixelMethod(:void *);
;;;
;;;extern WandExport :unsigned-char
;;;  *MagickGetImageProfile(:void *const char *:unsigned-long *)
;;;  *MagickRemoveImageProfile(:void *const char *:unsigned-long *)
;;;  *MagickWriteImageBlob(:void *size_t *);
;;;
(defun-ffx :void "imagick" "MagickResetIterator" (:void *wand))
;;;

(ffx::defun-ffx-multi :unsigned-int "imagick"
  "DestroyMagickWand" (:void *wand)
;;;  "MagickAdaptiveThresholdImage" (:void *wand :unsigned-long
;;;     :unsigned-long long)
  "MagickAddImage" (:void *wand :void *splice-wand)
;;;  "MagickAddNoiseImage" (:void *wand NoiseType)
;;;  "MagickAffineTransformImage" (:void *wand DrawingWand *)
;;;  "MagickAnnotateImage" (:void *wand DrawingWand * double
;;;     double double char *)
;;;  "MagickAnimateImages" (:void *wand char *)
;;;  "MagickBlackThresholdImage" (:void *wand PixelWand *)
  "MagickBlurImage" (:void *wand :double radius :double sigma)
  "MagickBorderImage" (:void *wand :void *pixelwand :unsigned-long width
                        :unsigned-long height)
;;;  "MagickChannelImage" (:void *wand ChannelType)
  "MagickCharcoalImage" (:void *wand :double radius :double sigma)
;;;  "MagickChopImage" (:void *wand :unsigned-long :unsigned-long
;;;     long long)
;;;  "MagickClipImage" (:void *wand)
;;;  "MagickClipPathImage" (:void *wand char * :unsigned-int)
;;;  "MagickColorFloodfillImage" (:void *wand PixelWand * double
;;;     PixelWand * long long)
  "MagickColorizeImage" (:void *magicwand :void *colorpixelwand :void *opacitypixelwand)
;;;  "MagickCommentImage" (:void *wand char *)
;;;  "MagickCompositeImage" (:void *wand :void *wand CompositeOperator
;;;     long long)
  "MagickContrastImage" (:void *wand :unsigned-int sharpen)
;;;  "MagickConvolveImage" (:void *wand :unsigned-long double *)
  "MagickCropImage" (:void *wand :unsigned-long width :unsigned-long height
                      :long offset-x :long offset-y)
;;;  "MagickCycleColormapImage" (:void *wand long)
  "MagickDespeckleImage" (:void *wand)
;;;  "MagickDisplayImage" (:void *wand char *)
;;;  "MagickDisplayImages" (:void *wand char *)
  "MagickDrawImage" (:void *wand :void *drawingwand)
;;;  "MagickEdgeImage" (:void *wand double)
  "MagickEmbossImage" (:void *wand :double radius :double sigma)
;;;  "MagickEnhanceImage" (:void *wand)
;;;  "MagickEqualizeImage" (:void *wand)
  "MagickFlipImage" (:void *wand)
  "MagickFlopImage" (:void *wand)
  "MagickFrameImage" (:void *m-wand :void *p-wand :unsigned-long width
                       :unsigned-long height :long inner-bevel :long outer-bevel)
;;;  "MagickFxImageChannel" (:void *wand :void *wand ChannelType
;;;     char *)
;;;  "MagickGammaImage" (:void *wand double)
;;;  "MagickGammaImageChannel" (:void *wand ChannelType double)
;;;  "MagickGetImageBackgroundColor" (:void *wandPixelWand *)
;;;  "MagickGetImageBluePrimary" (:void *wanddouble *double *)
;;;  "MagickGetImageBorderColor" (:void *wandPixelWand *)
;;;  "MagickGetImageChannelExtrema" (:void *wand ChannelType:unsigned-long *
;;;    :unsigned-long *)
;;;  "MagickGetImageChannelMean" (:void *wand ChannelTypedouble *double *)
;;;  "MagickGetImageColormapColor" (:void *wand :unsigned-longPixelWand *)
;;;  "MagickGetImageExtrema" (:void *wand:unsigned-long *:unsigned-long *)
;;;  "MagickGetImageGreenPrimary" (:void *wanddouble *double *)
;;;  "MagickGetImageMatteColor" (:void *wandPixelWand *)
;;;  "MagickGetImageMean" (:void *wanddouble *double *)
;;;  "MagickGetImagePixels" (:void *wand long long :unsigned-long
;;;     :unsigned-long char * StorageType:unsigned-char *)
;;;  "MagickGetImageRedPrimary" (:void *wanddouble *double *)
;;;  "MagickGetImageResolution" (:void *wanddouble *double *)
;;;  "MagickGetImageWhitePoint" (:void *wanddouble *double *)
;;;  "MagickGetSize" ( :void *wand:unsigned-long *:unsigned-long *)
  "MagickHasNextImage" (:void *wand)
  "MagickHasPreviousImage" (:void *wand)
;;;  "MagickImplodeImage" (:void *wand double)
;;;  "MagickLabelImage" (:void *wand char *)
;;;  "MagickLevelImage" (:void *wand double double double)
;;;  "MagickLevelImageChannel" (:void *wand ChannelType double
;;;     double double)
  "MagickMagnifyImage" (:void *wand)
;;;  "MagickMapImage" (:void *wand :void *wand :unsigned-int)
;;;  "MagickMatteFloodfillImage" (:void *wand Quantum double
;;;     PixelWand * long long)
;;;  "MagickMedianFilterImage" (:void *wand double)
;;;  "MagickMinifyImage" (:void *wand)
;;;  "MagickModulateImage" (:void *wand double double double)
;;;  "MagickMotionBlurImage" (:void *wand double double double)
;;;  "MagickNegateImage" (:void *wand :unsigned-int)
;;;  "MagickNegateImageChannel" (:void *wand ChannelType :unsigned-int)
;;;  "MagickNextImage" (:void *wand)
;;;  "MagickNormalizeImage" (:void *wand)
;;;  "MagickOilPaintImage" (:void *wand double)
;;;  "MagickOpaqueImage" (:void *wand PixelWand * PixelWand *
;;;     double)
;;;  "MagickPingImage" (:void *wand char *)
;;;  "MagickPreviousImage" (:void *wand)
;;;  "MagickProfileImage" (:void *wand char * :unsigned-char *
;;;     :unsigned-long)
;;;  "MagickQuantizeImage" (:void *wand :unsigned-long ColorspaceType
;;;     :unsigned-long :unsigned-int :unsigned-int)
;;;  "MagickQuantizeImages" (:void *wand :unsigned-long ColorspaceType
;;;     :unsigned-long :unsigned-int :unsigned-int)
;;;  "MagickRaiseImage" (:void *wand :unsigned-long :unsigned-long
;;;     long long :unsigned-int)
;;;  "MagickReadImage" (:void *wand char *)
;;;  "MagickReadImageBlob" (:void *wand :unsigned-char * size_t length)
;;;  "MagickReadImageFile" (:void *wandFILE *)
;;;  "MagickReduceNoiseImage" (:void *wand double)
;;;  "MagickRelinquishMemory" (void *wand)
  "MagickRemoveImage" (:void *wand)
  "MagickResizeImage" (:void *wand :unsigned-long columns :unsigned-long rows
                        :unsigned-int filter ;; See filter-types enum
                        :double blur)
;;;  "MagickRollImage" (:void *wand long long)
;;;  "MagickRotateImage" (:void *wand PixelWand * double)
;;;  "MagickSampleImage" (:void *wand :unsigned-long :unsigned-long)
  "MagickScaleImage" (:void *wand :unsigned-long width :unsigned-long height)
  "MagickSetImage" (:void *dest-wand :void *source-wand)
  "MagickSetFilename" (:void *wand :string filename)
;;;  "MagickSetImageBackgroundColor" (:void *wand PixelWand *)
;;;  "MagickSetImageBluePrimary" (:void *wand double double)
;;;  "MagickSetImageBorderColor" (:void *wand PixelWand *)
;;;  "MagickSetImageChannelDepth" (:void *wand ChannelType
;;;     :unsigned-long)
;;;  "MagickSetImageColormapColor" (:void *wand :unsigned-long
;;;     PixelWand *)
;;;  "MagickSetImageCompose" (:void *wand CompositeOperator)
;;;  "MagickSetImageCompression" (:void *wand CompressionType)
  "MagickSetImageDelay" (:void *wand :unsigned-long delay)
  "MagickSetImageDepth" (:void *wand :unsigned-long depth)
;;;  "MagickSetImageDispose" (:void *wand DisposeType)
;;;  "MagickSetImageColorspace" (:void *wand ColorspaceType)
;;;  "MagickSetImageGreenPrimary" (:void *wand double double)
;;;  "MagickSetImageGamma" (:void *wand double)
  "MagickSetImageFilename" (:void *wand :string filename$)
  "MagickSetImageIndex" (:void *wand :unsigned-long index)
;;;  "MagickSetImageInterlaceScheme" (:void *wand InterlaceType)
;;;  "MagickSetImageIterations" (:void *wand :unsigned-long)
;;;  "MagickSetImageMatteColor" (:void *wand PixelWand *)
;;;  "MagickSetImageOption" (:void *wand char * char * char *)
  "MagickSetImagePixels" (:void *wand :long x_offset :long y_offset :unsigned-long columns
    :unsigned-long rows :string map :unsigned-int storage :void *pixels )
;;;  "MagickSetImageRedPrimary" (:void *wand double double)
;;;  "MagickSetImageRenderingIntent" (:void *wand RenderingIntent)
;;;  "MagickSetImageResolution" (:void *wand double double)
;;;  "MagickSetImageScene" (:void *wand :unsigned-long)
  "MagickSetImageType" (:void *wand :unsigned-int imagetype)
;;;  "MagickSetImageUnits" (:void *wand ResolutionType)
;;;  "MagickSetImageVirtualPixelMethod" (:void *wand VirtualPixelMethod)
;;;  "MagickSetPassphrase" (:void *wand char *)
;;;  "MagickSetImageProfile" (:void *wand char * :unsigned-char *
;;;     :unsigned-long)
;;;  "MagickSetResourceLimit" ( ResourceType type :unsigned-long limit)
;;;  "MagickSetSamplingFactors" (:void *wand :unsigned-long double *)
;;;  "MagickSetSize" (:void *wand :unsigned-long :unsigned-long)
;;;  "MagickSetImageWhitePoint" (:void *wand double double)
;;;  "MagickSetInterlaceScheme" (:void *wand InterlaceType)
;;;  "MagickSharpenImage" (:void *wand double double)
;;;  "MagickShaveImage" (:void *wand :unsigned-long :unsigned-long)
;;;  "MagickShearImage" (:void *wand PixelWand * double double)
;;;  "MagickSolarizeImage" (:void *wand double)
;;;  "MagickSpreadImage" (:void *wand double)
;;;  "MagickStripImage" (:void *wand)
;;;  "MagickSwirlImage" (:void *wand double)
;;;  "MagickTintImage" (:void *wand PixelWand * PixelWand *)
;;;  "MagickThresholdImage" (:void *wand double)
;;;  "MagickThresholdImageChannel" (:void *wand ChannelType double)
;;;  "MagickTransparentImage" (:void *wand PixelWand * Quantum
;;;     double)
;;;  "MagickTrimImage" (:void *wand double)
;;;  "MagickUnsharpMaskImage" (:void *wand double double double
;;;     double)
  "MagickWaveImage" (:void *wand :double amplitude :double length)
;;;  "MagickWhiteThresholdImage" (:void *wand PixelWand *)
  "MagickWriteImage" (:void *wand :string filename)
;;;  "MagickWriteImageFile" (:void *wand FILE *)
  "MagickWriteImages" (:void *wand :string filename :unsigned-int adjoinp) ;; 0=false
  )

(dfenum storagetype
  char-pixel
  short-pixel
  integer-pixel
  long-pixel
  float-pixel
  double-pixel)

(dfenum filtertypes
  undefined-filter
  point-filter
  box-filter
  triangle-filter
  hermite-filter
  hanning-filter
  hamming-filter
  blackman-filter
  gaussian-filter
  quadratic-filter
  cubic-filter
  catrom-filter
  mitchell-filter
  lanczos-filter
  bessel-filter
  sinc-filter)

(ffx::defun-ffx-multi :unsigned-long "imagick"
  "MagickGetImageColors" (:void *wand)
  "MagickGetImageDelay" (:void *wand)
;;;  "MagickGetImageChannelDepth" (:void *wand ChannelType)
  "MagickGetImageDepth" (:void *wand)
  "MagickGetImageHeight" (:void *wand)
  "MagickGetImageIterations" (:void *wand)
  "MagickGetImageScene" (:void *wand)
  "MagickGetImageWidth" (:void *wand)
  "MagickGetNumberImages" (:void *wand)
;;;  "MagickGetResourceLimit" ( ResourceType)
  )
