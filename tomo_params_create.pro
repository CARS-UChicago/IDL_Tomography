;+
; NAME:
;   TOMO_PARAMS__DEFINE
;
; PURPOSE:
;   Defines a structure which controls tomography reconstruction parameters for tomo_recon.
;   This structure is passed directly to the C++ code in the shareable library.
;
; MODIFICATION HISTORY:
;   Written by:     Mark Rivers, August 1, 2012
;-

function tomo_params_create
  tp = {tomo_params,           $

    ; Sinogram parameters
    numPixels:              0L, $ ; Number of pixels in sinogram row before padding
    numProjections:         0L, $ ; Number of angles
    numSlices:              0L, $ ; Number of slices
    sinoScale:          10000., $ ; Scale factor to multiply sinogram when airPixels=0;
    reconScale:           1.e6, $ ; Scale factor to multiple reconstruction
    paddedSinogramWidth:    1L, $ ; Number of pixels in sinogram after padding
                                  ; There are 2 "special" values of paddedSinogramWidth
                                  ; 0 = automatically set the width to power of 2 that is >= actual width
                                  ; 1 = No padding, set the width to the actual width
    paddingAverage:        10L, $ ; Number of pixels to average on each side of sinogram to compute padding. 0 pixels pads with 0.0 
    airPixels:             10L, $ ; Number of pixels of air to average at each end of sinogram row
    ringWidth:              9L, $ ; Number of pixels to smooth by when removing ring artifacts
    fluorescence:           0L, $ ; 0=absorption data, 1=fluorescence
    
    ; Reconstruction method
    reconMethod: 0L,            $ ; 0=tomoRecon, 1=Gridrec, 2=Backproject
    reconMethodTomoRecon:   0L, $
    reconMethodGridrec:     1L, $
    reconMethodBackproject: 2L, $
    
    ; tomoRecon parameters
    numThreads:             8L, $
    slicesPerChunk:       128L, $
    debug:                  0L, $
    debugFile:     bytarr(256), $
    
    ; gridRec/tomoRecon parameters
    geom:                   0L, $ ; 0 if array of angles provided;
                                  ; 1,2 if uniform in half,full circle
    pswfParam:             6.0, $ ; PSWF parameter
    sampl:                 1.0, $ ; "Oversampling" ratio
    maxPixSize:            1.0, $ ; Max pixel size for reconstruction
    ROI:                   1.0, $ ; Region of interest (ROI) relative size
    X0:                    0.0, $ ; (X0,Y0)=Offset of ROI from rotation axis
    Y0:                    0.0, $ ; in units of center-to-edge distance.
    ltbl:                 512L, $ ; No. elements in convolvent lookup tables
    GR_filterName:  bytarr(16), $ ; Name of filter function - initialized to "shepp" below
    
    ; Backproject parameters
    BP_Method:                    0L, $ ; 0=Riemann, 1=Radon
    BP_MethodRiemann:             0L, $
    BP_MethodRadon:               1L, $
    BP_filterName:        bytarr(16), $ ; Name of filter function - initialized to "SHEPP_LOGAN" below
    BP_filterSize:                0L, $ ; Length of filter
    RiemannInterpolation:         0L, $ ; 0=none, 1=bilinear, 2=cubic
    RiemannInterpolationNone:     0L, $
    RiemannInterpolationBilinear: 1L, $
    RiemannInterpolationCubic:    2L, $
    RadonInterpolation:           0L, $ ; 0=none, 1=linear
    RadonInterpolationNone:       0L, $
    RadonInterpolationLinear:     1L  $
  }
  tp.GR_filterName = [byte("shepp"),0B]
  tp.BP_filterName = [byte("SHEPP_LOGAN"),0B]

  return, tp
end
