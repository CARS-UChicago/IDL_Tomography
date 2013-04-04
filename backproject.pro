function backproject, tomoParams, sinogram, angles, center = center

;+
; NAME:
;   BACKPROJECT
;
; PURPOSE:
;   Reconstructs a sinogram into an image using backprojection.
;
; CATEGORY:
;   Tomography data processing
;
; CALLING SEQUENCE:
;   Result = BACKPROJECT(tomoParams, Sinogram, Angles)
;
; INPUTS:
;   Sinogram:
;       The input sinogram, dimensions NX x NANGLES.  This should have been 
;       filtered before calling this function.
;   Angles:
;       An array of dimensions NANGLES which contains the angle in degrees of 
;       each projection.
;
; OUTPUTS:
;   This function returns the reconstructed image.
;
; PROCEDURE:
;   This function simply calls the IDL RIEMANN procedure for each row in the 
;   sinogram, using the ROW and BACKPROJECT keywords.  The BILINEAR and CUBIC 
;   keywords are passed from BACKPROJECT to RIEMANN if they are present.
;
; EXAMPLE:
;   r = backproject(sinogram, angles)
;
; MODIFICATION HISTORY:
;   Written by:     Mark Rivers, May 13, 1998
;-

nrho = tomoParams.numPixels
nangles = tomoParams.numProjections

if (tomoParams.BP_Method eq tomoParams.BP_MethodRiemann) then begin ; in case of Riemann backprojection
    b = fltarr(nrho, nrho)      ;Initial reconstructed image.
    if n_elements(center) then ctr = center
    for i=0,nangles-1 do begin
            riemann, sinogram, b, angles[i]*!dtor, row=i, /backproject, $
                     center=ctr, $
                     bilinear=(tomoParams.RiemannInterpolation eq tomoParams.RiemannInterpolationBilinear), $
                     cubic=(tomoParams.RiemannInterpolation eq tomoParams.RiemannInterpolationCubic)
    endfor
    b = b*!pi/nangles

endif else if(tomoParams.BP_Method eq tomoParams.BP_MethodRadon) then begin ; in case of Radon backprojection
    if (n_elements(center) eq 0) then center = (nrho-1)/2.
    rhos = (findgen(nrho) - center)
    rangles = angles*!dtor ; convert angles to radians
    b = radon(transpose(sinogram), /backproject, rho=rhos, theta=rangles, nx=nrho, ny=nrho, $
                        linear=(tomoParams.RadonInterpolation eq tomoParams.RadonInterpolationLinear))
endif

mask = shift(dist(nrho), nrho/2, nrho/2)
outside = where(mask gt nrho/2.)
b(outside) = 0.
return, b
end
