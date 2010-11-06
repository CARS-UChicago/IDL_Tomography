function backproject, sinogram, angles, center = center, RADON = RADON, $
                      linear = linear, bilinear=bilinear, cubic=cubic

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
;   Result = BACKPROJECT(Sinogram, Angles)
;
; INPUTS:
;   Sinogram:
;       The input sinogram, dimensions NX x NANGLES.  This should have been 
;       filtered before calling this function.
;   Angles:
;       An array of dimensions NANGLES which contains the angle in degrees of 
;       each projection.
;
; KEYWORD PARAMETERS:
;   This function accepts the BILINEAR and CUBIC keywords accepted by the 
;   IDL RIEMANN procedure.
;
; OUTPUTS:
;   This function returns the reconstructed image.
;
; RESTRICTIONS:
;   This function does not yet properly scale the result.  This will be fixed 
;   in the near future.
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
;   21-APR-1999 MLR Changed function to use keyword inheritance (_EXTRA) rather
;                   than hardcoding the BINLINEAR and CUBIC keywords
;   18-MAY-1999 MLR Changed function back to hardcoding the BINLINEAR and 
;                   CUBIC keywords, because the CENTER keyword was causing
;                   problems with keyword inheritance when called from
;                   RECONSTRUCT_SLICE, since CENTER is also used by SINOGRAM.
;   20-FEB-2000 MLR Set all pixels outside reconstructed area to 0.
;   19-APR-2001 MLR Change units of ANGLES from radians to degrees
;   16-SEP-2010 DTC Added CENTER keyword
;   07-OCT-2010 DTC Added option for user to reconstruct with RADON backprojection
;-

nrho = n_elements(sinogram[*,0])
nangles = n_elements(sinogram[0,*])
if (n_elements(RADON) eq 0) then radon = 0

if (RADON eq 0) then begin ; in case of RIEMANN backprojection
    b = fltarr(nrho, nrho)      ;Initial reconstructed image.
    if n_elements(center) then ctr = center ;;;;;
    for i=0,nangles-1 do begin
            riemann, sinogram, b, angles[i]*!dtor, row=i, /backproject, $
                             center=ctr, bilinear=bilinear, cubic=cubic
    endfor
    b = b*!pi/(1.*nangles)
endif else if(RADON eq 1) then begin ; in case of RADON backprojection
    if (n_elements(nx) eq 0) then nx = nrho
    if (n_elements(ny) eq 0) then ny = nx
    if (n_elements(center) eq 0) then center = (nrho-1)/2.
    rhos = (findgen(nrho) - center)
    angles = !pi*angles/180. ; convert angles to radians
    b = radon(transpose(sinogram), /BACKPROJECT, rho = rhos, theta = angles, nx = nx, ny = ny, linear = linear)
endif

mask = shift(dist(nrho), nrho/2, nrho/2)
outside = where(mask gt nrho/2.)
b(outside) = 0.
return, b
end
