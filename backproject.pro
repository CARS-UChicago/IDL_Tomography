function backproject, sinogram, angles, binlinear=bilinear, cubic=cubic

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
;-

nx = n_elements(sinogram[*,0])
nviews = n_elements(sinogram[0,*])
b = fltarr(nx, nx)      ;Initial reconstructed image.
for i=0,nviews-1 do begin
        riemann, sinogram, b, angles[i]*!dtor, row=i, /backproject, $
                         center=garbage, bilinear=bilinear, cubic=cubic
endfor
mask = shift(dist(nx), nx/2, nx/2)
outside = where(mask gt nx/2)
b(outside) = 0.
return, b

end
