;+
; NAME:
;   GRIDREC
;
; PURPOSE:
;   Performs tomographic reconstruction using the "gridrec" algorithm written
;   by Bob Marr and Graham Campbell (not sure about authors) at BNL in 1997.
;   The basic algorithm is based on FFTs.  It reconstructs 2 data sets at once, 
;   one in the real part of the FFT and one in the imaginary part.
;
;   This routine is 20-40 times faster than BACKPROJECT, and yields virtually
;   identical reconstructions.
;
;   This file uses CALL_EXTERNAL to call GridrecIDL.c which is a thin
;   wrapper to grid.c, 
;
; CATEGORY:
;   Tomography data processing
;
; CALLING SEQUENCE:
;   GRIDREC, tomoParams, Sinogram1, Sinogram2, Angles, Image1, Image2
;
; INPUTS:
;   Sinogram1:
;       The first input sinogram, dimensions NX x NANGLES.  
;   Sinogram2:
;       The second input sinogram, dimensions NX x NANGLES.  
;   Angles:
;       An array of dimensions NANGLES which contains the angle in degrees of 
;       each projection.
;
; KEYWORD PARAMETERS:
;   CENTER: The column containing the rotation axis.  The default is the center
;           of the sinogram.
;
; OUTPUTS:
;   Image1:
;       The reconstructed image from Sinogram1.  
;   Image2:
;       The reconstructed image from Sinogram2.  
;
;   Note that the sizes of Image1 and Image2 are controlled by "grid" and will not 
;   be equal NX*NX.  RECONSTRUCT_SLICE uses the IDL routine CONGRID to resize the 
;   images to be NX*NX.
;
; PROCEDURE:
;   This function uses CALL_EXTERNAL to call the shareable library GridrecIDL, 
;   which is written in C.
;
; RESTRICTIONS:
;   GRIDREC locates the GridrecIDL shareable library via the environment variable
;   GRIDREC_SHARE.  This environment variable must be defined and must contain the
;   name (typically including the path) to a valid shareable library or DLL.
;
; EXAMPLE:
;   GRIDREC, s1, s2, angles, image1, image2, center=419
;
; MODIFICATION HISTORY:
;   Written by:     Mark Rivers, March 4, 2000
;   19-APR-2001 MLR Change units of ANGLES from radians to degrees.  Write
;                   documentation header.
;-

pro gridrec, tomoParams, S1, S2, angles, I1, I2, $
             center=center

    ; We use a common block just to store info through calls
    common gridrec_common, gridrec_shareable_library

    image_size = 0L
    n_ang = n_elements(S1[0,*])
    n_det = n_elements(S1[*,0])
    if (n_elements(angles) ne n_ang) then message, 'Incorrect number of angles'

    if (n_elements(center) eq 0) then center=n_det/2.

    if (n_elements(gridrec_shareable_library) eq 0) then begin
        gridrec_shareable_library = getenv('GRIDREC_SHARE')
        if (gridrec_shareable_library eq "") then begin
            file = 'GridrecIDL_' + !version.os + '_' + !version.arch
            if (!version.os eq 'Win32') then file=file+'.dll' else file=file+'.so'
            gridrec_shareable_library = file_which(file)
        endif
    endif
    if (gridrec_shareable_library eq '') then message, 'Gridrec shareable library not defined'
    t = call_external(gridrec_shareable_library, 'recon_init_IDL', $
                          long(n_ang), $
                          long(n_det), $
                          tomoParams.geom, $
                          float(angles), $
                          float(center), $
                          tomoParams.pswfParam, $
                          tomoParams.sampl, $
                          tomoParams.ROI, $
                          tomoParams.MaxPixSize, $
                          tomoParams.X0, $
                          tomoParams.Y0, $
                          tomoParams.ltbl, $
                          tomoParams.GR_filterName, $
                          image_size)

    I1 = fltarr(image_size, image_size)
    I2 = fltarr(image_size, image_size)
    t = call_external(gridrec_shareable_library, 'do_recon_IDL', $
                          long(n_ang), $
                          long(n_det), $
                          long(image_size),$
                          float(S1), $
                          float(S2), $
                          I1, $
                          I2);
end
