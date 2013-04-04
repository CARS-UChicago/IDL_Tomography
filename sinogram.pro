pro sine_wave, x, a, f, pder

; Used to evaluate a sine wave with unit frequency. This routine is used by
; sinogram to fit the center-of-gravity data
; a(0) = rotation center
; a(1) = amplitude
; a(2) = phase
f = a(0) + a(1)*sin(x + a(2))
pder = fltarr(n_elements(x), n_elements(a))
pder(*,0) = 1.
pder(*,1) = sin(x + a(2))
pder(*,2) = a(1)*cos(x + a(2))
return
end


function sinogram, tomoParams, input, angles, cog = cog
;+
; NAME:
;   SINOGRAM
;
; PURPOSE:
;   To convert raw tomography data into a sinogram.
;
; CATEGORY:
;   Tomography
;
; CALLING SEQUENCE:
;   result = SINOGRAM(Input, Angles)
;
; INPUTS:
;   Input
;       An array of raw tomography data. INPUT(I, J) is the intensity at
;       position I for view angle J. Each row is assumed to contain at least
;       one air value at each end for normalization.
;   Angles
;       An array of the angles of each row of the input.  Units are degrees.
;
;   COG=cog
;       This keyword is used to return the measured and fitted
;       center-of-gravity data for the sinogram. The center-of-gravity data are
;       very useful for diagnosing problems such as backlash, beam hardening,
;       detector saturation, etc. COG is dimensioned (n_angles, 2), where
;       n_angles is the number of angles in the input array. COG(*,0) is the
;       measured center-of-gravity data. COG(*,1) is the fitted data. The
;       following command can then be given after the SINOGRAM command
;       IDL> PLOT, COG(*,0)
;       IDL> OPLOT, COG(*,1)
;       to see is the sinogram data are reasonable.
; RETURN:
;       The output array containing the corrected sinogram. It is always of
;       type FLOAT.
;
; PROCEDURE:
;       This routine creates a sinogram from raw tomography data. It does the
;       following:
;       -   Averages the air values for "airPixels" pixels on the left and 
;           right hand sides of the input.
;       -   Logarithmation. output = -log(input/air). The air values are
;           interpolated between the averaged values for the left and right 
;           hand edges of the image for each row.  This step is not performed
;           if the /FLUORESCENCE keyword is set.
;       -   The measured center-of-gravity is fitted to a sinusoidal curve
;           of the form Y = A(0) + A(1)*SIN(X + A(2)).
;               A(0) is the rotation axis
;               A(1) is the amplitude
;               A(2) is the phase
;           The fitting is done using routine CURVE_FIT in the User Library.
;           The shifting is done using routine POLY_2D which can shift by 
;           fractional pixels.
; MODIFICATION HISTORY:
;   Created 21-OCT-1991 by Mark Rivers.
;-


; Convert data to floating point
output = float(input)
nrows = n_elements(output(0,*))
ncols = n_elements(output(*,0))

cog = fltarr(nrows)                    ; Center-of-gravity array
linear = findgen(ncols) / (ncols-1)
no_air = fltarr(ncols) + tomoParams.sinoScale
lin2 = findgen(ncols) + 1.
weight = fltarr(nrows) + 1.
airPixels = tomoParams.airPixels
for i=0, nrows-1 do begin
    if (airPixels gt 0) then begin
       air_left = total(output(0:airPixels-1,i)) / airPixels
       air_right = total(output(ncols-airPixels:ncols-1,i)) / airPixels
       air = air_left + linear*(air_right-air_left)
    endif else begin
       air = no_air
    endelse
    if (not keyword_set(fluorescence)) then $
        output(0,i) = -alog(output(*,i)/air > 1.e-5)
    cog(i) = total(output(*,i) * lin2) / total(output(*,i))
endfor
x = angles*!dtor
a = [ncols/2., $               ; Initial estimate of rotation axis
     (max(cog) - min(cog))/2., $ ; Initial estimate of amplitude
     0.]                         ; Initial estimate of phase
cog_fit = curvefit(x, cog, weight, a, sigmaa, $
                   function_name='sine_wave')
cog_mean = a(0)

if (tomoParams.debug) then print, format='(a, f8.2, a, f8.2)', $
        'Fitted center of gravity = ', cog_mean, ' +-', sigmaa(0)
cog = [[cog], [cog_fit]]

return, output
end
