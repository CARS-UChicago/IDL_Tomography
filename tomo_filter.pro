;+
; NAME:
;   TOMO_FILTER
;
; PURPOSE:
;   Filters a sinogram before backprojection.  A selection of filters is 
;   available.
;
; CATEGORY:
;   Tomography data processing
;
; CALLING SEQUENCE:
;   Result = TOMO_FILTER(Sinogram, Filter_size, D)
;
; INPUTS:
;   Sinogram:       The unfiltered sinogram.  This must be a 2-D array.
;
; OPTIONAL INPUTS:
;   Filter_size:    The half-size of the filter in pixels.  The default is 32.
;   D:              An additional filter parameter.  The default is 1.0
;
; KEYWORD PARAMETERS:
;   GEN_HAMMING
;       Setting this keyword causes the function to use a GEN_HAMMING filter.
;   LP_COSINE
;       Setting this keyword causes the function to use an LP_COSINE filter.
;   SHEPP_LOGAN
;       Setting this keyword causes the function to use a Shepp-Logan filter.
;       This is the default.
;   RAMLAK
;       Setting this keyword causes the function to use a RAMLAK filter.
;   NONE
;       Setting this keyword causes the function to use no filter.
;
; OUTPUTS:
;   This function returns the filtered sinogram.
;
; PROCEDURE:
;   For each row in the sinogram, this function simply does the following:
;       Pads the input sinogram
;       Does a convolution with the appropriate filter
;   The code for the filters was taken from the IDL tomography demo program 
;   which is included in the IDL distribution.  It would be easy to add 
;   additional filters in the future.
;
; EXAMPLE:
;   f = tomo_filter(sinogram, /SHEPP_LOGAN)
;
; MODIFICATION HISTORY:
;   Written by:     Mark Rivers, May 13, 1998
;-


; *** *** Reconstruction Filters from IDL reconstruction demo *** ***

Function None, x, d
return, [1.0]
end

Function RAMLAK, x, d
zero = where(x eq 0.0, count)
q = x
if (count ne 0) then q(zero) = .01
y = -sin(!pi*x/2)^2 / (!pi^2 * q^2 * d)
if count ne 0 then y(zero) = 1./(4.*d)
return, y
end

Function Shepp_logan, x, d
d = !pi^2 * d * (1.-4.*x^2)
zeros = where(abs(d) le 1.0e-6, count)
if count ne 0 then d(zeros) = .001
return, 2./d
end

Function lp_cosine, x, d
return, 0.5 * (ramlak(x-.5,d) + ramlak(x+.5,d))
end

Function Gen_Hamming, x, d, alpha
if n_elements(alpha) le 0 then alpha = 0.5
return, alpha * ramlak(x,d) + ((1.-alpha)/2) * (ramlak(x-1,d) + ramlak(x+1,d))
end

function tomo_filter, image, filter_size, d, gen_hamming=gen_hamming, lp_cosine=lp_cosine, $
                            shepp_logan=shepp_logan, ramlak=ramlak, none=none
if (n_elements(filter_size) eq 0) then filter_size = 32
nfilter = 2*filter_size+1
x = findgen(nfilter)-filter_size
if (n_elements(d) eq 0) then d=1.0
if (keyword_set(gen_hamming)) then filter = gen_hamming(x, d) else $
if (keyword_set(lp_cosine))   then filter = lp_cosine(x, d)   else $
if (keyword_set(shepp_logan)) then filter = shepp_logan(x, d) else $
if (keyword_set(ramlak))      then filter = ramlak(x, d)      else $
if (keyword_set(none))        then filter = none(x, d)        else $
filter = shepp_logan(x,d)
size = size(image)
ncols = size[1]
nrows = size[2]
s = image
temp = fltarr(ncols + 2*nfilter)
for i=0, nrows-1 do begin
        ; Pad array with data from first and last columns
        temp[0:nfilter-1] = image[0,i]
        temp[nfilter+ncols-1:ncols+2*nfilter-1] = image[ncols-1,i]
        temp(nfilter) = image(*,i)
        temp = convol(temp, filter)
        s(0,i) = temp(nfilter : nfilter+ncols-1)
endfor
return, s
end
