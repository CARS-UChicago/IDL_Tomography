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
;   Filter_size:    The half-size of the filter in pixels.  The default is NX/4 where
;                   NX is the first dimension of the sinogram.
;   D:              An additional filter parameter.  The default is 1.0
;
; KEYWORD PARAMETERS:
;   FILTER_NAME:  A case-insensitive string specifying the filter to be used.
;                 Allowed values are:
;                 'GEN_HAMMING'
;                 'LP_COSINE'
;                 'SHEPP_LOGAN'
;                 'RAMLAK'
;                 'NONE'
;                 The default is 'SHEPP_LOGAN'
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
;   f = tomo_filter(sinogram, filter_name='SHEPP_LOGAN')
;
; MODIFICATION HISTORY:
;   Written by:     Mark Rivers, May 13, 1998
;   25-Nov-2001  MLR  Changed the keywords from names of individual filters
;                     (e.g. /SHEPP_LOGAN) to FILTER_NAME
;                     Changed the default filter size from 32 to NX/4
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

function tomo_filter, image, filter_size=filter_size, filter_param=d, filter_name=filter_name
    dims = size(image, /dimensions)
    if (n_elements(filter_size) eq 0) then filter_size = 0
    if (filter_size eq 0) then filter_size = dims[0]/4
    nfilter = 2*filter_size+1
    x = findgen(nfilter)-filter_size
    if (n_elements(d) eq 0) then d=1.0
    if (n_elements(filter_name) eq 0) then filter_name = 'SHEPP_LOGAN'
    if (strlen(filter_name) eq 0) then filter_name = 'SHEPP_LOGAN'
    case strupcase(filter_name) of
        'GEN_HAMMING': filter = gen_hamming(x, d)
        'LP_COSINE':   filter = lp_cosine(x, d)
        'SHEPP_LOGAN': filter = shepp_logan(x, d)
        'RAMLAK':      filter = ramlak(x, d)
        'NONE':        filter = none(x, d)
        else:  message, 'Unknown filter in tomo_filter'
    endcase
    size = size(image)
    ncols = size[1]
    nrows = size[2]
    s = image
    temp = fltarr(ncols + 2*nfilter)
    for i=0, nrows-1 do begin
        ; Pad array with data from first and last columns
        temp[0:nfilter-1] = image[0,i]
        temp[nfilter+ncols-1:ncols+2*nfilter-1] = image[ncols-1,i]
        temp(nfilter) = image[*,i]
        temp = convol(temp, filter)
        s(0,i) = temp[nfilter : nfilter+ncols-1]
    endfor
    return, s
end
