function remove_tomo_artifacts, image, image2=image2, width=width, $
         threshold=threshold, $
         zingers=zingers, double_correlation=double_correlation, $
         rings=rings, diffraction=diffraction, debug=debug

;+
; NAME:
;   REMOVE_TOMO_ARTIFACTS
;
; PURPOSE:
;   Removes artifacts from tomography images and sinograms.
;
; CATEGORY:
;   Tomography data processing
;
; CALLING SEQUENCE:
;   Result = REMOVE_TOMO_ARTIFACTS(Image)
;
; INPUTS:
;   Image:  The input array from which artifacts are to be removed.
;
; KEYWORD PARAMETERS:
;   IMAGE2:
;       A second input and output image when doing DOUBLE_CORRELATION
;   ZINGERS:
;       Set this keyword to remove zingers from the input image
;   DOUBLE_CORRELATION:
;       Set this keyword to remove zingers from the input using double 
;       correlation rather than a spatial detection
;   RINGS:
;       Set this keyword to remove ring artifacts from a sinogram
;   DIFFRACTION:
;       Set this keyword to removed diffraction artifacts from a sinogram
;   WIDTH:          
;       Set this keyword to adjust the size of the filter kernal used in the
;       artifact removal.  The default is 9 pixels.
;   THRESHOLD:  
;       Set this keyword to adjust the threshold used in detecting zingers and
;       diffraction artifacts.  The defaults are 1.2 for /ZINGER and 
;       /DOUBLE_CORRELATION and 0.8 for/DIFFRACTION
;   DEBUG:      
;       Set this keyword to print debugging information
;
; OUTPUTS:
;   This function returns an image with the specified artifacts removed.
;
; PROCEDURE:
;   THIS STILL NEEDS TO BE DOCUMENTED.  For now, see the source code.
;
; EXAMPLE:
;   r = remove_tomo_artifacts(sinogram, /Rings)
;
; MODIFICATION HISTORY:
;   Written by:     Mark Rivers, May 13, 1998
;   MLR 4/4/99 MLR  Added DOUBLE_CORRELATION keyword
;   MLR 4/5/99 MLR  Added /EDGE_TRUNCATE to all calls to SMOOTH so that
;                   smoothing works to the edges of the image
;-

; Copy input image to output
output = image
size = size(image)
ncols = size[1]
nrows = size[2]
if (keyword_set(debug)) then debug=1 else debug=0

if (keyword_set(zingers)) then begin
        ; Default kernal width for smoothing
        if (n_elements(width) eq 0) then width=9
        ; Default threshold for detecting zingers
        if (n_elements(threshold) eq 0) then threshold=1.2
        ; Compute ratio of unsmoothed image to smoothed image
        ratio = float(image)/smooth(image, width, /edge_truncate)
        ; Find indices of pixels which are above threshold
        zinger = where(ratio gt threshold, count)
        ; Replace pixels with zingers by the average of the pixels 2 to the left and
        ; 2 to the right.  We don't use nearest neighbor, since zingers sometimes hit 2
        ; adjacent pixels.
        if (count gt 0) then output[zinger] = $
                (output[zinger-2] + output[zinger+2])/2.
        if (debug) then print, 'Found ', count, ' zingers in image'
endif

if (keyword_set(double_correlation)) then begin
        ; Compute ratio of the two images
        ratio = float(image) / float(image2)
        ; Find the median value of the ratio
        m = median(ratio)
        ; Normalize the ratio to this value
        ratio = ratio / m
        if (n_elements(threshold) eq 0) then threshold=1.2
        zingers1 = where(ratio gt threshold, count1)
        zingers2 = where(ratio lt 1./threshold, count2)
        if (debug) then print, 'Found ', count1, ' zingers in image1 and ', $
                                         count2, ' zingers in image2'
        ; Copy the first image to the output to be returned in function value
        output = image
        ; Replace zinger pixels in the first image with pixels from the second
        ;   image scaled by median value of ratio
        if (count1 gt 0) then output[zingers1] = image2[zingers1] * m
        ; Replace zinger pixels in the second image with pixels from the first
        ;   image scaled by median value of ratio
        if (count2 gt 0) then image2[zingers2] = image[zingers2] / m
        ; Now deal with the case where there are zingers at the same pixel in 
        ;   both images
        if ((count1 gt 0) and (count2 gt 0)) then begin
            both = [zingers1, zingers2]
            both = both[sort(both)]
            indices = where(both eq shift(both, -1), count)
            if (debug) then print, 'Found ', count, $
                                   ' common zingers in image1 and image2'
            if (count gt 0) then both = both[indices]
            for i=0, count-1 do begin
                output[both[i]] = (output[both[i]-2] + output[both[i]+2])/2.
                image2[both[i]] = (image2[both[i]-2] + image2[both[i]+2])/2.
            endfor
        endif
endif

if (keyword_set(rings)) then begin
        ; Default kernal width for smoothing
        if (n_elements(width) eq 0) then width=9
        ; Sum all of the rows in the image, get average
        ave = total(image, 2)/nrows
        ; Get the difference between the average row and smoothed version
        ; of average row.  These are the column deviations.
        diff = ave - smooth(ave, width, /edge_truncate)
        ; Subtract this difference from each row
        for i=0, nrows-1 do output[0,i] = output[*,i] - diff
endif

if (keyword_set(diffraction)) then begin
        ; Default kernal width for smoothing
        if (n_elements(width) eq 0) then width=9
        ; Default threshold for detecting diffraction peaks
        if (n_elements(threshold) eq 0) then threshold=.8
        ; Loop over each column in the image
        for i=0, ncols-1 do begin
                col = reform(output[i,*])
                ratio = col/smooth(col, width, /edge_truncate)
                bad = where(ratio lt threshold)
                col[bad] = (col[(bad-2)>0] + col[(bad+2)<(ncols-1)]) / 2.
                output[i,*] = col
        endfor
endif

return, output
end
