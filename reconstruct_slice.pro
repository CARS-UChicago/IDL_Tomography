function reconstruct_slice, slice, volume, image2, noring=noring, $
                            pixel_size=pixel_size, normalize=normalize, $
                            scale=scale, back_project=back_project, $
                            resize=resize, angles=angles, center=center, $
                            _REF_EXTRA=extra

;+
; NAME:
;   RECONSTRUCT_SLICE
;
; PURPOSE:
;   Reconstructs a single slice in a tomography volume array.
;
; CATEGORY:
;   Tomography data processing
;
; CALLING SEQUENCE:
;   Result = RECONSTRUCT_SLICE(Slice, Volume)
;
; INPUTS:
;   Slice:      The number of the slice to be reconstructed.
;   Volume:     The 3-D volume array from which the slice is extracted
;
; KEYWORD PARAMETERS:
;   NORING:
;       Setting this keyword prevents the function from removing ring
;       artifacts.
;   PIXEL_SIZE:
;       Specifies the size of each pixel.  This is used only when the NORMALIZE
;       keyword is also set.  The normalized output will be in units of
;       "mu" in inverse length, where the units are controlled by the value
;       specified for pixel size.  For example, if the pixels are 10 microns
;       and the user specifies PIXEL_SIZE=.01 then the normalized values will
;       be inverse mm.  If the user specifies PIXEL_SIZE=.001 then the
;       output will be inverse cm, etc.  The default value is 1, which means
;       that the normalized output will be mu per pixel.
;   NORMALIZE:
;       Specifies that the output is to be normalized to absolute absorption
;       units.  If the SCALE keyword is not specified then this is done by
;       normalizing the reconstructed slice to have the same integrated area
;       as the average sum of each row of the sinogram.
;   SCALE:
;       Specifies the scale factor to be used in normalizing the output.  This
;       is only meaningful when the NORMALIZE keyword is used.  The default
;       value of SCALE is obtained normalizing the reconstructed slice to
;       have the same integrated area as the average sum of each row of the
;       sinogram, and this value is printed out.  The SCALE value can also be
;       explicitly set.  This is typically done under the following conditions:
;          1) Reconstruct a few slices using /NORMALIZE but without specifying
;             SCALE.  Note the average value of SCALE printed out by this
;             routine for these slices.
;          2) Reconstruct the entire volume with RECONSTRUCT_VOLUME, specifying
;             both /NORMALIZE and SCALE=scale, using the scale factor found in
;             step 1 above.
;       Doing these steps ensures that exactly the same scale factor is used
;       for normalizing each slice of the reconstructed volume.  This is
;       probably more accurate than using slightly different normalizing
;       factors for each slice.
;   ANGLES:
;       An array of angles (in degrees) at which each projection was collected.
;       If this keyword is not specified then the routine assumes that the data was
;       collected in evenly spaced increments of 180/n_angles.
;
;   All keywords accepted by SINOGRAM, GRIDREC and BACKPROJECT are passed to
;   those routines via keyword inheritance
;
; OUTPUTS:
;   This function returns the reconstructed slice.  It is a floating point
;   array of dimensions NX x NX.
;
; PROCEDURE:
;   Does the following:  extracts the slice, computes the sinogram with
;   centering and optional center tweaking, removes ring artifacts, filters
;   with a Shepp-Logan filter and backprojects.  It also prints the time
;   required for each step at the end.
;
; EXAMPLE:
;   r = reconstruct_slice(264, volume, tweak=-2)
;
; MODIFICATION HISTORY:
;   Written by: Mark Rivers, May 13, 1998
;   05-APR-1999 MLR  Fixed serious error.  Angles were being computed wrong.
;                    Previously it was calculating 0 to 180, rather than 0 to
;                    180-angle_step
;   05-APR-1999 MLR  Added FILTER_SIZE keyword, made the default be 1/4 of
;                    the image width
;   18-MAY-1999 MLR  Changed formal parameter _extra to _ref_extra to allow
;                    CENTER keyword value to be returned from sinogram
;   20-FEB-2000 MLR  Added NORMALIZE and PIXEL_SIZE keywords for normalization.
;   21-FEB-2000 MLR  Added SCALE keyword for normalization.
;   06-MAR-2000 MLR  Added support for GRIDREC reconstruction
;   22-JAN-2001 MLR  Fixed bug in passing center to sinogram with Gridrec
;   11-APR-2001 MLR  Added ANGLES keyword to allow specifying angle array
;                    Added CENTER keyword, because 22-JAN-2001 change still did not
;                    cause sinogram to ignore center keyword, which was passed both
;                    explicitly and via EXTRA.  Once that was fixed, however, it
;                    was found to not yield the desired results with GRIDREC, because
;                    part of the reconstructed object is not visible.  Change to pass
;                    center to sinogram even for gridrec.
;   17-APR-2001 MLR  Changed call to GRIDREC to pass angle array.
;                    Removed "center" keyword in call to GRIDREC, since sinogram
;                    has padded the array to put the rotation axis in the center.
;   19-APR-2001 MLR  Changed units of ANGLES from radians to degrees
;   25-NOV-2001 MLR  Removed FILTER_SIZE keyword.  This can now be passed to TOMO_FILTER
;                    by keyword inheritance, and the default in TOMO_FILTER now is the
;                    same as this routine previously used.
;                    Removed /SHEPP_LOGAN keyword in call to TOMO_FILTER, this is the default.
;                    Removed STOP keyword, the same result can be achieved with breakpoints.
;-

time1 = systime(1)
if (n_elements(pixel_size) eq 0) then pixel_size = 1
if (n_elements(resize) eq 0) then resize=1
size = size(volume)
nx = size[1]
ny = size[2]
nangles = size[3]
t1 = reform(volume[*,slice,*])
; Extract the next slice for GRIDREC
if (slice lt ny-1) then t2 = reform(volume[*,slice+1,*]) else t2=t1

if (n_elements(angles) ne 0) then begin
    if (n_elements(angles) ne nangles) then message, 'Incorrect number of angles'
endif else begin
    ; Assume evenly spaced angles 0 to 180-angle_step degrees
    angles = findgen(nangles)/(nangles) * 180.
endelse
time2 = systime(1)
; f = remove_tomo_artifacts(t, /diffraction)
if (keyword_set(back_project)) then begin
    s = sinogram(t1, angles, center=center, _EXTRA=extra)
    time3 = systime(1)
    if keyword_set(noring) then g = s else g = remove_tomo_artifacts(s, /rings)
    time4 = systime(1)
    ss = tomo_filter(g, _EXTRA=extra)
    time5 = systime(1)
    r = backproject(ss, angles, _EXTRA=extra)
    time6 = systime(1)
    if (keyword_set(normalize)) then  begin
        sum = total(s, 1)
        mom = moment(sum)
        if (n_elements(scale) eq 0) then begin
            scale = mom[0] / total(r) / pixel_size
        endif
        r = r * scale
        print, 'Average intensity in each row of sinogram = ', mom[0], $
                                                   '+-', sqrt(mom[1])
        print, 'Normalizing scale factor = ', scale
    endif
    print, 'Time to extract slice:    ', time2-time1
    print, 'Time to compute sinogram: ', time3-time2
    print, 'Time to remove artifacts: ', time4-time3
    print, 'Time to filter sinogram:  ', time5-time4
    print, 'Time to backproject:      ', time6-time5
endif else begin
    s1 = sinogram(t1, angles, center=center, _EXTRA=extra)
    cent = -1
    s2 = sinogram(t2, angles, center=center, _EXTRA=extra)
    time3 = systime(1)
    if keyword_set(noring) then begin
        g1 = s1
        g2 = s2
    endif else begin
        g1 = remove_tomo_artifacts(s1, /rings)
        g2 = remove_tomo_artifacts(s2, /rings)
    endelse
    time4 = systime(1)
    gridrec, g1, g2, angles, r, image2, _EXTRA=extra
    if (keyword_set(resize)) then begin
        r = congrid(r, nx, nx, /interp)
        image2 = congrid(image2, nx, nx, /interp)
    endif
    if (n_elements(scale) ne 0) then begin
        r = r * scale
        image2 = image2 * scale
    endif
    time5 = systime(1)
    print, 'Time to extract slice:    ', time2-time1
    print, 'Time to compute sinogram: ', time3-time2
    print, 'Time to remove artifacts: ', time4-time3
    print, 'Time to reconstruct:      ', time5-time4
endelse
if (keyword_set(stop)) then stop
return, r
end
