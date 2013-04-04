function reconstruct_slice, tomoParams, slice, volume, image2, $
                            angles=angles, center=center, $
                            sinogram=singram, cog=cog

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
;   Result = RECONSTRUCT_SLICE(tomoParams, Slice, Volume)
;
; INPUTS:
;   tomoParams: A tomo_params structure
;   Slice:      The number of the slice to be reconstructed.
;   Volume:     The 3-D volume array from which the slice is extracted
;
; KEYWORD PARAMETERS:
;   ANGLES:
;       An array of angles (in degrees) at which each projection was collected.
;       If this keyword is not specified then the routine assumes that the data was
;       collected in evenly spaced increments of 180/n_angles.
;   CENTER:
;       The rotation centers.  If this keyword is not specified then the
;       center is assumed to be the center pixel of the image 
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
;   r = reconstruct_slice(tomoParams, 264, volume)
;
; MODIFICATION HISTORY:
;   Written by: Mark Rivers, May 13, 1998
;   Many changes over time, see CVS log.
;-

    time1 = systime(1)
    
    size = size(volume)
    if (size[0] eq 2) then begin
        nx = size[1]
        ny = 1
        nangles = size[2]
        t1 = volume[*,*]
    endif else begin
        nx = size[1]
        ny = size[2]
        nangles = size[3]
        t1 = reform(volume[*,slice,*])
    endelse

    ; Extract the next slice for Gridrec and tomoRecon which can do 2 slices at once
    if (slice lt ny-1) then begin
        t2 = reform(volume[*,slice+1,*]) 
        input = volume[*,slice:slice+1,*]
    endif else begin
        t2=t1
        input = make_array(nx, 2, nangles, /float)
        input[*,0,*] = t1
        input[*,1,*] = t2
    endelse

    if (n_elements(angles) ne 0) then begin
        if (n_elements(angles) ne nangles) then message, 'Incorrect number of angles'
    endif else begin
        ; Assume evenly spaced angles 0 to 180-angle_step degrees
        angles = findgen(nangles)/(nangles) * 180.
    endelse
    time2 = systime(1)

    if (tomoParams.reconMethod eq tomoParams.reconMethodTomoRecon) then begin
        tomo_recon, tomoParams, input, output, center=center, angles=angles
        r = output[*,*,0]
        image2 = output[*,*,1]
    endif

    if (tomoParams.reconMethod eq tomoParams.reconMethodGridrec) then begin
        s1 = sinogram(tomoParams, t1, angles, cog=cog)
        singram = s1
        s2 = sinogram(tomoParams, t2, angles, cog=cog)
        time3 = systime(1)
        if (tomoParams.ringWidth eq 0) then begin
            g1 = s1
            g2 = s2
        endif else begin
            g1 = remove_tomo_artifacts(s1, /rings, width=tomoParams.ringWidth)
            g2 = remove_tomo_artifacts(s2, /rings, width=tomoParams.ringWidth)
        endelse

        ; pad sinogram
        pad = tomoParams.paddedSinogramWidth
        if (pad ne 0) then begin
            size = size(g1)
            if (size[1] gt pad) then begin
                 t = dialog_message('Padded sinogram too small, proceed without padding')
                 pad = 0
            endif else begin
                data_temp1 = temporary(g1)
                data_temp2 = temporary(g2)
                g1 = fltarr(pad-1, size[2])
                g2 = fltarr(pad-1, size[2])
                p = (pad - size[1] -1)/2
                g1[p:p+size[1]-1, *] = data_temp1
                g2[p:p+size[1]-1, *] = data_temp2
                center = center + p
            endelse
        endif

        time4 = systime(1)
        gridrec, tomoParams, g1, g2, angles, r, image2, center=center

        ; crop results if sinogram was padded
        if (pad ne 0 AND size[1] le pad) then begin
            r = r[p:p+size[1] - 1,p:p+size[1] - 1]
            image2 = image2[p:p+size[1] - 1, p:p+size[1] - 1]
            center = center - p
        endif
        
        ; Scale results
        if ((tomoParams.reconScale ne 0.) and (tomoParams.reconScale ne 1.0)) then begin
            r = r * tomoParams.reconScale
            image2 = image2 * tomoParams.reconScale
        endif

        time5 = systime(1)
        print, 'Gridrec: center= ', center
        print, 'Time to compute sinogram: ', time3-time2
        print, 'Time to reconstruct:      ', time5-time4
    endif

    if (tomoParams.reconMethod eq tomoParams.reconMethodBackproject) then begin
        s1 = sinogram(tomoParams, t1, angles, cog=cog)
        cent = -1
        s2 = sinogram(tomoParams, t2, angles, cog=cog)
        time3 = systime(1)
        if (tomoParams.ringWidth eq 0) then begin
            g1 = s1
            g2 = s2
        endif else begin
            g1 = remove_tomo_artifacts(s1, /rings, width=tomoParams.ringWidth)
            g2 = remove_tomo_artifacts(s2, /rings, width=tomoParams.ringWidth)
        endelse

        time4 = systime(1)
        ss1 = tomo_filter(g1, filter_size=tomoParams.BP_filterSize, filter_name=string(tomoParams.BP_filterName))
        ss2 = tomo_filter(g2, filter_size=tomoParams.BP_filterSize, filter_name=string(tomoParams.BP_filterName))
        singram = ss1
        time5 = systime(1)
        if (center ge (nx-1)/2) then ctr = center else ctr = center + 2*abs(round(center - (nx-1)/2))  
        r = backproject(tomoParams, ss1, angles, center=ctr)
        image2 = backproject(tomoParams, ss2, angles, center=ctr)
        time6 = systime(1)
        ; Scale results
        if ((tomoParams.reconScale ne 0.) and (tomoParams.reconScale ne 1.0)) then begin
          r = r * tomoParams.reconScale
          image2 = image2 * tomoParams.reconScale
        endif
        print, 'Backproject: center= ', center
        print, 'Time to compute sinogram: ', time3-time2
        print, 'Time to filter sinogram:  ', time5-time4
        print, 'Time to backproject:      ', time6-time5
    endif
    
    return, r
end
