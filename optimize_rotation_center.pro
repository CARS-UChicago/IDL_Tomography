pro optimize_rotation_center, tomoParams, slice, vol, center, entropy

   t0 = systime(1)
   ; This routine calculates an array of image figure of merit as the rotation center is varied
   ncenter = n_elements(center)
   entropy = dblarr(ncenter, 2)
   
   if (tomoParams.reconMethod eq tomoParams.reconMethodTomoRecon) then begin
       s = size(vol, /dimensions)
       if (n_elements(s) ne 3) then message, 'optimize_rotation_center, Must pass 3-D array'
       input = fltarr(s[0], ncenter*2, s[2])
       ctr = fltarr(ncenter*2)
       for i=0, ncenter-1 do begin
          ctr[i*2] = center[i]
          ctr[i*2+1] = center[i]
          input[*, i*2, *] = vol[*, slice[0], *]
          input[*, i*2+1, *] = vol[*, slice[1], *]
       endfor

       tomoParams.numSlices = ncenter*2
       tomo_recon, tomoParams, input, recon, center=ctr

       ; Use the slice in center of range to get min/max of reconstruction for histogram
       r1 = recon[*,*,ncenter]
       r2 = recon[*,*,ncenter+1]
   endif else begin

       if (n_elements(slice) eq 1) then begin
           v = vol
       endif else begin
           v = vol[*,[slice[0],slice[1]],*]
       endelse
       ; Reconstruct with rotation axis in center of range to get min/max of reconstruction for histogram
       r1 = reconstruct_slice(tomoParams, 0, v, center=center[ncenter/2], r2)
   endelse

   mn1 = min(r1)
   if ((mn1) lt 0) then mn1=2*mn1 else mn1=0.5*mn1
   mx1 = max(r1)
   if ((mx1) gt 0) then mx1=2*mx1 else mx1=0.5*mx1
   binsize1=(mx1-mn1)/1.e4

   mn2 = min(r2)
   if ((mn2) lt 0) then mn2=2*mn2 else mn2=0.5*mn2
   mx2 = max(r2)
   if ((mx2) gt 0) then mx2=2*mx2 else mx2=0.5*mx2
   binsize2=(mx2-mn2)/1.e4

   npixels = n_elements(r1)
   for i=0, ncenter-1 do begin
      if (tomoParams.reconMethod eq tomoParams.reconMethodTomoRecon) then begin
          r1 = recon[*,*,i*2]
          r2 = recon[*,*,i*2+1]
      endif else begin
          r1 = reconstruct_slice(tomoParams, 0, v, center=center[i], r2)
      endelse
      h1 = histogram(r1, min=mn1, max=mx1, bin=binsize1) > 1
      h1 = float(h1) / npixels
      h2 = histogram(r2, min=mn2, max=mx2, bin=binsize2) > 1
      h2 = float(h2) / npixels
      entropy[i,0] = -total(h1*alog(h1))
      entropy[i,1] = -total(h2*alog(h2))
   endfor
   t1 = systime(1)
   print, 'optimize_rotation_center, time=', t1-t0
end
