pro optimize_rotation_center, slice, vol, center, entropy, _REF_EXTRA=extra

   ; This routine calculates an array of image figure of merit as the rotation center is varied
   ncenter = n_elements(center)
   entropy = dblarr(ncenter)
   ; Reconstruct with rotation axis in center of range to get min/max of reconstruction for histogram
   r = reconstruct_slice(slice, vol, center=center[ncenter/2], r2, _EXTRA = extra)
   if (n_elements(r2) ne 0) then r = (r+r2)/2.
   mn = min(r)
   if ((mn) lt 0) then mn=2*mn else mn=0.5*mn
   mx = max(r)
   if ((mx) gt 0) then mx=2*mx else mx=0.5*mx
   binsize=(mx-mn)/1.e4
   npixels = n_elements(r)
   for i=0, ncenter-1 do begin
      r = reconstruct_slice(slice, vol, center=center[i], r2, _EXTRA = extra)
      ; Compute average of 2 slices if Gridrec is being used
      if (n_elements(r2) ne 0) then r = (r+r2)/2.
      h = histogram(r, min=mn, max=mx, bin=binsize) > 1
      entropy[i] = -total(h*alog(h))/npixels
   endfor
end
