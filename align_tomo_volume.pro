pro optimize_axis, v1, v2, max_shift, axis, min_sumsq, best_shift
   npts = 2*max_shift + 1
   sumsq = dblarr(npts)
   ishift = indgen(npts) - max_shift
   for i=0, npts-1 do begin
      case axis of
         0: diff = double(v1 - shift(v2, ishift[i],     best_shift[1], best_shift[2]))
         1: diff = double(v1 - shift(v2, best_shift[0], ishift[i],     best_shift[2]))
         2: diff = double(v1 - shift(v2, best_shift[0], best_shift[1], ishift[i]))
      endcase
      sumsq[i] = total(diff^2)
   endfor
   plot, ishift, sumsq, ystyle=1
   min_sumsq = min(sumsq, mindex)
   best_shift[axis] = ishift[mindex]
end

pro align_tomo_volume, v1, v2, min_sumsq, best_shift, max_shift=max_shift, max_iter=max_iter
   if (n_elements(max_shift) eq 0) then max_shift=[5, 5, 5]
   if (n_elements(max_iter) eq 0) then max_iter = 3
   best_shift = intarr(3)
   diff = double(v1-v2)
   sumsq = total(diff^2)
   print, 'initial sumsq=', sumsq
   for iter=0, max_iter-1 do begin
      prev_best = best_shift
      for axis=0, 2 do begin
          optimize_axis, v1, v2, max_shift[axis], axis, min_sumsq, best_shift
          wait, .01
      endfor
      print, 'iteration:', iter, ' minimum = ', min_sumsq, ' at ', best_shift
      if (min(prev_best eq best_shift) eq 1) then return
   endfor
end

