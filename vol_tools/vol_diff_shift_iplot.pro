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
;      print, 'sumsq = ', sumsq[i], ' at ', ishift[i]
   endfor

   case axis of
      0: iaxis = 'Direction X'
      1: iaxis = 'Direction Y'
      2: iaxis = 'Direction Z'
   endcase
   window, axis, title=iaxis, xpos=axis*50+10, ypos=axis*50+10
   ;plot, ishift, sumsq, ystyle=1, background='FFFFFF'x, color='000000'x, title=iaxis, xtitle='Shift', ytitle='Sum Sq Diff'
   iplot, ishift, sumsq, sym_index=1,xTICKINTERVAL=1, title=iaxis, view_title=iaxis, xtitle='Shift', ytitle='Sum Sq Diff', /no_saveprompt
   min_sumsq = min(sumsq, mindex)
   best_shift[axis] = ishift[mindex]
end

function align_tomo_volume, v1, v2, min_sumsq, best_shift, max_shift=max_shift, max_iter=max_iter
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
          ;wait, .01
      endfor
      print, 'iteration:', iter, ' minimum = ', min_sumsq, ' at ', best_shift
      if (min(prev_best eq best_shift) eq 1) then begin
	  return, best_shift
      endif
   endfor
   return, best_shift
end

pro vol_diff_shift_iplot

	file_above = dialog_pickfile(title='Select volume above edge...', filter='*.volume', /must_exist, get_path=get_path)
	if file_above eq '' then return
	file_below = dialog_pickfile(title='Select volume below edge...', filter='*.volume', /must_exist)
	if file_below eq '' then return

	file_diff=''
	if array_equal(strsplit(file_above,'_'), strsplit(file_below,'_')) eq 1 and strmid(file_above, 12, /reverse_offset) eq '_recon.volume' then $
		file_diff = strmid(file_above, 0, strlen(file_above)-13)+strmid(file_below, 13, 1, /reverse_offset)+'_diff.volume'
	file_diff = dialog_pickfile(title='Select name for volume output...', filter='*.volume', default_extension='volume', file=file_diff, /write, /overwrite_prompt)
	if file_diff eq '' then return

	vol_above = read_tomo_volume(file_above)
	vol_below = read_tomo_volume(file_below)

	s = size(vol_above)
	sx = s[1]
	sy = s[2]
	sz = s[3]

	my_shift = align_tomo_volume(vol_above[sx/4:sx/4*3,sy/4:sy/4*3,sz/4:sz/4*3], vol_below[sx/4:sx/4*3,sy/4:sy/4*3,sz/4:sz/4*3])

	vol_diff = vol_above - shift(vol_below, my_shift)
	write_tomo_volume, file_diff, vol_diff

	temp = dialog_message('Volume saved! Click OK to exit.', /information)

end
