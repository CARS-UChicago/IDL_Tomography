; This procedure corrects for rotation axis wobble.  
; It works as follows: 
;    - Computes the sinogram and center of gravity for each slice
;    - Computes the errors (fitted COG minus actual COG for each angle
;    - Averages the errors as a function of angle
;    - Shifts each angle to correct for the average error
;    - The calculation is only done for slices in which the maximum error in the fit is less
;      than max_error pixels (default=10).

pro correct_rotation_axis, infile, outfile, airPixels=airPixels, max_error=max_error, subpixel=subpixel

   if (n_elements(max_error) eq 0) then max_error = 10
   if (n_elements(airPixels) eq 0) then airPixels = 10

   print, 'Reading volume file ', infile
   vol = read_tomo_volume(infile)
   
   tomoParams = tomo_params_init(vol)
   tomoParams.airPixels = airPixels
   
   s = size(vol, /dimensions)
   nx = s[0]
   ny = s[1]
   nangles = s[2]
   poly_degree = 5
   ngood = 0
   errs = fltarr(nangles, ny)
   zeros = fltarr(nangles)
   angles = 180. * findgen(nangles)/nangles
   print, 'Determining rotation errors'
   for i=0, ny-1 do begin
      slice = reform(vol[*,i,*])
      s = sinogram(tomoParams, slice, angles, cog=cog)
      x = findgen(nangles)
      y = reform(cog[*,0])
      yfit = reform(cog[*,1])
      ;p = poly_fit(x, y, poly_degree, yfit=yfit, /double)
      err = y - yfit
      ; Don't count slices for which:
      ;  - the maximum error in the fitted center of gravity is more than max_error pixels
      if (max(abs(err)) ge max_error) then begin
         errs[0,i] = zeros
      endif else begin
         errs[0,i] = yfit-y
         ngood = ngood + 1
      endelse
      print, 'Slice=', i, ' max(err) =', max(abs(err)), $ 
          ' max(errs[*,i])=', max(errs[*,i]), ' min(errs[*,i])=', min(errs[*,i])
   endfor
   image_display, errs, min=-10, max=10
   sh = total(errs, 2)/ ngood
   if (not keyword_set(subpixel)) then begin
     sh = round(sh)
   endif
   p = plot(sh, xtitle='Angle', ytitle='Shift')
   v = vol
   for i=0, nangles-1 do begin
      print, 'shifting angle ', i, ' by ', sh[i]
      proj = vol[0,0,i]
      if (keyword_set(subpixel)) then begin
        t = rss2pq(0, 0, xshift=sh[i], p=p, q=q)
        v[0,0,i] =  poly_2d(proj,p,q,2,cubic=-0.5)
      endif else begin
        v[0,0,i] = shift(proj, sh[i], 0)
      endelse

   endfor
   print, 'Writing output file ', outfile
   write_tomo_volume, outfile, v
end
