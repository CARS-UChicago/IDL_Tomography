; This procedure measured rotation axis wobble.  
; It works as follows: 
;    - Computes the sinogram and center of gravity for each slice
;    - Computes the errors (fitted COG minus actual COG for each angle
;    - Averages the errors over all slices to produce a 1-D error array[num_angles]
;    - The calculation is only done for slices in the specified range

function measure_rotation_errors, vol, infile=infile, errorfile=errorfile, yrange=yrange, airPixels=airPixels

   if (n_elements(airPixels) eq 0) then airPixels = 0
   tomo = obj_new('tomo')
   tomo->setReconParams(airPixels = airPixels)
 
   if (n_elements(infile) ne 0) then begin
     print, 'Reading volume file ', infile
     vol = tomo->read_volume(infile)
   endif

;   tomo.airPixels = airPixels
   
   s = size(vol, /dimensions)
   nx = s[0]
   if (n_elements(yrange) eq 0) then begin
     yrange = [0, s[1]-1]
   endif
   ny = yrange[1] - yrange[0] + 1
   nangles = s[-1]
   errs = fltarr(nangles, ny)
   angles = 180. * findgen(nangles)/nangles
   x = findgen(nangles)
   print, 'Determining rotation errors'
   for i=yrange[0], yrange[1] do begin
      print, 'Fitting COG on slice ', i
      slice = reform(vol[*,i,*])
      s = tomo->sinogram(slice, cog=cog)
      y = reform(cog[*,0])
      yfit = reform(cog[*,1])
      errs[0,i-yrange[0]] = yfit-y
   endfor
   i1 = image(errs)
   errs = total(errs, 2)/ ny
   p = plot(errs, xtitle='Angle', ytitle='Shift')
   if (n_elements(errorfile) ne 0) then begin
     print, 'Writing output file ', errorfile
     write_rotation_errors, errs, errorfile
   endif
   return, errs
end
