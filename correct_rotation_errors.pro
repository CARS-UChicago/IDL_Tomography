; This procedure corrects for rotation axis wobble.  
; It works as follows: 
;    - Computes the sinogram and center of gravity for each slice
;    - Computes the errors (fitted COG minus actual COG for each angle
;    - Averages the errors as a function of angle
;    - Shifts each angle to correct for the average error
;    - The calculation is only done for slices in which the maximum error in the fit is less
;      than max_error pixels (default=10).

function correct_rotation_errors, errs=errs, vol=vol, infile=infile, outfile=outfile, errorfile=errorfile, round=round

  if (n_elements(infile) ne 0) then begin
    print, 'Reading volume file ', infile
    vol = read_tomo_volume(infile)
  endif

  s = size(vol, /dimensions)
  nx = s[0]
  ny = s[1]
  nangles = s[2]
  
  if (n_elements(errs) eq 0) then begin
    print, 'Reading error file ', errorfile
    errs = read_rotation_errors(errorfile)
  endif

  if (keyword_set(round)) then begin
    errs = round(errs)
  endif
  for i=0, nangles-1 do begin
     print, 'shifting angle ', i, ' by ', errs[i]
     proj = vol[*,*,i]
     if (keyword_set(round)) then begin
       vol[0,0,i] = shift(proj, errs[i], 0)
     endif else begin
       t = rss2pq(0, 0, xshift=errs[i], p=p, q=q)
       vol[0,0,i] =  poly_2d(proj,p,q,2,cubic=-0.5)
     endelse
   endfor
   if (n_elements(outfile) ne 0) then begin
     print, 'Writing output file ', outfile
     write_tomo_volume, outfile, vol
   endif
end
