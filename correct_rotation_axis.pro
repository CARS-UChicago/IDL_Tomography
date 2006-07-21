; This procedure corrects for rotation axis wobble.  
; It works as follows: 
;    - Computes the sinogram and center of gravity for each slice
;    - Computes the errors (fitted COG minus actual COG for each angle
;    - Averages the errors as a function of angle
;    - Shifts each angle to correct for the average error
;    - The calculation is only done for slices in which the minimum log (I/I0) is > 0.1 and
;      maximum is less than 4.

pro correct_rotation_axis, infile, outfile

print, 'Reading volume file ', infile
vol = read_tomo_volume(infile)
s = size(vol, /dimensions)
nx = s[0]
ny = s[1]
nangles = s[2]
poly_degree = 5
errs = fltarr(nangles, ny)
zeros = fltarr(nangles)
angles = 180. * findgen(nangles)/nangles
print, 'Determining rotation errors'
for i=0, ny-1 do begin
   slice = reform(vol[*,i,*])
   s = sinogram(slice, angles, cog=cog)
   x = findgen(nangles)
   y = reform(cog[*,0])
   yfit = reform(cog[*,1])
   ;p = poly_fit(x, y, poly_degree, yfit=yfit, /double)
   if ((min(s) lt -.1) or (max(s) gt 4)) then begin
      errs[0,i] = zeros
   end else begin
      errs[0,i] = yfit-y
   endelse
endfor
t = total(errs, 2)/nangles
sh = round(t)
v = vol
for i=0, nangles-1 do begin
    print, 'shifting angle ', i, ' by ', sh[i]
    v[0,0,i] = shift(reform(vol[*,*,i]), sh[i], 0)
endfor
print, 'Writing output file ', outfile
write_tomo_volume, outfile, v
end

