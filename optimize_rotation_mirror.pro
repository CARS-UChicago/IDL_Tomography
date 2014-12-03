pro optimize_rotation_mirror, tomoParams, slice, vol, angles, center, error, bestCenter, stripWidth=stripWidth

; Determines rotation center from 2 images that are close to 180 degrees apart 

if (n_elements(stripWidth) eq 0) then stripWidth = 11
s = size(vol, /dimensions)
nx = s[0]
ny = s[1]
nz = s[2]
numShift = n_elements(center)
error = dblarr(numShift, 2)
bestCenter = intarr(2)
shft = round((center - nx/2) * 2)

for j=0, 1 do begin
  ymin = slice[j] - stripWidth/2 > 0
  ymax = slice[j] + stripWidth/2 < (ny-1)
  
  proj1 = reform(vol[*, ymin:ymax, 0])
  proj2 = reform(vol[*, ymin:ymax, nz-1])
  proj1 = total(proj1,2)
  proj2 = total(proj2,2)
  proj2 = reverse(proj2)
  
  for i=0, numShift-1 do begin
    diff = proj1 - shift(proj2, shft[i])
    diff = diff[numShift:nx-numShift-1, *]
    d = deriv(diff)^2
    error[i, j] = total(d)
  endfor
  mn = min(error[*,j], mp)
  bestCenter[j] = mp
endfor

end
