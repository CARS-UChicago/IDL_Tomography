pro optimize_rotation_mirror, slice, proj0, proj180, center, error, stripWidth=stripWidth

; Determines rotation center from 2 images that are close to 180 degrees apart 

if (n_elements(stripWidth) eq 0) then stripWidth = 11
s = size(proj0, /dimensions)
nx = s[0]
ny = s[1]
numShift = n_elements(center)
error = dblarr(numShift, 2)
shft = round((center - nx/2) * 2)

for j=0, 1 do begin
  ymin = slice[j] - stripWidth[0]/2 > 0
  ymax = slice[j] + stripWidth[0]/2 < (ny-1)
  
  p0 = reform(proj0[*, ymin:ymax])
  p180 = reform(proj180[*, ymin:ymax])
  p0 = total(p0,2)
  p180 = total(p180,2)
  p180 = reverse(p180)
  
  for i=0, numShift-1 do begin
    diff = p0 - shift(p180, shft[i])
    diff = diff[numShift:nx-numShift-1]
    d = deriv(diff)^2
    error[i, j] = total(d)
  endfor
endfor

end
