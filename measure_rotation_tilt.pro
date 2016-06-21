pro measure_rotation_tilt, proj0, proj180, center, error, numShift=numShift, stripWidth=stripWidth

  ; Determines vertical shift of central strip near rotation axis for 0 and 180

  if (n_elements(stripWidth) eq 0) then stripWidth = 100
  if (n_elements(numShift) eq 0) then numShift = 20
  s = size(proj0, /dimensions)
  nx = s[0]
  ny = s[1]
  error = dblarr(numShift)
  shft = indgen(numShift) - numShift/2

  xmin = nx/2 - stripWidth/2 > 0
  xmax = nx/2 + stripWidth/2 < (nx-1)
  
  pr180 = rotate(proj180, 5)
  shift = round((center - nx/2) * 2)
  pr180 = shift(pr180, shift, 0)

  p0 = reform(proj0[xmin:xmax, *])
  p180 = reform(pr180[xmin:xmax, *])
  p0 = total(p0,1)
  p180 = total(p180,1)

  for i=0, numShift-1 do begin
    diff = p0 - shift(p180, shft[i])
    diff = diff[numShift:ny-numShift-1]
    d = deriv(diff)^2
    error[i] = total(d)
  endfor

  iplot, shft, error, sym_index=4, view_title='Vertical shift error', xtitle='Shift', ytitle='Error'
  m = min(error, minPos)
  before = proj0 - pr180
  after = proj0 - shift(pr180, 0, shft[minPos])
  iimage, (-1000 > before) < 1000, title='Before vertical shift', view_zoom=0.31
  iimage, (-1000 > after) < 1000, title='After vertical shift', view_zoom=0.31
end
