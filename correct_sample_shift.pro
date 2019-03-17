pro correct_sample_shift, file
  ; This procedure corrects for sample motion in the X direction for the Stack data
  ; It uses the left edge of the sample column which is a sharp edge
  ; It finds the location of the maximum of the derivative of this edge to determine the
  ; left-right shift for each projection

  print, 'Reading volume file ...'
  vol = read_tomo_volume(file + '.volume')
  dims = size(vol, /dimensions)
  nx = dims[0]
  ny = dims[1]
  nz = dims[2]
  
  print, 'Averaging rows ...'
  ; Average all the rows in the dataset
  v = rebin(vol, nx, 1, nz)
  
  ; Collapse to 2 dimensions
  v = reform(v)
  
  ; Take the derivative of the data in this X direction
  d = v - shift(v, 1, 0)
  
  ; Extract the left half of the image, without first column which is garbage from shift
  ds = d[1:nx/2,*]
  
  ; Find the location of the maximum in each row
  m = max(ds, max_index, dimension=1)
  
  max_index = max_index mod (nx/2)
  max_index = max_index - (moment(max_index))[0]
  
  ; Fit max_index to a 5th order polynomial and subtract
  x = findgen(nz)/(nz) * !pi
  coeffs = poly_fit(x, max_index, 5, yfit=yfit)
  error = round(max_index - yfit)
  window, 0
  plot, error

  ; Shift each row of the array by the error
  for i=0, nz-1 do begin
    print, 'Shifting row ' + strtrim(i, 2) + ' by ' + strtrim(-error[i],2) + ' pixels'
    vol[0,0,i] = shift(vol[*,*,i], -error[i], 0)
  endfor
 
  ; Uncomment this line to examine variables, etc. 
  ;stop
  
  print, 'Saving new volume file ...'
  write_tomo_volume, file + '_shifted.volume', vol

  
end
  
