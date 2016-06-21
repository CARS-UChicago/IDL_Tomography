; Tries to minimize difference between 2 images

temp = rotate(proj2, 5)

numShift = 21
shiftError = dblarr(numShift)
shft = findgen(numShift) - (numShift-1)/2.

for i=0, numShift-1 do begin
  diff = proj1 - shift(temp, shft[i])
  m = mean(diff)
  shiftError[i] = total((diff-m)^2)
  print, 'Shift=',i,' shiftError=', shiftError[i]
  ;image_display, diff, title='Shift' + strtrim(i,2)
endfor
window, 0
plot, shft, shiftError, ystyle=1

numRot = 21
rotError = dblarr(numRot)
rotStep = .001
r = (findgen(numRot) - (numRot-1)/2.) * rotStep - rotStep/2.
for i=0, numRot-1 do begin
  diff = proj1 - rot(temp, r[i]/!dtor)
  m = mean(diff)
  rotError[i] = total((diff-m)^2)
  print, 'Rot=',r[i],' rotError=', rotError[i]
  ;if (i eq 0) or (i eq numRot-1) then image_display, diff, title='Rotate' + string(r[i])
endfor
window, 1
plot, r, rotError, ystyle=1

end
