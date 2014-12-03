; This procedure corrects for rotation axis tilt.
; It works as follows: 
;    - Rotates each projection clockwise by the angle specified (angle is in mrad)
;    - Writes new volume file corrected by this angle

pro correct_rotation_tilt, infile, outfile, angle

   print, 'Reading volume file ', infile
   vol = read_tomo_volume(infile)
   out = vol
   
   s = size(vol, /dimensions)
   nx = s[0]
   ny = s[1]
   nangles = s[2]
   angle = angle/1000./!dtor
   t0 = systime(1)
   for i=0, nangles-1 do begin
      projection = reform(vol[*,*,i])
      print, 'Rotating projection', i
;      r = rot(projection, angle, /interp)
      r = rot(projection, angle, cubic=-0.5)
      out[0,0,i] = r
   endfor
   print, 'Time to rotate=', systime(1) - t0
   print, 'Writing output file ', outfile
   write_tomo_volume, outfile, out
end
