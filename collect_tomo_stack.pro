pro take_picture, suffix

file_PV = '13BMD:CCD_base_file'
synch_PV = '13BMD:CCD_synch'

status = caput(file_PV, suffix)
   status = caput(synch_PV, 1)
   repeat begin
      status = caget(synch_PV, busy)
      wait, .1
   endrep until (not busy)

end


pro collect_tomo_stack, y_start, y_step, points

ystage = obj_new('epics_motor', '13BMD:m25')

suffix = ['A', 'B', 'C', 'D', 'E', 'F', 'G',$
          'H', 'I', 'J', 'K', 'L', 'M', 'N',$
          'O', 'P', 'Q', 'R', 'S', 'T', 'U',$
          'V', 'W', 'X', 'Z']


;Enter your macro here
for i=0, points-1 do begin

   ypos = y_start + i*y_step
   print,i,ypos

   ;Move the stage
   ystage->move, ypos

   ;Wait until the stage stops moving before taking picture.
   ystage->wait

   ;Run a tomo image. IDL will wait here until
   ;the tomo image is finished.
   take_picture, suffix[i]
 endfor

end

