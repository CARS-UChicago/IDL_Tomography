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


pro collect_fast_tomo_stack, points, delay_time

suffix = ['A',  'B',  'C',  'D',  'E',  'F',  'G',$
          'H',  'I',  'J',  'K',  'L',  'M',  'N',$
          'O',  'P',  'Q',  'R',  'S',  'T',  'U',$
          'V',  'W',  'X',  'Y',  'Z',            $
         'ZA', 'ZB', 'ZC', 'ZD', 'ZE', 'ZF', 'ZG',$
         'ZH', 'ZI', 'ZJ', 'ZK', 'ZL', 'ZM', 'ZN',$
         'ZO', 'ZP', 'ZQ', 'ZR', 'ZS', 'ZT', 'ZU',$
         'ZV', 'ZW', 'ZX', 'ZY', 'ZZ',            $
         'ZZA', 'ZZB', 'ZZC', 'ZZD', 'ZZE', 'ZZF', 'ZZG',$
         'ZZH', 'ZZI', 'ZZJ', 'ZZK', 'ZZL', 'ZZM', 'ZZN',$
         'ZZO', 'ZZP', 'ZZQ', 'ZZR', 'ZZS', 'ZZT', 'ZZU',$
         'ZZV', 'ZZW', 'ZZX', 'ZZY', 'ZZZ']

;open_shutterPV = '13BMA:OpenBMDShutter.PROC'
;close_shutterPV = '13BMA:CloseBMDShutter.PROC'
;rotation_stage = obj_new('epics_motor', '13BMD:m38')
open_shutterPV = '13LAB:UnidigBo0'
close_shutterPV = '13LAB:UnidigBo1'
rotation_stage = obj_new('epics_motor', '13LAB:m1')
shutter_delay = 1

;Enter your macro here
for i=0, points-1 do begin

   ; Set the rotation stage back to 0
   print, 'Moving rotation stage back to 0'
   rotation_stage->move, 0
   rotation_stage->wait

   ;Open the shutter
   print, 'Opening shutter'
   t = caput(open_shutterPV, 1)
   wait, shutter_delay

   ;Run a tomo image. IDL will wait here until
   ;the tomo image is finished.
   print, 'collecting data set ', suffix[i]
   take_picture, suffix[i]
   
    ;Close the shutter
   print, 'Closing shutter'
   t = caput(close_shutterPV, 1)
   wait, shutter_delay
   
   ; Set the rotation stage back to 0
   print, 'Moving rotation stage back to 0'
   rotation_stage->move, 0
   rotation_stage->wait
   
   if (i lt points-1) then begin
     ; Wait to do next data set
     print, 'Wait for ', delay_time, ' seconds for next data set'
     wait, delay_time
   endif
   
 endfor

end

