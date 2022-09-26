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


pro collect_tomo_energy_ystack, y_start, y_step, ypoints, energy, feedback, $
                                amplifier=amplifier, exposure=exposure, tilt=tilt, wait_time=wait_time

ystage = obj_new('epics_motor', '13BMD:m90')

suffix = ['A', 'B', 'C', 'D', 'E', 'F', 'G',$
          'H', 'I', 'J', 'K', 'L', 'M', 'N',$
          'O', 'P', 'Q', 'R', 'S', 'T', 'U',$
          'V', 'W', 'X', 'Z', $
          '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']


energyPV =    '13BMA:E:Energy'
feedbackPV =  '13BMA:mono_pid1'
amplifierPV = '13BMD:A3sens_num.VAL'
exposurePV =  '13BMDRP1:cam1:AcquireTime'
tiltPV =      '13BMA:MON:t1.AX'

if (n_elements(amplifier) ne 0) then amplifier = strtrim(amplifier, 2)
if (n_elements(wait_time) eq 0) then wait_time=30

suffix_num=0
;Enter your macro here
for j=0, ypoints-1 do begin
   ypos = y_start + j*y_step
   print,'Moving Y stage for point ', j, ' to ', ypos
   ;Move the stage
   ystage->move, ypos

   ;Wait until the stage stops moving before taking picture.
   ystage->wait

   for i=0, n_elements(energy)-1 do begin
      print,'Scan=', i+1,' energy=', energy[i], ' feedback=',feedback[i], ' suffix=', suffix[suffix_num]

      ;Move the energy
      if (n_elements(amplifier) ne 0) then t = caput(amplifierPV, amplifier[i])
      if (n_elements(exposure) ne 0) then t = caput(exposurePV, exposure[i])
      if (n_elements(tilt) ne 0) then t = caput(tiltPV, tilt[i])
      t = caput(energyPV, energy[i], /wait)
      t = caput(feedbackPV, feedback[i], /wait)

      print, 'Waiting ' + strtrim(wait_time,2) + ' seconds for mono to stabilize ...'
      wait, wait_time

      ;Run a tomo image. IDL will wait here until
      ;the tomo image is finished.
      take_picture, suffix[suffix_num]
      suffix_num = suffix_num+1
    endfor
 endfor

end

