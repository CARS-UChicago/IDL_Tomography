pro collect_tomoscan_stack, filename, loops=loops, $
                            yposition=yposition, ystart=ystart, ystep=ystep, ny=ny, $
                            energy=energy, feedback=feedback, $
                            amplifier=amplifier, exposure=exposure, tilt=tilt, wait_time=wait_time, $
                            delay_time=delay_time, shutter=shutter

ystage = obj_new('epics_motor', '13BMD:m115')

suffix = [$
  'A',  'B',  'C',  'D',  'E',  'F',  'G',$
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

energyPV    = '13BMA:E:Energy'
feedbackPV  = '13BMA:mono_pid1'
amplifierPV = '13BMD:A3sens_num.VAL'
exposurePV  = '13BMDPG1:cam1:AcquireTime'
tiltPV      = '13BMA:MON:t1.AX'
file_PV     = '13BMDPG1:TS:FileName' 
acquire_PV  = '13BMDPG1:TS:StartScan'
shutter_PV  = '13BMD:Unidig2Bo10'

if (n_elements(ystart) ne 0) then begin
  yposition = ystart * findgen(ny)*ystep
endif
num_y = n_elements(yposition)
num_energy = n_elements(energy)

if (n_elements(loops) eq 0) then loops = 1
if (n_elements(amplifier) ne 0) then amplifier = strtrim(amplifier, 2)
if (n_elements(wait_time) eq 0) then wait_time=30
if (n_elements(delay_time) eq 0) then delay_time=0

suffix_num = 0
nj = max([num_y, 1])
ni = max([num_energy, 1])
help, num_energy, nj, ni
for loop=0, loops-1 do begin
  for j=0, nj-1 do begin
    if (delay_time ne 0) then begin
      if (keyword_set(shutter)) then t = caput(shutter_pv, 0)
      print, 'Waiting for ', delay_time, ' seconds'
      wait, delay_time
      if (keyword_set(shutter)) then t = caput(shutter_pv, 1)
      wait, 1
    endif
    if (num_y gt 0) then begin
      print,'Moving Y stage for point ', j, ' to ', yposition[j]
      ; Move the stage
      ystage->move, yposition[j]
      ; Wait until the stage stops moving before taking dataset
      ystage->wait
    endif
  
    for i=0, ni-1 do begin
      if (num_energy gt 0) then begin
        print,'Scan=', i+1,' energy=', energy[i]
        ; Move the energy
        if (n_elements(amplifier) ne 0) then t = caput(amplifierPV, amplifier[i])
        if (n_elements(exposure) ne 0) then t = caput(exposurePV, exposure[i])
        if (n_elements(tilt) ne 0) then t = caput(tiltPV, tilt[i])
        t = caput(energyPV, energy[i], /wait)
        if (n_elements(feedback) ne 0) then t = caput(feedbackPV, feedback[i], /wait)
        print, 'Waiting ' + strtrim(wait_time,2) + ' seconds for mono to stabilize ...'
        wait, wait_time
      endif
      file = filename + '_' + suffix[suffix_num]
      print, 'Collecting dataset ' + file
      t = caput(file_PV, [byte(file),0], /wait)
      wait, 1.0
      t = caput(file_PV, [byte(file),0], /wait)
      wait, 1.0
      status = caput(acquire_PV, 1)
      repeat begin
          wait, .1
          status = caget(acquire_PV, busy)
      endrep until (not busy)
  
      suffix_num = suffix_num + 1
    endfor
  endfor
endfor

end

