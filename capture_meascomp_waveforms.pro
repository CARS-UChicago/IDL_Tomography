pro capture_meascomp_waveforms, prefix, points, time, first_chan, num_chans, data, filename
  ; Collects data from Measurement Computing devices in "waveform digitizer" mode.
  
  t = caput(prefix + 'NumPoints', points)
  t = caput(prefix + 'Dwell',     time)
  t = caput(prefix + 'FirstChan', first_chan-1)
  t = caput(prefix + 'NumChans',  num_chans-1)
  
  time = string(systime(/julian), format= $
               '(C(CDI2.2,"-",CMoA,"-",CYI,X,CHI2.2,":",CMI2.2,":",CSI2.2))')
  t = caput(prefix + 'Run', 1)
  
  while (1) do begin
    t = caget(prefix + 'Run', status)
    if (status eq 0) then break
    wait, .1
  endwhile
  
  if (n_elements(filename) eq 0) then return
  
  t = caget(prefix + 'CurrentPoint', nord)
  data = dblarr(nord, num_chans)
  for i=0, num_chans-1 do begin
    chan = first_chan + i
    t = caget(prefix + strtrim(chan, 2) + 'VoltWF', d, max=nord)
    data[0, i] = d
  endfor
  t = caget(prefix + 'TimeWF', capture_time, max=nord)
  
  openw, lun, filename, /get, /append
  line = 'Prefix: ' + prefix
  printf, lun, line
  line = 'Start time: ' + time
  printf, lun, line
  line = 'Time | '
  for i=0, num_chans-1 do begin
    chan = first_chan + i
    line = line + prefix + strtrim(chan, 2) + 'VoltWF | '
  endfor
  printf, lun, line
  for i=0, nord-1 do begin
    line = string(capture_time[i]) + ' | '
    for j=0, num_chans-1 do begin
      line = line + strtrim(data[i, j], 2) + ' | '
    endfor
    printf, lun, line
  endfor
  
  free_lun, lun
end

 
  