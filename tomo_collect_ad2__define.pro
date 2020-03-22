pro tomo_collect_ad2::alignmentScan
  ; Set the file name
  self->SetFileName
  self.scan.time_per_angle = self->computeFrameTime()
  ; Move to 0 degrees
  widget_control, self.widgets.status, set_value='Moving to 0 degrees'
  self.scan.rotation_motor->move, 0.
  self.scan.rotation_motor->wait
  widget_control, self.widgets.status, set_value='Moving sample out'
  ; Move sample out
  self->moveSampleOut

  ; Collect flat fields
  comment1 = 'Start angle= 0.'
  comment2 = 'Angle step= 1.'
  comment3 = 'Flat Fields= '
  for i = 0, self.scan.num_flatfields-1 do begin
    comment3 = comment3 + strtrim(i,2) + ' '
  endfor
  self->setFileComments, [comment1, comment2, comment3]
  widget_control, self.widgets.status, set_value='Collecting flat fields'
  self->collectNFrames, self.scan.num_flatfields

   ; Move sample in
   widget_control, self.widgets.status, set_value='Moving sample in'
  self->moveSampleIn
  ; Collect 0 degree image into file
  ; write OTF comments
  self->setFileComments, [comment1, comment2, '']
  widget_control, self.widgets.status, set_value='Collecting 0 degree image'
  self->collectNFrames, 1

  ; Move to 180
  widget_control, self.widgets.status, set_value='Rotating to 180 degrees'
  self.scan.rotation_motor->move, 180.
  self.scan.rotation_motor->wait
  ; Collect 180 degree image into file
  comment1 = 'Start angle= 180.'
  self->setFileComments, [comment1, comment2, '']
  widget_control, self.widgets.status, set_value='Collecting 180 degree image'
  self->collectNFrames, 1

  ; Move back to 0 degrees
  widget_control, self.widgets.status, set_value='Moving to 0 degrees'
  self.scan.rotation_motor->move, 0.
  self.scan.rotation_motor->wait
  widget_control, self.widgets.status, set_value='Aligment scan complete' 
end


pro tomo_collect_ad2::startScan

  if (not self.epics_pvs_valid) then return

  ; check to see whether the desired filename exists or no
  t = file_search(self.scan.filename+'1'+'*', count = count)
  if (count ne 0) then t = dialog_message($
    'File name already exists at that location. Overwrite data?',/default_no,/question)
  if (t eq 'No') then begin
    t = dialog_message('Scan canceled')
    widget_control, self.widgets.status, set_value='Scan canceled'
    t = caput(self.epics_pvs.camera_name + 'TC:ScanStatus', [byte('Scan canceled'),0B])
    return
  endif

  ; Get initial time and start clock widget
  self.scan.start_clock = systime(1,/seconds)

  ; Stop CCD acquisition, since we could be in focus mode
  self.scan.ccd->setProperty,'Acquire',0
  busy = self.scan.ccd->getProperty('DetectorState_RBV',string=0)
  while (busy ne 0) do begin
    wait, .1
    busy = self.scan.ccd->getProperty('DetectorState_RBV',string=0)
  endwhile

  ; need to acquire an image to get image dimensions
  ; This is used in prepareScan to get ArraySize_RBV
  ; Point Grey needs to collect 3 frames because first 2 are bad exposure.
  self->setTriggerMode, 'MCSInternal', 3
  wait, .1
  self.scan.ccd->setProperty, 'Acquire', 1
  wait, .1
  t = caput(self.epics_pvs.sis_mcs+'EraseStart', 1)
 wait, .1
  ; wait for capturing to finish
  busy = self.scan.ccd->getProperty('Acquire_RBV',string=0)
  while (busy ne 0) do begin
    wait, .01
    busy = self.scan.ccd->getProperty('Acquire_RBV',string=0)
  endwhile
  t = caput(self.epics_pvs.sis_mcs+'StopAll', 1)
  ; The MCS LNE output stays low after stopping MCS for up to the exposure time = LNE output width
  ; Need to wait for the exposure time
  wait, self.scan.exposure_time

  ; begin clock
  if(self.scan.num_flatfields eq 0) then begin
    self.scan.num_groups = strtrim(CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment),2)
  endif else begin
    self.scan.num_groups = strtrim(2*CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment)+1,2)
  endelse
  widget_control, self.widgets.clock_timer, timer = .1

  ; reset current scan
  self.scan.current_point = 0

  widget_control, self.widgets.scan_point, set_value=''

  ; reset the number of frames between FF measurements to be =< the total number of frames
  if(self.scan.flatfield_increment gt self.scan.num_angles) then begin
    self.scan.flatfield_increment = (self.scan.num_angles)
  endif

  ; set number of flat fields in each flat field scan <=20, >1 for OTF scans, and >0 for Fast/slow scans
  if(self.scan.num_flatfields gt 20) then begin
    self.scan.num_flatfields = 20
  endif
  if(self.scan.num_flatfields eq 1) then begin
    self.scan.num_flatfields = 2
  endif
  self->copy_settings_to_widgets

  ; remember old motor speed
  self.scan.motor_speed_old = self.scan.rotation_motor->get_maximum_speed()
  if (self.scan.motor_speed_old eq 0) then begin
    print, 'Error, read maximum rotation speed, got 0, trying again'
    wait, .1
    self.scan.motor_speed_old = self.scan.rotation_motor->get_maximum_speed()
    print, 'Second try, after waiting 0.1 second got', self.scan.motor_speed_old
    ; If the second try also returned 0 then use 30.  TOTAL KLUDGE, WE NEED TO TRACK DOWN WHY IT IS GETTING A READ ERROR
    if (self.scan.motor_speed_old eq 0) then self.scan.motor_speed_old = 30.
  endif

  widget_control, self.widgets.motor_speed, set_value=self.scan.motor_speed_old
  widget_control, self.widgets.status, set_value='Zeroing motors'

  if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
    ; set #angles between flat fields to maximum value so OTF normal stack is not too big
    arraysize = self.scan.ccd->getProperty('ArraySize_RBV', string = 0)
    increment = floor(.800*10.^9/(1.0*arraysize))
    if (self.scan.flatfield_increment gt increment) then begin
      self.scan.flatfield_increment = increment
      widget_control, self.widgets.flatfield_increment, set_value=self.scan.flatfield_increment
    endif
  endif else begin
    t = caput(self.scan.file_control+'Capture', 0)
  endelse

  ; set up array to keep track of motor destinations during the OTF scan
  ptr_free, self.scan.otf_rotation_array
  otf_rotation_array_size = 2*ceil(1.0*self.scan.num_angles/(1.0*self.scan.flatfield_increment)) + 1
  self.scan.otf_rotation_array = ptr_new(fltarr(otf_rotation_array_size))
  (*self.scan.otf_rotation_array)[0] = self.scan.rotation_start
  for i = 1, otf_rotation_array_size-1 do begin
    if (fix(i/2.) ne i/2.) then $
      (*self.scan.otf_rotation_array)[i] = (i+1)/2.*self.scan.flatfield_increment * $
      self.scan.rotation_step + self.scan.rotation_start
    if (fix(i/2.) eq i/2.) then $
      (*self.scan.otf_rotation_array)[i] = i/2.*self.scan.flatfield_increment * $
      self.scan.rotation_step + self.scan.rotation_start
    if ((*self.scan.otf_rotation_array)[i] gt self.scan.rotation_stop) then $
      (*self.scan.otf_rotation_array)[i] = $
      self.scan.num_angles * self.scan.rotation_step + self.scan.rotation_start
  endfor
  ; if the number of flat fields is 0, change the path of the OTF code to skip all flat field collections
  if(self.scan.num_flatfields eq 0) then begin
    ptr_free, self.scan.otf_rotation_array
    otf_rotation_array_size = ceil(1.0*self.scan.num_angles/(1.0*self.scan.flatfield_increment))+1
    self.scan.otf_rotation_array = ptr_new(fltarr(otf_rotation_array_size))
    (*self.scan.otf_rotation_array)[0] = self.scan.rotation_start
    for i = 1, otf_rotation_array_size - 1 do begin
      (*self.scan.otf_rotation_array)[i] = $
        i*self.scan.flatfield_increment * self.scan.rotation_step + self.scan.rotation_start
      if ((*self.scan.otf_rotation_array)[i] gt self.scan.rotation_stop) then $
        (*self.scan.otf_rotation_array)[i] = $
        self.scan.num_angles * self.scan.rotation_step + self.scan.rotation_start
    endfor
  endif
  ; move the motor into the initial position
  (*self.scan.otf_rotation_array) = (*self.scan.otf_rotation_array) - self.scan.rotation_step/2.
  self.scan.rotation_motor->move, (*self.scan.otf_rotation_array)[0]
  ; The SIS does not put out pulses until after one dwell period so need to back up one angle step
  if (self.scan.camera_manufacturer eq self.camera_types.POINT_GREY) then begin
    self.scan.rotation_motor->move, (*self.scan.otf_rotation_array)[0] - self.scan.rotation_step
  endif
  self.scan.rotation_motor->wait

  ; Set motor speed
  self.scan.time_per_angle = self->computeFrameTime()
  speed = self.scan.rotation_step / self.scan.time_per_angle
  motor_resolution = self.scan.rotation_motor->get_scale()
  self.scan.motor_speed = floor(abs(speed*motor_resolution))/abs(motor_resolution)

  widget_control, self.widgets.motor_speed, set_value=self.scan.motor_speed
  self.scan.rotation_motor->SET_SLEW_SPEED, self.scan.motor_speed

  ; Write the setup file
  status = self->saveSettings(self.scan.filename + '.setup')

  widget_control, self.widgets.scan_point, $
    set_value=strtrim(0,2) + '/' + strtrim(self.scan.num_angles,2)

  ; set the external prescale according to the step size, use motor resolution steps per degree (user unit)
  t = caput(self.epics_pvs.sis_mcs+'StopAll', 1)
  t = caput(self.epics_pvs.sis_mcs+'Prescale', FLOOR(ABS(self.scan.rotation_step  * motor_resolution)))

  ; Collect dark currents if non-zero
  if (self.scan.num_dark_currents gt 0) then begin
    widget_control, self.widgets.status, set_value='Collecting dark current'
    t = caput(self.epics_pvs.close_shutter, self.scan.close_shutter_value)
    wait, 2.
    self->setFileComments, ['','Type=DARK_FIELD','']
    self->SetFileName, 'DarkCurrent'
    self->collectNFrames, self.scan.num_dark_currents
    t = caput(self.epics_pvs.open_shutter, self.scan.open_shutter_value)
    wait, 2.
  endif

  ; Set the file name
  self->SetFileName
  
  ; send state to FLAT_FIELD
  self->setState, self.scan.states.FLAT_FIELD

  widget_control, self.widgets.num_groups, set_value = self.scan.num_groups

  ; if the number of flat fields is 0, change the path of the OTF code to skip all flat field collections
  if(self.scan.num_flatfields eq 0) then begin
    self->setState, self.scan.states.NORMAL
    self.scan.current_point++
    self->moveSampleIn
  endif else begin
    self->moveSampleOut
  endelse

  if(self.scan.num_flatfields eq 0) then begin
    self.scan.num_groups = strtrim(CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment),2)
  endif else begin
    self.scan.num_groups = strtrim(2*CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment)+1,2)
  endelse
  widget_control, self.widgets.num_groups, set_value = self.scan.num_groups
  widget_control, self.widgets.clock_timer, timer = .1

  ; reset widgets to the start
  widget_control, self.widgets.start_scan, sensitive=0
  widget_control, self.widgets.abort_scan, sensitive=1
  widget_control, self.widgets.alignment_scan, sensitive=0

  wait, .1 ; Wait for motors to definitely start moving
  widget_control, self.widgets.scan_timer, timer=self.scan_timer_interval
end


pro tomo_collect_ad2::collectNFrames, num_frames

  self->setTriggerMode, 'MCSInternal', num_frames

  if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
    ; start capturing
    self.scan.ccd->setProperty, 'Acquire', 1
    wait,.1
    ; Start the MCS
    t = caput(self.epics_pvs.sis_mcs+'EraseStart', 1)

    ; check for when measurements are finished
    ccd_busy1 = self.scan.ccd->getProperty('DetectorState_RBV', string = 0)
    ccd_busy2 = self.scan.ccd->getProperty('Acquire_RBV',string = 0)
    while (ccd_busy1 OR ccd_busy2) do begin
      wait, .01
      ccd_busy1 = self.scan.ccd->getProperty('DetectorState_RBV', string = 0)
      ccd_busy2 = self.scan.ccd->getProperty('Acquire_RBV',string = 0)
    endwhile
  endif else begin  ; Not Roper
    ; start ncdf generator
    t = caput(self.scan.file_control+'Capture', 1)
    ; start capturing
    self.scan.ccd->setProperty, 'Acquire', 1
    wait, .5
    ; Start the MCS
    t = caput(self.epics_pvs.sis_mcs+'EraseStart', 1)
    ; check for when flat field measurements are finished
    t = caget(self.scan.file_control+'NumCaptured_RBV',images_captured)
    while (images_captured ne num_frames) do begin
      wait, .01
      t = caget(self.scan.file_control+'NumCaptured_RBV',images_captured)
    endwhile
    self.scan.ccd->setProperty,'Acquire', 0
  endelse

  ; save images and close file
  widget_control, self.widgets.status, set_value='Saving File'
  if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
    self.scan.ccd->setProperty, 'Acquire', 0

    ; save file
    widget_control, self.widgets.status, set_value='Saving File'
    t = caput(self.scan.file_control+'WriteFile', 1)
    wait, .1
    busy = 1
    while (busy ne 0) do begin
      widget_control, self.widgets.status, set_value='Waiting for WinView on last frame'
      wait, .01
      t = caget(self.scan.file_control+'WriteFile_RBV', busy)
    endwhile
    wait, .1
    ; wait for winview
    biny = self.scan.ccd->getProperty('BinY', string = 0)
    binx = self.scan.ccd->getProperty('BinX', string = 0)
    wait, 5.*num_frames/100/biny/binx
  endif else begin  ; Not Roper
    self.scan.ccd->setProperty, 'Acquire', 0
    wait, .1
    t = caput(self.scan.file_control+'Capture', 0)
    busy1 = 1
    busy2 = 1
    while (busy1 eq 1 OR busy2 eq 1) do begin
      wait, .01
      busy1 = self.scan.ccd->getProperty('Acquire_RBV', string = 0)
      t = caget(self.scan.file_control+'WriteFile_RBV',busy2)
    endwhile
    wait, 1
  endelse

  ; Clear comments
  widget_control, self.widgets.status, set_value='File Saved'
  self->setFileComments, ['', '', '']
end

pro tomo_collect_ad2::SetFileName, fileNameExtra
  sep = path_sep()
  filename = (strsplit(self.scan.filename, sep, /extract)) $
    [n_elements(strsplit(self.scan.filename, sep, /extract))-1]
  if (n_elements(fileNameExtra) ne 0) then filename = filename + fileNameExtra
  t = caput(self.scan.file_control+'FileName', [byte(filename), 0B])
  if (n_elements(strsplit(self.scan.filename, sep, /extract)) ge 2) then begin
    filepath = (strjoin((strsplit(self.scan.filename, sep, /extract)) $
      [0:n_elements(strsplit(self.scan.filename, sep, /extract))-2], sep) + sep)
    t = caput(self.scan.file_control+'FilePath', [byte(filepath), 0B])
  endif
  t = caput(self.scan.file_control+'FileNumber', 1)
  t = caput(self.scan.file_control+'AutoIncrement', 'Yes')
  wait, .01
  ; set capture mode to stream capture
  t = caput(self.scan.file_control+'FileWriteMode', 'Stream')
end

function tomo_collect_ad2::checkBeam
  t = 1
  while (t ne 0) do begin
    t = caget(self.epics_pvs.beam_ready, beam_ready)
    return, beam_ready
  endwhile
end

pro tomo_collect_ad2::scanPoll
  ; This procedure is called when the scan timer expires.  It takes action depending
  ; upon what phase the scan is in and whether it can go to the next phase
  if (self.scan.current_state eq self.scan.states.ABORT_SCAN) then begin
    self->stopScan

    ; OTF scanning state machine
  endif else if (self.scan.current_state eq self.scan.states.FLAT_FIELD) then begin
    ; wait until motors have stopped adjusting
    if (self.scan.rotation_motor->done() eq 0) then return
    if (self.scan.sample_x_motor->done() eq 0) then return
    if (self.scan.sample_y_motor->done() eq 0) then return

    ; Create OTF comments
    comment1 = 'Start angle= ' + $
      strtrim((*self.scan.otf_rotation_array)[self.scan.current_point] + self.scan.rotation_step/2., 2)
    comment2 = 'Angle step= '+strtrim(self.scan.rotation_step, 2)
    comment3 = 'Flat Fields= '
    for i = 0, self.scan.num_flatfields-1 do begin
      comment3 = comment3 + strtrim(i,2) + ' '
    endfor
    self->setFileComments, [comment1, comment2, comment3]

    self->collectNFrames, self.scan.num_flatfields

    ; send OTF scan to STOP_SCAN if has finished
    if(self.scan.current_point eq n_elements((*self.scan.otf_rotation_array))-1) then begin
      self->stopScan
      return
    endif

    ; increase point index
    self.scan.current_point++

    ; move sample in
    self->moveSampleIn

    ; send state to NORMAL
    self->setState, self.scan.states.NORMAL

  endif else if (self.scan.current_state eq self.scan.states.NORMAL) then begin
    ; check that motors are done moving
    if (self.scan.rotation_motor->done() eq 0) then return
    if (self.scan.sample_x_motor->done() eq 0) then return
    if (self.scan.sample_y_motor->done() eq 0) then return

    ; prepare for OTF scanning
    widget_control, self.widgets.status, set_value='Normal Scan'

    ; rotate motors back by one motor step
    motor_resolution = self.scan.rotation_motor->get_scale()
    res_sign = (abs(motor_resolution) eq motor_resolution)
    step_sign = (abs(self.scan.rotation_step) eq self.scan.rotation_step)
    if (res_sign eq step_sign) then sign=1 else sign=-1
    self.scan.rotation_motor->move, abs(1./motor_resolution)*sign, relative = 1
    self.scan.rotation_motor->wait

    ; calculate number of images/struck triggers and set up camera to acquire that number
    struck_triggers = round(((*self.scan.otf_rotation_array)[self.scan.current_point] - $
      (*self.scan.otf_rotation_array)[self.scan.current_point-1])/self.scan.rotation_step)

    self->setTriggerMode, 'MCSExternal', abs(struck_triggers)

    ; write OTF comments
    comment1 = 'Start angle= ' + $
      strtrim((*self.scan.otf_rotation_array)[self.scan.current_point-1] + self.scan.rotation_step/2., 2)
    comment2 = 'Angle step= '+strtrim(self.scan.rotation_step, 2)
    comment3 = ''
    self->setFileComments, [comment1, comment2, comment3]

    ; ready image capturing
    if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
      ccd_busy1 = 1
      ccd_busy2 = 1
      while (ccd_busy1 OR ccd_busy2) do begin
        wait, .01
        ccd_busy1 = self.scan.ccd->getProperty('DetectorState_RBV', string = 0)
        ccd_busy2 = self.scan.ccd->getProperty('Acquire_RBV',string = 0)
      endwhile
    endif else begin
      ; start netCDF generator
      t = caput(self.scan.file_control+'Capture', 1)
      ; check that capture stack is ready
      t = caget(self.scan.file_control+'NumCaptured_RBV', images_captured)
      while (images_captured ne 0) do begin
        wait, .1
        t = caget(self.scan.file_control+'NumCaptured_RBV',images_captured)
        ; allow for abort event
        event = widget_event(/nowait, self.widgets.abort_scan)
        if (event.id ne 0) then begin
          self->abort_scan
          return
        endif
      endwhile
    endelse

    ; start camera
    self.scan.ccd->setProperty, 'Acquire', 1
    wait, .1
    if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
      wait, 1
      ccd_busy1 = self.scan.ccd->getProperty('DetectorState_RBV', string = 0)
      ccd_busy2 = self.scan.ccd->getProperty('Acquire_RBV',string = 0)
      while (~ccd_busy1 OR ~ccd_busy2) do begin
        wait, .01
        ccd_busy1 = self.scan.ccd->getProperty('DetectorState_RBV', string = 0)
        ccd_busy2 = self.scan.ccd->getProperty('Acquire_RBV',string = 0)
      endwhile
    endif
    ; turn on MCS
    t = caput(self.epics_pvs.sis_mcs+'EraseStart', 1)
    wait, .1
    ; turn on motor, begin triggering camera through MCS
    self.scan.rotation_motor->move,(*self.scan.otf_rotation_array)[self.scan.current_point]

    ; send state to NORMAL_Acquisition
    self->setState, self.scan.states.NORMAL_ACQUISITION

  endif else if (self.scan.current_state eq self.scan.states.NORMAL_ACQUISITION) then begin
    ; acquire last motor position, check that the current motor position is not equal to it
    current_motor_position = self.scan.rotation_motor->get_position(readback = 'RBV')

    widget_control, self.widgets.scan_point, $
      set_value = strtrim(floor(max([(current_motor_position-self.scan.rotation_start) /  $
      self.scan.rotation_step+1, 0])), 2) + $
      '/' + strtrim(self.scan.num_angles, 2)
    if (NOT self.scan.rotation_motor->done()) then begin
      ; update scan point widget
      widget_control, self.widgets.status, set_value='Normal Scan Image Acquisition'
      return
    endif
    ; update widgets
    widget_control, self.widgets.status, set_value='Normal Scan Complete'
    ; stop motor
    ; send state to Normal_readout
    self->setState, self.scan.states.NORMAL_READOUT

  endif else if (self.scan.current_state eq self.scan.states.NORMAL_READOUT) then begin
    ; save images and close file
    if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
      ; If Roper camera is still acquiring and the data file is still open, close it
      exposure = self.scan.ccd->getProperty('AcquireTime',string = 0)
      wait, 5.* exposure
      widget_control, self.widgets.status, set_value='Stopping run'

      ; wait for camera to return to idle state
      ccd_busy1 = self.scan.ccd->getProperty('DetectorState_RBV', string = 0)
      ccd_busy2 = self.scan.ccd->getProperty('Acquire_RBV',string = 0)
      while (ccd_busy1 OR ccd_busy2) do begin
        wait, .01
        ccd_busy1 = self.scan.ccd->getProperty('DetectorState_RBV', string = 0)
        ccd_busy2 = self.scan.ccd->getProperty('Acquire_RBV',string = 0)
        event = widget_event(/nowait, self.widgets.abort_scan)
        if (event.id ne 0) then begin
          self->abort_scan
          return
        endif
      endwhile
      wait,.1
      ; save images
      widget_control, self.widgets.status, set_value='Saving data'
      t = caget(self.scan.file_control+'WriteFile_RBV',busy); sets the monitor to 0
      t = caput(self.scan.file_control+'WriteFile', 1)
      wait, .1
      newmonitor = 0
      while (~newmonitor) do begin ; check that the camera has begun writing out
        wait, .01
        newmonitor = caCheckmonitor(self.scan.file_control+'WriteFile_RBV')
        event = widget_event(/nowait, self.widgets.abort_scan)
        if (event.id ne 0) then begin
          self->abort_scan
          return
        endif
      endwhile
      t = caget(self.scan.file_control+'WriteFile_RBV',busy); sets the monitor to 0
      newmonitor = 0
      while (~newmonitor) do begin ; check that the camera has finished writing out
        wait, .1
        newmonitor = caCheckmonitor(self.scan.file_control+'WriteFile_RBV')
        event = widget_event(/nowait, self.widgets.abort_scan)
        if (event.id ne 0) then begin
          self->abort_scan
          return
        endif
      endwhile
      t = caget(self.scan.file_control+'WriteFile_RBV',busy); sets the monitor to 0
      ; close docfile
      widget_control, self.widgets.status, set_value='Closing docFile'
      ; wait for  winview
      nimages = self.scan.ccd->getProperty('NumImages', string = 0)
      biny = self.scan.ccd->getProperty('BinY', string = 0)
      binx = self.scan.ccd->getProperty('BinX', string = 0)
      wait, 5.*nimages/100/biny/binx
      ; wait, 30
    endif else begin
      widget_control, self.widgets.status, set_value='Saving data'
      ; wait for the stream capture buffer to be written
      t = caget(self.scan.file_control+'NumCaptured_RBV',captured_images)
      t = caget(self.scan.file_control+'NumCapture',total_images)
      CaptureState = 1
      while (captured_images ne total_images) $
        AND (CaptureState ne 0) do begin
        wait, .1
        t = caget(self.scan.file_control+'NumCaptured_RBV', captured_images)
        t = caget(self.scan.file_control+'Capture_RBV', CaptureState)
        ; allow for abort event
        event = widget_event(/nowait, self.widgets.abort_scan)
        if (event.id ne 0) then begin
          self->abort_scan
          return
        endif
      endwhile

      ; Force Acquire and Capture to 0
      self.scan.ccd->setProperty,'Acquire', 0
      wait, .1
      t = caput(self.scan.file_control+'Capture', 0)
      busy1 = 1
      busy2 = 1
      while (busy1 eq 1 OR busy2 eq 1) do begin
        wait, .01
        busy1 = self.scan.ccd->getProperty('Acquire_RBV', string = 0)
        t = caget(self.scan.file_control+'WriteFile_RBV',busy2)
      endwhile

    endelse
    widget_control, self.widgets.status, set_value='File Saved'
    self->setFileComments, ['', '', '']


    ; send OTF scan to STOP_SCAN if has finished
    if(self.scan.current_point eq n_elements((*self.scan.otf_rotation_array))-1) then begin
      self->stopScan
      return
    endif

    self.scan.current_point++
    ; stop MCS
    t = caput(self.epics_pvs.sis_mcs+'StopAll',1)
    ; The MCS LNE output stays low after stopping MCS for up to the exposure time = LNE output width
    ; Need to wait for the exposure time
    wait, self.scan.exposure_time

    if (self.scan.num_flatfields eq 0) then begin
      self->setState, self.scan.states.NORMAL
    endif else begin
      ; send state to Flat Field
      self->setState, self.scan.states.FLAT_FIELD
      ; move sample out
      self->moveSampleOut
    endelse

  endif
end


pro tomo_collect_ad2::setState, state
  self.scan.current_state = state
  widget_control, self.widgets.status, set_value=self.scan.state_strings[state]
end


pro tomo_collect_ad2::abortScan
  self->setState, self.scan.states.ABORT_SCAN
end


pro tomo_collect_ad2::stopScan
  self->moveSampleIn

  ; In case of OTF
  ; first stop the external triggers
  t = caput(self.epics_pvs.sis_mcs+'StopAll',1)
  ; stop the motor
  t = caput(self.epics_pvs.rotation+'.SPMG',0)
  wait, .1
  t = caput(self.epics_pvs.rotation+'.SPMG',3)

  if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
    ; check if camera is still acquiring
    busy = self.scan.ccd-> getProperty('Acquire_RBV', string = 0)
    if (busy ne 0) then begin ; if the abort was triggered mid-scan
      self.scan.ccd->setProperty, 'Acquire',0
      wait, .1
      ; wait for acquisition
      ccd_busy1 = 1
      ccd_busy2 = 1
      while (ccd_busy1 OR ccd_busy2) do begin
        wait, .01
        ccd_busy1 = self.scan.ccd->getProperty('DetectorState_RBV', string = 0)
        ccd_busy2 = self.scan.ccd->getProperty('Acquire_RBV',string = 0)
      endwhile
      wait,.1

      ; write OTF comments
      comment1 = 'Start angle= ' + $
        strtrim((*self.scan.otf_rotation_array)[self.scan.current_point-1] + self.scan.rotation_step/2., 2)
      comment2 = 'Angle step= ' + strtrim(self.scan.rotation_step, 2)
      comment3 = ''
      self->setFileComments, [comment1, comment2, comment3]

      ; save images
      widget_control, self.widgets.status, set_value='Saving data'
      t = caput(self.scan.file_control+'WriteFile',1)
      wait, .1
      busy = 1
      while (busy) do begin
        wait, .01
        t = caget(self.scan.file_control+'WriteFile_RBV',busy)
      endwhile
      wait, .1
      widget_control, self.widgets.status, set_value='Closing docFile'
    endif else begin ; if the scan was already finished
      self.scan.ccd -> setProperty, 'Acquire',0
      wait, .1
    endelse

    ; wait for winview
    busy = 1
    while (busy ne 0) do begin
      widget_control, self.widgets.status, set_value='Waiting for WinView on last frame'
      wait, .01
      busy = self.scan.ccd->getProperty('Acquire_RBV', string = 0)
    endwhile
  endif else begin
    ; stop camera and image capture
    self.scan.ccd->setProperty, 'Acquire', 0
    ; end capturing if it hasn't already ended
    t = caput(self.scan.file_control+'Capture',0)
    wait, .1
  endelse

  ; set motor speed high to expedite the motor reset
  self.scan.motor_speed = self.scan.motor_speed_old
  widget_control, self.widgets.motor_speed, set_value=self.scan.motor_speed
  ; Sometimes the speed is read back as zero.  NEED TO FIGURE OUT WHY!
  self.scan.rotation_motor->set_slew_speed, self.scan.motor_speed
  pos = self.scan.rotation_motor->get_slew_speed()
  if (pos eq 0) then begin
    print, 'Error, read rotation speed as 0, which is not correct. Using 15 instead!'
    self.scan.rotation_motor->set_slew_speed, 15
  endif
  if(~self.scan.leave_motor) then begin
    ; reset motor to original position
    self.scan.rotation_motor->move, (*self.scan.otf_rotation_array)[0] + self.scan.rotation_step/2.
    widget_control, self.widgets.status, set_value='Zeroing motors'
    zeroing = 1
    while (zeroing ne 0) do begin
      if ((self.scan.rotation_motor->done() eq 1) AND $
        (self.scan.sample_x_motor->done() eq 1) AND $
        (self.scan.sample_y_motor->done() eq 1)) then $
        zeroing = 0
      wait, 1
    endwhile
    wait, 1.0
  endif

  ; save any remaining data
  if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
    widget_control, self.widgets.status, set_value='Saving data'
    t = caput(self.epics_pvs.camera_name+'cam1:WriteFile',1)
    widget_control, self.widgets.status, set_value='Closing docFile'
  endif else begin
    widget_control, self.widgets.status, set_value='Saving data'
    ; wait for capture buffer to empty
    busy1 = 1
    busy2 = 1
    while (busy1 eq 1 OR busy2 eq 1) do begin
      wait, .01
      busy1 = self.scan.ccd->getProperty('Acquire_RBV', string = 0)
      t = caget(self.scan.file_control+'WriteFile_RBV',busy2)
    endwhile
    ; Turn off file saving
    wait, .1
    t = caput(self.scan.file_control+'Capture',0)
  endelse

  ; reset to idle mode
  self->setTriggerMode, 'FreeRun'
  self.scan.ccd->setProperty,'NumImages',1
  if (self.scan.camera_manufacturer ne self.camera_types.ROPER) then $
    t = caput(self.scan.file_control+'NumCapture',1)

  if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
    ; Wait for winview to return to idle state
    widget_control, self.widgets.status, set_value='Waiting for WinView'
    busy = 1
    while (busy) do begin
      wait, .1
      t = caget(self.scan.file_control+'WriteFile_RBV',busy)
    endwhile
  endif
  self->setFileComments, ['', '', '']

  widget_control, self.widgets.status, set_value='Scan complete'
  ; sensitize start widget
  widget_control, self.widgets.start_scan, sensitive=1
  widget_control, self.widgets.abort_scan, sensitive=0
  widget_control, self.widgets.alignment_scan, sensitive=1

  self->updateScreen, /force_update
  
  ; Start the camera acquiring in free-run so user sees live image again
  self.scan.ccd->setProperty, 'Acquire', 1

  self->setState, self.scan.states.SCAN_COMPLETE
end


pro tomo_collect_ad2::moveSampleIn
  if (not self.epics_pvs_valid) then return
  if (self.scan.flatfield_axis ne 1) then begin
    self.scan.sample_x_motor->move, self.scan.sample_x_in_position
  endif
  if (self.scan.flatfield_axis ne 0) then begin
    self.scan.sample_y_motor->move, self.scan.sample_y_in_position
  endif
end

pro tomo_collect_ad2::moveSampleOut
  if (not self.epics_pvs_valid) then return
  if (self.scan.flatfield_axis ne 1) then begin
    self.scan.sample_x_motor->move, self.scan.sample_x_out_position
  endif
  if (self.scan.flatfield_axis ne 0) then begin
    self.scan.sample_y_motor->move, self.scan.sample_y_out_position
  endif
end

pro tomo_collect_ad2::setExposureTime
  if (not self.epics_pvs_valid) then return
  self.scan.ccd->setProperty, 'AcquireTime', self.scan.exposure_time
  if (self.scan.camera_manufacturer eq self.camera_types.POINT_GREY) then begin
    ; Seems to be necessary to write Format7 mode to make mode changes work?
    t = caput(self.epics_pvs.camera_name+'cam1:VideoMode', 'Format7')
    t = caput(self.epics_pvs.sis_mcs+'StopAll', 1)
    ; Need to wait for camera to compute actual acquire time after setting it above
    wait, 0.1
    time = self->computeFrameTime()
    t = caput(self.epics_pvs.sis_mcs+'Dwell', time)
    if (self.scan.pg_trigger_mode eq 'Bulb') then begin
      t = caput(self.epics_pvs.sis_mcs+'LNEOutputWidth', self.scan.exposure_time)
      t = caput(self.epics_pvs.sis_mcs+'EraseStart', 1)
    endif else begin
      t = caput(self.epics_pvs.sis_mcs+'LNEOutputWidth', 0.0001)
      self.scan.ccd->setProperty, 'AcquireTime', self.scan.exposure_time
    endelse
  endif
end

pro tomo_collect_ad2::setFileComments, comments
  if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
    t = caput(self.epics_pvs.camera_name+'cam1:Comment1',[byte(comments[0]), 0B])
    t = caput(self.epics_pvs.camera_name+'cam1:Comment2',[byte(comments[1]), 0B])
    t = caput(self.epics_pvs.camera_name+'cam1:Comment3',[byte(comments[2]), 0B])
  endif else begin
    t = caput(self.epics_pvs.camera_name+'TIFF1:FileTemplate',[byte(comments[0]), 0B])
    t = caput(self.epics_pvs.camera_name+'TIFF1:FilePath',    [byte(comments[1]), 0B])
    t = caput(self.epics_pvs.camera_name+'TIFF1:FileName',    [byte(comments[2]), 0B])
  endelse
end

function tomo_collect_ad2::computeFrameTime
  exposure = self.scan.exposure_time
  biny = self.scan.ccd->getProperty('BinY', string = 0)
  if (self.scan.camera_manufacturer eq self.camera_types.PROSILICA) then begin
    ; This sets the time per angle to the largest of: exposure time*1.006; 20 ms; 40ms divided by biny
    ; This speed calculation works for exposure times shorter than 10 seconds
    readout = Max([.04/biny, .02])
    time = (exposure + readout) * 1.01
  endif else if (self.scan.camera_manufacturer eq self.camera_types.POINT_GREY) then begin
    ; The readout time of the camera depends both on the Format7Mode and the PixelFormat.
    ; These measurements were done with firmware 2.14.3 and Flycap2 8.3.1.
    ; The measured times in ms with 100 microsecond exposure time and 1000 frames without dropping any are:
    ;                Raw8 Raw12  Raw16  Mono8  Mono12  Mono16      Format 7 mode
    all_times  = [ $
                  ; Grasshopper 3 GS3-U3-23S6M-C 
                  [[ 6.2,  9.2, 12.2,  11.5,   11.5,  15.0],  $  ; 0 (1920X1200)
                   [ 6.2,  6.2,  6.2,  11.5,   11.5,  11.5],  $  ; 1 (960X600) 
                   [   0,    0,    0,     0,      0,     0],  $  ; 2 (960X600)  Not supported!
;                  [ 7.9,  9.2, 12.2,  11.5,   11.5,  12.2]], $  ; 7 (1920X1200)
; Mark increased from 9.2 because it was failing on the beamline
                   [ 7.9, 13, 12.2,  11.5,   11.5,  14]], $  ; 7 (1920X1200)
                  ; Grasshopper 3 GS3-U3-28S5M-C 
;                  [[   0,    0, 39.1,  39.1,   39.1,  39.1],  $  ; 0 (1920X1440)
;                   [   0,    0, 25.0,  25.0,   25.0,  25.0],  $  ; 1 (960X720)
; Mark increased from above values because it was failing for Tony at long exposures
; Need to figure out what is required for long exposures
                  [[   0,    0, 45.0,  45.0,   45.0,  45.0],  $  ; 0 (1920X1440)
                   [   0,    0, 28.0,  28.0,   28.0,  28.0],  $  ; 1 (960X720)
                   [   0,    0,    0,     0,      0,     0],  $  ; 2 (960X600)  Not supported!
                   [   0,    0,    0,     0,      0,     0]], $  ; 7 (1920X1200) Supported but don't use; single tap readout slower and no quality improvement
                  ; Grasshopper 3 GS3-U3-51S5M-C 
                  [[13.3, 20.0, 26.4,  25.8,   25.8,  26.5],  $  ; 0 (2448x2048)
                   [ 6.7,    0,  6.8,  13.3,      0,  13.3],  $  ; 1 (1224x1024)
                   [ 4.7,    0,  6.5,   6.5,      0,   6.5],  $  ; 2 (1224x1024)
                   [14.8, 20.0, 26.5,  25.8,   25.8,  26.6]]]    ; 7 (2448x2048)
    ; I fudged Raw12 Format 7 mode 7 from 9.5 to 10.5 because it was failing on beamline
    t = caget(self.epics_pvs.camera_name+'cam1:PixelFormat_RBV', pixel_format, /string)
    t = caget(self.epics_pvs.camera_name+'cam1:Format7Mode_RBV', format7_mode, /string)
    case self.scan.camera_model of
      'Grasshopper3 GS3-U3-23S6M': cm = 0
      'Grasshopper3 GS3-U3-28S5M': cm = 1
      'Grasshopper3 GS3-U3-51S5M': cm = 2
      else: message, 'Unsupported camera model: ' + self.scan.camera_model
    endcase
    case pixel_format of
      'Raw8':   pf = 0
      'Raw12':  pf = 1
      'Raw16':  pf = 2
      'Mono8':  pf = 3
      'Mono12': pf = 4
      'Mono16': pf = 5
      else: message, 'Unsupported pixel format: ' + pixel_format
    endcase
    case strmid(format7_mode, 0, 1) of
      '0': f7m = 0
      '1': f7m = 1
      '2': f7m = 2
      '7': f7m = 3
      else: message, 'Unsupported format7 mode= ' + format7_mode
    endcase
    readout = all_times[pf, f7m, cm]/1000.
    if (readout eq 0) then begin
      message, 'Unsupported combination of camera model, pixel format and format7 mode: ' $
        + self.scan.camera_model + pf + f7m
    endif
    ; We slow it down 1% from theoretical
    if (self.scan.pg_trigger_mode eq 'Bulb') then begin
      time = (exposure + readout) * 1.01
    endif 
    if (self.scan.pg_trigger_mode eq 'Overlapped') then begin
      ; We need to use the actual exposure time that the camera is using, not the requested exposure time
      exposure = self.scan.ccd->getProperty('AcquireTime_RBV',string = 0)
      ; Add 1, 2, or 5 ms to exposure time for margin
      if (exposure gt 4.0) then begin
        time = exposure + .005
      endif else if (exposure gt 2.0) then begin
        time = exposure + .005 
      endif else begin
        time = exposure + .001
      endelse
      ; If the time is less than the readout time then use the readout time
      if (time lt readout) then time = readout
    endif
  endif else if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
    ; Roper is assumed to have 100 msec readout unbinned, 50 ms if bin >= 2
    time = (exposure + 0.1/Min([biny,2]))
  endif
  return, time
end

pro tomo_collect_ad2::setTriggerMode, triggerMode, numImages
  if (triggerMode eq 'FreeRun') then begin
    if (self.scan.camera_manufacturer eq self.camera_types.PROSILICA) then begin
      self.scan.ccd->setProperty, 'TriggerMode', 'Free Run'
      self.scan.ccd->setProperty, 'ImageMode', 'Continuous'
    endif else if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
      self.scan.ccd->setProperty, 'TriggerMode', 'Internal'
      self.scan.ccd->setProperty, 'ImageMode', 'Focus'
    endif else if (self.scan.camera_manufacturer eq self.camera_types.POINT_GREY) then begin
      self.scan.ccd->setProperty, 'ImageMode', 'Continuous'
      if (self.scan.pg_trigger_mode eq 'Bulb') then begin
        self.scan.ccd->setProperty, 'TriggerMode',  'Bulb'
        t = caput(self.epics_pvs.sis_mcs+'StopAll', 1)
        t = caput(self.epics_pvs.sis_mcs+'ChannelAdvance', 'Internal')
        t = caget(self.epics_pvs.sis_mcs+'MaxChannels', maxChannels)
        if ((t ne 0) or (maxChannels eq 0)) then begin
          print, 'Error reading maxChannels, status= ', t, ' value= ', maxChannels, ' Trying again.'
          t = caget(self.epics_pvs.sis_mcs+'MaxChannels', maxChannels)
          print, 'Second try, status= ', t, ' value = ', maxChannels
        endif
        t = caput(self.epics_pvs.sis_mcs+'NuseAll', maxChannels)
        if (t ne 0) then begin
          print, 'Error writing NuseAll, status= ', t
        endif
        ; Sometimes read of NuseAll returns 0.  NEED TO FIGURE OUT WHY!
        t = caget(self.epics_pvs.sis_mcs+'NuseAll', val)
        if ((t ne 0) or (val eq 0)) then begin
          print, 'Error reading NUseAll, status=', t, ' value= ', val, ' writing 2048'
          t = caput(self.epics_pvs.sis_mcs+'NuseAll', 2048)
        endif
        wait, .1
        t = caput(self.epics_pvs.sis_mcs+'EraseStart', 1)
      endif else if (self.scan.pg_trigger_mode eq 'Overlapped') then begin
        self.scan.ccd->setProperty, 'TriggerMode',  'Internal'
      endif
    endif
  endif else if (triggerMode eq 'Single') then begin
    if (self.scan.camera_manufacturer eq self.camera_types.PROSILICA) then begin
      self.scan.ccd->setProperty, 'TriggerMode', 'Free Run'
      self.scan.ccd->setProperty, 'ImageMode', 'Single'
    endif else if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
      self.scan.ccd->setProperty, 'TriggerMode', 'Internal'
      self.scan.ccd->setProperty, 'ImageMode', 'Normal'
    endif else if (self.scan.camera_manufacturer eq self.camera_types.POINT_GREY) then begin
      self.scan.ccd->setProperty, 'TriggerMode', 'Internal'
      self.scan.ccd->setProperty, 'ImageMode', 'Single'
    endif
    self.scan.ccd->setProperty, 'NumImages', 1
  endif else begin    ; set camera to external triggering
    if (self.scan.camera_manufacturer eq self.camera_types.PROSILICA) then begin
      self.scan.ccd->setProperty, 'TriggerMode', 'Sync In 2'
      self.scan.ccd->setProperty, 'ImageMode', 'Multiple'
    endif else if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
      self.scan.ccd->setProperty, 'TriggerMode', 'External'
      self.scan.ccd->setProperty, 'ImageMode', 'Normal'
    endif else if (self.scan.camera_manufacturer eq self.camera_types.POINT_GREY) then begin
      self.scan.ccd->setProperty, 'TriggerMode',  self.scan.pg_trigger_mode
      self.scan.ccd->setProperty, 'ImageMode', 'Multiple'
    endif
    ; set number of MCS channels, NumImages, and NumCapture
    t = caput(self.epics_pvs.sis_mcs+'StopAll', 1)
    t = caput(self.epics_pvs.sis_mcs+'NuseAll', numImages)
    self.scan.ccd->setProperty, 'NumImages', numImages
    if (self.scan.camera_manufacturer ne self.camera_types.ROPER) then begin
      t = caput(self.scan.file_control+'NumCapture', numImages)
    endif
  endelse
  
  if (triggerMode eq 'MCSExternal') then begin
    ; Put MCS in external trigger mode
    t = caput(self.epics_pvs.sis_mcs+'StopAll', 1)
    t = caput(self.epics_pvs.sis_mcs+'ChannelAdvance', 'External')
  endif
  
  if (triggerMode eq 'MCSInternal') then begin
    t = caput(self.epics_pvs.sis_mcs+'StopAll', 1)
    ; Put MCS in internal trigger mode
    t = caput(self.epics_pvs.sis_mcs+'ChannelAdvance', 'Internal')
    ; Set MCS dwell time to time per angle
    t = caput(self.epics_pvs.sis_mcs+'Dwell', self->computeFrameTime())
  endif
  
end

function tomo_collect_ad2::validateEpicsPvs
  ; Check that all EPICS PVs are valid.
  ; Display error dialog and return error on first non-valid PV
  n_pvs = n_tags(self.epics_pvs)
  tags = tag_names(self.epics_pvs)
  pvs_ok = 1
  ; Make the cursor be an hourglass, since timeouts take a while
  widget_control, /hourglass

  ; connect to camera through epics pvs
  if (strlen(self.epics_pvs.camera_name) gt 0) then begin
    widget_control, self.pv_widgets.camera_name, set_value=self.epics_pvs.camera_name
    if (obj_valid(self.scan.ccd)) then obj_destroy, self.scan.ccd
    self.scan.ccd=obj_new('epics_ad_base',self.epics_pvs.camera_name+'cam1:')
    if (not obj_valid(self.scan.ccd)) then begin
      widget_control, self.widgets.status, set_value='Could not connect to camera'
      t = dialog_message('Could not connect to camera, invalid camera name')
      pvs_ok = 0
      goto, finish
    endif
    widget_control, self.widgets.status, set_value='Connected to '+self.epics_pvs.camera_name


    self.scan.ccd->setProperty,'Acquire',0
    self.scan.ccd->setProperty,'NumImages',1

    name = self.scan.ccd.getProperty('Manufacturer_RBV')

    if(name eq 'Roper Scientific') then begin
      self.scan.camera_manufacturer = self.camera_types.ROPER
    endif else if(name eq 'Prosilica') then begin
      self.scan.camera_manufacturer = self.camera_types.PROSILICA
    endif else if(name eq 'Point Grey Research') then begin
      self.scan.camera_manufacturer = self.camera_types.POINT_GREY

    endif else begin
      widget_control, self.widgets.status, set_value='Could not connect to camera'
      pvs_ok = 0
      goto, finish
    endelse
  endif else begin
    widget_control, self.widgets.status, set_value='Could not connect to camera'
    t = dialog_message('Could not connect to camera, invalid camera name')
    pvs_ok = 0
    goto, finish
  endelse
  
  self.scan.camera_model = self.scan.ccd.getProperty('Model_RBV')

  if(self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
    self.scan.file_control = self.epics_pvs.camera_name + 'cam1:'
  endif else begin
    self.scan.file_control = self.epics_pvs.camera_name + 'netCDF1:'
    t = caput(self.scan.file_control+'EnableCallbacks', 1)
    t = caput(self.scan.file_control+'Capture', 0)
    t = caput(self.scan.file_control+'NumCapture', 1)
    t = caput(self.scan.file_control+'FileWriteMode', 'Stream')
  endelse
  t = caput(self.scan.file_control+'AutoSave', 0)
  self->setExposureTime

  ; for OTF, check SIS generator PVs, assume that if one PV is good the others are also good
  pv = self.epics_pvs.sis_mcs
  if (strlen(pv) eq 0) then begin
    t = dialog_message('Error: sis_mcs is blank, not a valid EPICS PV name.')
    pvs_ok = 0
    goto, finish
  endif
  t = caget(pv+'EraseStart', value)
  if (t ne 0) then begin
    t = dialog_message('Error: ' + pv + ' is not a valid EPICS PV name, not found.')
    pvs_ok = 0
    goto, finish
  endif

  self->setTriggerMode, 'FreeRun'

  ; This loop assumes that the first 3 PVs are camera, pulse generator and OTF trigger.
  ; The remaining PVs are simple PVs with a VAL field
  for i=3, n_pvs-1 do begin
    pv = self.epics_pvs.(i)
    if (strlen(pv) eq 0) then begin
      t = dialog_message('Error: ' + tags[i] + ' is blank, not a valid EPICS PV name.')
      pvs_ok = 0
      goto, finish
    endif
    t = caget(pv, value)
    if (t ne 0) then begin
      t = dialog_message('Error: ' + pv + ' is not a valid EPICS PV name, not found.')
      pvs_ok = 0
      goto, finish
    endif
  endfor

  self.scan.rotation_motor = obj_new('epics_motor', self.epics_pvs.rotation)
  if (not obj_valid(self.scan.rotation_motor)) then begin
    t = dialog_message('Error: ' + self.epics_pvs.rotation + ' is not a valid motor')
    pvs_ok = 0
    goto, finish
  endif
  self.scan.sample_x_motor = obj_new('epics_motor', self.epics_pvs.sample_x)
  if (not obj_valid(self.scan.sample_x_motor)) then begin
    t = dialog_message('Error: ' + self.epics_pvs.sample_x + ' is not a valid motor')
    pvs_ok = 0
    goto, finish
  endif
  self.scan.sample_y_motor = obj_new('epics_motor', self.epics_pvs.sample_y)
  if (not obj_valid(self.scan.sample_y_motor)) then begin
    t = dialog_message('Error: ' + self.epics_pvs.sample_y + ' is not a valid motor')
    pvs_ok = 0
    goto, finish
  endif
  ; Put monitors on the PVs we need them on
  t = caSetMonitor(self.epics_pvs.rotation + '.RBV')
  t = caSetMonitor(self.epics_pvs.rotation + '.VAL')
  t = caSetMonitor(self.epics_pvs.sample_x + '.RBV')
  t = caSetMonitor(self.epics_pvs.sample_x + '.VAL')
  t = caSetMonitor(self.epics_pvs.sample_y + '.RBV')
  t = caSetMonitor(self.epics_pvs.sample_y + '.VAL')
  t = caSetMonitor(self.epics_pvs.autoscan_sync)
  t = caSetMonitor(self.epics_pvs.autoscan_suffix)
  t = caSetMonitor(self.epics_pvs.rotation + '.VELO')
  t = caSetmonitor(self.epics_pvs.beam_ready)
  t = caSetmonitor(self.scan.file_control+'WriteFile')
  t = caSetmonitor(self.scan.file_control+'WriteFile_RBV')

  ; set up Attributes_filename
  widget_control, self.widgets.attributes_filename, sensitive = ~self.scan.camera_manufacturer
  widget_control, self.widgets.attributes, sensitive = ~self.scan.camera_manufacturer
  if (self.scan.camera_manufacturer eq self.camera_types.ROPER) then begin
    widget_control, self.widgets.attributes_filename, set_value = ''
  endif else begin
    t = caget(self.epics_pvs.camera_name+'cam1:NDAttributesFile', file)
    widget_control, self.widgets.attributes_filename, set_value = strtrim(file,2)
    self.scan.attributes_filename = strtrim(file,2)
  endelse

  finish:

  if (pvs_ok) then sensitive=1 else sensitive=0
  self.epics_pvs_valid = pvs_ok
  ; Enable/disable the widgets that require valid EPICS PVs
  widget_control, self.widgets.move_sample_in, sensitive=sensitive
  widget_control, self.widgets.move_sample_out, sensitive=sensitive
  widget_control, self.widgets.rotation_drive, sensitive=sensitive
  widget_control, self.widgets.sample_x_drive, sensitive=sensitive
  widget_control, self.widgets.sample_y_drive, sensitive=sensitive
  widget_control, self.widgets.exposure_time, sensitive=sensitive
  widget_control, self.widgets.start_scan, sensitive=sensitive
  widget_control, self.widgets.alignment_scan, sensitive=sensitive
  widget_control, self.widgets.abort_scan, sensitive=0

  self->updateScreen, /force_update
  if (pvs_ok) then begin
    return, 0
  endif else begin
    return, -1
  endelse
end


function tomo_collect_ad2::saveSettings, file, all=all
  openw, lun, file, error=error, /get_lun, width = 1024
  if (error ne 0) then return, 0
  ; Copy information from widgets
  self->copy_expinfo_from_widgets
  self->copy_settings_from_widgets
  printf, lun, 'TITLE: ',           self.expinfo.title
  printf, lun, 'OPERATOR: ',        self.expinfo.operator
  printf, lun, 'CAMERA: ',          self.expinfo.camera
  printf, lun, 'SAMPLE: ',          self.expinfo.sample
  for i=0, n_elements(self.expinfo.comments)-1 do begin
    if (strlen(self.expinfo.comments[i]) ne 0) then begin
      printf, lun, 'COMMENT: ', self.expinfo.comments[i]
    endif
  endfor
  printf, lun, 'DARK_CURRENT: ',    self.expinfo.dark_current
  printf, lun, 'ENERGY: ',          self.expinfo.energy
  printf, lun, 'X_PIXEL_SIZE: ',    self.expinfo.x_pixel_size
  printf, lun, 'Y_PIXEL_SIZE: ',    self.expinfo.y_pixel_size
  if (not keyword_set(all)) then begin
    free_lun, lun
    return, 1
  endif
  ; Write all the other settings
  ; These are in self.epics_pvs
  printf, lun, 'CAMERA_NAME: ',         self.epics_pvs.camera_name
  printf, lun, 'ROTATION_PV: ',         self.epics_pvs.rotation
  printf, lun, 'SAMPLE_X_PV: ',         self.epics_pvs.sample_x
  printf, lun, 'SAMPLE_Y_PV: ',         self.epics_pvs.sample_y
  printf, lun, 'BEAM_READY_PV: ',       self.epics_pvs.beam_ready
  printf, lun, 'AUTOSCAN_SYNC_PV: ',    self.epics_pvs.autoscan_sync
  printf, lun, 'AUTOSCAN_SUFFIX_PV: ',  self.epics_pvs.autoscan_suffix
  printf, lun, 'SIS_MCS_BASE_NAME_PV: ',self.epics_pvs.sis_mcs
  printf, lun, 'CLOSE_SHUTTER_PV: ',    self.epics_pvs.close_shutter
  printf, lun, 'OPEN_SHUTTER_PV: ',     self.epics_pvs.open_shutter
  ; These are in self.scan
  printf, lun, 'ROTATION_START: ',      self.scan.rotation_start
  printf, lun, 'ROTATION_STOP: ',       self.scan.rotation_stop
  printf, lun, 'ROTATION_STEP: ',       self.scan.rotation_step
  printf, lun, 'NUM_ANGLES: ',          self.scan.num_angles
  printf, lun, 'FLATFIELD_AXIS: ',      self.scan.flatfield_axis
  printf, lun, 'FLATFIELD_INCREMENT: ', self.scan.flatfield_increment
  printf, lun, 'NUM_IMAGES_PER_FLATFIELD: ', self.scan.num_flatfields
  printf, lun, 'SAMPLE_X_IN: ',         self.scan.sample_x_in_position
  printf, lun, 'SAMPLE_X_OUT: ',        self.scan.sample_x_out_position
  printf, lun, 'SAMPLE_Y_IN: ',         self.scan.sample_y_in_position
  printf, lun, 'SAMPLE_Y_OUT: ',        self.scan.sample_y_out_position
  printf, lun, 'AUTOSCAN: ',            self.scan.autoscan
  printf, lun, 'EXPOSURE_TIME: ',       self.scan.exposure_time
  printf, lun, 'NUM_DARK_CURRENTS:',    self.scan.num_dark_currents
  printf, lun, 'LEAVE_MOTOR: ',         self.scan.leave_motor
  printf, lun, 'CLOSE_SHUTTER_VALUE: ', self.scan.close_shutter_value
  printf, lun, 'OPEN_SHUTTER_VALUE: ',  self.scan.open_shutter_value
  free_lun, lun
  self.settings_file = file
  return, 1
end


function tomo_collect_ad2::restoreSettings, file
  ncomments = 0
  max_comments = n_elements(self.expinfo.comments)
  comment = strarr(100)
  line = ''
  openr, lun, file, error=error, /get_lun
  if (error ne 0) then return, 0
  while (not eof(lun)) do begin
    readf, lun, line
    pos = strpos(line, ' ')
    tag = strupcase(strmid(line, 0, pos))
    value = strtrim(strmid(line, pos, 1000), 2)
    case tag of
      ; The are in self.expinfo
      'TITLE:':         self.expinfo.title = value
      'OPERATOR:':      self.expinfo.operator = value
      'CAMERA:':        self.expinfo.camera = value
      'SAMPLE:':        self.expinfo.sample = value
      'COMMENT:':       begin
        comment[ncomments] = value
        ncomments = ncomments + 1
      end
      'DARK_CURRENT:':  self.expinfo.dark_current = value
      'ENERGY:':        self.expinfo.energy = value
      'X_PIXEL_SIZE:':  self.expinfo.x_pixel_size = value
      'Y_PIXEL_SIZE:':  self.expinfo.y_pixel_size = value
      ; These are in self.epics_pvs
      'CAMERA_NAME:':         self.epics_pvs.camera_name = value
      'ROTATION_PV:':         self.epics_pvs.rotation = value
      'SAMPLE_X_PV:':         self.epics_pvs.sample_x = value
      'SAMPLE_Y_PV:':         self.epics_pvs.sample_y = value
      'BEAM_READY_PV:':       self.epics_pvs.beam_ready = value
      'AUTOSCAN_SYNC_PV:':    self.epics_pvs.autoscan_sync = value
      'AUTOSCAN_SUFFIX_PV:':  self.epics_pvs.autoscan_suffix = value
      'SIS_MCS_BASE_NAME_PV:': self.epics_pvs.sis_mcs = value
      'CLOSE_SHUTTER_PV:':    self.epics_pvs.close_shutter = value
      'OPEN_SHUTTER_PV:':     self.epics_pvs.open_shutter = value
      ; These are in self.scan
      'ROTATION_START:':      self.scan.rotation_start = float(value)
      'ROTATION_STOP:':       self.scan.rotation_stop = float(value)
      'ROTATION_STEP:':       self.scan.rotation_step = float(value)
      'NUM_ANGLES:':          self.scan.num_angles = long(value)
      'NUM_PASSES:':          ; Ignore, this will be in old settings files
      'FLATFIELD_AXIS:':      self.scan.flatfield_axis = value
      'FLATFIELD_INCREMENT:': self.scan.flatfield_increment = long(value)
      'NUM_IMAGES_PER_FLATFIELD:': self.scan.num_flatfields = long(value)
      'SAMPLE_X_IN:':         self.scan.sample_x_in_position = float(value)
      'SAMPLE_X_OUT:':        self.scan.sample_x_out_position = float(value)
      'SAMPLE_Y_IN:':         self.scan.sample_y_in_position = float(value)
      'SAMPLE_Y_OUT:':        self.scan.sample_y_out_position = float(value)
      'AUTOSCAN:':            self.scan.autoscan = long(value)
      'EXPOSURE_TIME:':       self.scan.exposure_time = float(value)
      'NUM_DARK_CURRENTS:':   self.scan.num_dark_currents = long(value)
      'LEAVE_MOTOR:':         self.scan.leave_motor = long(value)
      'CLOSE_SHUTTER_VALUE:': self.scan.close_shutter_value = value
      'OPEN_SHUTTER_VALUE:':  self.scan.open_shutter_value = value
      'FAST_SCAN:':           ; Ignore, this will be in old settings files
      'CANCEL_MOTOR_RESET:':  ; Ignore, this will be in old settings files
      'EXTERNAL_TRIGGER_PV:': ; Ignore, this will be in old settings files
      'CCD_READY_PV:':        ; Ignore, this will be in old settings files
      'DARK_CURRENT_MEASUREMENT:': ; Ignore, this will be in old settings files
      else: begin
        t = dialog_message('Settings file contained unrecognized keyword: ' + tag)
      endelse
    endcase
  endwhile
  self.expinfo.comments = strarr(max_comments)
  if (ncomments gt max_comments) then ncomments = max_comments
  for i=0, ncomments-1 do begin
    self.expinfo.comments[i] = comment[i]
  endfor
  ; Update the widgets
  self->copy_expinfo_to_widgets
  widget_control, self.widgets.sample_info, set_value = self.expinfo.sample
  self->copy_settings_to_widgets
  free_lun, lun
  self.settings_file = file
  status = self->validateEpicsPvs()

  return, 1
end


pro tomo_collect_ad2::updateScreen, force_update=force_update

  if (not self.epics_pvs_valid) then return
  if (n_elements(force_update) eq 0) then force_update=0

  ; Update the drive and readbacks whenever there is a change or a force
  status = caCheckMonitor(self.epics_pvs.sample_x+'.VAL')
  if (status or force_update) then begin
    status = caGet(self.epics_pvs.sample_x+'.VAL', drive)
    widget_control, self.widgets.sample_x_drive, set_value=drive
  endif
  status = caCheckMonitor(self.epics_pvs.sample_x+'.RBV')

  if (status or force_update) then begin
    status = caGet(self.epics_pvs.sample_x+'.RBV', readback)
    widget_control, self.widgets.sample_x_readback, set_value=readback
  endif
  status = caCheckMonitor(self.epics_pvs.sample_y+'.VAL')
  if (status or force_update) then begin
    status = caGet(self.epics_pvs.sample_y+'.VAL', drive)
    widget_control, self.widgets.sample_y_drive, set_value=drive
  endif
  status = caCheckMonitor(self.epics_pvs.sample_y+'.RBV')
  if (status or force_update) then begin
    status = caGet(self.epics_pvs.sample_y+'.RBV', readback)
    widget_control, self.widgets.sample_y_readback, set_value=readback
  endif
  status = caCheckMonitor(self.epics_pvs.rotation+'.VAL')
  if (status or force_update) then begin
    status = caGet(self.epics_pvs.rotation+'.VAL', drive)
    widget_control, self.widgets.rotation_drive, set_value=drive
  endif
  status = caCheckMonitor(self.epics_pvs.rotation+'.RBV')
  if (status or force_update) then begin
    status = caGet(self.epics_pvs.rotation+'.RBV', readback)
    widget_control, self.widgets.rotation_readback, set_value=readback
  endif
  status = caCheckMonitor(self.epics_pvs.rotation+'.VELO')
  if (status or force_update) then begin
    status = caGet(self.epics_pvs.rotation+'.VELO', readback)
    widget_control, self.widgets.motor_speed, set_value=readback
  endif

end


pro tomo_epics_pvs_event, event
  widget_control, event.top, get_uvalue=tomo_collect
  tomo_collect->epics_pvs_event, event
end


pro tomo_collect_ad2::epics_pvs_event, event
  if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
    ; Convert event.id to cancel
    event.id = self.pv_widgets.cancel
  endif

  case event.id of
    self.pv_widgets.accept: begin
      ; Verify EPICS PVs, create motor objects
      self->copy_settings_from_widgets
      status = self->validateEpicsPvs()
      self->copy_settings_to_widgets
      ; Unmap window
      widget_control, self.pv_widgets.base, map=0
    end

    self.pv_widgets.cancel: begin
      ; Restore widgets to original state
      self->copy_settings_to_widgets
      ; Unmap window
      widget_control, self.pv_widgets.base, map=0
    end
    else:
    ; Ignore all other events, which come from text widgets
  endcase
end


pro tomo_collect_ad2::copy_expinfo_to_widgets
  widget_control, self.expinfo_widgets.title, set_value=self.expinfo.title
  widget_control, self.expinfo_widgets.sample, set_value=self.expinfo.sample
  widget_control, self.expinfo_widgets.comments, set_value=self.expinfo.comments
  widget_control, self.expinfo_widgets.operator, set_value=self.expinfo.operator
  widget_control, self.expinfo_widgets.camera, set_value=self.expinfo.camera
  widget_control, self.expinfo_widgets.x_pixel_size, set_value=strtrim(self.expinfo.x_pixel_size,2)
  widget_control, self.expinfo_widgets.y_pixel_size, set_value=strtrim(self.expinfo.y_pixel_size,2)
  widget_control, self.expinfo_widgets.energy, set_value=strtrim(self.expinfo.energy,2)
  widget_control, self.expinfo_widgets.dark_current, set_value=strtrim(self.expinfo.dark_current,2)
end


pro tomo_collect_ad2::copy_expinfo_from_widgets
  widget_control, self.expinfo_widgets.sample, get_value=sample
  self.expinfo.sample = sample
  widget_control, self.expinfo_widgets.comments, get_value=comments
  max_comments = n_elements(self.expinfo.comments)
  n_comments = max_comments < n_elements(comments)
  self.expinfo.comments = comments[0:n_comments-1]
  for i=n_comments, max_comments-1 do self.expinfo.comments[i] = ''
  widget_control, self.expinfo_widgets.title, get_value=title
  self.expinfo.title = title
  widget_control, self.expinfo_widgets.operator, get_value=operator
  self.expinfo.operator = operator
  widget_control, self.expinfo_widgets.camera, get_value=camera
  self.expinfo.camera = camera
  widget_control, self.expinfo_widgets.x_pixel_size, get_value=x_pixel_size
  self.expinfo.x_pixel_size = x_pixel_size
  widget_control, self.expinfo_widgets.y_pixel_size, get_value=y_pixel_size
  self.expinfo.y_pixel_size = y_pixel_size
  widget_control, self.expinfo_widgets.energy, get_value=energy
  self.expinfo.energy = energy
  widget_control, self.expinfo_widgets.dark_current, get_value=dark_current
  self.expinfo.dark_current = dark_current
end


pro tomo_collect_ad2::copy_settings_to_widgets
  ; These are in self.epics_pvs
  widget_control, self.pv_widgets.camera_name,      set_value=self.epics_pvs.camera_name
  widget_control, self.pv_widgets.sis_mcs,          set_value=self.epics_pvs.sis_mcs
  widget_control, self.pv_widgets.close_shutter,    set_value=self.epics_pvs.close_shutter
  widget_control, self.pv_widgets.open_shutter,     set_value=self.epics_pvs.open_shutter
  widget_control, self.pv_widgets.rotation,         set_value=self.epics_pvs.rotation
  widget_control, self.pv_widgets.sample_x,         set_value=self.epics_pvs.sample_x
  widget_control, self.pv_widgets.sample_y,         set_value=self.epics_pvs.sample_y
  widget_control, self.pv_widgets.beam_ready,       set_value=self.epics_pvs.beam_ready
  widget_control, self.pv_widgets.autoscan_sync,    set_value=self.epics_pvs.autoscan_sync
  widget_control, self.pv_widgets.autoscan_suffix,  set_value=self.epics_pvs.autoscan_suffix
  ; These are in self.scan
  widget_control, self.widgets.rotation_start,        set_value=self.scan.rotation_start
  widget_control, self.widgets.rotation_stop,         set_value=self.scan.rotation_stop
  widget_control, self.widgets.rotation_step,         set_value=self.scan.rotation_step
  widget_control, self.widgets.num_angles,            set_value=self.scan.num_angles
  widget_control, self.widgets.flatfield_axis,        set_droplist_select=self.scan.flatfield_axis
  widget_control, self.widgets.flatfield_increment,   set_value=self.scan.flatfield_increment
  widget_control, self.widgets.sample_x_in_position,  set_value=self.scan.sample_x_in_position
  widget_control, self.widgets.sample_x_out_position, set_value=self.scan.sample_x_out_position
  widget_control, self.widgets.sample_y_in_position,  set_value=self.scan.sample_y_in_position
  widget_control, self.widgets.sample_y_out_position, set_value=self.scan.sample_y_out_position
  widget_control, self.widgets.exposure_time,         set_value=self.scan.exposure_time
  widget_control, self.widgets.autoscan,              set_value=self.scan.autoscan
  widget_control, self.widgets.motor_speed,           set_value=self.scan.motor_speed
  widget_control, self.widgets.num_flatfields,        set_value=self.scan.num_flatfields
  widget_control, self.widgets.num_dark_currents,     set_value = self.scan.num_dark_currents
  widget_control, self.widgets.leave_motor,           set_value = self.scan.leave_motor
  widget_control, self.pv_widgets.close_shutter_value,set_value=self.scan.close_shutter_value
  widget_control, self.pv_widgets.open_shutter_value, set_value=self.scan.open_shutter_value
end


pro tomo_collect_ad2::copy_settings_from_widgets
  ; These are in self.epics_pvs
  widget_control, self.pv_widgets.camera_name, get_value=value
  self.epics_pvs.camera_name = value
  widget_control, self.pv_widgets.sis_mcs, get_value=value
  self.epics_pvs.sis_mcs = value
  widget_control, self.pv_widgets.close_shutter, get_value=value
  self.epics_pvs.close_shutter = value
  widget_control, self.pv_widgets.open_shutter, get_value=value
  self.epics_pvs.open_shutter = value
  widget_control, self.pv_widgets.rotation, get_value=value
  self.epics_pvs.rotation = value
  widget_control, self.pv_widgets.sample_x, get_value=value
  self.epics_pvs.sample_x = value
  widget_control, self.pv_widgets.sample_y, get_value=value
  self.epics_pvs.sample_y = value
  widget_control, self.pv_widgets.beam_ready, get_value=value
  self.epics_pvs.beam_ready = value
  widget_control, self.pv_widgets.autoscan_sync, get_value=value
  self.epics_pvs.autoscan_sync = value
  widget_control, self.pv_widgets.autoscan_suffix, get_value=value
  self.epics_pvs.autoscan_suffix = value
  ; These are in self.scan
  widget_control, self.widgets.rotation_start, get_value=value
  self.scan.rotation_start = value
  widget_control, self.widgets.rotation_stop, get_value=value
  self.scan.rotation_stop = value
  widget_control, self.widgets.rotation_step, get_value=value
  self.scan.rotation_step = value
  widget_control, self.widgets.num_angles, get_value=value
  self.scan.num_angles = value
  value = widget_info(self.widgets.flatfield_axis, /droplist_select)
  self.scan.flatfield_axis = value
  widget_control, self.widgets.flatfield_increment, get_value=value
  self.scan.flatfield_increment = value
  widget_control, self.widgets.sample_x_in_position, get_value=value
  self.scan.sample_x_in_position = value
  widget_control, self.widgets.sample_x_out_position, get_value=value
  self.scan.sample_x_out_position = value
  widget_control, self.widgets.sample_y_in_position, get_value=value
  self.scan.sample_y_in_position = value
  widget_control, self.widgets.sample_y_out_position, get_value=value
  self.scan.sample_y_out_position = value
  widget_control, self.widgets.autoscan, get_value=value
  self.scan.autoscan = value
  widget_control, self.widgets.motor_speed, get_value = value
  self.scan.motor_speed = value
  widget_control, self.widgets.num_flatfields, get_value = value
  self.scan.num_flatfields = value
  widget_control, self.widgets.num_dark_currents, get_value = value
  self.scan.num_dark_currents = value
  widget_control, self.widgets.leave_motor, get_value = value
  self.scan.leave_motor = value
  widget_control, self.pv_widgets.close_shutter_value, get_value = value
  self.scan.close_shutter_value = value
  widget_control, self.pv_widgets.open_shutter_value, get_value = value
  self.scan.open_shutter_value = value

end


pro tomo_expinfo_event, event
  widget_control, event.top, get_uvalue=tomo_collect
  tomo_collect->expinfo_event, event
end


pro tomo_collect_ad2::expinfo_event, event
  if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
    ; Convert event.id to cancel
    event.id = self.expinfo_widgets.cancel
  endif
  case event.id of
    self.expinfo_widgets.accept: begin
      ; Copy values to structure
      self->copy_expinfo_from_widgets
      self->copy_expinfo_to_widgets
      ; Copy sample description to main window
      widget_control, self.widgets.sample_info, set_value = self.expinfo.sample
      ; Unmap window
      widget_control, self.expinfo_widgets.base, map=0
    end

    self.expinfo_widgets.cancel: begin
      ; Restore widgets
      self->copy_expinfo_to_widgets
      ; Unmap window
      widget_control, self.expinfo_widgets.base, map=0
    end
    else:
    ; Ignore all other events, which come from text widgets
  endcase
end



pro tomo_collect_event, event
  widget_control, event.top, get_uvalue=tomo_collect
  tomo_collect->event, event
end


pro tomo_collect_ad2::event, event
  if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
    ; close all open secondary windows
    self->copy_settings_to_widgets
    widget_control, self.pv_widgets.base, map=0
    widget_control, self.expinfo_widgets.base, map=0

    ; close main window
    widget_control, event.top, /destroy
    obj_destroy, self
    return
  endif

  catch, err
  if (err ne 0) then begin
    t = dialog_message(!error_state.msg, /error)
    widget_control, self.widgets.status, set_value=!error_state.msg
    self->stopScan
    goto, end_event
  endif

  case event.id of

    self.widgets.base_filename: begin
      file = dialog_pickfile(/write, get_path=path)
      if (file eq '') then return
      cd, path
      widget_control, self.widgets.base_file, set_value=file
      self.scan.base_filename = file
      self.scan.filename = file
    end

    self.widgets.attributes: begin
      file = dialog_pickfile(/write, get_path=path)
      if (file eq '') then return
      widget_control, self.widgets.attributes_filename, set_value = file
      self.scan.attributes_filename = file
      t = caput(self.epics_pvs.camera_name+'cam1:NDAttributesFile', [byte(file),0B])
    end

    self.widgets.experiment_information: begin
      widget_control, self.expinfo_widgets.base, map=1
    end

    self.widgets.epics_process_variables: begin
      widget_control, self.pv_widgets.base, map=1
    end

    self.widgets.save_settings: begin
      file = dialog_pickfile(/write)
      if (file ne '') then begin
        status = self->saveSettings(file, /all)
      endif
    end

    self.widgets.restore_settings: begin
      file = dialog_pickfile(/read, /must_exist)
      if (file ne '') then begin
        status = self->restoreSettings(file)
      endif
    end

    self.widgets.exit: begin
      file = getenv('TOMO_COLLECT_SETTINGS')
      if (file eq '') then file = 'tomo_collect_settings.dat'
      status = self->saveSettings(file, /all)
      widget_control, event.top, /destroy
      obj_destroy, self
      return
    end

    self.widgets.scan_timer: begin
      ; Timer event for polling during a scan
      self->scanPoll
      if (self.scan.current_state ne self.scan.states.SCAN_COMPLETE) then begin
        widget_control, self.widgets.scan_timer, timer=self.scan_timer_interval
      endif
    end

    self.widgets.display_timer: begin
      ; Timer event to update screen.
      self->updateScreen
      widget_control, self.widgets.display_timer, timer=self.display_timer_interval
    end

    self.widgets.autoscan_timer: begin
      ; Timer event for autoscan functions
      if ((self.scan.autoscan) and $
        (self.scan.current_state eq self.scan.states.SCAN_COMPLETE)) then begin
        start = 0
        sync_changed = 0
        sync_changed = caCheckMonitor(self.epics_pvs.autoscan_sync)
        t = caget(self.epics_pvs.autoscan_sync, start)
        if (sync_changed and start) then begin
          ; The autoscan_sync PV has made a 0 to 1 transition so start a scan
          status = caget(self.epics_pvs.autoscan_suffix, suffix)
          self.scan.filename = self.scan.base_filename + '_' + suffix + '_'
          widget_control, self.widgets.base_file, set_value=self.scan.filename
          self->startScan
        endif else if (start) then begin
          ; The scan is done and the sync PV is still 1, so set it to 0.  This
          ; lets the client know that the scan is complete
          t = caput(self.epics_pvs.autoscan_sync, 0)
        endif
      endif
      widget_control, self.widgets.autoscan_timer, timer=self.autoscan_timer_interval
    end

    self.widgets.clock_timer: begin
      ; timer event for clock widgets
      self.scan.elapsed_time = systime(1, /seconds) - self.scan.start_clock
      hours = floor((self.scan.elapsed_time)/3600.)
      minutes = floor((self.scan.elapsed_time-hours*3600.)/60.)
      seconds = floor(self.scan.elapsed_time-hours*3600.-minutes*60.)
      widget_control, self.widgets.elapsed_time, set_value = $
        string(hours,   format='(i2.2)') + ':' + $
        string(minutes, format='(i2.2)') + ':' + $
        string(seconds, format='(i2.2)')
      current_motor_position = self.scan.rotation_motor->get_position(readback='rbv')
      ncapture = floor(max([(current_motor_position-self.scan.rotation_start)/self.scan.rotation_step+1,0]))
      images_collected = ncapture + max([0,ceil(self.scan.current_point/2.)*self.scan.num_flatfields])
      num_groups = 1.0*self.scan.num_groups
      if(self.scan.num_flatfields gt 0) then begin
        self.scan.est_remaining_time = 1.*self.scan.elapsed_time/(1+images_collected)* $
          ((num_groups-1)/2.*self.scan.flatfield_increment $
          +(num_groups+1)/2.*self.scan.num_flatfields-images_collected)
      endif else begin
        self.scan.est_remaining_time = 1.*self.scan.elapsed_time/(1+images_collected)* $
          ((num_groups)*self.scan.flatfield_increment-images_collected)
      endelse
      hours = floor((self.scan.est_remaining_time)/3600.)
      minutes = floor((self.scan.est_remaining_time-hours*3600.)/60.)
      seconds = floor((self.scan.est_remaining_time-hours*3600.-minutes*60.)/5)*5
      widget_control,self.widgets.est_remaining_time, set_value = $
        string(hours,   format='(i2.2)') + ':' + $
        string(minutes, format='(i2.2)') + ':' + $
        string(seconds, format='(i2.2)')
      if (self.scan.current_state ne self.scan.states.SCAN_COMPLETE) then begin
        widget_control, self.widgets.clock_timer, timer=.1
      endif else     widget_control, self.widgets.est_remaining_time, set_value=''
    end


    self.widgets.rotation_drive: begin
      widget_control, self.widgets.rotation_drive, get_value=position
      if (self.epics_pvs_valid) then self.scan.rotation_motor->move, position
    end

    self.widgets.rotation_start: begin
      self.scan.rotation_start = event.value
      self.scan.rotation_step = (self.scan.rotation_stop - self.scan.rotation_start) / $
        (self.scan.num_angles - 1.)
      widget_control, self.widgets.rotation_step, set_value=self.scan.rotation_step
    end

    self.widgets.rotation_stop: begin
      self.scan.rotation_stop = event.value
      self.scan.rotation_step = (self.scan.rotation_stop - self.scan.rotation_start) / $
        (self.scan.num_angles - 1.)
      widget_control, self.widgets.rotation_step, set_value=self.scan.rotation_step
    end

    self.widgets.rotation_step: begin
      self.scan.rotation_step = event.value
      self.scan.rotation_stop = 180. - self.scan.rotation_step
      self.scan.num_angles = round((self.scan.rotation_stop - self.scan.rotation_start) / $
        self.scan.rotation_step) + 1
      widget_control, self.widgets.rotation_stop, set_value=self.scan.rotation_stop
      widget_control, self.widgets.num_angles, set_value=self.scan.num_angles
    end

    self.widgets.num_angles: begin
      self.scan.num_angles = event.value
      self.scan.rotation_step = 180./self.scan.num_angles
      self.scan.rotation_stop = 180. - self.scan.rotation_step
      widget_control, self.widgets.rotation_stop, set_value=self.scan.rotation_stop
      widget_control, self.widgets.rotation_step, set_value=self.scan.rotation_step
      widget_control, self.widgets.num_groups, set_value = $
        strtrim(2*CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment)+1,2)
      if (self.scan.num_flatfields eq 0) then widget_control, self.widgets.num_groups, set_value = $
        strtrim(CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment),2)
    end

    self.widgets.sample_x_drive: begin
      if (self.epics_pvs_valid) then self.scan.sample_x_motor->move, event.value
    end

    self.widgets.sample_x_in_position: begin
      self.scan.sample_x_in_position = event.value
    end

    self.widgets.sample_x_out_position: begin
      self.scan.sample_x_out_position = event.value
    end

    self.widgets.sample_y_drive: begin
      if (self.epics_pvs_valid) then self.scan.sample_y_motor->move, event.value
    end

    self.widgets.sample_y_in_position: begin
      self.scan.sample_y_in_position = event.value
    end

    self.widgets.sample_y_out_position: begin
      self.scan.sample_y_out_position = event.value
    end

    self.widgets.flatfield_axis: begin
      ; Set the appropriate widget base to be active/inactive
      ; 0=horizontal, 1=vertical
      self.scan.flatfield_axis = event.index
      if (event.index eq 0) then ysens=0 else ysens=1
      if (event.index eq 1) then xsens=0 else xsens=1
      widget_control, self.widgets.sample_x_base, sensitive=xsens
      widget_control, self.widgets.sample_y_base, sensitive=ysens
    end

    self.widgets.flatfield_increment: begin
      widget_control, self.widgets.flatfield_increment, get_value = value
      self.scan.flatfield_increment = value
      widget_control, self.widgets.num_groups, set_value = $
        strtrim(2*CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment)+1,2)
      if (self.scan.num_flatfields eq 0) then widget_control, self.widgets.num_groups, set_value = $
        strtrim(CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment),2)
      self-> copy_settings_to_widgets
    end

    self.widgets.num_flatfields: begin
      widget_control, self.widgets.num_flatfields, get_value = value
      self.scan.num_flatfields = value
      self->copy_settings_to_widgets
      if (self.scan.num_flatfields eq 0) then widget_control, self.widgets.num_groups, set_value = $
        strtrim(CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment),2)
    end

    self.widgets.move_sample_in: begin
      ; Move sample in
      self->moveSampleIn
    end

    self.widgets.move_sample_out: begin
      ; Move sample out
      self->moveSampleOut
    end

    self.widgets.num_dark_currents: begin
      self.scan.num_dark_currents = event.value
    end

    self.widgets.leave_motor: begin
      self.scan.leave_motor = event.select
    end

    self.widgets.autoscan: begin
      self.scan.autoscan = event.select
    end

    self.widgets.exposure_time: begin
      self.scan.exposure_time = event.value
      self->setExposureTime
    end

    self.widgets.start_scan: begin
      self->startScan
      ; Start a collection
    end

    self.widgets.abort_scan: begin
      ; Abort a collection
      self->abortScan
    end

    self.widgets.alignment_scan: begin
      ; Do an alignment scan
      self->alignmentScan
    end
    
    else:  t = dialog_message('Unknown event')
  endcase

  end_event:
end


function tomo_collect_ad2::init
  ;+
  ; NAME:
  ;       tomo_collect_ad2::init
  ;
  ; PURPOSE:
  ;       This function initializes an object of class tomo_collect.  It is
  ;       not called directly, but is called indirectly when a new object of
  ;       class tomo_collect is created via OBJ_NEW('tomo_collect')
  ;
  ;       The tomo_collect object is a GUI display which provides control
  ;       for collecting tomographic data
  ;
  ; CATEGORY:
  ;       Data acquisition
  ;
  ; CALLING SEQUENCE:
  ;       obj = OBJ_NEW('tomo_collect')
  ;
  ; EXAMPLE:
  ;       IDL> obj = OBJ_NEW('tomo_collect')
  ;
  ; MODIFICATION HISTORY:
  ;       Written by:     Mark Rivers (26-Aug=2006).  Converted from previous
  ;                       Visual Basic program called "tomo".
  ;-
  self.fonts.normal = get_font_name(/helvetica)
  self.fonts.heading1 = get_font_name(/large, /bold)
  self.fonts.heading2 = get_font_name(/bold)


  ; Go back to free-run timing mode and 1 image per file

  self.camera_types.PROSILICA = 0
  self.camera_types.ROPER = 1
  self.camera_types.POINT_GREY = 2
  self.scan.camera_manufacturer = self.camera_types.ROPER
  self.scan.rotation_start = 0.
  self.scan.rotation_stop  = 179.75
  self.scan.rotation_step  = 0.25
  self.scan.num_angles     = 720
  self.scan.flatfield_axis        = 0
  self.scan.flatfield_increment   = 100
  self.scan.num_flatfields    = 10
  self.scan.sample_x_in_position  = 0.0
  self.scan.sample_x_out_position = 10.0
  self.scan.sample_y_in_position  = 0.0
  self.scan.sample_y_out_position = 0.0
  self.scan.autoscan = 0
  self.scan.num_dark_currents = 0
  self.scan.leave_motor = 0
  self.scan.ezca_timeout = .005
  self.scan.ezca_retry_count = 10
  cainit  ; Need to call this in case it was not called in startup script, i.e. Virtual Machine
  casettimeout, self.scan.ezca_timeout
  casetretrycount, self.scan.ezca_retry_count
  self.scan.flatfield =    0
  self.scan.normal_image = 1
  self.scan.states.SCAN_COMPLETE       =  0
  self.scan.state_strings[self.scan.states.SCAN_COMPLETE]      = 'Scan complete'
  self.scan.states.ABORT_SCAN          =  1
  self.scan.state_strings[self.scan.states.ABORT_SCAN]         = 'Scan aborted'
  self.scan.states.FLAT_FIELD          =  2
  self.scan.state_strings[self.scan.states.FLAT_FIELD]         = 'Scanning Flat Field'
  self.scan.states.NORMAL              =  3
  self.scan.state_strings[self.scan.states.NORMAL]             = 'Normal Scan'
  self.scan.states.NORMAL_ACQUISITION  =  4
  self.scan.state_strings[self.scan.states.NORMAL_ACQUISITION] = 'Preparing for Normal Scan'
  self.scan.states.NORMAL_READOUT      =  5
  self.scan.state_strings[self.scan.states.NORMAL_READOUT]     = 'Normal Scan File Readout'
  self.scan.current_state = self.scan.states.SCAN_COMPLETE
  self.scan.base_filename = 'Test'
  self.scan.filename = 'Test'
  self.scan.attributes_filename = ''
;  self.scan.pg_trigger_mode = 'Bulb'
  self.scan.pg_trigger_mode = 'Overlapped'

  self.expinfo.sample       = 'My sample'
  self.expinfo.comments[0]  = 'A comment'
  self.expinfo.title        = 'My title'
  self.expinfo.operator     = 'My name'
  self.expinfo.camera       = 'MicroMAX 5MHz, Nikon macro lens'
  self.expinfo.x_pixel_size = 1.0
  self.expinfo.y_pixel_size = 1.0
  self.expinfo.energy       = 20.0
  self.expinfo.dark_current = 100.

  ; Create a hidden base for creating widgets solely for purpose of getting their size
  hidden_base = widget_base()

  self.widgets.base= widget_base(column=1, /tlb_kill_request_events, $
    title='IDL Tomography Collection', $
    mbar=mbar, tab_mode=1)

  ; File menu
  file = widget_button(mbar, /menu, value = 'File')
  self.widgets.base_filename = widget_button(file, $
    value = 'Base filename ...')
  self.widgets.attributes = widget_button(file, $
    value = 'Attributes file ...')
  widget_control, self.widgets.attributes, sensitive = self.scan.camera_manufacturer ne self.camera_types.ROPER
  self.widgets.experiment_information = widget_button(file, $
    value = 'Experiment information ...')
  self.widgets.epics_process_variables = widget_button(file, $
    value = 'EPICS process variables ...')
  self.widgets.save_settings = widget_button(file, $
    value = 'Save settings ...')
  self.widgets.restore_settings = widget_button(file, $
    value = 'Restore settings ...')
  self.widgets.exit = widget_button(file, $
    value = 'Exit')

  undefined_position=-12345.
  ; Rotation
  col0 = widget_base(self.widgets.base, /column, /frame)
  self.widgets.rotation_base = col0
  row = widget_base(col0, /row, /align_center)
  t = widget_label(row, value='Rotation', font=self.fonts.heading1)
  row = widget_base(col0, /row)
  self.widgets.rotation_drive = cw_field(row, /column, title='Drive', $
    /float, xsize=10, value=undefined_position, /return_events)
  self.widgets.rotation_readback = cw_field(row, /column, title='Readback', $
    /float, xsize=10, value=undefined_position, /noedit)
  self.widgets.motor_speed = cw_field(row, /column, title = 'Motor Speed', $
    /float, xsize = 10, value = undefined_position,/noedit)
  self.widgets.num_groups = cw_field(row, /column, title = 'OTF Groups', $
    /float, xsize = 10, value = undefined_position, /noedit)
  widget_control, self.widgets.num_groups, set_value = $
    strtrim(2*CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment)+1,2)
  if (self.scan.num_flatfields eq 0) then widget_control, self.widgets.num_groups, set_value = $
    strtrim(CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment),2)
  widget_control, self.widgets.num_groups, sensitive=0
  self.widgets.leave_motor = cw_bgroup(row, /column, ["Don't return motor to start of scan"], /nonexclusive, $
    set_value = [self.scan.leave_motor])
  row = widget_base(col0, /row)
  self.widgets.rotation_start = cw_field(row, /column, title='Start position', $
    /float, xsize=10, value=self.scan.rotation_start, /return_events)
  self.widgets.rotation_stop = cw_field(row, /column, title='End position', $
    /float, xsize=10, value=self.scan.rotation_stop, /return_events)
  self.widgets.rotation_step = cw_field(row, /column, title='Step size', $
    /float, xsize=10, value=self.scan.rotation_step, /return_events)
  self.widgets.num_angles = cw_field(row, /column, title='# angles', $
    /integer, xsize=10, value=self.scan.num_angles, /return_events)

  ; Horizontal translation
  col0 = widget_base(self.widgets.base, /column, /frame)
  self.widgets.sample_x_base = col0
  row = widget_base(col0, /row, /align_center)
  t = widget_label(row, value='Horizontal Translation', font=self.fonts.heading1)
  row = widget_base(col0, /row, /base_align_bottom)
  self.widgets.sample_x_drive = cw_field(row, /column, title='Drive', $
    /float, xsize=10, value=undefined_position, /return_events)
  self.widgets.sample_x_readback = cw_field(row, /column, title='Readback', $
    /float, xsize=10, value=undefined_position, /noedit)
  self.widgets.sample_x_in_position = cw_field(row, /column, title='Sample in position', $
    /float, xsize=10, value=self.scan.sample_x_in_position, $
    /return_events)
  self.widgets.sample_x_out_position = cw_field(row, /column, title='Sample out position', $
    /float, xsize=10, value=self.scan.sample_x_out_position, $
    /return_events)

  ; Vertical translation
  col0 = widget_base(self.widgets.base, /column, /frame)
  self.widgets.sample_y_base = col0
  row = widget_base(col0, /row, /align_center)
  t = widget_label(row, value='Vertical Translation', font=self.fonts.heading1)
  row = widget_base(col0, /row, /base_align_bottom)
  self.widgets.sample_y_drive = cw_field(row, /column, title='Drive', $
    /float, xsize=10, value=undefined_position, /return_events)
  self.widgets.sample_y_readback = cw_field(row, /column, title='Readback', $
    /float, xsize=10, value=undefined_position, /noedit)
  self.widgets.sample_y_in_position = cw_field(row, /column, title='Sample in position', $
    /float, xsize=10, value=self.scan.sample_y_in_position, $
    /return_events)
  self.widgets.sample_y_out_position = cw_field(row, /column, title='Sample out position', $
    /float, xsize=10, value=self.scan.sample_y_out_position, $
    /return_events)

  ; Flat field control
  col0 = widget_base(self.widgets.base, /column, /frame)
  self.widgets.flatfield_base = col0
  row = widget_base(col0, /row, /align_center)
  t = widget_label(row, value='Flat Field Control', font=self.fonts.heading1)
  row = widget_base(col0, /row, /base_align_bottom)
  col1 = widget_base(row, /column)
  t = widget_label(col1, value='Axis to move for flat fields')
  self.widgets.flatfield_axis = widget_droplist(col1, $
    value=['Horizontal', 'Vertical', 'Both'])
  ; Select horizontal axis as default
  widget_control, self.widgets.flatfield_axis, set_droplist_select=self.scan.flatfield_axis
  widget_control, self.widgets.sample_x_base, sensitive=(self.scan.flatfield_axis ne 1)
  widget_control, self.widgets.sample_y_base, sensitive=(self.scan.flatfield_axis ne 0)
  col1 = widget_base(row, /column)
  self.widgets.move_sample_in = widget_button(col1, $
    value='Move sample in')
  self.widgets.move_sample_out = widget_button(col1, $
    value='Move sample out')
  ;col2 = widget_base(row, /column)
  self.widgets.flatfield_increment = cw_field(row, /column, title='# angles between flat fields', $
    /integer, xsize=10, value=self.scan.flatfield_increment,/return_events)
  self.widgets.num_flatfields = cw_field(row, /column, title = '# images per flat field scan' , $
    /integer, xsize = 10, value = self.scan.num_flatfields,/return_events)

  ; Data collection
  col0 = widget_base(self.widgets.base, /column, /frame)
  self.widgets.scan_base = col0
  row = widget_base(col0, /row, /align_center)
  t = widget_label(row, value='Data Collection', font=self.fonts.heading1)
  row = widget_base(col0, /row, /base_align_bottom)
  self.widgets.exposure_time = cw_field(row, /column, title='Exposure time', $
    /float, xsize=10, value=undefined_position, /return_events)
  self.widgets.num_dark_currents = cw_field(row, /column, title='# dark currents', $
    /integer, xsize=10, value=self.scan.num_dark_currents, /return_events)
  self.widgets.autoscan = cw_bgroup(row, /column, ['Auto scan'], /nonexclusive, $
    set_value = [self.scan.autoscan])
  self.widgets.start_scan = widget_button(row, value='Start scan')
  self.widgets.abort_scan = widget_button(row, value='Abort scan')
  self.widgets.alignment_scan = widget_button(row, value='Alignment scan')

  ; Status
  col0 = widget_base(self.widgets.base, /column, /frame, tab_mode=0)
  self.widgets.status_base = col0
  row = widget_base(col0, /row, /align_center)
  t = widget_label(row, value='Status', font=self.fonts.heading1)
  row = widget_base(col0, /row)
  self.widgets.base_file = cw_field(row, title="Output file name:", $
    xsize=50, value=self.scan.base_filename, /noedit)
  row = widget_base(col0, /row)
  self.widgets.attributes_filename = cw_field(row, title="Attributes .xml file:", $
    xsize=50, value=self.scan.attributes_filename, /noedit)
  widget_control, self.widgets.attributes_filename, sensitive = self.scan.camera_manufacturer ne self.camera_types.ROPER
  if (self.scan.camera_manufacturer ne self.camera_types.ROPER) then begin
    t = caget(self.epics_pvs.camera_name+'cam1:NDAttributesFile', file)
    widget_control, self.widgets.attributes_filename, set_value = strtrim(file,2)
    self.scan.attributes_filename = strtrim(file,2)
  endif else begin
    widget_control, self.widgets.attributes_filename, set_value = ''
  endelse

  row = widget_base(col0, /row)
  self.widgets.sample_info = cw_field(row, title="Sample description:", $
    xsize=50, value=self.expinfo.sample, /noedit)
  row = widget_base(col0, /row)
  self.widgets.status = cw_field(row, title="Scan status:", $
    xsize=30, /noedit)
  self.widgets.scan_point = cw_field(row, title="Scan point:", $
    xsize=15, /noedit)

  ; Clocks
  col0 = widget_base(self.widgets.base, /column, /frame, tab_mode=0)
  self.widgets.clock_base = col0
  row = widget_base(col0, /row, /align_center)
  self.widgets.elapsed_time = cw_field(row, title = "Time Elapsed:", $
    xsize = 20, /noedit)
  self.widgets.est_remaining_time = cw_field(row, title = "Estimated Remaining Time:",$
    xsize = 20, /noedit)


  widget_control, self.widgets.base, set_uvalue=self

  ; Make all of the base widgets the same size so they line up nicely
  g = widget_info(self.widgets.base, /geometry)
  widget_control, self.widgets.rotation_base, xsize=g.xsize
  widget_control, self.widgets.sample_x_base, xsize=g.xsize
  widget_control, self.widgets.sample_y_base, xsize=g.xsize
  widget_control, self.widgets.flatfield_base, xsize=g.xsize
  widget_control, self.widgets.scan_base, xsize=g.xsize
  widget_control, self.widgets.status_base, xsize=g.xsize
  widget_control, self.widgets.clock_base, xsize=g.xsize
  widget_control, self.widgets.base, /realize

  ; Timer widgets
  self.widgets.scan_timer = self.widgets.scan_base
  self.scan_timer_interval = 0.01
  self.widgets.display_timer = self.widgets.base
  self.display_timer_interval = 0.1
  widget_control, self.widgets.display_timer, timer=self.display_timer_interval
  self.widgets.autoscan_timer = self.widgets.status_base
  self.autoscan_timer_interval = 1.0
  widget_control, self.widgets.autoscan_timer, timer=self.autoscan_timer_interval
  self.widgets.clock_timer = self.widgets.clock_base


  ; The "EPICS process variables" screen.  Normally not visible
  x_entry_size = 30
  self.pv_widgets.base= widget_base(column=1, /tlb_kill_request_events, $
    title='EPICS Process Variables', $
    tab_mode=1)
  col0 = widget_base(self.pv_widgets.base, /column)
  row = widget_base(col0, /row)
  ; Create the label with the string of the longest one to get its size
  t = widget_label(hidden_base, value='Pulse generator for Point Grey:')
  g = widget_info(t, /geometry)
  x_label_size = g.xsize
  t = widget_label(row, value='Camera name:', xsize=x_label_size, /align_right)
  self.pv_widgets.camera_name = widget_text(row,  xsize=x_entry_size, /editable)
  row = widget_base(col0, /row)
  t = widget_label(row, value='SIS MCS base name PV:', xsize=x_label_size, /align_right)
  self.pv_widgets.sis_mcs = widget_text(row,  xsize=x_entry_size, /editable)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Close shutter PV:', xsize=x_label_size, /align_right)
  self.pv_widgets.close_shutter = widget_text(row,  xsize=x_entry_size, /editable)
  t = widget_label(row, value='Close shutter value:', xsize=x_label_size, /align_right)
  self.pv_widgets.close_shutter_value = widget_text(row,  xsize=x_entry_size, /editable)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Open shutter PV:', xsize=x_label_size, /align_right)
  self.pv_widgets.open_shutter = widget_text(row,  xsize=x_entry_size, /editable)
  t = widget_label(row, value='Open shutter value:', xsize=x_label_size, /align_right)
  self.pv_widgets.open_shutter_value = widget_text(row,  xsize=x_entry_size, /editable)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Rotation motor:', xsize=x_label_size, /align_right)
  self.pv_widgets.rotation = widget_text(row,  xsize=x_entry_size, /editable)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Horizontal translation motor:', xsize=x_label_size, /align_right)
  self.pv_widgets.sample_x = widget_text(row,  xsize=x_entry_size, /editable)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Vertical translation motor:', xsize=x_label_size, /align_right)
  self.pv_widgets.sample_y = widget_text(row,  xsize=x_entry_size, /editable)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Beam ready PV:', xsize=x_label_size, /align_right)
  self.pv_widgets.beam_ready = widget_text(row,  xsize=x_entry_size, /editable)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Autoscan synchronization PV:', xsize=x_label_size, /align_right)
  self.pv_widgets.autoscan_sync = widget_text(row,  xsize=x_entry_size, /editable)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Autoscan suffix PV:', xsize=x_label_size, /align_right)
  self.pv_widgets.autoscan_suffix = widget_text(row,  xsize=x_entry_size, /editable)
  row = widget_base(col0, /row)
  self.pv_widgets.accept = widget_button(row, value='Accept')
  self.pv_widgets.cancel = widget_button(row, value='Cancel')

  widget_control, self.pv_widgets.base, /realize, map=0
  widget_control, self.pv_widgets.base, set_uvalue=self


  ; The "Experiment information" screen.  Normally not visible
  self.expinfo_widgets.base= widget_base(column=1, /tlb_kill_request_events, $
    title='Experiment Information', $
    tab_mode=1)
  x_entry_size = 50
  col0 = widget_base(self.expinfo_widgets.base, /column)
  row = widget_base(col0, /row)
  t = widget_label(hidden_base, value='X-ray energy (keV):')
  g = widget_info(t, /geometry)
  x_label_size = g.xsize
  t = widget_label(row, value='Sample:', xsize=x_label_size, /align_right)
  self.expinfo_widgets.sample = widget_text(row, xsize=x_entry_size, /editable, $
    value=self.expinfo.sample)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Title:', xsize=x_label_size, /align_right)
  self.expinfo_widgets.title = widget_text(row, xsize=x_entry_size, /editable, $
    value=self.expinfo.title)
  row = widget_base(col0, /row)
  ysize = n_elements(self.expinfo.comments)
  t = widget_label(row, value='Comments:', xsize=x_label_size, /align_right)
  self.expinfo_widgets.comments = widget_text(row, xsize=x_entry_size, ysize=ysize, $
    /editable, /no_newline)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Operator:', xsize=x_label_size, /align_right)
  self.expinfo_widgets.operator = widget_text(row, xsize=x_entry_size, /editable, $
    value=self.expinfo.operator)
  row = widget_base(col0, /row)
  t = widget_label(row, value='Camera/optics', xsize=x_label_size, /align_right)
  self.expinfo_widgets.camera = widget_text(row, xsize=x_entry_size, /editable, $
    value=self.expinfo.camera)
  row = widget_base(col0, /row)
  t = widget_label(row, value='X pixel size:', xsize=x_label_size, /align_right)
  self.expinfo_widgets.x_pixel_size = widget_text(row, xsize=x_entry_size, /editable, $
    value=string(self.expinfo.x_pixel_size))
  row = widget_base(col0, /row)
  t = widget_label(row, value='Y pixel size:', xsize=x_label_size, /align_right)
  self.expinfo_widgets.y_pixel_size = widget_text(row, xsize=x_entry_size, /editable, $
    value=string(self.expinfo.y_pixel_size))
  row = widget_base(col0, /row)
  t = widget_label(row, value='X-ray energy (keV):', xsize=x_label_size, /align_right)
  self.expinfo_widgets.energy = widget_text(row, xsize=x_entry_size, /editable, $
    value=string(self.expinfo.energy))
  row = widget_base(col0, /row)
  t = widget_label(row, value='Dark Current:', xsize=x_label_size, /align_right)
  self.expinfo_widgets.dark_current = widget_text(row, xsize=x_entry_size, /editable, $
    value=string(self.expinfo.dark_current))
  row = widget_base(col0, /row)
  self.expinfo_widgets.accept = widget_button(row, value='Accept')
  self.expinfo_widgets.cancel = widget_button(row, value='Cancel')

  widget_control, self.expinfo_widgets.base, /realize, map=0
  widget_control, self.expinfo_widgets.base, set_uvalue=self

  xmanager, 'tomo_collect', self.widgets.base, /no_block
  xmanager, 'tomo_epics_pvs', self.pv_widgets.base, /no_block
  xmanager, 'tomo_expinfo', self.expinfo_widgets.base, /no_block

  ; Restore settings
  ; Try to open a file with environment variable name 'TOMO_COLLECT_SETTINGS'
  exists = 0
  file = getenv('TOMO_COLLECT_SETTINGS')
  if (file ne '') then exists = file_test(file)
  if (not exists) then begin
    ; Try to open file in the current directory
    file = 'tomo_collect_settings.dat'
    exists = file_test(file)
  endif
  if (exists) then status = self->restoreSettings(file)
  widget_control, self.widgets.num_groups, set_value = $
    strtrim(2*CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment)+1,2)
  if (self.scan.num_flatfields eq 0) then widget_control, self.widgets.num_groups, set_value = $
    strtrim(CEIL(1.0*self.scan.num_angles/self.scan.flatfield_increment),2)
  widget_control, self.widgets.num_groups, sensitive=1

  t = self->validateEpicsPvs()

  return, 1
end

pro tomo_collect_ad2::cleanup

end

pro tomo_collect_ad2__define

  widgets={ tomo_collect_widgets, $
    base: 0L, $
    base_filename: 0L, $
    attributes: 0L, $
    epics_process_variables: 0L, $
    experiment_information: 0L, $
    save_settings: 0L, $
    restore_settings: 0L, $
    exit: 0L, $
    rotation_base: 0L, $
    rotation_drive: 0L, $
    rotation_readback: 0L, $
    rotation_start: 0L, $
    rotation_stop: 0L, $
    rotation_step: 0L, $
    num_angles: 0L, $
    sample_x_base: 0L, $
    sample_x_drive: 0L, $
    sample_x_readback: 0L, $
    sample_x_in_position: 0L, $
    sample_x_out_position: 0L, $
    sample_y_base: 0L, $
    sample_y_drive: 0L, $
    sample_y_readback: 0L, $
    sample_y_in_position: 0L, $
    sample_y_out_position: 0L, $
    flatfield_base: 0L, $
    flatfield_axis: 0L, $
    move_sample_in: 0L, $
    move_sample_out: 0L, $
    flatfield_increment: 0L, $
    exposure_time: 0L, $
    scan_base: 0L, $
    autoscan: 0L, $
    start_scan: 0L, $
    abort_scan: 0L, $
    alignment_scan: 0L, $
    status_base: 0L, $
    base_file: 0L, $
    attributes_filename: 0L, $
    sample_info: 0L, $
    scan_point: 0L, $
    status: 0L, $
    scan_timer: 0L, $
    display_timer: 0L, $
    autoscan_timer: 0L, $
    motor_speed: 0L, $
    num_groups: 0L, $
    leave_motor: 0L, $
    num_flatfields: 0L, $
    num_dark_currents: 0L, $
    clock_base: 0L, $
    clock_timer: 0L, $
    elapsed_time: 0L, $
    est_remaining_time: 0L $
  }

  ; These widgets are in the "EPICS process variables" page
  pv_widgets = {tomo_collect_pv_widgets, $
    camera_name: 0L, $
    sis_mcs: 0L, $
    close_shutter: 0L, $
    close_shutter_value: 0L, $
    open_shutter: 0L, $
    open_shutter_value: 0L, $
    base: 0L, $
    rotation: 0L, $
    sample_x: 0L, $
    sample_y: 0L, $
    beam_ready: 0L, $
    autoscan_sync: 0L, $
    autoscan_suffix: 0L, $
    accept: 0L, $
    cancel: 0L $
  }

  ; These widgets are in the "experiment information" page
  expinfo_widgets = {tomo_collect_expinfo_widgets, $
    base: 0L, $
    sample: 0L, $
    comments: 0L, $
    title: 0L, $
    operator: 0L, $
    camera: 0L, $
    x_pixel_size: 0L, $
    y_pixel_size: 0L, $
    dark_current: 0L, $
    energy: 0L, $
    accept: 0L, $
    cancel: 0L $
  }

  expinfo = {tomo_collect_expinfo, $
    sample: "", $
    comments: strarr(5), $
    title: "", $
    operator: "", $
    camera: "", $
    x_pixel_size: 0., $
    y_pixel_size: 0., $
    energy: 0., $
    dark_current: 0. $
  }

  epics_pvs = {tomo_epics_pvs, $
    ; These are all saved to the settings file
    camera_name: "", $
    sis_mcs: "", $
    close_shutter: "", $
    open_shutter: "", $
    rotation: "", $
    sample_x: "", $
    sample_y: "", $
    beam_ready: "", $
    autoscan_sync: "", $
    autoscan_suffix: "" $
  }

  scan_states = {tomo_scan_states, $
    ABORT_SCAN: 0, $
    SCAN_COMPLETE: 0, $
    FLAT_FIELD: 0, $
    NORMAL: 0, $
    NORMAL_ACQUISITION: 0, $
    NORMAL_READOUT: 0 $
  }

  camera_types = {tomo_scan_camera_types, $
    PROSILICA: 0, $
    ROPER: 0, $
    POINT_GREY: 0 $
  }

  scan = {tomo_scan, $
    rotation_start: 0.0, $
    rotation_stop: 0.0, $
    rotation_step: 0.0, $
    num_angles: 0L, $
    flatfield_axis: 0L, $
    flatfield_increment: 0L, $
    sample_x_in_position: 0.0, $
    sample_x_out_position: 0.0, $
    sample_y_in_position: 0.0, $
    sample_y_out_position: 0.0, $
    autoscan: 0L, $
    ; Items above this are saved to the settings file
    num_points: 0L, $
    current_point: 0L, $
    current_state: 0L, $
    rotation_motor: obj_new(), $
    sample_x_motor: obj_new(), $
    sample_y_motor: obj_new(), $
    ccd:            obj_new(), $
    rotation_array: ptr_new(), $
    sample_x_array: ptr_new(), $
    sample_y_array: ptr_new(), $
    image_type_array: ptr_new(), $
    base_filename: '', $
    filename: '', $
    attributes_filename: '',$
    states: scan_states, $
    state_strings: strarr(6), $
    ezca_timeout: 0.0, $
    ezca_retry_count: 0L, $
    flatfield: 0L, $
    normal_image: 0L, $
    motor_position: 0L, $
    motor_speed: 0.0, $
    motor_speed_old: 0.0, $
    exposure_time: 0.0, $
    time_per_angle: 0.0, $
    num_groups: 0L, $
    leave_motor: 0L, $
    otf_rotation_array: ptr_new(), $
    num_flatfields: 0L ,$
    camera_name: '' ,$
    camera_manufacturer: 0L, $
    camera_model: '', $
    pg_trigger_mode: '', $
    file_control: '', $
    num_dark_currents: 0L, $
    start_clock: 0L, $
    elapsed_time: 0L, $
    est_remaining_time: 0L, $
    close_shutter_value: '', $
    open_shutter_value: '' $
  }

  fonts = {tomo_fonts, $
    normal: '', $
    heading1: '', $
    heading2: '' $
  }

  tomo_collect_ad2 = {tomo_collect_ad2, $
    widgets: widgets, $
    pv_widgets: pv_widgets, $
    expinfo_widgets: expinfo_widgets, $
    epics_pvs: epics_pvs, $
    camera_types: camera_types, $
    scan: scan, $
    expinfo: expinfo, $
    settings_file: "", $
    epics_pvs_valid: 0L, $
    scan_timer_interval: 0.0, $
    display_timer_interval: 0.0, $
    autoscan_timer_interval: 0.0, $
    fonts: fonts $
  }
end
