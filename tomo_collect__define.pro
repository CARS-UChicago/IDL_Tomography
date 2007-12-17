pro tomo_collect::start_scan
    ; Construct array of positions
    ptr_free, self.scan.rotation_array
    ptr_free, self.scan.sample_x_array
    ptr_free, self.scan.sample_y_array
    self.scan.num_points = self.scan.num_angles + $
                               2 + self.scan.num_angles/self.scan.flatfield_increment
    self.scan.rotation_array = ptr_new(fltarr(self.scan.num_points))
    self.scan.sample_x_array = ptr_new(fltarr(self.scan.num_points))
    self.scan.sample_y_array = ptr_new(fltarr(self.scan.num_points))
    self.scan.image_type_array = ptr_new(lonarr(self.scan.num_points))
    angle_array = self.scan.rotation_start + $
                      findgen(self.scan.num_angles)*self.scan.rotation_step

    step = 0
    for pass=0, self.scan.num_passes-1 do begin
        for i=0, (self.scan.num_angles / self.scan.num_passes) - 1 do begin
            angle = angle_array[pass + (i * self.scan.num_passes)]
            if ((i mod self.scan.flatfield_increment) eq 0) then begin
                (*self.scan.rotation_array)[step] = angle
                if (self.scan.flatfield_axis ne 0) then begin
                    (*self.scan.sample_y_array)[step] = self.scan.sample_y_out_position
                endif
                if (self.scan.flatfield_axis ne 1) then begin
                    (*self.scan.sample_x_array)[step] = self.scan.sample_x_out_position
                endif
                (*self.scan.image_type_array)[step] = self.scan.flatfield
                step = step + 1
            endif
            (*self.scan.rotation_array)[step] = angle
            if (self.scan.flatfield_axis ne 0) then begin
                (*self.scan.sample_y_array)[step] = self.scan.sample_y_in_position
            endif
            if (self.scan.flatfield_axis ne 1) then begin
                (*self.scan.sample_x_array)[step] = self.scan.sample_x_in_position
            endif
            (*self.scan.image_type_array)[step] = self.scan.normal_image
            step = step + 1
        endfor
        ; Force an additional flat field at the end of each pass
        if (self.scan.flatfield_axis ne 0) then begin
            (*self.scan.sample_y_array)[step] = self.scan.sample_y_out_position
        endif
        if (self.scan.flatfield_axis ne 1) then begin
            (*self.scan.sample_x_array)[step] = self.scan.sample_x_out_position
        endif
        (*self.scan.image_type_array)[step] = self.scan.flatfield
        step = step + 1
    endfor

    self.scan.num_points = step

    ; Stop CCD acquisition, since we could be in focus mode
    t = self.scan.ccd->stop()

    ; Write the setup file
    status = self->save_settings(self.scan.filename + '.setup')

    ; Start the scan
    status = self.scan.ccd->setProperty(file_name=self.scan.filename)
    status = self.scan.ccd->setProperty(file_increment_count=1)
    status = self.scan.ccd->setProperty(file_increment_enable=1)
    if (self.scan.fast_scan) then begin
        status = self.scan.ccd->setProperty(nframes=self.scan.num_points)
        status = self.scan.ccd->setProperty(timing_mode=3)
    endif else begin
        status = self.scan.ccd->setProperty(nframes=1)
        status = self.scan.ccd->setProperty(timing_mode=1)
    endelse
    self.scan.current_point = 0
    widget_control, self.widgets.start_scan, sensitive=0
    widget_control, self.widgets.abort_scan, sensitive=1
    self->set_state, self.scan.states.MOTOR_WAIT

    self->move_sample_out
    self.scan.rotation_motor->move, (*self.scan.rotation_array)[0]
    wait, .01 ; Wait for motors to definitely start moving
    widget_control, self.widgets.scan_timer, timer=self.scan_timer_interval
end

function tomo_collect::check_beam
    t = caget(self.epics_pvs.beam_ready, beam_ready)
    return, beam_ready
end

pro tomo_collect::scanPoll
    ; This procedure is called when the scan timer expires.  It takes action depending
    ; upon what phase the scan is in and whether it can go to the next phase
    if (self.scan.current_state eq self.scan.states.ABORT_SCAN) then begin
        self->stop_scan
    endif else if (self.scan.current_state eq self.scan.states.MOTOR_WAIT) then begin
        if (self.scan.rotation_motor->done() eq 0) then return
        if (self.scan.sample_x_motor->done() eq 0) then return
        if (self.scan.sample_y_motor->done() eq 0) then return

        ; See if there is beam, exit if there is not
        if (self->check_beam() eq 0) then return

        if (self.scan.fast_scan) then begin
            ; If first point in scan then start acquisition
            if (self.scan.current_point eq 0) then begin
                ; Make sure external sync line is high to start
                t = caput(self.epics_pvs.external_trigger, 1)
                ; Turn on the detector
                t = self.scan.ccd->start()
                ; The CoolSnap cameras need a delay after we start WinView acquisition
                ; 1 second seems to work
                wait, 1
            endif
            self->set_state, self.scan.states.DETECTOR_WAIT_READY
        endif else begin  ; Not a fast scan
            t = self.scan.ccd->start()
            self->set_state, self.scan.states.DETECTOR_WAIT_STOP
        endelse
    endif else if (self.scan.current_state eq self.scan.states.DETECTOR_WAIT_READY) then begin
        if (self.scan.fast_scan) then begin
            ; See if detector busy signal is idle
            t = caget(self.epics_pvs.ccd_ready, state)
            if (state ne self.scan.ccd_ready) then return
            ; The monitor flag on the ccd_ready PV was cleared by reading it above
            ; In waiting for DETECTOR_WAIT_STOP we will just wait for a transition
            ; Trigger detector by setting External Sync low then high
            t = caput(self.epics_pvs.external_trigger, 0)
            wait, .001
            t = caput(self.epics_pvs.external_trigger, 1)
            self->set_state, self.scan.states.DETECTOR_WAIT_STOP
            widget_control, self.widgets.status, set_value='Waiting for readout'
        endif
    endif else if (self.scan.current_state eq self.scan.states.DETECTOR_WAIT_STOP) then begin
        if (self.scan.fast_scan) then begin
            ; See if detector has started its readout, meaning exposure is done, OK to move motor
            newMonitor = caCheckMonitor(self.epics_pvs.ccd_ready)
            if (newMonitor) then begin
                ; Note we used to check state of ccd_ready line here, but that is subject to timing
                ; problems, because the line might have made a second transition, back to "ready"
                ; before we check it.  Still need to read the PV to clear the monitor?
                t = caget(self.epics_pvs.ccd_ready, ready)
                ccd_busy = 0
            endif else begin
                ccd_busy = 1
            endelse
        endif else begin
            ; Not a fast scan
            ccd_busy = self.scan.ccd->busy()
        endelse
        if (ccd_busy) then return
        ; Detector is done
        ; See if there is still beam, it could have dumped during exposure
        ; If the beam has dumped, then set the mode back to MOTOR_WAIT, so it will start this
        ; exposure again when the beam comes back.
        if (self->check_beam() eq 0) then begin
            ; WinView has incremented the file number, we need to decrement it.
            ; Only do this for slow scans
            if (not self.scan.fast_scan) then begin
                t = self.scan.ccd->getProperty(file_increment_count=file_increment_count)
                t = self.scan.ccd->getProperty(file_increment_count=file_increment_count-1)
            endif
            self->set_state, self.scan.states.MOTOR_WAIT
            widget_control, self.widgets.status, $
                            set_value=self.scan.state_strings[self.scan.states.BEAM_WAIT]
            return
        endif

        if (self.scan.fast_scan eq 0) then begin
            ; Record the angle and image type in the data file
            comment1 = 'Angle='+strtrim((*self.scan.rotation_array)[self.scan.current_point], 2)
            if ((*self.scan.image_type_array)[self.scan.current_point] eq self.scan.flatfield) then begin
                comment2 = 'Type=FLAT_FIELD'
            endif else begin
                comment2 = 'Type=NORMAL'
            endelse
            comments = [comment1, comment2]
            t = self.scan.ccd->setProperty(comments=comments)
            t = self.scan.ccd->save()
            t = self.scan.ccd->closeDocfile()
        endif

        self.scan.current_point++
        if (self.scan.current_point lt self.scan.num_points) then begin
            if (self.scan.flatfield_axis ne 0) then begin
                self.scan.sample_y_motor->move, (*self.scan.sample_y_array)[self.scan.current_point]
            endif
            if (self.scan.flatfield_axis ne 1) then begin
                self.scan.sample_x_motor->move, (*self.scan.sample_x_array)[self.scan.current_point]
            endif
            self.scan.rotation_motor->move, (*self.scan.rotation_array)[self.scan.current_point]
            self->set_state, self.scan.states.MOTOR_WAIT
            wait, .01 ; Wait for motor to definitely start moving, else we may sense done incorrectly
        endif else begin
            self.scan.current_point--
            self->stop_scan
        endelse
        ; Update current point string
        widget_control, self.widgets.scan_point, $
            set_value=strtrim(self.scan.current_point,2) + '/' + strtrim(self.scan.num_points,2)
    endif
end

pro tomo_collect::set_state, state
    self.scan.current_state = state
    widget_control, self.widgets.status, set_value=self.scan.state_strings[state]
end


pro tomo_collect::abort_scan
    self->set_state, self.scan.states.ABORT_SCAN
end

pro tomo_collect::stop_scan
    self->move_sample_in
    self.scan.rotation_motor->move, (*self.scan.rotation_array)[0]
    if (self.scan.fast_scan) then begin
        ; Wait for 1 second for detector readout to complete
        ; We would like to wait for notScan to go high, but it seems to go high
        ; only very briefly and then stay low once the required number of frames have
        ; been collected.
        wait, 1.0
        if (self.scan.ccd->docfileValid()) then begin
            ; Record the start anglem angle step, and flat fields in the data file
            comment1 = 'Start Angle=' + strtrim((*self.scan.rotation_array)[0], 2)
            comment2 = 'Angle Step=' + strtrim(self.scan.rotation_step, 2)
            comment3 = 'Flat Fields='
            for i=0, self.scan.num_points-1 do begin
                if ((*self.scan.image_type_array)[i] eq self.scan.flatfield) then begin
                    comment3 = comment3 + strtrim(i,2) + ' '
                endif
            endfor
            comments = [comment1, comment2, comment3]
            t = self.scan.ccd->setProperty(comments=comments)
            ; It is still acquiring and the data file is still open, close it
            widget_control, self.widgets.status, set_value='Stopping run'
            t = self.scan.ccd->stop()
            busy = 1
            while (busy) do begin
                widget_control, self.widgets.status, set_value='Waiting for WinView on last frame'
                wait, .01
                busy = self.scan.ccd->busy()
            endwhile
            widget_control, self.widgets.status, set_value='Saving data'
            t = self.scan.ccd->save()
            widget_control, self.widgets.status, set_value='Closing docFile'
            t = self.scan.ccd->closeDocfile()
        endif
        ; Go back to free-run timing mode and 1 image per file
        t = self.scan.ccd->stop()
        t = self.scan.ccd->setProperty(nframes=1)
        t = self.scan.ccd->setProperty(timing_mode=1)
    endif
    widget_control, self.widgets.start_scan, sensitive=1
    widget_control, self.widgets.abort_scan, sensitive=0
    self->set_state, self.scan.states.SCAN_COMPLETE
end


pro tomo_collect::move_sample_in
    if (not self.epics_pvs_valid) then return
    if (self.scan.flatfield_axis ne 1) then begin
        self.scan.sample_x_motor->move, self.scan.sample_x_in_position
    endif
    if (self.scan.flatfield_axis ne 0) then begin
        self.scan.sample_y_motor->move, self.scan.sample_y_in_position
    endif
end

pro tomo_collect::move_sample_out
    if (not self.epics_pvs_valid) then return
    if (self.scan.flatfield_axis ne 1) then begin
        self.scan.sample_x_motor->move, self.scan.sample_x_out_position
    endif
    if (self.scan.flatfield_axis ne 0) then begin
        self.scan.sample_y_motor->move, self.scan.sample_y_out_position
    endif
end


function tomo_collect::validate_epics_pvs
    ; Check that all EPICS PVs are valid.
    ; Display error dialog and return error on first non-valid PV
    n_pvs = n_tags(self.epics_pvs)
    tags = tag_names(self.epics_pvs)
    pvs_ok = 1
    ; Make the cursor be an hourglass, since timeouts take a while
    widget_control, /hourglass
    for i=0, n_pvs-1 do begin
        pv = self.epics_pvs.(i)
        if (strlen(pv) eq 0) then begin
            t = dialog_message('Error: ' + tags[i] + ' is blank, not a valid EPICS PV name.')
            pvs_ok = 0
            break
        endif
        t = caget(pv, value)
        if (t ne 0) then begin
            t = dialog_message('Error: ' + pv + ' is not a valid EPICS PV name, not found.')
            pvs_ok = 0
            break
        endif
    endfor

    if (pvs_ok) then begin
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
        t = caSetMonitor(self.epics_pvs.ccd_ready)
        t = caSetMonitor(self.epics_pvs.autoscan_sync)
        t = caSetMonitor(self.epics_pvs.autoscan_suffix)
    endif

    finish:

    if (pvs_ok) then sensitive=1 else sensitive=0
    self.epics_pvs_valid = pvs_ok
    ; Enable/disable the widgets that require valid EPICS PVs
    widget_control, self.widgets.move_sample_in, sensitive=sensitive
    widget_control, self.widgets.move_sample_out, sensitive=sensitive
    widget_control, self.widgets.rotation_drive, sensitive=sensitive
    widget_control, self.widgets.sample_x_drive, sensitive=sensitive
    widget_control, self.widgets.sample_y_drive, sensitive=sensitive
    if (not self.ccd_valid) then sensitive=0
    widget_control, self.widgets.start_scan, sensitive=sensitive
    widget_control, self.widgets.abort_scan, sensitive=0

    self->update_screen, /force_update
    if (pvs_ok) then begin
        return, 0
    endif else begin
        return, -1
    endelse
end


function tomo_collect::save_settings, file, all=all
    openw, lun, file, error=error, /get_lun
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
    printf, lun, 'ROTATION_PV: ',         self.epics_pvs.rotation
    printf, lun, 'SAMPLE_X_PV: ',         self.epics_pvs.sample_x
    printf, lun, 'SAMPLE_Y_PV: ',         self.epics_pvs.sample_y
    printf, lun, 'BEAM_READY_PV: ',       self.epics_pvs.beam_ready
    printf, lun, 'AUTOSCAN_SYNC_PV: ',    self.epics_pvs.autoscan_sync
    printf, lun, 'AUTOSCAN_SUFFIX_PV: ',  self.epics_pvs.autoscan_suffix
    printf, lun, 'EXTERNAL_TRIGGER_PV: ', self.epics_pvs.external_trigger
    printf, lun, 'CCD_READY_PV: ',        self.epics_pvs.ccd_ready
    ; These are in self.scan
    printf, lun, 'ROTATION_START: ',      self.scan.rotation_start
    printf, lun, 'ROTATION_STOP: ',       self.scan.rotation_stop
    printf, lun, 'ROTATION_STEP: ',       self.scan.rotation_step
    printf, lun, 'NUM_ANGLES: ',          self.scan.num_angles
    printf, lun, 'NUM_PASSES: ',          self.scan.num_passes
    printf, lun, 'FLATFIELD_AXIS: ',      self.scan.flatfield_axis
    printf, lun, 'FLATFIELD_INCREMENT: ', self.scan.flatfield_increment
    printf, lun, 'SAMPLE_X_IN: ',         self.scan.sample_x_in_position
    printf, lun, 'SAMPLE_X_OUT: ',        self.scan.sample_x_out_position
    printf, lun, 'SAMPLE_Y_IN: ',         self.scan.sample_y_in_position
    printf, lun, 'SAMPLE_Y_OUT: ',        self.scan.sample_y_out_position
    printf, lun, 'AUTOSCAN: ',            self.scan.autoscan
    printf, lun, 'FAST_SCAN: ',           self.scan.fast_scan
    free_lun, lun
    self.settings_file = file
    return, 1
end


function tomo_collect::restore_settings, file
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
            'ROTATION_PV:':         self.epics_pvs.rotation = value
            'SAMPLE_X_PV:':         self.epics_pvs.sample_x = value
            'SAMPLE_Y_PV:':         self.epics_pvs.sample_y = value
            'BEAM_READY_PV:':       self.epics_pvs.beam_ready = value
            'AUTOSCAN_SYNC_PV:':    self.epics_pvs.autoscan_sync = value
            'AUTOSCAN_SUFFIX_PV:':  self.epics_pvs.autoscan_suffix = value
            'EXTERNAL_TRIGGER_PV:': self.epics_pvs.external_trigger = value
            'CCD_READY_PV:':        self.epics_pvs.ccd_ready = value
            ; These are in self.scan
            'ROTATION_START:':      self.scan.rotation_start = float(value)
            'ROTATION_STOP:':       self.scan.rotation_stop = float(value)
            'ROTATION_STEP:':       self.scan.rotation_step = float(value)
            'NUM_ANGLES:':          self.scan.num_angles = long(value)
            'NUM_PASSES:':          self.scan.num_passes = long(value)
            'FLATFIELD_AXIS:':      self.scan.flatfield_axis = value
            'FLATFIELD_INCREMENT:': self.scan.flatfield_increment = long(value)
            'SAMPLE_X_IN:':         self.scan.sample_x_in_position = float(value)
            'SAMPLE_X_OUT:':        self.scan.sample_x_out_position = float(value)
            'SAMPLE_Y_IN:':         self.scan.sample_y_in_position = float(value)
            'SAMPLE_Y_OUT:':        self.scan.sample_y_out_position = float(value)
            'AUTOSCAN:':            self.scan.autoscan = long(value)
            'FAST_SCAN:':           self.scan.fast_scan = long(value)
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
    self->copy_settings_to_widgets
    free_lun, lun
    self.settings_file = file
    status = self->validate_epics_pvs()

    return, 1
end


pro tomo_collect::update_screen, force_update=force_update

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

end



pro tomo_epics_pvs_event, event
    widget_control, event.top, get_uvalue=tomo_collect
    tomo_collect->epics_pvs_event, event
end

pro tomo_collect::epics_pvs_event, event
    if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
       ; Convert event.id to cancel
       event.id = self.pv_widgets.cancel
    endif

    case event.id of
        self.pv_widgets.accept: begin
            ; Verify EPICS PVs, create motor objects
            self->copy_settings_from_widgets
            status = self->validate_epics_pvs()
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


pro tomo_collect::copy_expinfo_to_widgets
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


pro tomo_collect::copy_expinfo_from_widgets
    widget_control, self.expinfo_widgets.sample, get_value=sample
    self.expinfo.sample = sample
    widget_control, self.expinfo_widgets.comments, get_value=comments
    n_comments = n_elements(self.expinfo.comments) < n_elements(comments)
    self.expinfo.comments = comments[0:n_comments-1]
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


pro tomo_collect::copy_settings_to_widgets
    ; These are in self.epics_pvs
    widget_control, self.pv_widgets.rotation,         set_value=self.epics_pvs.rotation
    widget_control, self.pv_widgets.sample_x,         set_value=self.epics_pvs.sample_x
    widget_control, self.pv_widgets.sample_y,         set_value=self.epics_pvs.sample_y
    widget_control, self.pv_widgets.beam_ready,       set_value=self.epics_pvs.beam_ready
    widget_control, self.pv_widgets.autoscan_sync,    set_value=self.epics_pvs.autoscan_sync
    widget_control, self.pv_widgets.autoscan_suffix,  set_value=self.epics_pvs.autoscan_suffix
    widget_control, self.pv_widgets.external_trigger, set_value=self.epics_pvs.external_trigger
    widget_control, self.pv_widgets.ccd_ready,        set_value=self.epics_pvs.ccd_ready
    ; These are in self.scan
    widget_control, self.widgets.rotation_start,        set_value=self.scan.rotation_start
    widget_control, self.widgets.rotation_stop,         set_value=self.scan.rotation_stop
    widget_control, self.widgets.rotation_step,         set_value=self.scan.rotation_step
    widget_control, self.widgets.num_angles,            set_value=self.scan.num_angles
    widget_control, self.widgets.num_passes,            set_value=self.scan.num_passes
    widget_control, self.widgets.flatfield_axis,        set_droplist_select=self.scan.flatfield_axis
    widget_control, self.widgets.flatfield_increment,   set_value=self.scan.flatfield_increment
    widget_control, self.widgets.sample_x_in_position,  set_value=self.scan.sample_x_in_position
    widget_control, self.widgets.sample_x_out_position, set_value=self.scan.sample_x_out_position
    widget_control, self.widgets.sample_y_in_position,  set_value=self.scan.sample_y_in_position
    widget_control, self.widgets.sample_y_out_position, set_value=self.scan.sample_y_out_position
    widget_control, self.widgets.autoscan,              set_value=self.scan.autoscan
    widget_control, self.widgets.fast_scan,             set_value=self.scan.fast_scan
end

pro tomo_collect::copy_settings_from_widgets
    ; These are in self.epics_pvs
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
    widget_control, self.pv_widgets.external_trigger, get_value=value
    self.epics_pvs.external_trigger = value
    widget_control, self.pv_widgets.ccd_ready, get_value=value
    self.epics_pvs.ccd_ready = value
    ; These are in self.scan
    widget_control, self.widgets.rotation_start, get_value=value
    self.scan.rotation_start = value
    widget_control, self.widgets.rotation_stop, get_value=value
    self.scan.rotation_stop = value
    widget_control, self.widgets.rotation_step, get_value=value
    self.scan.rotation_step = value
    widget_control, self.widgets.num_angles, get_value=value
    self.scan.num_angles = value
    widget_control, self.widgets.num_passes, get_value=value
    self.scan.num_passes = value
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
    widget_control, self.widgets.fast_scan, get_value=value
    self.scan.fast_scan = value
end


pro tomo_expinfo_event, event
    widget_control, event.top, get_uvalue=tomo_collect
    tomo_collect->expinfo_event, event
end

pro tomo_collect::expinfo_event, event
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


pro tomo_collect::event, event
    if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
        widget_control, event.top, /destroy
        obj_destroy, self
        return
    endif

    catch, err
    if (err ne 0) then begin
       t = dialog_message(!error_state.msg, /error)
        widget_control, self.widgets.status, set_value=!error_state.msg
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

        self.widgets.experiment_information: begin
            widget_control, self.expinfo_widgets.base, map=1
        end

        self.widgets.epics_process_variables: begin
            widget_control, self.pv_widgets.base, map=1
        end

        self.widgets.save_settings: begin
            file = dialog_pickfile(/write)
            if (file ne '') then begin
                status = self->save_settings(file, /all)
            endif
        end

        self.widgets.restore_settings: begin
            file = dialog_pickfile(/read, /must_exist)
            if (file ne '') then begin
                status = self->restore_settings(file)
            endif
        end

        self.widgets.exit: begin
            file = getenv('TOMO_COLLECT_SETTINGS')
            if (file eq '') then file = 'tomo_collect_settings.dat'
            status = self->save_settings(file, /all)
            widget_control, event.top, /destroy
            obj_destroy, self
            return
        end

        self.widgets.scan_timer: begin
           ; Timer event for polling during a scan.
           self->scanPoll
          if (self.scan.current_state ne self.scan.states.SCAN_COMPLETE) then begin
              widget_control, self.widgets.scan_timer, timer=self.scan_timer_interval
          endif
        end

        self.widgets.display_timer: begin
           ; Timer event to update screen.
           self->update_screen
           widget_control, self.widgets.display_timer, timer=self.display_timer_interval
        end

        self.widgets.autoscan_timer: begin
           ; Timer event for autoscan functions
           if ((self.scan.autoscan) and $
               (self.scan.current_state eq self.scan.states.SCAN_COMPLETE)) then begin
               sync_changed = caCheckMonitor(self.epics_pvs.autoscan_sync)
               t = caget(self.epics_pvs.autoscan_sync, start)
               if (sync_changed and start) then begin
                   ; The autoscan_sync PV has made a 0 to 1 transition so start a scan
                   status = caget(self.epics_pvs.autoscan_suffix, suffix)
                   self.scan.filename = self.scan.base_filename + '_' + suffix + '_'
                   self->start_scan
               endif else if (start) then begin
                   ; The scan is done and the sync PV is still 1, so set it to 0.  This
                   ; lets the client know that the scan is complete
                   t = caput(self.epics_pvs.autoscan_sync, 0)
               endif
           endif
           widget_control, self.widgets.autoscan_timer, timer=self.autoscan_timer_interval
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
        end

        self.widgets.num_passes: begin
            ; Nothing to do
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

        self.widgets.move_sample_in: begin
            ; Move sample in
            self->move_sample_in
        end

        self.widgets.move_sample_out: begin
            ; Move sample out
            self->move_sample_out
        end

        self.widgets.fast_scan: begin
           ; Nothing to do
        end

        self.widgets.autoscan: begin
           self.scan.autoscan = event.select
        end

        self.widgets.start_scan: begin
           self->start_scan
           ; Start a collection
        end

        self.widgets.abort_scan: begin
           ; Abort a collection
           self->abort_scan
        end

        else:  t = dialog_message('Unknown event')
    endcase

    end_event:
end



function tomo_collect::init
;+
; NAME:
;       tomo_collect::init
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

    ; Try to connect to CCD
    catch, error
    if (error ne 0) then begin
        self.ccd_valid = 0
    endif else begin
        self.scan.ccd = obj_new('winx32_ccd')
        self.ccd_valid=1
        t = self.scan.ccd->stop()
        t = self.scan.ccd->setProperty(nframes=1)
        t = self.scan.ccd->setProperty(timing_mode=1)
        t = self.scan.ccd->getProperty(controller_name=controller_name)
        if (controller_name eq 21) then begin
            ; 21 is PV-CAM.  We assume CoolSnap with FrameReadout output=TTL low=0 when
            ; ready
            self.scan.ccd_ready = 0
        endif else begin
            ; We assume anything else is MicroMAX 5MHz, which has notScan=TTL high = 1 when ready
            self.scan.ccd_ready = 1
        endelse
    endelse
    catch, /cancel
    ; Go back to free-run timing mode and 1 image per file
    self.scan.rotation_start = 0.
    self.scan.rotation_stop  = 179.75
    self.scan.rotation_step  = 0.25
    self.scan.num_angles     = 720
    self.scan.num_passes     = 1
    self.scan.flatfield_axis        = 0
    self.scan.flatfield_increment   = 100
    self.scan.sample_x_in_position  = 0.0
    self.scan.sample_x_out_position = 10.0
    self.scan.sample_y_in_position  = 0.0
    self.scan.sample_y_out_position = 0.0
    self.scan.fast_scan = 1
    self.scan.autoscan = 0
    self.scan.ezca_timeout = .005
    self.scan.ezca_retry_count = 10
    cainit  ; Need to call this in case it was not called in startup script, i.e. Virtual Machine
    casettimeout, self.scan.ezca_timeout
    casetretrycount, self.scan.ezca_retry_count
    self.scan.flatfield =    0
    self.scan.normal_image = 1
    self.scan.states.MOTOR_WAIT          =  0
    self.scan.state_strings[0]           = 'Waiting for motors'
    self.scan.states.BEAM_WAIT           =  1
    self.scan.state_strings[1]           = 'Waiting for beam'
    self.scan.states.DETECTOR_WAIT_READY =  2
    self.scan.state_strings[2]           = 'Waiting for detector exposure'
    self.scan.states.DETECTOR_WAIT_STOP  =  3
    self.scan.state_strings[3]           = 'Waiting for detector readout'
    self.scan.states.SCAN_COMPLETE       =  4
    self.scan.state_strings[4]           = 'Scan complete'
    self.scan.states.ABORT_SCAN          =  5
    self.scan.state_strings[5]           = 'Scan aborted'
    self.scan.current_state = self.scan.states.SCAN_COMPLETE
    self.scan.base_filename = 'Test'
    self.scan.filename = 'Test'

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
    row = widget_base(col0, /row)
    self.widgets.rotation_start = cw_field(row, /column, title='Start position', $
                                      /float, xsize=10, value=self.scan.rotation_start, /return_events)
    self.widgets.rotation_stop = cw_field(row, /column, title='End position', $
                                      /float, xsize=10, value=self.scan.rotation_stop, /return_events)
    self.widgets.rotation_step = cw_field(row, /column, title='Step size', $
                                      /float, xsize=10, value=self.scan.rotation_step, /return_events)
    self.widgets.num_angles = cw_field(row, /column, title='# angles', $
                                      /integer, xsize=10, value=self.scan.num_angles, /return_events)
    self.widgets.num_passes = cw_field(row, /column, title='# passes', $
                                      /integer, xsize=10, value=self.scan.num_passes, /return_events)

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
    self.widgets.flatfield_increment = cw_field(row, /column, title='# angles between flat fields', $
                                      /integer, xsize=10, value=self.scan.flatfield_increment)

    ; Scan control
    col0 = widget_base(self.widgets.base, /column, /frame)
    self.widgets.scan_base = col0
    row = widget_base(col0, /row, /align_center)
    t = widget_label(row, value='Data Collection', font=self.fonts.heading1)
    row = widget_base(col0, /row)
    self.widgets.fast_scan = cw_bgroup(row, /column, ['Fast scan'], /nonexclusive, $
                                       set_value = [self.scan.fast_scan])
    self.widgets.autoscan = cw_bgroup(row, /column, ['Auto scan'], /nonexclusive, $
                                       set_value = [self.scan.autoscan])
    self.widgets.start_scan = widget_button(row, value='Start scan')
    self.widgets.abort_scan = widget_button(row, value='Abort scan')

    ; Status
    col0 = widget_base(self.widgets.base, /column, /frame, tab_mode=0)
    self.widgets.status_base = col0
    row = widget_base(col0, /row, /align_center)
    t = widget_label(row, value='Status', font=self.fonts.heading1)
    row = widget_base(col0, /row)
    self.widgets.base_file = cw_field(row, title="Base file name:", $
                                      xsize=50, value=self.scan.base_filename, /noedit)
    row = widget_base(col0, /row)
    self.widgets.sample_info = cw_field(row, title="Sample description:", $
                                        xsize=50, value=self.expinfo.sample, /noedit)
    row = widget_base(col0, /row)
    self.widgets.status = cw_field(row, title="Scan status:", $
                                   xsize=30, /noedit)
    self.widgets.scan_point = cw_field(row, title="Scan point:", $
                                       xsize=15, /noedit)

    widget_control, self.widgets.base, set_uvalue=self

    ; Make all of the base widgets the same size so they line up nicely
    g = widget_info(self.widgets.base, /geometry)
    widget_control, self.widgets.rotation_base, xsize=g.xsize
    widget_control, self.widgets.sample_x_base, xsize=g.xsize
    widget_control, self.widgets.sample_y_base, xsize=g.xsize
    widget_control, self.widgets.flatfield_base, xsize=g.xsize
    widget_control, self.widgets.scan_base, xsize=g.xsize
    widget_control, self.widgets.status_base, xsize=g.xsize
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


    ; The "EPICS process variables" screen.  Normally not visible
    x_entry_size = 30
    self.pv_widgets.base= widget_base(column=1, /tlb_kill_request_events, $
                                      title='EPICS Process Variables', $
                                      tab_mode=1)
    col0 = widget_base(self.pv_widgets.base, /column)
    row = widget_base(col0, /row)
    ; Create the label with the string of the longest one to get its size
    t = widget_label(hidden_base, value='Horizontal translation motor:')
    g = widget_info(t, /geometry)
    x_label_size = g.xsize
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
    t = widget_label(row, value='CCD trigger PV:', xsize=x_label_size, /align_right)
    self.pv_widgets.external_trigger = widget_text(row,  xsize=x_entry_size, /editable)
    row = widget_base(col0, /row)
    t = widget_label(row, value='CCD ready PV:', xsize=x_label_size, /align_right)
    self.pv_widgets.ccd_ready = widget_text(row,  xsize=x_entry_size, /editable)
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
    if (exists) then status = self->restore_settings(file)
    t = self->validate_epics_pvs()

    return, 1
end

pro tomo_collect::cleanup

end

pro tomo_collect__define

    widgets={ tomo_collect_widgets, $
        base: 0L, $
        base_filename: 0L, $
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
        num_passes: 0L, $
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
        scan_base: 0L, $
        autoscan: 0L, $
        fast_scan: 0L, $
        start_scan: 0L, $
        abort_scan: 0L, $
        status_base: 0L, $
        base_file: 0L, $
        sample_info: 0L, $
        scan_point: 0L, $
        status: 0L, $
        scan_timer: 0L, $
        display_timer: 0L, $
        autoscan_timer: 0L $
    }

    ; These widgets are in the "EPICS process variables" page
    pv_widgets = {tomo_collect_pv_widgets, $
        base: 0L, $
        rotation: 0L, $
        sample_x: 0L, $
        sample_y: 0L, $
        beam_ready: 0L, $
        autoscan_sync: 0L, $
        autoscan_suffix: 0L, $
        external_trigger: 0L, $
        ccd_ready: 0L, $
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
        rotation: "", $
        sample_x: "", $
        sample_y: "", $
        beam_ready: "", $
        autoscan_sync: "", $
        autoscan_suffix: "", $
        external_trigger: "", $
        ccd_ready: "" $
    }

    scan_states = {tomo_scan_states, $
        MOTOR_WAIT: 0, $
        BEAM_WAIT: 0, $
        DETECTOR_WAIT_READY: 0, $
        DETECTOR_WAIT_STOP: 0, $
        ABORT_SCAN: 0, $
        SCAN_COMPLETE: 0 $
    }

    scan = {tomo_scan, $
        rotation_start: 0.0, $
        rotation_stop: 0.0, $
        rotation_step: 0.0, $
        num_angles: 0L, $
        num_passes: 0L, $
        flatfield_axis: 0L, $
        flatfield_increment: 0L, $
        sample_x_in_position: 0.0, $
        sample_x_out_position: 0.0, $
        sample_y_in_position: 0.0, $
        sample_y_out_position: 0.0, $
        autoscan: 0L, $
        fast_scan: 0L, $
        ; Items above this are saved to the settings file
        num_points: 0L, $
        current_point: 0L, $
        current_state: 0L, $
        rotation_motor: obj_new(), $
        sample_x_motor: obj_new(), $
        sample_y_motor: obj_new(), $
        ccd:            obj_new(), $
        ccd_ready: 0L, $
        rotation_array: ptr_new(), $
        sample_x_array: ptr_new(), $
        sample_y_array: ptr_new(), $
        image_type_array: ptr_new(), $
        base_filename: '', $
        filename: '', $
        states: scan_states, $
        state_strings: strarr(10), $
        ezca_timeout: 0.0, $
        ezca_retry_count: 0L, $
        flatfield: 0L, $
        normal_image: 0L $
    }

    fonts = {tomo_fonts, $
        normal: '', $
        heading1: '', $
        heading2: '' $
    }

    tomo_collect = {tomo_collect, $
        widgets: widgets, $
        pv_widgets: pv_widgets, $
        expinfo_widgets: expinfo_widgets, $
        epics_pvs: epics_pvs, $
        scan: scan, $
        expinfo: expinfo, $
        settings_file: "", $
        epics_pvs_valid: 0L, $
        ccd_valid: 0L, $
        scan_timer_interval: 0.0, $
        display_timer_interval: 0.0, $
        autoscan_timer_interval: 0.0, $
        fonts: fonts $
    }
end
