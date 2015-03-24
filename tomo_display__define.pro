pro tomo_display::set_tomo_params
    widget_control, self.widgets.recon_method, get_value=reconMethod
    widget_control, self.widgets.recon_scale, get_value=reconScale
    widget_control, self.widgets.ringWidth, get_value=ringWidth
    widget_control, self.widgets.airPixels, get_value=airPixels
    widget_control, self.widgets.fluorescence, get_value=fluorescence
    widget_control, self.widgets.filter_size, get_value=BP_filterSize
    index = widget_info(self.widgets.backproject_filter, /droplist_select)
    widget_control, self.widgets.backproject_filter, get_uvalue=choices
    BP_filterName = choices[index]
    BP_method = widget_info(self.widgets.backproject_method, /droplist_select)
    widget_control, self.widgets.backproject_riemann_interpolation, get_value=RiemannInterpolation
    widget_control, self.widgets.backproject_radon_interpolation, get_value=RadonInterpolation
    index = widget_info(self.widgets.gridrec_filter, /droplist_select)
    widget_control, self.widgets.gridrec_filter, get_uvalue=choices
    GR_filterName = choices[index]
    index = widget_info(self.widgets.sino_padding, /droplist_select)
    widget_control, self.widgets.sino_padding, get_uvalue=choices
    paddedSinogramWidth = choices[index]
    widget_control, self.widgets.paddingAverage, get_value=paddingAverage
    widget_control, self.widgets.gridrec_sampl_parameter, get_value=sampl
    widget_control, self.widgets.numThreads, get_value=numThreads
    widget_control, self.widgets.slicesPerChunk, get_value=slicesPerChunk
 
    self.tomoParams = tomo_params_init(*self.pvolume, $
            sinoScale = 0.0001, $
            reconScale = reconScale, $
            paddedSinogramWidth=paddedSinogramWidth, $
            paddingAverage=paddingAverage, $
            airPixels = airPixels, $
            ringWidth = ringWidth, $
            fluorescence = fluorescence, $
            reconMethod = reconMethod, $
            numThreads = numThreads, $
            slicesPerChunk = slicesPerChunk, $
            debug = 0, $            ; Make a widget for this!
            dbgFile = dbgFile, $    ; Make a widget for this!
            geom=0, $
            pswfParam=6., $         ; Make a widget for this?
            sampl=sampl, $
            maxPixSize=1.0, $ ; Make a widget for this?
            ROI=1.0, $       ; Make a widget for this?
            X0=0, $
            Y0=0, $
            ltbl=512, $
            GR_filterName=GR_filterName, $
            BP_method = BP_method, $
            BP_filterName = BP_filterName, $
            BP_filterSize = BP_filterSize, $
            RadonInterpolation = RadonInterpolation, $
            RiemannInterpolation = RiemannInterpolation)
end


pro tomo_display::rebin, image, x_dist, y_dist
    ; This function is called to rebin a 2-D array, either shrinking or expanding it
    ; by the selected "zoom" factor
    widget_control, self.widgets.zoom, get_value=zoom, get_uvalue=all_zooms
    zoom = all_zooms[zoom]
    if (zoom eq 1) then return
    image = reform(image)
    ncols = n_elements(image[*,0])
    nrows = n_elements(image[0,*])
    if (zoom gt 1) then begin
        last_col = ncols - 1
        last_row = nrows - 1
        ncols = ncols * fix(zoom)
        nrows = nrows * fix(zoom)
    endif
    if (zoom lt -1) then begin
        izoom = fix(abs(zoom))
        last_col = (ncols/izoom)*izoom - 1
        last_row = (nrows/izoom)*izoom - 1
        ncols = ncols / izoom
        nrows = nrows / izoom
    endif
    image = rebin(image[0:last_col, 0:last_row], ncols, nrows)
    x_dist = rebin(x_dist[0:last_col], ncols)
    y_dist = rebin(y_dist[0:last_row], nrows)

end


pro tomo_display::reconstruct, islice
    ; This function is called to reconstruct a single slice or to reconstruct all
    ; If islice is defined then we are to reconstruct a single slice
    if (not ptr_valid(self.pvolume)) then begin
        t = dialog_message('Must read in volume file first.', /error)
        return
    endif
    widget_control, /hourglass
    
    self.set_tomo_params

    if (n_elements(islice) ne 0) then begin
        widget_control, self.widgets.rotation_center[islice], get_value=center
        widget_control, self.widgets.recon_slice[islice], get_value=slice
        slice = slice < (self.ny-1)
        angles=*self.setup.angles
        r = reconstruct_slice(self.tomoParams, angles=angles, slice, *self.pvolume, $
                              center=center, sinogram=sinogram, cog=cog)
        ; If reconstruction was with backproject, rotate image so it is the same
        ; orientation as with gridrec
        if (self.tomoParams.reconMethod eq self.tomoParams.reconMethodBackproject) then r = rotate(r, 4)
        widget_control, self.widgets.auto_intensity, get_value=auto
        if (auto) then begin
            min=min(r, max=max)
        endif else begin
            widget_control, self.widgets.display_min, get_value=min
            widget_control, self.widgets.display_max, get_value=max
        endelse
        dims = size(r, /dimensions)
        xdist = findgen(dims[0])*self.setup.x_pixel_size
        ydist = xdist
        ; Change the size of the image before calling image_display
        self->rebin, r, xdist, ydist
        widget_control, self.widgets.volume_file, get_value=file
        dims = size(r, /dimensions)
        xdist = findgen(dims[0])*self.setup.x_pixel_size

        title = file + '    Center='+strtrim(string(center),2) + $
                       '     Slice='+strtrim(string(slice),2)
        image_display, r, min=min, max=max, title=title, $
                          xdist=xdist, ydist=ydist
        ; Display sinogram if desired
        widget_control, self.widgets.display_sinogram, get_value=display_sinogram
        if (display_sinogram and (n_elements(sinogram) ne 0)) then begin
            image_display, sinogram
        endif
        ; Plot center-of-gravity if desired
        widget_control, self.widgets.plot_cog, get_value=plot_cog
        if (plot_cog and (n_elements(cog) ne 0)) then begin
            angles = *self.setup.angles
            iplot, angles, cog[*,0], ytitle='Center of gravity', xtitle='Angle (degrees)', $
                   name='Measured', color=[0,0,255], identifier=id, /disable_splash_screen, /no_saveprompt
            iplot, angles, cog[*,1], /overplot,  $
                   name='Fit', color=[255,0,0], identifier=id
            diff = cog[*,0]-cog[*,1]
            iplot, angles, diff, ytitle='COG measured-fit', xtitle='Angle (degrees)', id=id, $
                   /disable_splash_screen, /no_saveprompt
        endif
    endif else begin
        ; Reconstruct entire file
        ; Delete any volume array to free up memory
        self->free_memory
        widget_control, self.widgets.base_file, get_value=base_file
        base_file = base_file[0]
        widget_control, self.widgets.abort, set_uvalue=0
        widget_control, self.widgets.status, set_value=""
        widget_control, self.widgets.rotation_center[0], get_value=center0
        widget_control, self.widgets.rotation_center[1], get_value=center1
        widget_control, self.widgets.recon_slice[0], get_value=slice0
        widget_control, self.widgets.recon_slice[1], get_value=slice1
        slice = float([slice0, slice1])
        cent = float([center0, center1])
        ; Compute the rotation center of the first and last slices
        coeffs = poly_fit(slice, cent, 1)
        center0 = coeffs[0]
        center1 = coeffs[0] + coeffs[1]*(self.ny-1)
        center = [center0, center1]
        angles = *self.setup.angles
        self.ptomo->reconstruct_volume, self.tomoParams, base_file, $
                              angles=angles, center=center, $
                              abort_widget=self.widgets.abort, $
                              status_widget=self.widgets.status
    endelse
end


pro tomo_display::optimize_rotation_center
    if (not ptr_valid(self.pvolume)) then begin
        t = dialog_message('Must read in volume file first.', /error)
        return
    endif
    widget_control, /hourglass
    widget_control, self.widgets.recon_slice[0], get_value=top_slice
    top_slice = top_slice < (self.ny-1)
    widget_control, self.widgets.recon_slice[1], get_value=bottom_slice
    bottom_slice = bottom_slice < (self.ny-1)

    self.set_tomo_params
    
    widget_control, self.widgets.rotation_optimize_range, get_value=range
    widget_control, self.widgets.rotation_optimize_step, get_value=step
    widget_control, self.widgets.rotation_optimize_center, get_value=center
    widget_control, self.widgets.rotation_optimize_method, get_value=method
    npoints = long(range/step) + 1
    centers = findgen(npoints)*step + (center-range/2.)
    
    proj0 = reform((*self.pvolume)[*,*,0])
    proj180 = reform((*self.pvolume)[*,*,self.setup.nz-1])
    if (method eq 0) then begin
      optimize_rotation_center, self.tomoParams, [top_slice, bottom_slice], *self.pvolume, centers, entropy
    endif else begin
      optimize_rotation_mirror, [top_slice, bottom_slice], proj0, proj180, centers, error
      entropy = error  ; Give variable the same name for plotting
    endelse
    t = min(entropy[*,0], min_pos1)
    t = min(entropy[*,1], min_pos2)
    center1 = centers[min_pos1]
    center2 = centers[min_pos2]
    if (method eq 1) then begin
      ; It appears that the center is 0.5 pixel larger than the center position used by gridrec
      center1 = center1 - 0.5
      center2 = center2 - 0.5
    endif
    widget_control, self.widgets.volume_file, get_value=file
    title = file + '   Slice=['+strtrim(string(top_slice),2)+','+strtrim(string(bottom_slice),2)+']'
    iplot, centers, entropy[*,0], xtitle='Rotation center', ytitle='Image entropy', sym_index=2, $
           view_title=title, id=id, /disable_splash_screen, /no_saveprompt
    entropy_diff = min(entropy[*,1]) - min(entropy[*,0])
    iplot, centers, entropy[*,1]-entropy_diff, sym_index=4, $
           view_title=title, overplot=id
    widget_control, self.widgets.rotation_center[0], set_value=center1
    widget_control, self.widgets.rotation_center[1], set_value=center2
    center = (center1 + center2)/2.
    widget_control, self.widgets.rotation_optimize_center, set_value=center
    ;measure_rotation_tilt, proj0, proj180, center, vertError
    self->reconstruct, 0
    self->reconstruct, 1
end


pro tomo_display::correct_rotation_tilt
  if (not ptr_valid(self.pvolume)) then begin
    t = dialog_message('Must read in volume file first.', /error)
    return
  endif
  widget_control, /hourglass
  widget_control, self.widgets.recon_slice[0], get_value=top_slice
  top_slice = top_slice < (self.ny-1)
  widget_control, self.widgets.recon_slice[1], get_value=bottom_slice
  bottom_slice = bottom_slice < (self.ny-1)
  widget_control, self.widgets.rotation_center[0], get_value=top_center
  widget_control, self.widgets.rotation_center[1], get_value=bottom_center
  angle = (top_center-bottom_center) / (bottom_slice - top_slice) / !dtor
  for i=0, self.nz-1 do begin
    proj = (*self.pvolume)[*,*,i]
    r = rot(proj, angle, cubic=-0.5)
    (*self.pvolume)[0,0,i] = r
    widget_control, self.widgets.status, $
      set_value='Correcting projection ' + strtrim(i+1, 2) + '/' + strtrim(self.nz, 2)
  endfor
  widget_control, self.widgets.status, set_value='Optimizing rotation center ...'
  self.optimize_rotation_center
  widget_control, self.widgets.volume_file, get_value=file
  widget_control, self.widgets.status, set_value='Saving volume file ...'
  write_tomo_volume, file, *self.pvolume
  widget_control, self.widgets.status, set_value=''
end


pro tomo_display::set_base_file, base_file
    widget_control, self.widgets.base_file, set_value=base_file
    widget_control, self.widgets.first_file, set_value=1
    widget_control, self.widgets.file_type, get_value=file_type
    files = file_search(base_file + '*' + file_type, count=count)
    
    ; if the number of found files of the given type equals 0, try looking for other types
    if(count eq 0) then begin
      file_type = '*.SPE'
      files = file_search(base_file + file_type, count=count)
      if (count ne 0) then widget_control, self.widgets.file_type, set_value = '.SPE'
    endif
    if(count eq 0) then begin
      file_type = '*.nc'
      files = file_search(base_file + file_type, count=count)
      if (count ne 0) then widget_control, self.widgets.file_type, set_value = '.nc'
    endif
    
    widget_control, self.widgets.last_file, set_value=count
    ; Read the setup file.  If there are non-zero values for the dark current
    ; or rotation axis then use them.
    s = self.ptomo->read_setup(base_file + '.setup')
    self.setup = self.ptomo->get_setup()
    if (self.setup.dark_current ne 0.) then begin
        widget_control, self.widgets.dark_current, $
                        set_value=self.setup.dark_current
    endif
    if (self.setup.center ne 0.) then begin
        center = self.setup.center
        widget_control, self.widgets.rotation_center[0], set_value=center
        widget_control, self.widgets.rotation_center[1], set_value=center
        widget_control, self.widgets.rotation_optimize_center, set_value=center
    endif
    
    ; look for a dark current file
    dc_file = file_search('Dark_Current_Measurement*',count = count)
    if (count ne 0) then begin
      cd, current = current
      widget_control, self.widgets.dc_filename, set_value = current + '\'+ dc_file[0]
    endif else widget_control, self.widgets.dc_filename, set_value = ''
    
end

pro tomo_display::set_directory
    cd, current=current
    widget_control, self.widgets.directory, set_value=current
    ; See if there are any .setup files in this directory
    ; If so, make the first one the default base file
    files = file_search('*.setup', count=count)
    if (count ge 1) then begin
        file = files[0]
        index = strpos(file, '.setup')
        file = strmid(file, 0, index)
        self->set_base_file, file
    endif
end

pro tomo_display::set_limits
    widget_control, self.widgets.direction, get_value=direction
    case direction of
        0: last_slice=self.nx-1
        1: last_slice=self.ny-1
        2: last_slice=self.nz-1
    endcase
    widget_control, self.widgets.last_slice, set_value=last_slice
    widget_control, self.widgets.disp_slice, set_value=last_slice/2
    widget_control, self.widgets.disp_slider, set_slider_max=last_slice
    widget_control, self.widgets.disp_slider, set_value=last_slice/2
    widget_control, self.widgets.recon_slice[0], set_value=self.ny*0.1
    widget_control, self.widgets.recon_slice[1], set_value=self.ny*0.9
    widget_control, self.widgets.filter_size, set_value=self.nx;;/4
end

pro tomo_display::free_memory
    ptr_free, self.pvolume
    widget_control, self.widgets.volume_file, set_value=''
end


; method for loading dark current data into program before preprocessing
pro tomo_display::measure_dc, dc_filename, dark = dark_current, file_type = file_type
    dark_old = dark_current
    spe = strpos(dc_filename,'.SPE')
    nc = strpos(dc_filename,'.nc')    
    if (spe ne -1) then begin
        if (file_type ne '.SPE') then begin
          dark_current = dark_old
          message, 'Wrong dark field file format'
        endif
        read_princeton, dc_filename, dc_data, header=header, comment=comment
        image_type = strmid(comment[1],5)
    endif else if (nc ne -1) then begin
        if (file_type ne '.nc') then begin
          dark_current = dark_old
          message, 'Wrong dark field file format'
        endif
        dc_data = read_nd_netcdf(dc_filename, attributes=comment)
        image_type = strmid((*(comment.pvalue)[8])[0],5)
    endif
    if (image_type ne 'DARK_FIELD') then begin ; if image is not the right type, return
        dark_current = dark_old
        return
    endif else if (image_type eq 'DARK_FIELD') then begin ; average together dark fields, return averaged value
        dark_current = total(dc_data, 3)/(size(dc_data,/dimensions))[2]
        return
    endif
end



pro tomo_options_event, event
    widget_control, event.top, get_uvalue=tomo_display
    tomo_display->options_event, event
end

pro tomo_display::options_event, event
    if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
        widget_control, self.widgets.options_base, map=0
        return
    endif
    case event.id of
        self.widgets.recon_method: begin
            sens = (event.value eq 2)
            widget_control, self.widgets.backproject_base, sensitive=sens
            widget_control, self.widgets.gridrec_base, sensitive=1-sens
            index = widget_info(self.widgets.backproject_method, /droplist_select)
            widget_control, self.widgets.backproject_radon_interpolation, sensitive = index
            widget_control, self.widgets.backproject_riemann_interpolation, sensitive = 1-index
        end
        self.widgets.white_average: begin
            ; Nothing to do
        end
        self.widgets.fluorescence: begin
            ; Nothing to do
        end
        self.widgets.display_sinogram: begin
            ; Nothing to do
        end
        self.widgets.plot_cog: begin
            ; Nothing to do
        end
        self.widgets.backproject_filter: begin
            ; Nothing to do
        end
        self.widgets.backproject_method: begin
             index = widget_info(self.widgets.backproject_method, /droplist_select)
             widget_control, self.widgets.backproject_radon_interpolation, sensitive = index
             widget_control, self.widgets.backproject_riemann_interpolation, sensitive = 1-index
        end        
        self.widgets.backproject_radon_interpolation: begin
            ; Nothing to do
        end
        self.widgets.backproject_riemann_interpolation: begin
            ; Nothing to do
        end
        self.widgets.gridrec_filter: begin
            ; Nothing to do
        end
        self.widgets.gridrec_sampl_parameter: begin
            ; Nothing to do
        end
        self.widgets.sino_padding: begin
            ; Nothing to do
        end
        self.widgets.paddingAverage: begin
            ; Nothing to do
        end
        else:  t = dialog_message('Unknown event')
    endcase
end

pro tomo_abort_event, event
    ; This procedure is called when an abort event is received.
    widget_control, event.id, set_uvalue=1
end


pro tomo_display_event, event
    widget_control, event.top, get_uvalue=tomo_display
    tomo_display->event, event
end

pro tomo_display::display_slice, new_window=new_window
    if (ptr_valid(self.pvolume)) then begin
        widget_control, self.widgets.disp_slice, get_value=slice
        widget_control, self.widgets.direction, get_value=direction
        widget_control, self.widgets.volume_file, get_value=file
        ; Set the axis dimensions
        if (self.setup.image_type eq 'RECONSTRUCTED') then begin
            xdist = findgen(self.nx)*self.setup.x_pixel_size
            ydist = findgen(self.ny)*self.setup.x_pixel_size
            zdist = findgen(self.nz)*self.setup.y_pixel_size
        endif else begin
            xdist = findgen(self.nx)*self.setup.x_pixel_size
            ydist = findgen(self.ny)*self.setup.y_pixel_size
            zdist = *self.setup.angles
        endelse
        case direction of
            0: begin
                slice = (slice > 0) < (self.nx-1)
                r = (*(self.pvolume))[slice, *, *]
                xdist = ydist
                ydist = zdist
                end
            1: begin
                slice = (slice > 0) < (self.ny-1)
                r = (*(self.pvolume))[*, slice, *]
                ydist = zdist
                end
            2: begin
                slice = (slice > 0) < (self.nz-1)
                r = (*(self.pvolume))[*, *, slice]
                end
        endcase
        axes = ['X', 'Y', 'Z']
        widget_control, self.widgets.rotation_center[0], get_value=center
        title = file + '    Center='+strtrim(string(center),2) + $
                    '     '+axes[direction]+'='+strtrim(string(slice),2)
        widget_control, self.widgets.auto_intensity, get_value=auto
        if (auto) then begin
            min=min(r, max=max)
        endif else begin
            widget_control, self.widgets.display_min, get_value=min
            widget_control, self.widgets.display_max, get_value=max
        endelse
        ; Change the size of the image before calling image_display
        self->rebin, r, xdist, ydist
        if (keyword_set(new_window)) or (obj_valid(self.image_display) eq 0) then begin
            self.image_display = obj_new('image_display', r, min=min, max=max, $
                                          title=title, xdist=xdist, ydist=ydist)
        endif else begin
            self.image_display->scale_image, r, min=min, max=max, $
                                          title=title, xdist=xdist, ydist=ydist, /leave_mouse
        endelse

    endif else begin
        t = dialog_message('Must read in volume file first.', /error)
    endelse
end

pro tomo_display::volume_render
    if (ptr_valid(self.pvolume)) then begin
        widget_control, self.widgets.display_min, get_value=min
        widget_control, self.widgets.display_max, get_value=max
        v = bytscl(*self.pvolume, min=min, max=max)
        volume_render, v
    endif else begin
        t = dialog_message('Must read in volume file first.', /error)
    endelse
end

pro tomo_display::event, event
    if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
        widget_control, self.widgets.options_base, /destroy
        widget_control, event.top, /destroy
        obj_destroy, self
        return
    endif

;    catch, err
;    if (err ne 0) then begin
;       t = dialog_message(!error_state.msg, /error)
;        widget_control, self.widgets.status, set_value=!error_state.msg
;        goto, end_event
;    endif

    case event.id of
        self.widgets.change_directory: begin
            f = dialog_pickfile(/directory, get_path=p)
            if (p ne "") then begin
                cd, p
                self->set_directory
            endif
        end

        self.widgets.read_volume_file: begin
            file = dialog_pickfile(filter='*.volume', get_path=path)
            if (file eq '') then break
            pos = strpos(file, path)
            if (pos ge 0) then begin
                pos = pos + strlen(path)
                file = strmid(file, pos)
            endif
            cd, path
            self->set_directory
            pos = strpos(file, 'recon.volume')
            if (pos ge 0) then begin
                base_file = strmid(file, 0, pos)
                self->set_base_file, base_file
            endif else begin
                pos = strpos(file, '.volume')
                if (pos ge 0) then begin
                    base_file = strmid(file, 0, pos)
                    self->set_base_file, base_file
                endif
            endelse
            ptr_free, self.pvolume
            widget_control, /hourglass
            widget_control, self.widgets.status, $
                            set_value='Reading volume file ...'
            vol = self.ptomo->read_volume(file)
            self.setup = self.ptomo->get_setup()
            widget_control, self.widgets.volume_type, $
                            set_value=self.setup.image_type
            self.pvolume = ptr_new(vol, /no_copy)
            dims = size(*self.pvolume, /dimensions)
            ; Set the volume filename and path
            widget_control, self.widgets.volume_file, set_value=file
            widget_control, self.widgets.directory, set_value=path
            ; Set the array dimensions
            widget_control, self.widgets.nx, set_value=dims[0]
            self.nx = dims[0]
            widget_control, self.widgets.ny, set_value=dims[1]
            self.ny = dims[1]
            widget_control, self.widgets.nz, set_value=dims[2]
            self.nz = dims[2]
            ; Set the intensity range
            min = min(*self.pvolume, max=max)
            widget_control, self.widgets.data_min, set_value=min
            widget_control, self.widgets.data_max, set_value=max
            ; Set the slice display range
            self->set_limits
            ; Build the angle array if it does not exist
            if (not ptr_valid(self.setup.angles)) then begin
                ; Assume evenly spaced angles 0 to 180-angle_step degrees
                self.setup.angles = ptr_new(findgen(self.nz)/(self.nz) * 180.)
            endif
            ; Set the pixel sizes to 1 if they are zero
            if (self.setup.x_pixel_size eq 0.) then self.setup.x_pixel_size=1.0
            if (self.setup.y_pixel_size eq 0.) then self.setup.y_pixel_size=1.0
            if (self.setup.z_pixel_size eq 0.) then self.setup.z_pixel_size=1.0
            ; Set the rotation center either from the .setup file or to nx/2.
            if (self.setup.center ne 0.) then begin
                center = self.setup.center
            endif else begin 
                center = self.nx/2.
            endelse
            widget_control, self.widgets.rotation_center[0], set_value=center
            widget_control, self.widgets.rotation_center[1], set_value=center
            widget_control, self.widgets.rotation_optimize_center, set_value=center
            widget_control, self.widgets.status, $
                            set_value='Done reading volume file ' + file
        end

        self.widgets.read_camera_file: begin
            file = dialog_pickfile(filter=['*.SPE','*.nc'], get_path=path)
            if (file eq '') then break
            widget_control, /hourglass
            widget_control, self.widgets.status, $
                            set_value='Reading camera file ...'
            if (strpos(file, '.SPE') ne -1) then begin
              read_princeton, file, vol
            endif else if (strpos(file, '.nc') ne -1) then begin
              vol = read_nd_netcdf(file)
            endif
            widget_control, self.widgets.status, $
                            set_value='Done reading camera file ' + file
            if (size(vol, /n_dimensions) eq 3) then begin
               ptr_free, self.pvolume
               widget_control, self.widgets.volume_type, $
                            set_value='RAW'
               self.pvolume = ptr_new(vol, /no_copy)
               dims = size(*self.pvolume, /dimensions)
               ; Set the array dimensions
               widget_control, self.widgets.nx, set_value=dims[0]
               self.nx = dims[0]
               widget_control, self.widgets.ny, set_value=dims[1]
               self.ny = dims[1]
               widget_control, self.widgets.nz, set_value=dims[2]
               self.nz = dims[2]
               ; Set the intensity range
               min = min(*self.pvolume, max=max)
               widget_control, self.widgets.data_min, set_value=min
               widget_control, self.widgets.data_max, set_value=max
               ; Set the slice display range
                self->set_limits
               ; Build the angle array if it does not exist
               if (not ptr_valid(self.setup.angles)) then begin
                   ; Assume evenly spaced angles 0 to 180-angle_step degrees
                   self.setup.angles = ptr_new(findgen(self.nz)/(self.nz) * 180.)
               endif
               ; Set the pixel sizes to 1 if they are zero
               if (self.setup.x_pixel_size eq 0.) then self.setup.x_pixel_size=1.0
               if (self.setup.y_pixel_size eq 0.) then self.setup.y_pixel_size=1.0
               if (self.setup.z_pixel_size eq 0.) then self.setup.z_pixel_size=1.0
            endif else begin
               image_display, vol
            endelse
        end

        self.widgets.free_memory: begin
            self->free_memory
        end

        self.widgets.exit: begin
            widget_control, event.top, /destroy
            obj_destroy, self
            return
        end

        self.widgets.processing_options: begin
            widget_control, self.widgets.options_base, map=1
        end

        self.widgets.base_file: begin
            widget_control, self.widgets.base_file, get_value=base_file
            self->set_base_file, base_file[0]
        end

        self.widgets.file_type: begin
            widget_control, self.widgets.base_file, get_value=base_file
            self->set_base_file, base_file[0]
        end

        self.widgets.flip_data: begin
            ; Nothing to be doe
        end
        
        self.widgets.preprocess_go: begin
            ; Delete any volume array to free up memory
            self->free_memory
            widget_control, self.widgets.base_file, get_value=base_file
            base_file = base_file[0]
            widget_control, self.widgets.file_type, get_value=file_type
            file_type = file_type[0]
            widget_control, self.widgets.first_file, get_value=first_file
            widget_control, self.widgets.last_file, get_value=last_file
            widget_control, self.widgets.dark_current, get_value=dark_current
            widget_control, self.widgets.threshold, get_value=threshold
            widget_control, self.widgets.double_threshold, get_value=double_threshold
            widget_control, self.widgets.white_average, get_value=white_average
            widget_control, self.widgets.white_smooth, get_value=white_smooth
            widget_control, self.widgets.flip_data, get_value = flip_data
            widget_control, self.widgets.abort, set_uvalue=0
            widget_control, self.widgets.status, set_value=""
            ; dark current file loading
            widget_control, self.widgets.dc_filename, get_value = dc_filename
              dc_filename = dc_filename[0]
              if (dc_filename ne '') then begin
                self->measure_dc, dc_filename, dark = dark_current, file_type = file_type
              endif
            self.ptomo->preprocess, base_file, file_type, first_file, last_file, $
                            dark=dark_current, $
                            threshold=threshold, double_threshold=double_threshold, $
                            white_average=white_average, white_smooth=white_smooth, $
                            abort_widget=self.widgets.abort, $
                            status_widget=self.widgets.status,$
                            flip_data = flip_data
        end

        self.widgets.dc_search : begin
            dc_filename = dialog_pickfile()
            widget_control, self.widgets.dc_filename, set_value = dc_filename
        end

        self.widgets.reconstruct_slice[0]: begin
            self->reconstruct, 0
        end

        self.widgets.reconstruct_slice[1]: begin
            self->reconstruct, 1
        end

        self.widgets.rotation_optimize_method: begin
            ; Nothing to do
        end

        self.widgets.rotation_optimize: begin
          self->optimize_rotation_center
        end

        self.widgets.reconstruct_all: begin
            self->reconstruct
        end

        self.widgets.correct_rotation_tilt: begin
          self->correct_rotation_tilt
        end

        self.widgets.direction: begin
            self->set_limits
        end

        self.widgets.order: begin
            widget_control, self.widgets.order, get_value=order
            !order=order
        end

        self.widgets.auto_intensity: begin
            ; Nothing to do
        end

        self.widgets.disp_slice: begin
            widget_control, self.widgets.disp_slider, set_value=event.value
            self->display_slice
        end

        self.widgets.disp_slider: begin
            widget_control, self.widgets.disp_slice, set_value=event.value
            self->display_slice
        end

        self.widgets.display_slice: begin
            self->display_slice, /new_window
        end

        self.widgets.volume_render: begin
            self->volume_render
        end

        self.widgets.movie_output: begin
            ; Nothing to do
        end

        self.widgets.tiff_scale: begin
          ; Nothing to do
        end

        self.widgets.zoom: begin
            ; Nothing to do
        end

        self.widgets.make_movie: begin
            widget_control, self.widgets.disp_slice, get_value=slice
            if (ptr_valid(self.pvolume)) then begin
                widget_control, self.widgets.auto_intensity, get_value=auto
                if (auto) then begin
                    widget_control, self.widgets.data_min, get_value=min
                    widget_control, self.widgets.data_max, get_value=max
                endif else begin
                    widget_control, self.widgets.display_min, get_value=min
                    widget_control, self.widgets.display_max, get_value=max
                endelse
                widget_control, self.widgets.movie_output, get_value=output
                widget_control, self.widgets.direction, get_value=direction
                widget_control, self.widgets.movie_file, get_value=file
                widget_control, self.widgets.zoom, get_value=zoom, get_uvalue=all_zooms
                widget_control, self.widgets.first_slice, get_value=start
                widget_control, self.widgets.last_slice, get_value=stop
                widget_control, self.widgets.slice_step, get_value=step
                widget_control, self.widgets.movie_wait, get_value=wait
                widget_control, self.widgets.movie_fps, get_value=fps
                widget_control, self.widgets.movie_bps, get_value=bps
                widget_control, self.widgets.tiff_scale, get_value=unscaled_tiff
                scale = all_zooms[zoom]
                label=0
                case output of
                    0: label=1
                    1: widget_control, self.widgets.movie_file, get_value=jpeg_file
                    2: widget_control, self.widgets.movie_file, get_value=tiff_file
                    3: widget_control, self.widgets.movie_file, get_value=mp4_file
                endcase
                widget_control, self.widgets.abort, set_uvalue=0
                widget_control, self.widgets.status, set_value=""
                make_movie, index=direction+1, scale=scale, *self.pvolume, $
                            jpeg_file=jpeg_file, tiff_file=tiff_file, mp4_file=mp4_file, $
                            min=min, max=max, start=start, stop=stop, step=step, wait=wait, $
                            unscaled_tiff=unscaled_tiff, fps=fps, bps=bps, $
                            label=label, abort_widget=self.widgets.abort, /color, $
                            status_widget=self.widgets.status
            endif else begin
                t = dialog_message('Must read in volume file first.', /error)
            endelse
        end

        else:  t = dialog_message('Unknown event')
    endcase

end_event:
    ; If there is a valid volume array make the visualize base sensitive
    sensitive = ptr_valid(self.pvolume)
    widget_control, self.widgets.visualize_base, sensitive=sensitive

end


function tomo_display::init
;+
; NAME:
;       TOMO_DISPLAY::INIT
;
; PURPOSE:
;       This function initializes an object of class TOMO_DISPLAY.  It is
;       not called directly, but is called indirectly when a new object of
;       class TOMO_DISPLAY is created via OBJ_NEW('TOMO_DISPLAY')
;
;       The TOMO_DISPLAY object is a GUI display which provides control
;       for preprocessing, reconstructing and visualizing tomographic data
;
; CATEGORY:
;       Imaging
;
; CALLING SEQUENCE:
;       obj = OBJ_NEW('TOMO_DISPLAY')
;
; EXAMPLE:
;       IDL> obj = OBJ_NEW('TOMO_DISPLAY')
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers (16-Nov-2001)
;       June 4, 2002    MLR  Made the display slice entry and slider display an
;                            image in an existing window if it exists.
;       Jan. 9, 2004    MLR  Added TIFF output for make_movie
;
;-

    self.fonts.normal = get_font_name(/helvetica)
    self.fonts.heading1 = get_font_name(/large, /bold)
    self.fonts.heading2 = get_font_name(/bold)

    self.widgets.base= widget_base(column=1, /tlb_kill_request_events, $
                                   title='IDL Tomography Processing', mbar=mbar)

    file = widget_button(mbar, /menu, value = 'File')
    self.widgets.change_directory = widget_button(file, $
                                            value = 'Change directory ...')
    self.widgets.read_volume_file = widget_button(file, $
                                            value = 'Read volume file ...')
    self.widgets.read_camera_file = widget_button(file, $
                                            value = 'Read camera file ...')
    self.widgets.free_memory = widget_button(file, value='Free volume array')
    self.widgets.exit = widget_button(file, $
                                            value = 'Exit')
    options = widget_button(mbar, /menu, value = 'Options')
    self.widgets.processing_options = widget_button(options, $
                                            value = 'Processing options ...')

    whole_display = widget_base(self.widgets.base, /row)
    left_column = widget_base(whole_display, /column)
    col = widget_base(left_column, /column, /frame)
    self.widgets.main_base = col
    t = widget_label(col, value='File/Status', font=self.fonts.heading1)
    self.widgets.base_file = cw_field(col, title="Base file name:", $
                                        xsize=25, /return_events)
    self.widgets.file_type = cw_field(col, title="Base file type:", $
                                        xsize=25, /return_events)
    widget_control, self.widgets.file_type, set_value = '.SPE'
    self.widgets.directory = cw_field(col, title="Working directory:", $
                                        xsize=50, /noedit)
    self.widgets.volume_file = cw_field(col, title="Volume file name:", $
                                        xsize=50, /noedit)
    row = widget_base(col, /row)
    self.widgets.status = cw_field(row, title="Status:", $
                                        xsize=50, /noedit, $
                                        fieldfont=self.fonts.heading2)
    self.widgets.abort = widget_button(row, value='Abort', $
                                       event_pro='tomo_abort_event')


    ; Preprocessing
    col = widget_base(left_column, /column, /frame)
    self.widgets.preprocess_base = col
    t = widget_label(col, value='Preprocess', font=self.fonts.heading1)
    row = widget_base(col, /row, /base_align_bottom)
    self.widgets.first_file = cw_field(row, /column, title='First file', $
                                      /integer, xsize=10, value=1)
    self.widgets.last_file = cw_field(row, /column, title='Last file', $
                                      /integer, xsize=10, value=1)
    self.widgets.dark_current = cw_field(row, /column, title='Dark current', $
                                      /integer, xsize=10, value=100)
    self.widgets.flip_data = cw_bgroup(row, /column, ['Transpose Data'], /nonexclusive, $
                                       set_value = 0)
    col1 = widget_base(row, /column)
    self.widgets.preprocess_go = widget_button(col1, value=' Preprocess ')
    row = widget_base(col, /row, /base_align_bottom)
    self.widgets.dc_filename = cw_field(row, title="Dark Field file name:", $
                                        xsize=25)
    self.widgets.dc_search = widget_button(row, value = ' Search for Dark Field file ')


    ; Reconstruction
    col = widget_base(left_column, /column, /frame)
    self.widgets.reconstruct_base = col
    t = widget_label(col, value='Reconstruct', font=self.fonts.heading1)
    for i=0, 1 do begin
        row = widget_base(col, /row, /base_align_bottom)
        if (i eq 0) then label='Upper slice:' else label='Lower slice:'
        t = widget_label(row, value=label)
        self.widgets.recon_slice[i] = cw_field(row, /column, title='Slice', $
                                          /integer, xsize=10, value=100)
        self.widgets.rotation_center[i] = cw_field(row, /column, $
                                                title='Rotation Center', /float, $
                                                xsize=10, value=325.000)
        self.widgets.reconstruct_slice[i] = widget_button(row, value='Reconstruct slice')
    endfor
    row = widget_base(col, /row, /base_align_bottom)
    t = widget_label(row, value='Optimize rotation center:')
    self.widgets.rotation_optimize_center = cw_field(row, /column, $
                                                title='Rotation Center', /float, $
                                                xsize=10, value=325.000)
    self.widgets.rotation_optimize_range = cw_field(row, /column, title='Optimize range', $
                                  /float, xsize=10, value=6)
    self.widgets.rotation_optimize_step = cw_field(row, /column, title='Optimize step', $
                                  /float, xsize=10, value=.25)
    self.widgets.rotation_optimize = widget_button(row, value='Optimize center')
    row = widget_base(col, /row, /base_align_bottom)
    self.widgets.rotation_optimize_method = cw_bgroup(row, ['Entropy', '0-180'], $
      label_left='Optimize center method:', $
      row=1, set_value=0, /exclusive)
    row = widget_base(col, /row, /base_align_bottom)
    t = widget_label(row, value='Correct rotation tilt:')
    self.widgets.correct_rotation_tilt = widget_button(row, value='Correct rotation tilt')
    row = widget_base(col, /row, /base_align_bottom)
    t = widget_label(row, value='Reconstruct all:')
    self.widgets.reconstruct_all = widget_button(row, $
                                                   value='Reconstruct all')

    ; Visualization
    right_column = widget_base(whole_display, /column)
    self.widgets.visualize_base = right_column
    widget_control, right_column, sensitive=0
    col = widget_base(right_column, /column, /frame)
    t = widget_label(col, value='Visualize', font=self.fonts.heading1)

    row = widget_base(col, /row)
    t = widget_label(row, value='Volume array:')
    self.widgets.nx = cw_field(row, title='NX', /integer, /noedit, /column, $
                               xsize=8, value=0)
    self.widgets.ny = cw_field(row, title='NY', /integer, /noedit, /column, $
                               xsize=8, value=0)
    self.widgets.nz = cw_field(row, title='NZ', /integer, /noedit, /column, $
                               xsize=8, value=0)
    self.widgets.volume_type = cw_field(row, title='Type', /noedit, /column, $
                               xsize=18, value='')

    row = widget_base(col, /row, /base_align_center)
    t = widget_label(row, value='Actual intensity range:')
    self.widgets.data_min = cw_field(row, title='Min.', /float, $
                                        /column, xsize=10, value=0, /noedit)
    self.widgets.data_max = cw_field(row, title='Max.', /float, $
                                        /column, xsize=10, value=0, /noedit)
    row = widget_base(col, /row, /base_align_center)
    t = widget_label(row, value='Display intensity range:')
    self.widgets.display_min = cw_field(row, title='Min.', /float, $
                                        /column, xsize=10, value=0)
    self.widgets.display_max = cw_field(row, title='Max.', /float, $
                                        /column, xsize=10, value=5000)
    self.widgets.auto_intensity = cw_bgroup(row, ['Manual', 'Auto'], $
                                             row=1, set_value=1, /exclusive)


    row = widget_base(col, /row)
    self.widgets.direction = cw_bgroup(row, ['X', 'Y', 'Z'], $
                                            label_left='Direction:', $
                                            row=1, set_value=2, /exclusive)
    !order=1
    self.widgets.order = cw_bgroup(row, ['Bottom to top', 'Top to bottom'], $
                                            label_left='Order:', row=1, $
                                            set_value=1, /exclusive)

    row = widget_base(col, /row)
    self.widgets.zoom = cw_bgroup(row, ['1/4', '1/2', '1', '2', '4'], $
                                    label_left='Zoom:', row=1, $
                                    set_value=2, /exclusive, $
                                    uvalue=[-4, -2, 1, 2, 4])

    row = widget_base(col, /row)
    t = widget_label(row, value='Display slice:')
    col1=widget_base(row, /column)
    self.widgets.disp_slice = cw_field(col1, /integer, title='',$
                                       xsize=10, value=100, /return_events)
    self.widgets.disp_slider = widget_slider(col1, value=100, min=0, max=100, $
                                             /suppress_value)
    col1 = widget_base(row, /column, /align_center)
    self.widgets.display_slice = widget_button(col1, value='Display slice')

    row = widget_base(col, /row)
    t = widget_label(row, value='Volume render:')
    self.widgets.volume_render = widget_button(row, value='Volume render')

    ; Movies
    col = widget_base(right_column, /column, /frame)
    t = widget_label(col, value='Movies', font=self.fonts.heading1)

    row = widget_base(col, /row)
    self.widgets.movie_output = cw_bgroup(row, ['Screen', 'JPEG', 'TIFF', 'MP4'], $
                                            label_left='Output:', row=1, $
                                            set_value=0, /exclusive)
    col1 = widget_base(row, /column, /align_center)
    self.widgets.make_movie = widget_button(col1, value='Make movie')

    row = widget_base(col, /row)
    self.widgets.tiff_scale = cw_bgroup(row, ['Scaled (8-bit)', 'Unscaled'], $
      label_left='TIFF scaling:', row=1, $
      set_value=0, /exclusive)

    row = widget_base(col, /row)
    self.widgets.movie_fps = cw_field(row, title='MP4 frames/s', /float, $
      /column, xsize=10, value=30)
    self.widgets.movie_bps = cw_field(row, title='MP4 bits/s', /float, $
      /column, xsize=10, value=3e4)

    row = widget_base(col, /row)
    self.widgets.first_slice = cw_field(row, title='First slice', /integer, $
                                        /column, xsize=10, value=0)
    self.widgets.last_slice = cw_field(row, title='Last slice', /integer, $
                                        /column, xsize=10, value=0)
    self.widgets.slice_step = cw_field(row, title='Step', /integer, $
                                        /column, xsize=10, value=1)
    self.widgets.movie_wait = cw_field(row, title='Delay time', /float, $
                                        /column, xsize=10, value=0.)

    row = widget_base(col, /row)
    self.widgets.movie_file = cw_field(row, title="JPEG/TIFF/MP4 file name:", $
                                        xsize=40)

    self.ptomo = obj_new('tomo')

    widget_control, self.widgets.base, set_uvalue=self
    ; Make all of the base widgets the same size so they line up nicely
    g = widget_info(self.widgets.main_base, /geometry)
    widget_control, self.widgets.preprocess_base, xsize=g.xsize
    widget_control, self.widgets.reconstruct_base, xsize=g.xsize
    widget_control, self.widgets.visualize_base, xsize=g.xsize
    widget_control, self.widgets.base, /realize

    ; The "options" screen.  Normally not visible
    self.widgets.options_base= widget_base(row=1, /tlb_kill_request_events, $
                                   title='IDL Tomography Processing Options')

    c1 = widget_base(self.widgets.options_base, /column)
    c2 = widget_base(self.widgets.options_base, /column)
    col = widget_base(c1, /column, /frame)
    preprocess_base=col
    t = widget_label(col, value='Preprocessing', font=self.fonts.heading1)
    col1 = col
    row = widget_base(col1, /row)
    t = widget_label(row, value='Zinger thresholds')
    self.widgets.threshold = cw_field(row, title='Normal frames', /float, $
                                        /column, xsize=10, value=1.25)
    self.widgets.double_threshold = cw_field(row, title='Double correlation (flat fields)', /float, $
                                        /column, xsize=10, value=1.05)
    row = widget_base(col1, /row)
    self.widgets.white_average = cw_bgroup(row, ['Interpolate', 'Average'], $
                                           label_left='Flat field processing:', row=1, $
                                           set_value=1, /exclusive)
    self.widgets.white_smooth  = cw_field(row, title='Flat field smoothing', /integer, $
                                        /column, xsize=10, value=0)
    col = widget_base(c1, /column, /frame)
    t = widget_label(col, value='Sinogram', font=self.fonts.heading1)
    col1 = col
    sinogram_base = col1
    row = widget_base(col1, /row)
    self.widgets.airPixels = cw_field(row, title='Air pixels (0=no air correction)', /integer, $
                                        /column, xsize=10, value=10)
    self.widgets.fluorescence = cw_bgroup(row, ['Absorption', 'Fluorescence'], $
                                            label_top='Data type', row=1, $
                                            set_value=0, /exclusive)
    row = widget_base(col1, /row)
    self.widgets.display_sinogram = cw_bgroup(row, ['No', 'Yes'], $
                                            label_left='Display sinogram', row=1, $
                                            set_value=0, /exclusive)
    row = widget_base(col1, /row)
    self.widgets.plot_cog = cw_bgroup(row, ['No', 'Yes'], $
                                            label_left='Plot center-of-gravity', row=1, $
                                            set_value=0, /exclusive)
    col = widget_base(c2, /column, /frame)
    recon_base=col
    t = widget_label(col, value='Reconstruction', font=self.fonts.heading1)
    row = widget_base(col, /row)
    self.widgets.recon_method = cw_bgroup(row, ['tomoRecon', 'Gridrec', 'Backproject'], $
                                            label_left='Reconstruction method:', row=1, $
                                            set_value=0, /exclusive)
    row = widget_base(col, /row)
    self.widgets.recon_scale = cw_field(row, /row, title='Scale factor', $
                                            /float, xsize=15, value=1e6)
    row = widget_base(col, /row)
    self.widgets.ringWidth = cw_field(row, title='Ring smoothing width (0=None)', /integer, $
                                        /row, xsize=10, value=9)
    col1 = widget_base(col, /column, /frame)
    self.widgets.backproject_base = col1
    widget_control, self.widgets.backproject_base, sensitive=0
    t = widget_label(col1, value='Backproject', font=self.fonts.heading2)
    row = widget_base(col1, /row)
    t = widget_label(row, value='Filter: ', font=self.fonts.heading2)
    self.widgets.filter_size = cw_field(row, title='Size', /integer, $
                                        /row, xsize=10, value=100)
    choices=['Gen_Hamming', 'Shepp_Logan', 'LP_Cosine', 'Ramlak', 'None']
    self.widgets.backproject_filter = widget_droplist(row, value=choices, $
                                                      uvalue=choices, /align_center)
    widget_control, self.widgets.backproject_filter, set_droplist_select=1
    row = widget_base(col1, /row)
    t = widget_label(row, value='Backprojection Method: ', font=self.fonts.heading2)
    self.widgets.backproject_method = widget_droplist(row, value = ['Riemann', 'Radon'], $
                                                      uvalue=['riemann', 'radon'], /align_center)
    widget_control, self.widgets.backproject_method, set_droplist_select = 0
    row = widget_base(col1, /row)
    self.widgets.backproject_riemann_interpolation = cw_bgroup(row, $
                                ['None', 'Bilinear', 'Cubic'], $
                                label_left='Riemann Interpolation', row=1, $
                                set_value=0, /exclusive)
    row = widget_base(col1, /row)
    self.widgets.backproject_radon_interpolation = cw_bgroup(row, $
                                ['None', 'Linear'], $
                                label_left='Radon Interpolation', row=1, $
                                set_value=0, /exclusive)                        
    widget_control, self.widgets.backproject_radon_interpolation, sensitive = 0  
    col1 = widget_base(col, /column, /frame)
    self.widgets.gridrec_base = col1
    t = widget_label(col1, value='Gridrec/tomoRecon', font=self.fonts.heading2)
    choices=['Shepp-Logan', 'Hann', 'Hamming', 'Ramlak']
    uval_choices=['shepp', 'hann', 'hamming', 'ramlak']
    row = widget_base(col1, /row)
    t = widget_label(row, value='Filter: ')
    self.widgets.gridrec_filter = widget_droplist(row, value=choices, $
                                                  uvalue=uval_choices, /align_center)
    ; Make the Hann filter be the default
    widget_control, self.widgets.gridrec_filter, set_droplist_select=1
    row = widget_base(col1, /row)
    t = widget_label(row, value='Sample Parameter: ')
    self.widgets.gridrec_sampl_parameter = cw_field(row, title='', $
                                        /row, xsize=10, value=1)    
    row = widget_base(col1, /row)
    t = widget_label(row, value = 'Padded Sinogram Width:')
    choices = ['Auto', 'No Padding', '1024','2048', '4096']
    uval_choices = [0, 1, 1024, 2048, 4096]
    self.widgets.sino_padding = widget_droplist(row, value = choices, $
                                                uvalue = uval_choices, /align_center)
    ; Make auto padding the default
    widget_control, self.widgets.sino_padding, set_droplist_select = 0

    row = widget_base(col1, /row)
    self.widgets.paddingAverage = cw_field(row, title='Pixels to average for padding (0=pad with 0.0)', /integer, $
                                        /column, xsize=10, value=10)
    row = widget_base(col1, /row)
    t = widget_label(col1, value='tomoRecon', font=self.fonts.heading2)
    row = widget_base(col1, /row)
    self.widgets.numThreads = cw_field(row, title='Number of of threads', /integer, $
                                        /row, xsize=10, value=8)
 ;
    row = widget_base(col1, /row)
    self.widgets.slicesPerChunk = cw_field(row, title='Slices per chunk', /integer, $
                                        /row, xsize=10, value=128)
                                       
    ; Make all of the base widgets the same size so they line up nicely
    g = widget_info(c1, /geometry)
    widget_control, preprocess_base, xsize=g.xsize
    ;widget_control, zinger_base, xsize=g.xsize
    widget_control, sinogram_base, xsize=g.xsize
    g = widget_info(c2, /geometry)
    widget_control, self.widgets.backproject_base, xsize=g.xsize
    widget_control, self.widgets.gridrec_base, xsize=g.xsize

    widget_control, self.widgets.options_base, /realize, map=0
    widget_control, self.widgets.options_base, set_uvalue=self

    self->set_directory

    xmanager, 'tomo_display', self.widgets.base, /no_block
    xmanager, 'tomo_options', self.widgets.options_base, /no_block
    return, 1
end

pro tomo_display::cleanup
    ptr_free, self.pvolume
end

pro tomo_display__define

    widgets={ tomo_display_widgets, $
        base: 0L, $
        change_directory: 0L, $
        read_volume_file: 0L, $
        read_camera_file: 0L, $
        free_memory: 0L, $
        exit: 0L, $
        processing_options: 0L, $
        main_base: 0L, $
        base_file: 0L, $
        file_type: 0L, $
        directory: 0L, $
        volume_file: 0L, $
        status: 0L, $
        abort: 0L, $
        preprocess_base: 0L, $
        first_file: 0L, $
        last_file: 0L, $
        dark_current: 0L, $
        preprocess_go: 0L, $
        dc_filename: 0L, $
        dc_search: 0L, $
        reconstruct_base: 0L, $
        recon_slice: lonarr(2), $
        rotation_center: lonarr(2), $
        reconstruct_slice: lonarr(2), $
        rotation_optimize_center: 0L, $
        rotation_optimize_range: 0L, $
        rotation_optimize_step: 0L, $
        correct_rotation_tilt: 0L, $
        rotation_optimize_method: 0L, $
        rotation_optimize: 0L, $
        reconstruct_all: 0L, $
        visualize_base: 0L, $
        nx: 0L, $
        ny: 0L, $
        nz: 0L, $
        volume_type: 0L, $
        direction: 0L, $
        order: 0L, $
        data_min: 0L, $
        data_max: 0L, $
        display_min: 0L, $
        display_max: 0L, $
        auto_intensity: 0L, $
        disp_slice: 0L, $
        disp_slider: 0L, $
        display_slice: 0L, $
        volume_render: 0L, $
        movie_output: 0L, $
        first_slice: 0L, $
        last_slice: 0L, $
        slice_step: 0L, $
        movie_wait: 0L, $
        movie_fps: 0L, $
        movie_bps: 0L, $
        tiff_scale: 0L, $
        zoom: 0L, $
        movie_file: 0L, $
        make_movie: 0L, $
        flip_data: 0L, $

        ; These widgets are in the "options" page
        options_base: 0L, $
        threshold: 0L, $
        double_threshold: 0L, $
        recon_method: 0L, $
        recon_scale: 0L, $
        ringWidth: 0L, $
        airPixels: 0L, $
        fluorescence: 0L, $
        display_sinogram: 0L, $
        plot_cog: 0L, $
        backproject_base: 0L, $
        filter_size: 0L, $
        backproject_filter: 0L, $
        backproject_riemann_interpolation: 0L, $
        backproject_radon_interpolation: 0L, $
        backproject_method: 0L, $
        white_average: 0L, $
        white_smooth: 0L, $
        gridrec_base: 0L, $
        gridrec_filter: 0L, $
        sino_padding: 0L, $
        paddingAverage: 0L, $
        gridrec_sampl_parameter: 0L, $
        numThreads: 0L, $
        slicesPerChunk: 0L $
    }

    fonts = {tomo_fonts, $
        normal: '', $
        heading1: '', $
        heading2: '' $
    }

    tomo_display = {tomo_display, $
        widgets: widgets, $
        pvolume: ptr_new(), $
        ptomo: obj_new(), $
        setup: {tomo}, $
        tomoParams: {tomo_params}, $
        nx: 0, $
        ny: 0, $
        nz: 0, $
        image_display: obj_new(), $
        fonts: fonts $
    }
end
