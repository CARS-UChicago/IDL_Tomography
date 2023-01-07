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
    widget_control, self.widgets.reconThreads, get_value=threads
    widget_control, self.widgets.slicesPerChunk, get_value=slicesPerChunk
 
    self.tomoObj->set_recon_params, $
            reconScale = reconScale, $
            paddedSinogramWidth=paddedSinogramWidth, $
            paddingAverage=paddingAverage, $
            airPixels = airPixels, $
            ringWidth = ringWidth, $
            fluorescence = fluorescence, $
            reconMethod = reconMethod, $
            threads = threads, $
            slicesPerChunk = slicesPerChunk, $
            ;debug = 0, $            ; Make a widget for this!
            ;dbgFile = dbgFile, $    ; Make a widget for this!
            ;pswfParam=6., $         ; Make a widget for this?
            sampl=sampl, $
            ;maxPixSize=1.0, $ ; Make a widget for this?
            ;ROI=1.0, $       ; Make a widget for this?
            GR_filterName=GR_filterName, $
            BP_method = BP_method, $
            BP_filterName = BP_filterName, $
            BP_filterSize = BP_filterSize, $
            RadonInterpolation = RadonInterpolation, $
            RiemannInterpolation = RiemannInterpolation
    self->update_tomo_struct
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
    if (not ptr_valid(self.tomoStruct.pVolume)) then begin
        t = dialog_message('Must read in file first.', /error)
        return
    endif
    widget_control, /hourglass
    
    self->set_tomo_params

    if (n_elements(islice) ne 0) then begin
        widget_control, self.widgets.rotation_center[islice], get_value=center
        widget_control, self.widgets.recon_slice[islice], get_value=slice
        slice = slice < (self.tomoStruct.ny-1)
        angles=*self.tomoStruct.pAngles
        r = self.tomoObj->reconstruct_slice((*self.tomoStruct.pVolume)[*, slice, *], $
                                             center=center, sinogram=sinogram, cog=cog)
        ; If reconstruction was with backproject, rotate image so it is the same
        ; orientation as with gridrec
        if (self.tomoStruct.reconMethod eq self.tomoStruct.reconMethodBackproject) then r = rotate(r, 4)
        widget_control, self.widgets.auto_intensity, get_value=auto
        if (auto) then begin
            min=min(r, max=max)
        endif else begin
            widget_control, self.widgets.display_min, get_value=min
            widget_control, self.widgets.display_max, get_value=max
        endelse
        dims = size(r, /dimensions)
        xdist = findgen(dims[0])*self.tomoStruct.xPixelSize
        ydist = xdist
        ; Change the size of the image before calling image_display
        self->rebin, r, xdist, ydist
        widget_control, self.widgets.input_file, get_value=file
        dims = size(r, /dimensions)
        xdist = findgen(dims[0])*self.tomoStruct.xPixelSize

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
            angles = *self.tomoStruct.pAngles
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
        widget_control, self.widgets.abort, set_uvalue=0
        widget_control, self.widgets.status, set_value=""
        widget_control, self.widgets.recon_data_type, get_value=index, get_uvalue=data_types
        data_type = data_types[index]
        widget_control, self.widgets.recon_write_output, get_value=write_output
        widget_control, self.widgets.recon_file_format, get_value=file_format
        netcdf = file_format eq 1
        widget_control, self.widgets.recon_slice[0], get_value=top_slice
        widget_control, self.widgets.recon_slice[1], get_value=bottom_slice
        widget_control, self.widgets.rotation_center[0], get_value=top_center
        widget_control, self.widgets.rotation_center[1], get_value=bottom_center
        self.tomoObj->set_rotation, top_slice, top_center, bottom_slice, bottom_center
        self.tomoObj->reconstruct_volume, data_type=data_type, write_output=write_output, netcdf=netcdf
        self->update_tomo_struct
        self->update_volume_widgets
    endelse
end

pro tomo_display::update_tomo_struct
  self.tomoStruct = self.tomoObj->get_struct()
end

pro tomo_display::update_file_widgets
  widget_control, self.widgets.input_file, set_value=self.tomoStruct.inputFilename
  widget_control, self.widgets.base_file, set_value=self.tomoStruct.baseFilename
  widget_control, self.widgets.directory, set_value=self.tomoStruct.directory
end

pro tomo_display::update_volume_widgets
    widget_control, self.widgets.volume_type, set_value=self.tomoStruct.imageType
    widget_control, self.widgets.nx, set_value=self.tomoStruct.nx
    widget_control, self.widgets.ny, set_value=self.tomoStruct.ny
    widget_control, self.widgets.nz, set_value=self.tomoStruct.nz
    ; Set the intensity range
    min = min(*self.tomoStruct.pVolume, max=max)
    widget_control, self.widgets.data_min, set_value=min
    widget_control, self.widgets.data_max, set_value=max
    ; Set the slice display range
    self->set_limits
end

pro tomo_display::set_limits
  widget_control, self.widgets.direction, get_value=direction
  case direction of
    0: last_slice=self.tomoStruct.nx-1
    1: last_slice=self.tomoStruct.ny-1
    2: last_slice=self.tomoStruct.nz-1
  endcase
  widget_control, self.widgets.last_slice, set_value=last_slice
  widget_control, self.widgets.disp_slice, set_value=last_slice/2
  widget_control, self.widgets.disp_slider, set_slider_max=last_slice
  widget_control, self.widgets.disp_slider, set_value=last_slice/2
  widget_control, self.widgets.recon_slice[0], set_value=self.tomoStruct.upperSlice
  widget_control, self.widgets.recon_slice[1], set_value=self.tomoStruct.lowerSlice
  center1 = self.tomoStruct.rotationCenter + self.tomoStruct.upperSlice*self.tomoStruct.rotationCenterSlope
  center2 = self.tomoStruct.rotationCenter + self.tomoStruct.lowerSlice*self.tomoStruct.rotationCenterSlope
  widget_control, self.widgets.rotation_center[0], set_value=center1
  widget_control, self.widgets.rotation_center[1], set_value=center2
  center = round((center1 + center2)/2.)
  widget_control, self.widgets.rotation_optimize_center, set_value=center
  widget_control, self.widgets.filter_size, set_value=self.tomoStruct.nx
end


pro tomo_display::optimize_rotation_center
    widget_control, /hourglass
    widget_control, self.widgets.recon_slice[0], get_value=top_slice
    top_slice = top_slice < (self.tomoStruct.ny-1)
    widget_control, self.widgets.recon_slice[1], get_value=bottom_slice
    bottom_slice = bottom_slice < (self.tomoStruct.ny-1)

    self->set_tomo_params
    
    widget_control, self.widgets.rotation_optimize_range, get_value=range
    widget_control, self.widgets.rotation_optimize_step, get_value=step
    widget_control, self.widgets.rotation_optimize_center, get_value=centers
    widget_control, self.widgets.rotation_optimize_method, get_value=index, get_uvalue=uvalue
    method = uvalue[index]

    self.tomoObj->optimize_center, [top_slice, bottom_slice], centers,  merit, width=range, step=step, method=method
    self->update_tomo_struct
    center1 = self.tomoStruct.rotationCenter + top_slice*self.tomoStruct.rotationCenterSlope
    center2 = self.tomoStruct.rotationCenter + bottom_slice*self.tomoStruct.rotationCenterSlope
    widget_control, self.widgets.input_file, get_value=file
    title = file + '   Slice=['+strtrim(string(top_slice),2)+','+strtrim(string(bottom_slice),2)+']'
    iplot, centers, merit[*,0], xtitle='Rotation center', ytitle='Figure of merit', sym_index=2, $
           view_title=title, id=id, /disable_splash_screen, /no_saveprompt
    merit_diff = min(merit[*,1]) - min(merit[*,0])
    iplot, centers, merit[*,1]-merit_diff, sym_index=4, $
           view_title=title, overplot=id
    self->reconstruct, 0
    self->reconstruct, 1
    self->set_limits
end


pro tomo_display::correct_rotation_tilt
  if (not ptr_valid(self.tomoStruct.pVolume)) then begin
    t = dialog_message('Must read in volume file first.', /error)
    return
  endif
  widget_control, /hourglass
  widget_control, self.widgets.recon_slice[0], get_value=top_slice
  top_slice = top_slice < (self.tomoStruct.ny-1)
  widget_control, self.widgets.recon_slice[1], get_value=bottom_slice
  bottom_slice = bottom_slice < (self.tomoStruct.ny-1)
  widget_control, self.widgets.rotation_center[0], get_value=top_center
  widget_control, self.widgets.rotation_center[1], get_value=bottom_center
  angle = (top_center-bottom_center) / (bottom_slice - top_slice) / !dtor
  self.tomoObj->correct_rotation_tilt, angle
  self->optimize_rotation_center
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
    if (ptr_valid(self.tomoStruct.pVolume)) then begin
        widget_control, self.widgets.disp_slice, get_value=slice
        widget_control, self.widgets.direction, get_value=direction
        widget_control, self.widgets.input_file, get_value=file
        ; Set the axis dimensions
        if (self.tomoStruct.imageType eq 'RECONSTRUCTED') then begin
            xdist = findgen(self.tomoStruct.nx)*self.tomoStruct.xPixelSize
            ydist = findgen(self.tomoStruct.ny)*self.tomoStruct.xPixelSize
            zdist = findgen(self.tomoStruct.nz)*self.tomoStruct.yPixelSize
        endif else begin
            xdist = findgen(self.tomoStruct.nx)*self.tomoStruct.xPixelSize
            ydist = findgen(self.tomoStruct.ny)*self.tomoStruct.yPixelSize
            zdist = *self.tomoStruct.pAngles
        endelse
        case direction of
            0: begin
                slice = (slice > 0) < (self.tomoStruct.nx-1)
                r = (*(self.tomoStruct.pVolume))[slice, *, *]
                xdist = ydist
                ydist = zdist
                end
            1: begin
                slice = (slice > 0) < (self.tomoStruct.ny-1)
                r = (*(self.tomoStruct.pVolume))[*, slice, *]
                ydist = zdist
                end
            2: begin
                slice = (slice > 0) < (self.tomoStruct.nz-1)
                r = (*(self.tomoStruct.pVolume))[*, *, slice]
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
    if (ptr_valid(self.tomoStruct.pVolume)) then begin
        widget_control, self.widgets.display_min, get_value=min
        widget_control, self.widgets.display_max, get_value=max
        v = bytscl(*self.tomoStruct.pVolume, min=min, max=max)
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
        self.widgets.read_camera_file: begin
          file = dialog_pickfile(filter=['*.nc', '*.h5'], get_path=path)
          if (file eq '') then break
          cd, path
          self->update_file_widgets
          widget_control, /hourglass
          widget_control, self.widgets.status, $
            set_value='Reading camera file ' + file + ' ...'
          self.tomoObj->read_camera_file, file
          self->update_tomo_struct
          self->update_file_widgets
          self->update_volume_widgets
          widget_control, self.widgets.dark_current, set_value=*self.tomoStruct.pDarks[0]
          widget_control, self.widgets.status, $
            set_value='Done reading camera file ' + file
        end

        self.widgets.read_processed_file: begin
            file = dialog_pickfile(filter=['*.nc', '*.h5'], get_path=path)
            if (file eq '') then break
            cd, path
            self->update_file_widgets
            widget_control, /hourglass
            widget_control, self.widgets.status, $
                            set_value='Reading input file ' + file + ' ...'
            t = self.tomoObj->read_volume(file, /store)
            self->update_tomo_struct
            self->update_file_widgets
            self->update_volume_widgets
            widget_control, self.widgets.dark_current, set_value=*self.tomoStruct.pDarks[0]
            widget_control, self.widgets.status, $
                            set_value='Done reading file ' + file
        end

        self.widgets.free_memory: begin
            self.tomoObj->free_memory
        end

        self.widgets.exit: begin
            widget_control, event.top, /destroy
            obj_destroy, self
            return
        end

        self.widgets.processing_options: begin
            widget_control, self.widgets.options_base, map=1
        end

        self.widgets.preprocess_go: begin
          widget_control, self.widgets.zingerWidth, get_value=zingerWidth
            widget_control, self.widgets.threshold, get_value=threshold
            widget_control, self.widgets.double_threshold, get_value=double_threshold
            write_output = widget_info(self.widgets.preprocess_write_output, /droplist_select)
            widget_control, self.widgets.preprocess_data_type, get_uvalue=data_types
            index = widget_info(self.widgets.preprocess_write_output, /droplist_select)
            data_type = data_types[index]
            widget_control, self.widgets.preprocess_file_format, get_uvalue=file_formats
            index = widget_info(self.widgets.preprocess_file_format, /droplist_select)
            file_format = file_formats[index]
            widget_control, self.widgets.preprocess_threads, get_value=threads
            widget_control, self.widgets.preprocess_scale, get_value=scale
            widget_control, self.widgets.abort, set_uvalue=0
            widget_control, self.widgets.status, set_value=""
            widget_control, /hourglass
            widget_control, self.widgets.status, set_value='Preprocessing ...'
            self.tomoObj->set_preprocess_params, zingerWidth=zingerWidth, zingerThreshold=threshold, zingerDoubleThreshold=double_threshold, $
                                                 scale=scale, threads=threads, $
                                                 writeDataType=data_type, writeOutput=write_output, writeFormat=file_format
            self.tomoObj->preprocess
            self->update_tomo_struct
            self->update_volume_widgets
            widget_control, self.widgets.status, set_value='Preprocessing complete'
        end

        self.widgets.preprocess_data_type: begin
            ; Nothing to do
        end

        self.widgets.preprocess_write_output: begin
          ; Nothing to do
        end

        self.widgets.preprocess_file_format: begin
          ; Nothing to do
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

        self.widgets.recon_data_type: begin
          ; Nothing to do
        end

        self.widgets.recon_write_output: begin
          ; Nothing to do
        end

        self.widgets.recon_file_format: begin
          ; Nothing to do
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
            if (ptr_valid(self.tomoStruct.pVolume)) then begin
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
                make_movie, index=direction+1, scale=scale, *self.tomoStruct.pVolume, $
                            jpeg_file=jpeg_file, tiff_file=tiff_file, mp4_file=mp4_file, $
                            min=min, max=max, start=start, stop=stop, step=step, wait=wait, $
                            unscaled_tiff=unscaled_tiff, fps=fps, bps=bps, $
                            label=label, abort_widget=self.widgets.abort, /color, $
                            status_widget=self.widgets.status
            endif else begin
                t = dialog_message('Must read in file first.', /error)
            endelse
        end

        else:  t = dialog_message('Unknown event')
    endcase

end_event:
    ; Set the sensitivity of preprocess, reconstruct, and visualize base widgets based on valid volume and image type
    valid_volume = ptr_valid(self.tomoStruct.pVolume)
    image_type = self.tomoStruct.imageType
    widget_control, self.widgets.visualize_base, sensitive=valid_volume
    widget_control, self.widgets.preprocess_base, sensitive=(valid_volume and (image_type eq 'RAW'))
    widget_control, self.widgets.reconstruct_base, sensitive=(valid_volume and (image_type eq 'NORMALIZED'))

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
    self.widgets.read_camera_file = widget_button(file, $
                                            value = 'Read camera file ...')
    self.widgets.read_processed_file = widget_button(file, $
                                            value = 'Read processed file ...')
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
                                        xsize=25, /noedit)
    self.widgets.directory = cw_field(col, title="Working directory:", $
                                        xsize=50, /noedit)
    self.widgets.input_file = cw_field(col, title="Input file name:", $
                                        xsize=50, /noedit)
    row = widget_base(col, /row)
    self.widgets.status = cw_field(row, title="Status:", $
                                        xsize=50, /noedit, $
                                        fieldfont=self.fonts.heading2)
    self.widgets.abort = widget_button(row, value='Abort', $
                                       event_pro='tomo_abort_event')

    ; Cannot create this object until the status and abort widgets are created.  Might want to change this.
    self.tomoObj = obj_new('tomo', abortWidget=self.widgets.abort, statusWidget=self.widgets.status)
    self->update_tomo_struct


    ; Preprocessing
    col = widget_base(left_column, /column, /frame)
    self.widgets.preprocess_base = col
    t = widget_label(col, value='Preprocess', font=self.fonts.heading1)
    row = widget_base(col, /row)
    t = widget_label(row, value='Output:  Save result:')
    choices=['No', 'Yes']
    self.widgets.preprocess_write_output = widget_droplist(row, value=choices, $
      uvalue=choices, /align_center)
    widget_control, self.widgets.preprocess_write_output, set_droplist_select=0
    choices = ['UInt16', 'Float32']
    t = widget_label(row, value='  Data type:')
    self.widgets.preprocess_data_type = widget_droplist(row, value=choices, $
      uvalue=choices, /align_center)
    widget_control, self.widgets.preprocess_data_type, set_droplist_select=0
    choices = ['HDF5', 'netCDF']
    t = widget_label(row, value='  File format:')
    self.widgets.preprocess_file_format = widget_droplist(row, value=choices, $
      uvalue=choices, /align_center)
    row = widget_base(col, /row, /base_align_bottom)
    t = widget_label(row, value='Preprocess:')
    self.widgets.preprocess_go = widget_button(row, value=' Preprocess ')

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
    choices = ['Entropy', '0-180']
    self.widgets.rotation_optimize_method = cw_bgroup(row, choices, $
      label_left='Optimize center method:', $
      row=1, set_value=0, uvalue=choices, /exclusive)
    row = widget_base(col, /row, /base_align_bottom)
    t = widget_label(row, value='Correct rotation tilt:')
    self.widgets.correct_rotation_tilt = widget_button(row, value='Correct rotation tilt')
    row = widget_base(col, /row)
    choices = ['Int16', 'UInt16', 'Float32']
    self.widgets.recon_data_type = cw_bgroup(row, choices, $
      label_left='Data type:', $
      row=1, set_value=0, /exclusive, ypad=0, uvalue=choices)
    row = widget_base(col, /row)
    self.widgets.recon_write_output = cw_bgroup(row, ['No', 'Yes'], $
      label_left='Save result:', $
      row=1, set_value=1, /exclusive, ypad=0)
    self.widgets.recon_file_format = cw_bgroup(row, ['HDF5', 'netCDF'], $
      label_left='File format:', $
      row=1, set_value=0, /exclusive, ypad=0)
    row = widget_base(col, /row)
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
    self.widgets.zingerWidth = cw_field(row, title='Zinger width', /float, $
                                        /row, xsize=10, value=self.tomoStruct.zingerWidth)
    row = widget_base(col1, /row)
    t = widget_label(row, value='Zinger thresholds')
    self.widgets.threshold = cw_field(row, title='Normal frames', /float, $
                                        /column, xsize=10, value=self.tomoStruct.zingerThreshold)
    self.widgets.double_threshold = cw_field(row, title='Double correlation (flat fields)', /float, $
                                        /column, xsize=10, value=self.tomoStruct.zingerDoubleThreshold)
    row = widget_base(col1, /row)
    self.widgets.dark_current = cw_field(row, /row, title='Dark current', $
                                        /integer, xsize=10, value=0)
    row = widget_base(col1, /row)
    self.widgets.preprocess_scale = cw_field(row, /row, title='Scale factor', $
                                             xsize=10, value=self.tomoStruct.preprocessScale)
    row = widget_base(col1, /row)
    self.widgets.preprocess_threads = cw_field(row, /row, title='Number of threads', $
                                               /integer, xsize=10, value=self.tomoStruct.preprocessThreads)
    col = widget_base(c1, /column, /frame)
    t = widget_label(col, value='Sinogram', font=self.fonts.heading1)
    col1 = col
    sinogram_base = col1
    row = widget_base(col1, /row)
    self.widgets.airPixels = cw_field(row, title='Air pixels (0=no air correction)', /integer, $
                                        /column, xsize=10, value=self.tomoStruct.airPixels)
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
                                            /float, xsize=15, value=self.tomoStruct.reconScale)
    row = widget_base(col, /row)
    self.widgets.ringWidth = cw_field(row, title='Ring smoothing width (0=None)', /integer, $
                                        /row, xsize=10, value=self.tomoStruct.ringWidth)
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
                                        /row, xsize=10, value=self.tomoStruct.sampl)    
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
                                        /column, xsize=10, value=self.tomoStruct.paddingAverage)
    row = widget_base(col1, /row)
    t = widget_label(col1, value='tomoRecon', font=self.fonts.heading2)
    row = widget_base(col1, /row)
    self.widgets.reconThreads = cw_field(row, title='Number of of threads', /integer, $
                                         /row, xsize=10, value=self.tomoStruct.reconThreads)
 ;
    row = widget_base(col1, /row)
    self.widgets.slicesPerChunk = cw_field(row, title='Slices per chunk', /integer, $
                                        /row, xsize=10, value=self.tomoStruct.slicesPerChunk)
                                       
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

    xmanager, 'tomo_display', self.widgets.base, /no_block
    xmanager, 'tomo_options', self.widgets.options_base, /no_block
    return, 1
end

pro tomo_display::cleanup
end

pro tomo_display__define

    widgets={ tomo_display_widgets, $
        base: 0L, $
        read_camera_file: 0L, $
        read_processed_file: 0L, $
        free_memory: 0L, $
        exit: 0L, $
        processing_options: 0L, $
        main_base: 0L, $
        base_file: 0L, $
        directory: 0L, $
        input_file: 0L, $
        status: 0L, $
        abort: 0L, $
        preprocess_base: 0L, $
        preprocess_go: 0L, $
        preprocess_data_type: 0L, $
        preprocess_write_output: 0L, $
        preprocess_file_format: 0L, $
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
        recon_data_type: 0L, $
        recon_write_output: 0L, $
        recon_file_format: 0L, $
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
        zingerWidth: 0L, $
        threshold: 0L, $
        double_threshold: 0L, $
        preprocess_scale: 0L, $
        preprocess_threads: 0L, $
        dark_current: 0L, $
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
        reconThreads: 0L, $
        slicesPerChunk: 0L $
    }

    fonts = {tomo_fonts, $
        normal: '', $
        heading1: '', $
        heading2: '' $
    }

    tomo_display = {tomo_display, $
        widgets: widgets, $
        tomoObj: obj_new(), $
        tomoStruct: {tomo}, $
        image_display: obj_new(), $
        fonts: fonts $
    }
end
