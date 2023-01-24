pro tomo_display::update_widgets
  ; Updates widgets, after initially creating or after reading a settings file
  self->update_tomo_struct
  ; First update the widgets for the tomo object
  widget_control, self.widgets.preprocessScale,        set_value=self.tomoStruct.preprocessScale
  widget_control, self.widgets.preprocessDataType,     get_uvalue=dataTypes
  index = where(self.tomoStruct.preprocessDataType eq dataTypes)
  widget_control, self.widgets.preprocessDataType,     set_droplist_select=index[0]
  widget_control, self.widgets.preprocessWriteOutput,  set_droplist_select=self.tomoStruct.preprocessWriteOutput
  widget_control, self.widgets.preprocessWriteFormat,  get_uvalue=fileFormats
  index = where(self.tomoStruct.preprocessWriteFormat eq fileFormats)
  widget_control, self.widgets.preprocessWriteFormat,  set_droplist_select=index[0]
  widget_control, self.widgets.preprocessThreads,      set_value=self.tomoStruct.preprocessThreads
  widget_control, self.widgets.airPixels,              set_value=self.tomoStruct.airPixels
  widget_control, self.widgets.fluorescence,           set_value=self.tomoStruct.fluorescence
  widget_control, self.widgets.zingerWidth,            set_value=self.tomoStruct.zingerWidth
  widget_control, self.widgets.zingerThreshold,        set_value=self.tomoStruct.zingerThreshold
  widget_control, self.widgets.zingerDoubleThreshold,  set_value=self.tomoStruct.zingerDoubleThreshold
  widget_control, self.widgets.reconMethod,            set_value=self.tomoStruct.reconMethod
  widget_control, self.widgets.reconScale,             set_value=self.tomoStruct.reconScale
  widget_control, self.widgets.ringWidth,              set_value=self.tomoStruct.ringWidth
  widget_control, self.widgets.reconDataType,          get_uvalue=dataTypes
  index = where(self.tomoStruct.reconDataType eq dataTypes)
  widget_control, self.widgets.reconDataType,          set_droplist_select=index[0]
  widget_control, self.widgets.reconWriteOutput,       set_droplist_select=self.tomoStruct.reconWriteOutput
  widget_control, self.widgets.reconWriteFormat,       get_uvalue=fileFormats
  index = where(self.tomoStruct.reconWriteFormat eq fileFormats)
  widget_control, self.widgets.reconWriteFormat,       set_droplist_select=index[0]
  widget_control, self.widgets.BP_filterSize,          set_value=self.tomoStruct.BP_filterSize
  widget_control, self.widgets.BP_filterName,          get_uvalue=filters
  index = where(self.tomoStruct.BP_filterName eq filters)
  widget_control, self.widgets.BP_filterName,          set_droplist_select=index[0]
  widget_control, self.widgets.BP_method,              set_droplist_select = self.tomoStruct.BP_method
  widget_control, self.widgets.RiemannInterpolation,   set_droplist_select = self.tomoStruct.RiemannInterpolation
  widget_control, self.widgets.RadonInterpolation,     set_droplist_select = self.tomoStruct.RadonInterpolation
  widget_control, self.widgets.GR_filterName,          get_uvalue=filters
  index = where(self.tomoStruct.GR_filterName eq filters)
  widget_control, self.widgets.GR_filterName,          set_droplist_select=index[0]
  widget_control, self.widgets.sampl,                  set_value=self.tomoStruct.sampl
  widget_control, self.widgets.paddedSinogramWidth,    get_uvalue=widths
  index = where(self.tomoStruct.paddedSinogramWidth eq widths)
  widget_control, self.widgets.paddedSinogramWidth,    set_droplist_select=index[0]
  widget_control, self.widgets.paddingAverage,         set_value=self.tomoStruct.paddingAverage
  widget_control, self.widgets.reconThreads,           set_value=self.tomoStruct.reconThreads
  widget_control, self.widgets.reconSlicesPerChunk,    set_value=self.tomoStruct.reconSlicesPerChunk
  ; Next update the widgets for the tomo_display object
  widget_control, self.widgets.optimizeMethod,         set_value=self.optimizeMethod
  widget_control, self.widgets.optimizeRange,          set_value=self.optimizeRange
  widget_control, self.widgets.optimizeStep,           set_value=self.optimizeStep
  widget_control, self.widgets.displayZoom,            set_value=self.displayZoom
  widget_control, self.widgets.displayOrder,           set_value=self.displayOrder
  widget_control, self.widgets.displayDirection,       set_value=self.displayDirection
  widget_control, self.widgets.displayAuto,            set_value=self.displayAuto
  widget_control, self.widgets.displayMin,             set_value=self.displayMin
  widget_control, self.widgets.displayMax,             set_value=self.displayMax
  !order = self.displayOrder

end

pro tomo_display::set_recon_params
  widget_control, self.widgets.reconDataType,         get_uvalue=data_types
  index =                                             widget_info(self.widgets.reconDataType, /droplist_select)
  data_type = data_types[index]
  write_output =                                      widget_info(self.widgets.reconWriteOutput, /droplist_select)
  widget_control, self.widgets.reconWriteFormat,      get_uvalue=file_formats
  index =                                             widget_info(self.widgets.reconWriteFormat, /droplist_select)
  file_format = file_formats[index]
  widget_control, self.widgets.reconMethod,           get_value=reconMethod
  widget_control, self.widgets.reconScale,            get_value=reconScale
  widget_control, self.widgets.ringWidth,             get_value=ringWidth
  widget_control, self.widgets.airPixels,             get_value=airPixels
  widget_control, self.widgets.fluorescence,          get_value=fluorescence
  widget_control, self.widgets.BP_filterSize,         get_value=BP_filterSize
  index =                                             widget_info(self.widgets.BP_filterName, /droplist_select)
  widget_control, self.widgets.BP_filterName,         get_uvalue=choices
  BP_filterName = choices[index]
  BP_method =                                         widget_info(self.widgets.BP_method, /droplist_select)
  widget_control, self.widgets.RiemannInterpolation,  get_value=RiemannInterpolation
  widget_control, self.widgets.RadonInterpolation,    get_value=RadonInterpolation
  index =                                             widget_info(self.widgets.GR_filterName, /droplist_select)
  widget_control, self.widgets.GR_filterName,         get_uvalue=choices
  GR_filterName = choices[index]
  index =                                             widget_info(self.widgets.paddedSinogramWidth, /droplist_select)
  widget_control, self.widgets.paddedSinogramWidth,   get_uvalue=choices
  paddedSinogramWidth = choices[index]
  widget_control, self.widgets.paddingAverage,        get_value=paddingAverage
  widget_control, self.widgets.sampl,                 get_value=sampl
  widget_control, self.widgets.reconThreads,          get_value=threads
  widget_control, self.widgets.reconSlicesPerChunk,   get_value=slicesPerChunk
 
  self.tomoObj->set_recon_params, $
          dataType = data_type, $
          writeOutput = write_output, $
          writeFormat = file_format, $
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

pro tomo_display::set_preprocess_params
  widget_control, self.widgets.zingerWidth,             get_value=zingerWidth
  widget_control, self.widgets.zingerThreshold,         get_value=zingerThreshold
  widget_control, self.widgets.zingerDoubleThreshold,   get_value=zingerDoubleThreshold
  widget_control, self.widgets.preprocessDataType,      get_uvalue=data_types
  index =                                               widget_info(self.widgets.preprocessDataType, /droplist_select)
  data_type = data_types[index]
  write_output =                                        widget_info(self.widgets.preprocessWriteOutput, /droplist_select)
  widget_control, self.widgets.preprocessWriteFormat,   get_uvalue=file_formats
  index =                                               widget_info(self.widgets.preprocessWriteFormat, /droplist_select)
  file_format = file_formats[index]
  widget_control, self.widgets.preprocessThreads,       get_value=threads
  widget_control, self.widgets.preprocessScale,         get_value=scale
  widget_control, self.widgets.abort,                   set_uvalue=0
  widget_control, self.widgets.status,                  set_value=""
  widget_control, /hourglass
  self.tomoObj->set_preprocess_params, zingerWidth=zingerWidth, zingerThreshold=zingerThreshold, zingerDoubleThreshold=zingerDoubleThreshold, $
                                       scale=scale, threads=threads, dataType=data_type, writeOutput=write_output, writeFormat=file_format
end

pro tomo_display::rebin, image, x_dist, y_dist
  ; This function is called to rebin a 2-D array, either shrinking or expanding it
  ; by the selected "zoom" factor
  widget_control, self.widgets.displayZoom, get_value=zoom, get_uvalue=all_zooms
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
  
  self->set_recon_params

  if (n_elements(islice) ne 0) then begin
    widget_control, self.widgets.rotation_center[islice], get_value=center
    widget_control, self.widgets.recon_slice[islice], get_value=slice
    slice = slice < (self.tomoStruct.ny-1)
    angles=*self.tomoStruct.pAngles
    r = self.tomoObj->reconstruct_slice((*self.tomoStruct.pVolume)[*, slice, *], center=center, sinogram=sinogram, cog=cog)
    ; If reconstruction was with backproject, rotate image so it is the same
    ; orientation as with gridrec
    if (self.tomoStruct.reconMethod eq self.tomoStruct.reconMethodBackproject) then r = rotate(r, 4)
    widget_control, self.widgets.displayAuto, get_value=auto
    if (auto) then begin
      min=min(r, max=max)
    endif else begin
      widget_control, self.widgets.displayMin, get_value=min
      widget_control, self.widgets.displayMax, get_value=max
    endelse
    dims = size(r, /dimensions)
    xdist = findgen(dims[0])*self.tomoStruct.xPixelSize
    ydist = xdist
    ; Change the size of the image before calling image_display
    self->rebin, r, xdist, ydist
    widget_control, self.widgets.input_file, get_value=file
    dims = size(r, /dimensions)
    xdist = findgen(dims[0])*self.tomoStruct.xPixelSize

    title = file + '    Center='+strtrim(string(center),2) + '     Slice='+strtrim(string(slice),2)
    image_display, r, min=min, max=max, title=title, xdist=xdist, ydist=ydist
    ; Display sinogram if desired
    widget_control, self.widgets.display_sinogram, get_value=display_sinogram
    if (display_sinogram and (n_elements(sinogram) ne 0)) then begin
      image_display, sinogram
    endif
    ; Plot center-of-gravity if desired
    widget_control, self.widgets.plogCog, get_value=plogCog
    if (plogCog and (n_elements(cog) ne 0)) then begin
      angles = *self.tomoStruct.pAngles
      iplot, angles, cog[*,0], ytitle='Center of gravity', xtitle='Angle (degrees)', $
             name='Measured', color=[0,0,255], identifier=id, /disable_splash_screen, /no_saveprompt
      iplot, angles, cog[*,1], /overplot,  $
             name='Fit', color=[255,0,0], identifier=id
      diff = cog[*,0]-cog[*,1]
      iplot, angles, diff, ytitle='COG measured-fit', xtitle='Angle (degrees)', id=id, /disable_splash_screen, /no_saveprompt
    endif
  endif else begin
    ; Reconstruct entire file
    widget_control, self.widgets.abort, set_uvalue=0
    widget_control, self.widgets.status, set_value=""
    widget_control, self.widgets.recon_slice[0], get_value=top_slice
    widget_control, self.widgets.recon_slice[1], get_value=bottom_slice
    widget_control, self.widgets.rotation_center[0], get_value=top_center
    widget_control, self.widgets.rotation_center[1], get_value=bottom_center
    self.tomoObj->set_rotation, top_slice, top_center, bottom_slice, bottom_center
    self.tomoObj->reconstruct_volume
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
  widget_control, self.widgets.displayDirection, get_value=direction
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
  widget_control, self.widgets.BP_filterSize, set_value=self.tomoStruct.nx
end


pro tomo_display::optimize_rotation_center
  widget_control, /hourglass
  widget_control, self.widgets.recon_slice[0], get_value=top_slice
  top_slice = top_slice < (self.tomoStruct.ny-1)
  widget_control, self.widgets.recon_slice[1], get_value=bottom_slice
  bottom_slice = bottom_slice < (self.tomoStruct.ny-1)

  self->set_recon_params
  
  widget_control, self.widgets.optimizeRange, get_value=range
  widget_control, self.widgets.optimizeStep, get_value=step
  widget_control, self.widgets.rotation_optimize_center, get_value=centers
  widget_control, self.widgets.optimizeMethod, get_value=index, get_uvalue=uvalue
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
  iplot, centers, merit[*,1]-merit_diff, sym_index=4, view_title=title, overplot=id
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
    self.widgets.reconMethod: begin
      sens = (event.value eq 2)
      widget_control, self.widgets.backproject_base, sensitive=sens
      widget_control, self.widgets.gridrec_base, sensitive=1-sens
      index = widget_info(self.widgets.BP_method, /droplist_select)
      widget_control, self.widgets.RadonInterpolation, sensitive = index
      widget_control, self.widgets.RiemannInterpolation, sensitive = 1-index
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
    self.widgets.plogCog: begin
      ; Nothing to do
    end
    self.widgets.BP_filterName: begin
      ; Nothing to do
    end
    self.widgets.BP_method: begin
       index = widget_info(self.widgets.BP_method, /droplist_select)
       widget_control, self.widgets.RadonInterpolation, sensitive = index
       widget_control, self.widgets.RiemannInterpolation, sensitive = 1-index
    end    
    self.widgets.RadonInterpolation: begin
      ; Nothing to do
    end
    self.widgets.RiemannInterpolation: begin
      ; Nothing to do
    end
    self.widgets.GR_filterName: begin
      ; Nothing to do
    end
    self.widgets.sampl: begin
      ; Nothing to do
    end
    self.widgets.paddedSinogramWidth: begin
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
    widget_control, self.widgets.displayDirection, get_value=direction
    widget_control, self.widgets.base_file, get_value=file
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
    title = file + '    Center='+strtrim(string(center),2) + '     '+axes[direction]+'='+strtrim(string(slice),2)
    widget_control, self.widgets.displayAuto, get_value=auto
    if (auto) then begin
      min=min(r, max=max)
    endif else begin
      widget_control, self.widgets.displayMin, get_value=min
      widget_control, self.widgets.displayMax, get_value=max
    endelse
    ; Change the size of the image before calling image_display
    self->rebin, r, xdist, ydist
    if (keyword_set(new_window)) or (obj_valid(self.image_display) eq 0) then begin
      self.image_display = obj_new('image_display', r, min=min, max=max, title=title, xdist=xdist, ydist=ydist)
    endif else begin
      self.image_display->set_image_data, r, title=title
    endelse

  endif else begin
    t = dialog_message('Must read in volume file first.', /error)
  endelse
end

pro tomo_display::display_volume
  if (ptr_valid(self.tomoStruct.pVolume)) then begin
    widget_control, /hourglass
    widget_control, self.widgets.disp_slice, get_value=slice
    widget_control, self.widgets.displayDirection, get_value=direction
    widget_control, self.widgets.base_file, get_value=file
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
    axes = ['X', 'Y', 'Z']
    widget_control, self.widgets.rotation_center[0], get_value=center
    title = file + '    Center='+strtrim(string(center),2)
    widget_control, self.widgets.displayAuto, get_value=auto
      widget_control, self.widgets.displayMin, get_value=min
      widget_control, self.widgets.displayMax, get_value=max
    image_display, *self.tomoStruct.pVolume, xdist=xdist, ydist=ydist, zdist=zdist, min=min, max=max, title=title

  endif else begin
    t = dialog_message('Must read in volume file first.', /error)
  endelse
end

pro tomo_display::volume_render
  if (ptr_valid(self.tomoStruct.pVolume)) then begin
    widget_control, self.widgets.displayMin, get_value=min
    widget_control, self.widgets.displayMax, get_value=max
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
      widget_control, self.widgets.dark_current,      set_value=*self.tomoStruct.pDarks[0]
      widget_control, self.widgets.status,            set_value='Done reading camera file ' + file
    end

    self.widgets.read_processed_file: begin
      file = dialog_pickfile(filter=['*.nc', '*.h5'], get_path=path)
      if (file eq '') then break
      cd, path
      self->update_file_widgets
      widget_control, /hourglass
      widget_control, self.widgets.status,            set_value='Reading input file ' + file + ' ...'
      t = self.tomoObj->read_volume(file, /store)
      self->update_tomo_struct
      self->update_file_widgets
      self->update_volume_widgets
      if (ptr_valid(self.tomoStruct.pDarks)) then $
        widget_control, self.widgets.dark_current,    set_value=*self.tomoStruct.pDarks[0]
      widget_control, self.widgets.status,            set_value='Done reading file ' + file
    end

    self.widgets.save_settings: begin
      file = dialog_pickfile()
      if (file eq '') then break
      self->set_preprocess_params
      self->set_recon_params
      self->save_settings, file
    end

    self.widgets.restore_settings: begin
      file = dialog_pickfile()
      if (file eq '') then break
      widget_control, /hourglass
      self.tomoObj->restore_settings, file
      self->update_widgets
    end

    self.widgets.free_memory: begin
      self.tomoObj->free_memory
    end

    self.widgets.exit: begin
      self->set_preprocess_params
      self->set_recon_params
      self->save_settings
      widget_control, event.top, /destroy
      obj_destroy, self
      return
    end

    self.widgets.processing_options: begin
      widget_control, self.widgets.options_base, map=1
    end

    self.widgets.preprocess_go: begin
      self->set_preprocess_params
      self.tomoObj->preprocess
      self->update_tomo_struct
      self->update_volume_widgets
    end

    self.widgets.preprocessDataType: begin
      ; Nothing to do
    end

    self.widgets.preprocessWriteOutput: begin
      ; Nothing to do
    end

    self.widgets.preprocessWriteFormat: begin
      ; Nothing to do
    end

    self.widgets.reconstruct_slice[0]: begin
      self->reconstruct, 0
    end

    self.widgets.reconstruct_slice[1]: begin
      self->reconstruct, 1
    end

    self.widgets.optimizeMethod: begin
      self.optimizeMethod = event.value
    end

    self.widgets.optimizeRange: begin
      self.optimizeRange = event.value
    end

    self.widgets.optimizeStep: begin
      self.optimizeStep = event.value
    end

    self.widgets.rotation_optimize: begin
      self->optimize_rotation_center
    end

    self.widgets.reconDataType: begin
      ; Nothing to do
    end

    self.widgets.reconWriteOutput: begin
      ; Nothing to do
    end

    self.widgets.reconWriteFormat: begin
      ; Nothing to do
    end

    self.widgets.reconstruct_all: begin
      self->reconstruct
    end

    self.widgets.correct_rotation_tilt: begin
      self->correct_rotation_tilt
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

    self.widgets.display_volume: begin
      self->display_volume
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

    self.widgets.displayDirection: begin
      self.displayDirection = event.value
      self->set_limits
    end

    self.widgets.displayOrder: begin
      !order = event.value
      self.displayOrder = event.value
    end

    self.widgets.displayZoom: begin
      self.displayZoom = event.value
    end

    self.widgets.displayAuto: begin
      self.displayAuto = event.value
    end

     self.widgets.displayMin: begin
      self.displayMin = event.value
    end

    self.widgets.displayMax: begin
      self.displayMax = event.value
    end

    self.widgets.make_movie: begin
      widget_control, self.widgets.disp_slice, get_value=slice
      if (ptr_valid(self.tomoStruct.pVolume)) then begin
        widget_control, self.widgets.displayAuto, get_value=auto
        if (auto) then begin
          widget_control, self.widgets.data_min, get_value=min
          widget_control, self.widgets.data_max, get_value=max
        endif else begin
          widget_control, self.widgets.displayMin, get_value=min
          widget_control, self.widgets.displayMax, get_value=max
        endelse
        widget_control, self.widgets.movie_output, get_value=output
        widget_control, self.widgets.displayDirection, get_value=direction
        widget_control, self.widgets.movie_file, get_value=file
        widget_control, self.widgets.displayZoom, get_value=zoom, get_uvalue=all_zooms
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

  ; Set the sensitivity of preprocess, reconstruct, and visualize base widgets based on valid volume and image type
  valid_volume = ptr_valid(self.tomoStruct.pVolume)
  image_type = self.tomoStruct.imageType
  widget_control, self.widgets.visualize_base, sensitive=valid_volume
  widget_control, self.widgets.preprocess_base, sensitive=(valid_volume and (image_type eq 'RAW'))
  widget_control, self.widgets.reconstruct_base, sensitive=(valid_volume and (image_type eq 'NORMALIZED'))

end

function tomo_display::get_saved_fields
  fields = ['optimizeMethod', 'optimizeRange', 'optimizeStep', $
            'displayZoom', 'displayOrder', 'displayDirection', $
            'displayAuto', 'displayMin', 'displayMax' ]
  return, fields
end

function tomo_display::find_saved_field, field
  myFieldNames = tag_names(self)
  index = where(field.toUpper() eq myFieldNames)
  return, index[0]
end

pro tomo_display::save_settings, file
  if (n_elements(file) eq 0) then file = self.settingsFile
  ; Get the settings from the tomo object
  settings = self.tomoObj->get_settings()
  savedFields = self->get_saved_fields()
  for i=0, n_elements(savedFields)-1 do begin
    field = savedFields[i]
    index = self->find_saved_field(field)
    if (index lt 0) then message, 'save_settings: unknown field: ' + field
    settings[field] = self.(index)
  endfor
  openw, lun, /get_lun, file
  printf, lun, /implied_print, settings
  close, lun
end

pro tomo_display::restore_settings, file
  if (n_elements(file) eq 0) then file = self.settingsFile
  ; First have tomo object restore settings
  if (file_test(file, /read)) then begin
    self.tomoObj->restore_settings, file
    print, 'Restoring settings from file: ', self.settingsFile
    ; Now parse the file again for this tomo_display object
    settings = json_parse(file)
    keys = settings.keys()
    values = settings.values()
    for i=0, n_elements(keys)-1 do begin
      key = keys[i]
      value = values[i]
      index = self->find_saved_field(key)
      if (index ge 0) then self.(index) = value
    endfor
  endif
  self->update_widgets
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

  self.optimizeMethod   = 0
  self.optimizeRange    = 6
  self.optimizeStep     = 0.25
  self.displayZoom      = 2
  self.displayAuto      = 1
  self.displayOrder     = 1
  self.displayDirection = 2
  self.displayMin       = 0
  self.displayMax       = 5000
  
  self.fonts.normal   = get_font_name(/helvetica)
  self.fonts.heading1 = get_font_name(/large, /bold)
  self.fonts.heading2 = get_font_name(/bold)

  self.widgets.base                     = widget_base(column=1, /tlb_kill_request_events, title='IDL Tomography Processing', mbar=mbar)

  file                                  = widget_button(mbar, /menu, value = 'File')
  self.widgets.read_camera_file         = widget_button(file, value = 'Read camera file ...')
  self.widgets.read_processed_file      = widget_button(file, value = 'Read processed file ...')
  self.widgets.save_settings            = widget_button(file, value = 'Save settings ...')
  self.widgets.restore_settings         = widget_button(file, value = 'Restore settings ...')
  self.widgets.free_memory              = widget_button(file, value = 'Free volume array')
  self.widgets.exit                     = widget_button(file, value = 'Exit')
  options                               = widget_button(mbar, /menu, value = 'Options')
  self.widgets.processing_options       = widget_button(options, value = 'Processing options ...')

  whole_display                         = widget_base(self.widgets.base, /row)
  left_column                           = widget_base(whole_display, /column)
  col                                   = widget_base(left_column, /column, /frame)
  self.widgets.main_base = col
  t                                     = widget_label(col, value='File/Status', font=self.fonts.heading1)
  self.widgets.base_file                = cw_field(col, xsize=25, /noedit, title="Base file name:")
  self.widgets.directory                = cw_field(col, xsize=50, /noedit, title="Working directory:")
  self.widgets.input_file               = cw_field(col, xsize=50, /noedit, title="Input file name:")
  row                                   = widget_base(col, /row)
  self.widgets.status                   = cw_field(row, xsize=50, /noedit, title="Status:", fieldfont=self.fonts.heading2)
  self.widgets.abort                    = widget_button(row, value='Abort', event_pro='tomo_abort_event')

  ; Cannot create this object until the status and abort widgets are created.  Might want to change this.
  self.tomoObj = obj_new('tomo', abortWidget=self.widgets.abort, statusWidget=self.widgets.status)
  self->update_tomo_struct

  ; Preprocessing
  col                                   = widget_base(left_column, /column, /frame)
  self.widgets.preprocess_base = col
  t                                     = widget_label(col, value='Preprocess', font=self.fonts.heading1)
  row                                   = widget_base(col, /row)
  choices = ['UInt16', 'Float32']
  t                                     = widget_label(row, value='Data type:')
  self.widgets.preprocessDataType       = widget_droplist(row, value=choices, uvalue=choices, /align_center)
  row                                   = widget_base(col, /row)
  t                                     = widget_label(row, value='Output:')
  choices=['No', 'Yes']
  t                                     = widget_label(row, value='  Save result:')
  self.widgets.preprocessWriteOutput    = widget_droplist(row, value=choices, uvalue=choices, /align_center)
  choices = ['HDF5', 'netCDF']
  t                                     = widget_label(row, value='  File format:')
  self.widgets.preprocessWriteFormat    = widget_droplist(row, value=choices, uvalue=choices, /align_center)
  row                                   = widget_base(col, /row, /base_align_bottom)
  t                                     = widget_label(row, value='Preprocess:')
  self.widgets.preprocess_go            = widget_button(row, value=' Preprocess ')

  ; Reconstruction
  col                                   = widget_base(left_column, /column, /frame)
  self.widgets.reconstruct_base = col
  t                                     = widget_label(col, value='Reconstruct', font=self.fonts.heading1)
  for i=0, 1 do begin
    row                                 = widget_base(col, /row, /base_align_bottom)
    if (i eq 0) then label='Upper slice:' else label='Lower slice:'
    t                                   = widget_label(row, value=label)
    self.widgets.recon_slice[i]         = cw_field(row, /column, title='Slice', /integer, xsize=10, value=100)
    self.widgets.rotation_center[i]     = cw_field(row, /column, title='Rotation Center', /float, xsize=10, value=325.000)
    self.widgets.reconstruct_slice[i] = widget_button(row, value='Reconstruct slice')
  endfor
  row                                   = widget_base(col, /row, /base_align_bottom)
  t                                     = widget_label(row, value='Optimize rotation center:')
  self.widgets.rotation_optimize_center = cw_field(row, /column, title='Rotation Center', /float,  xsize=10, value=325.000)
  self.widgets.optimizeRange            = cw_field(row, /column, title='Optimize range', /float, xsize=10, /all_events)
  self.widgets.optimizeStep             = cw_field(row, /column, title='Optimize step', /float, xsize=10, /all_events)
  self.widgets.rotation_optimize        = widget_button(row, value='Optimize center')
  row                                   = widget_base(col, /row, /base_align_bottom)
  choices = ['Entropy', '0-180']
  self.widgets.optimizeMethod           = cw_bgroup(row, choices, label_left='Optimize center method:', row=1, uvalue=choices, /exclusive)
  row                                   = widget_base(col, /row, /base_align_bottom)
  t                                     = widget_label(row, value='Correct rotation tilt:')
  self.widgets.correct_rotation_tilt    = widget_button(row, value='Correct rotation tilt')
  row                                   = widget_base(col, /row)
  choices = ['Int16', 'UInt16', 'Float32']
  t                                     = widget_label(row, value='Data type:')
  self.widgets.reconDataType            = widget_droplist(row, value=choices, uvalue=choices, /align_center)
  row                                   = widget_base(col, /row)
  t                                     = widget_label(row, value='Output:')
  choices=['No', 'Yes']
  t                                     = widget_label(row, value='  Save result:')
  self.widgets.reconWriteOutput         = widget_droplist(row, value=choices, uvalue=choices, /align_center)
  widget_control, self.widgets.reconWriteOutput, set_droplist_select=1
  choices = ['HDF5', 'netCDF']
  t                                     = widget_label(row, value='  File format:')
  self.widgets.reconWriteFormat         = widget_droplist(row, value=choices, uvalue=choices, /align_center)
  widget_control, self.widgets.reconWriteFormat, set_droplist_select=0
  row                                   = widget_base(col, /row)
  t                                     = widget_label(row, value='Reconstruct all:')
  self.widgets.reconstruct_all          = widget_button(row, value='Reconstruct all')

  ; Visualization
  right_column                          = widget_base(whole_display, /column)
  self.widgets.visualize_base = right_column
  widget_control, right_column, sensitive=0
  col                                   = widget_base(right_column, /column, /frame)
  t                                     = widget_label(col, value='Visualize', font=self.fonts.heading1)

  row                                   = widget_base(col, /row)
  t                                     = widget_label(row, value='Volume array:')
  self.widgets.nx                       = cw_field(row, title='NX', /integer, /noedit, /column, xsize=8, value=0)
  self.widgets.ny                       = cw_field(row, title='NY', /integer, /noedit, /column, xsize=8, value=0)
  self.widgets.nz                       = cw_field(row, title='NZ', /integer, /noedit, /column, xsize=8, value=0)
  self.widgets.volume_type              = cw_field(row, title='Type', /noedit, /column, xsize=18, value='')

  row                                   = widget_base(col, /row, /base_align_center)
  t                                     = widget_label(row, value='Actual intensity range:')
  self.widgets.data_min                 = cw_field(row, title='Min.', /float,  /column, xsize=10, value=0, /noedit)
  self.widgets.data_max                 = cw_field(row, title='Max.', /float,  /column, xsize=10, value=0, /noedit)

  row                                   = widget_base(col, /row, /base_align_center)
  t                                     = widget_label(row, value='Display intensity range:')
  self.widgets.displayMin               = cw_field(row, title='Min.', /float, /column, xsize=10, value=0, /all_events)
  self.widgets.displayMax               = cw_field(row, title='Max.', /float, /column, xsize=10, value=5000, /all_events)
  self.widgets.displayAuto              = cw_bgroup(row, ['Manual', 'Auto'], row=1, /exclusive)

  row                                   = widget_base(col, /row)
  self.widgets.displayDirection         = cw_bgroup(row, ['X', 'Y', 'Z'], label_left='Direction:', row=1, /exclusive)
  self.widgets.displayOrder             = cw_bgroup(row, ['Bottom to top', 'Top to bottom'], label_left='Order:', row=1, set_value=1, /exclusive)

  row                                   = widget_base(col, /row)
  self.widgets.displayZoom              = cw_bgroup(row, ['1/4', '1/2', '1', '2', '4'], label_left='Zoom:', row=1, set_value=2, /exclusive, uvalue=[-4, -2, 1, 2, 4])

  row                                   = widget_base(col, /row)
  t                                     = widget_label(row, value='Display slice:')
  col1                                  = widget_base(row, /column)
  self.widgets.disp_slice               = cw_field(col1, /integer, title='', xsize=10, value=100, /return_events)
  self.widgets.disp_slider              = widget_slider(col1, value=100, min=0, max=100, /suppress_value)
  col1                                  = widget_base(row, /column, /align_center)
  self.widgets.display_slice            = widget_button(col1, value='Display slice')
  col1                                  = widget_base(row, /column, /align_center)
  self.widgets.display_volume           = widget_button(col1, value='Display volume')

  row                                   = widget_base(col, /row)
  t                                     = widget_label(row, value='Volume render:')
  self.widgets.volume_render            = widget_button(row, value='Volume render')

  ; Movies
  col                                   = widget_base(right_column, /column, /frame)
  t                                     = widget_label(col, value='Movies', font=self.fonts.heading1)

  row                                   = widget_base(col, /row)
  self.widgets.movie_output             = cw_bgroup(row, ['Screen', 'JPEG', 'TIFF', 'MP4'], label_left='Output:', row=1, set_value=0, /exclusive)
  col1                                  = widget_base(row, /column, /align_center)
  self.widgets.make_movie               = widget_button(col1, value='Make movie')

  row                                   = widget_base(col, /row)
  self.widgets.tiff_scale               = cw_bgroup(row, ['Scaled (8-bit)', 'Unscaled'], label_left='TIFF scaling:', row=1, set_value=0, /exclusive)

  row                                   = widget_base(col, /row)
  self.widgets.movie_fps                = cw_field(row, title='MP4 frames/s', /float, /column, xsize=10, value=30)
  self.widgets.movie_bps                = cw_field(row, title='MP4 bits/s', /float, /column, xsize=10, value=3e4)

  row                                   = widget_base(col, /row)
  self.widgets.first_slice              = cw_field(row, title='First slice', /integer, /column, xsize=10, value=0)
  self.widgets.last_slice               = cw_field(row, title='Last slice', /integer, /column, xsize=10, value=0)
  self.widgets.slice_step               = cw_field(row, title='Step', /integer, /column, xsize=10, value=1)
  self.widgets.movie_wait               = cw_field(row, title='Delay time', /float, /column, xsize=10, value=0.)

  row                                   = widget_base(col, /row)
  self.widgets.movie_file               = cw_field(row, title="JPEG/TIFF/MP4 file name:", xsize=40)

  widget_control, self.widgets.base, set_uvalue=self
  ; Make all of the base widgets the same size so they line up nicely
  g = widget_info(self.widgets.main_base, /geometry)
  widget_control, self.widgets.preprocess_base, xsize=g.xsize
  widget_control, self.widgets.reconstruct_base, xsize=g.xsize
  widget_control, self.widgets.visualize_base, xsize=g.xsize
  widget_control, self.widgets.base, /realize

  ; The "options" screen.  Normally not visible
  self.widgets.options_base             = widget_base(row=1, /tlb_kill_request_events, title='IDL Tomography Processing Options')

  c1                                    = widget_base(self.widgets.options_base, /column)
  c2                                    = widget_base(self.widgets.options_base, /column)
  col                                   = widget_base(c1, /column, /frame)
  preprocess_base=col
  t                                     = widget_label(col, value='Preprocessing', font=self.fonts.heading1)
  col1 = col

  row                                   = widget_base(col1, /row)
  self.widgets.zingerWidth              = cw_field(row, /row, /float, xsize=10, title='Zinger width')

  row                                   = widget_base(col1, /row)
  t                                     = widget_label(row, value='Zinger thresholds')
  self.widgets.zingerThreshold          = cw_field(row, /column, /float, xsize=10, title='Normal frames')
  self.widgets.zingerDoubleThreshold    = cw_field(row, /column, /float, xsize=10, title='Double correlation (flat fields)')

  row                                   = widget_base(col1, /row)
  self.widgets.dark_current             = cw_field(row, /row, /integer, xsize=10, title='Dark current', value=0)

  row                                   = widget_base(col1, /row)
  self.widgets.preprocessScale          = cw_field(row, /row, /float, xsize=10, title='Scale factor')

  row                                   = widget_base(col1, /row)
  self.widgets.preprocessThreads        = cw_field(row, /row,  /integer, xsize=10, title='Number of threads')
  col                                   = widget_base(c1, /column, /frame)
  t                                     = widget_label(col, value='Sinogram', font=self.fonts.heading1)
  col1 = col
  sinogram_base = col1

  row                                   = widget_base(col1, /row)
  self.widgets.airPixels                = cw_field(row, /column, /integer, xsize=10, title='Air pixels (0=no air correction)')
  self.widgets.fluorescence             = cw_bgroup(row, ['Absorption', 'Fluorescence'], row=1,  /exclusive, label_top='Data type')

  row                                   = widget_base(col1, /row)
  self.widgets.display_sinogram         = cw_bgroup(row, ['No', 'Yes'], row=1, /exclusive, label_left='Display sinogram', set_value=0)

  row                                   = widget_base(col1, /row)
  self.widgets.plogCog                  = cw_bgroup(row, ['No', 'Yes'], row=1, /exclusive, label_left='Plot center-of-gravity', set_value=0)
  col                                   = widget_base(c2, /column, /frame)
  recon_base=col

  t                                     = widget_label(col, value='Reconstruction', font=self.fonts.heading1)
  row                                   = widget_base(col, /row)
  self.widgets.reconMethod              = cw_bgroup(row, ['tomoRecon', 'Gridrec', 'Backproject'], row=1, /exclusive, label_left='Reconstruction method:')

  row                                   = widget_base(col, /row)
  self.widgets.reconScale               = cw_field(row, /row, /float, xsize=15, title='Scale factor')

  row                                   = widget_base(col, /row)
  self.widgets.ringWidth                = cw_field(row, /row, /integer, xsize=10, title='Ring smoothing width (0=None)')
  col1                                  = widget_base(col, /column, /frame)
  self.widgets.backproject_base = col1
  widget_control, self.widgets.backproject_base, sensitive=0

  t                                     = widget_label(col1, value='Backproject', font=self.fonts.heading2)
  row                                   = widget_base(col1, /row)
  t                                     = widget_label(row, value='Filter: ', font=self.fonts.heading2)
  self.widgets.BP_filterSize            = cw_field(row, /row, /integer, xsize=10, title='Size')
  choices=['Gen_Hamming', 'Shepp_Logan', 'LP_Cosine', 'Ramlak', 'None']
  self.widgets.BP_filterName            = widget_droplist(row, value=choices, uvalue=choices, /align_center)

  row                                   = widget_base(col1, /row)
  t                                     = widget_label(row, value='Backprojection Method: ', font=self.fonts.heading2)
  self.widgets.BP_method                = widget_droplist(row, value = ['Riemann', 'Radon'], uvalue=['riemann', 'radon'], /align_center)

  row                                   = widget_base(col1, /row)
  self.widgets.RiemannInterpolation     = cw_bgroup(row, ['None', 'Bilinear', 'Cubic'], row=1, /exclusive, label_left='Riemann Interpolation')

  row                                   = widget_base(col1, /row)
  self.widgets.RadonInterpolation       = cw_bgroup(row, ['None', 'Linear'],  row=1, /exclusive, label_left='Radon Interpolation')                        
  widget_control, self.widgets.RadonInterpolation, sensitive = 0  
  col1                                  = widget_base(col, /column, /frame)
  self.widgets.gridrec_base = col1

  t                                     = widget_label(col1, value='Gridrec/tomoRecon', font=self.fonts.heading2)
  choices=['Shepp-Logan', 'Hann', 'Hamming', 'Ramlak']
  uval_choices=['shepp', 'hann', 'hamming', 'ramlak']
  row                                   = widget_base(col1, /row)
  t                                     = widget_label(row, value='Filter: ')
  self.widgets.GR_filterName            = widget_droplist(row, value=choices, uvalue=uval_choices, /align_center)

  row                                   = widget_base(col1, /row)
  t                                     = widget_label(row, value='Sample Parameter: ')
  self.widgets.sampl = cw_field(row, /row, xsize=10, title='')    

  row                                   = widget_base(col1, /row)
  t                                     = widget_label(row, value = 'Padded Sinogram Width:')
  choices = ['Auto', 'No Padding', '1024','2048', '4096']
  uval_choices = [0, 1, 1024, 2048, 4096]
  self.widgets.paddedSinogramWidth      = widget_droplist(row, value = choices, uvalue = uval_choices, /align_center)

  row                                   = widget_base(col1, /row)
  self.widgets.paddingAverage           = cw_field(row, /column, /integer, xsize=10, title='Pixels to average for padding (0=pad with 0.0)')

  row                                   = widget_base(col1, /row)
  t                                     = widget_label(col1, value='tomoRecon', font=self.fonts.heading2)
  row                                   = widget_base(col1, /row)
  self.widgets.reconThreads             = cw_field(row, /row, /integer, xsize=10, title='Number of of threads')

  row                                   = widget_base(col1, /row)
  self.widgets.reconSlicesPerChunk      = cw_field(row, /row, /integer, xsize=10, title='Slices per chunk')
                                       
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

  widget_control, self.widgets.visualize_base, sensitive=0
  widget_control, self.widgets.preprocess_base, sensitive=0
  widget_control, self.widgets.reconstruct_base, sensitive=0

  xmanager, 'tomo_display', self.widgets.base, /no_block
  xmanager, 'tomo_options', self.widgets.options_base, /no_block
  
  if (!version.os eq 'Win32') then begin
    self.settingsFile = getenv('USERPROFILE') + '\tomo_settings.txt'
  endif else begin
    self.settingsFile = '~/tomo_settings.txt'
  endelse
  self->restore_settings
  return, 1
end

pro tomo_display::cleanup
  ; Save the settings
  ; We cannot
  if (!version.os eq 'Win32') then begin
    file = getenv('USERPROFILE') + '\tomo_settings.txt'
  endif else begin
    file = '~/tomo_settings.txt'
  endelse
  self->save_settings, file
  print, 'Saving settings file: ', file
end

pro tomo_display__define
  widgets={ tomo_display_widgets, $
    base:                     0L, $
    read_camera_file:         0L, $
    read_processed_file:      0L, $
    save_settings:            0L, $
    restore_settings:         0L, $
    free_memory:              0L, $
    exit:                     0L, $
    processing_options:       0L, $
    main_base:                0L, $
    base_file:                0L, $
    directory:                0L, $
    input_file:               0L, $
    status:                   0L, $
    abort:                    0L, $
    preprocess_base:          0L, $
    preprocess_go:            0L, $
    preprocessDataType:       0L, $
    preprocessWriteOutput:    0L, $
    preprocessWriteFormat:    0L, $
    reconstruct_base:         0L, $
    recon_slice:              lonarr(2), $
    rotation_center:          lonarr(2), $
    reconstruct_slice:        lonarr(2), $
    rotation_optimize_center: 0L, $
    optimizeRange:            0L, $
    optimizeStep:             0L, $
    correct_rotation_tilt:    0L, $
    optimizeMethod:           0L, $
    rotation_optimize:        0L, $
    reconDataType:            0L, $
    reconWriteOutput:         0L, $
    reconWriteFormat:         0L, $
    reconstruct_all:          0L, $
    visualize_base:           0L, $
    nx:                       0L, $
    ny:                       0L, $
    nz:                       0L, $
    volume_type:              0L, $
    displayDirection:         0L, $
    displayOrder:             0L, $
    data_min:                 0L, $
    data_max:                 0L, $
    displayMin:               0L, $
    displayMax:               0L, $
    displayAuto:              0L, $
    disp_slice:               0L, $
    disp_slider:              0L, $
    display_slice:            0L, $
    display_volume:           0L, $
    volume_render:            0L, $
    movie_output:             0L, $
    first_slice:              0L, $
    last_slice:               0L, $
    slice_step:               0L, $
    movie_wait:               0L, $
    movie_fps:                0L, $
    movie_bps:                0L, $
    tiff_scale:               0L, $
    displayZoom:              0L, $
    movie_file:               0L, $
    make_movie:               0L, $
    flip_data:                0L, $

    ; These widgets are in the "options" page
    options_base:             0L, $
    zingerWidth:              0L, $
    zingerThreshold:          0L, $
    zingerDoubleThreshold:    0L, $
    preprocessScale:          0L, $
    preprocessThreads:        0L, $
    dark_current:             0L, $
    reconMethod:              0L, $
    reconScale:               0L, $
    ringWidth:                0L, $
    airPixels:                0L, $
    fluorescence:             0L, $
    display_sinogram:         0L, $
    plogCog:                  0L, $
    backproject_base:         0L, $
    BP_filterSize:            0L, $
    BP_filterName:            0L, $
    RiemannInterpolation:     0L, $
    RadonInterpolation:       0L, $
    BP_method:                0L, $
    white_average:            0L, $
    white_smooth:             0L, $
    gridrec_base:             0L, $
    GR_filterName:            0L, $
    paddedSinogramWidth:      0L, $
    paddingAverage:           0L, $
    sampl:                    0L, $
    reconThreads:             0L, $
    reconSlicesPerChunk:      0L  $
  }

  fonts = {tomo_fonts, $
    normal:   '', $
    heading1: '', $
    heading2: '' $
  }

  tomo_display = {tomo_display, $
    widgets:          widgets, $
    tomoObj:          obj_new(), $
    tomoStruct:       {tomo}, $
    image_display:    obj_new(), $
    fonts:            fonts, $
    optimizeMethod:   0L, $
    optimizeRange:    0., $
    optimizeStep:     0., $
    displayZoom:      0L, $
    displayAuto:      0L, $
    displayOrder:     0L, $
    displayDirection: 0L, $
    displayMin:       0., $
    displayMax:       0., $
    settingsFile:     "" $
  }
end
