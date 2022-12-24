;+
; NAME:
;  TOMO::READ_DATA_FILE
;
; PURPOSE:
;   This procedure reads a tomography data set from 2 types of data files:
;     netCDF files.  There are 3 files
;       base_1.nc contains flat fields collected at the beginning
;       base_2.nc contains the projections
;       base_3.nc contains the flat fields collected at the end
;     HDF5 files.  There is a single file that contains the flat fields and the projections
;
; CATEGORY:
;   Tomography
;
; CALLING SEQUENCE:
;   TOMO->READ_DATA_FILE, Filename, Projections, Flats, Darks, Angles
;
; INPUTS:
;   Filename:
;       For netCDF this can be the name of any of the 3 files
;       For HDF5 this is the name of the file
;       to be of the form Base_file + strtrim(file_number,2) + '.SPE'.
;
; OUTPUTS:
;   Projections:
;       A 3-D array of the projection data, [NCOLS, NROWS, NANGLES]
;
;   Flats:
;       A 3-D array of flat field data, [NCOLS, NROWS, NFLATS]
;
;   Darks:
;       A 3-D array of dark field data, [NCOLS, NROWS, NDARKS]
;
;   Angles:
;       A 1-D array of the projection angles
;
pro tomo::read_camera_file, filename

  t0 = systime(1)
  self->set_file_components, filename
  ptr_free, self.pvolume

  ; See if the file is netCDF
  if (ncdf_is_ncdf(filename)) then begin
    self.base_filename = filename.remove(-4)
    file = self.base_filename + '1.nc'
    self->display_status, 'Reading ' + file
    flat1       = read_nd_netcdf(file)
    file = self.base_filename + '3.nc'
    self->display_status, 'Reading ' + file
    flat2       = read_nd_netcdf(file)
    file = self.base_filename + '2.nc'
    self->display_status, 'Reading ' + file
    projections = read_nd_netcdf(file)
    dims = size(projections, /dimensions)
    num_projections = dims[2]
    flats = [[[flat1]], [[flat2]]]
    status = self.read_setup(self.base_filename + '.setup')
    darks = fix(self.dark_current)
    self.rotation_start = 0.
    self.rotation_step = 180./num_projections
    return
  endif

  ; See if the file is HDF5
  if (h5f_is_hdf5(filename)) then begin
    flats = h5_getdata(filename, '/exchange/data_white')
    projections = h5_getdata(filename, '/exchange/data')
    dims = size(projections, /dimensions)
    num_projections = dims[2]
    ; For now we assume there are no dark field images
    self.dark_current = h5_getdata(filename, '/process/acquisition/dark_fields/dark_field_value')
    darks = fix(self.dark_current[0])
    self.rotation_start = h5_getdata(filename, '/process/acquisition/rotation/rotation_start')
    self.rotation_step = h5_getdata(filename, '/process/acquisition/rotation/rotation_step')
    self.operator = h5_getdata(filename, '/measurement/sample/experimenter/name')
    self.title = h5_getdata(filename, '/measurement/sample/description_1')
    self.sample = h5_getdata(filename, '/measurement/sample/name')
    self.camera = h5_getdata(filename, '/measurement/instrument/detector/model')
    self.x_pixel_size = h5_getdata(filename, '/measurement/instrument/detection_system/objective/resolution')
    self.y_pixel_size = self.x_pixel_size
    self.z_pixel_size = self.x_pixel_size
  endif
 
  self.image_type = 'RAW'
  angles = self.rotation_start + self.rotation_step * findgen(num_projections)
  self.angles = ptr_new(angles, /no_copy)
  self.pvolume = ptr_new(projections, /no_copy)
  self.pflats = ptr_new(flats, /no_copy)
  self.pdarks = ptr_new(darks, /no_copy)
  self.rotation_center = dims[0]/2.
  self.rotation_center_slope = 0.
  self.nx = dims[0]
  self.ny = dims[1]
  self.nz = dims[2]

  print, 'Time to read camera file=', systime(1)-t0
end

function tomo::find_attribute, attributes, name
  for i=0, n_elements(attributes) do begin
    if (attributes[i].name eq name) then return, i
  endfor
  message, 'Could not find attribute ' + name
end

pro tomo::display_status, message, debug_level
  if (n_elements(debug_level) eq 0) then debug_level = 0
  if (widget_info(self.status_widget, /valid_id)) then begin
    widget_control, self.status_widget, set_value=message
  endif
  if (self.debug_level ge debug_level) then print, systime() + ' ' + message
end

function tomo::check_abort
  if (widget_info(self.abort_widget, /valid_id)) then begin
    event = widget_event(/nowait, self.abort_widget)
    widget_control, self.abort_widget, get_uvalue=abort
    return, abort
  endif
end

;+
; NAME:
;  TOMO::PREPROCESS
;
; PURPOSE:
;   This procedure reads a tomography data set from netCDF or HDF5 raw data files.
;   It corrects the flat fields for dark current and zingers, and averages them together.
;   It then corrects each projection for dark current, flat field, and zingers.
;   It optionally saves the normalized data to an HDF5 file.
;
; CATEGORY:
;   Tomography
;
; CALLING SEQUENCE:
;   TOMO->PREPROCESS, Filename
;
; INPUTS:
;   Filename
;       The name of the raw data file.  For netCDF input this can be the name of any of the 3 files.
;
;; KEYWORD PARAMETERS:
;   THRESHOLD:
;       The threshold for zinger removal in normal frames.  See documentation
;       for REMOVE_TOMO_ARTIFACTS for details.  Default=1.25
;   DOUBLE_THRESHOLD:
;       The threshold for zinger removal in white field frames using double
;       correlation.  Default=1.05
;   DARK:
;       The dark current, either a scaler or a 2-D array.  If this is a scaler
;       value then a constant dark current is subtracted from each pixel in
;       every frame.  If this is a 2-D array then it must have the same
;       dimensions as each frame in the data set.  In this case the specified
;       2-D dark current will be substracted from each frame in the data set.
;       With netCDF files the dark current scalar value is taken from the .setup file.
;       With HDF5 files the dark current scalar value is in the HDF5 file, or there are
;       actual dark current frames in the file.
;       This keyword is thus normally not used.
;   FIRST_ROW:
;       The starting row (slice) to be processed.  The default is 0.  This
;       keyword, together with LAST_ROW below are provided for processing
;       data sets which are too large to be read into memory in their
;       entirety.  It lets one create multiple volume arrays from a single
;       data set, for example rows 0-300 in file 1, (FIRST_ROW=0, LAST_ROW=300)
;       rows 301-600 in file 2, etc.
;   LAST_ROW:
;       The ending row (slice) to be processed.  The defaults is the last row
;       in each image.  See comments under FIRST_ROW above.
;  FLAT_FIELD:
;       The flat field value, either a scaler or a 2-D array.  If this is a
;       scaler value then each pixel in each data frame is normalized by this
;       constant value.  If this is a 2-D array then it must have the same
;       dimensions as each frame in the data set.  In this case then each data
;       frame in the data set is normalized by the specified 2-D array.
;       Note that if the data set contains flat field frames which is typically the case, 
;       then this keyword is normally not used.
;   OUTPUT:
;       The name of the output file.  The default is Base_file + 'volume.h5'
;   DEBUG:  A debugging flag.  Allowed values are:
;           0: No informational output
;           1: Prints each input filename as it is read, and prints limited
;              information on processing steps
;           2: Prints detailed information on processing steps
; OUTPUTS:
;   This function returns a 3-dimensional signed 16-bit integer volume array
;   of size [NCOLS, NROWS, NPROJECTIONS].  The data is the ratio of the input image
;   to the flat field, multiplied by 10,000.  The ratio of the data to the
;   flat field should be in the range 0 to 1 (with occasional values slightly
;   greater than 1).  Multiplying by 10000 should give sufficient resolution,
;   since even values with 99% absorption will be stored with a precision of
;   1%.
;
; PROCEDURE:
;   This function performs the following steps:
;   - Reads the raw data into a 3-D array usually of type UINT (unsigned 16-bit integer)
;     Stores the rotation angle at which each frame was collected.
;   - Subtracts the dark current from each data frame and white field frame,
;     using dark current scalar value or images in the data set or passed as argument
;   - Removes zingers from white field frames using REMOVE_TOMO_ARTIFACTS with
;     /DOUBLE_CORRELATION if possible, or /ZINGERS if not.
;   - Divides each data frame by the white field, using white field images in
;     the data set, or the input white field if present.
;     The ratio of each frame to the white field is multiplied by 10,000 to be
;     able to use 16 bit integers, rather than floats to store the results,
;     saving a factor of 2 in memory, which is important for these large 3-D
;     data sets.
;   - Corrects for zingers in the white-field normalized data frames, using
;     REMOVE_TOMO_ARTIFACTS, /ZINGERS.
;   - Optionally writes the normalized data frames to a single disk file.  The default
;     file name is Base_file + 'volume.h5'.  
;     The volume file can be read back in to IDL with function READ_TOMO_VOLUME
;

pro tomo::preprocess, dark=input_dark, $
  white_field=input_white, threshold=threshold, $
  double_threshold=double_threshold, $
  data_type=data_type, $
  write_output=write_output, netcdf=netcdf 
  
  tStart = systime(1)
  if (n_elements(threshold) eq 0) then threshold=1.25
  if (n_elements(double_threshold) eq 0) then double_threshold=1.05
  if (n_elements(data_type) eq 0) then data_type = 'UInt16'
  if (n_elements(setup) eq 0) then setup=self.base_filename + '.setup'
  if (keyword_set(netcdf)) then output_file = self.base_filename + 'norm.nc' $
                           else output_file = self.base_filename + 'norm.h5'

  status = self->read_setup(setup)
  
  if (data_type eq 'UInt16') then begin
    normalized = uintarr(self.nx, self.ny, self.nz, /nozero)
  endif else begin
    normalized = fltarr(self.nx, self.ny, self.nz, /nozero)
  endelse

  ; Convert darks and flats to float for efficiency
  darks = float(*self.pdarks)
  flats = float(*self.pflats)
  dims = size(darks, /dimensions)
  ndarks = 1
  if (n_elements(dims) eq 3) then ndarks = dims[2]
  dims = size(flats, /dimensions)
  nflats = 1
  if (n_elements(dims) eq 3) then nflats = dims[2]
 
  ; Do dark current correction
  self->display_status, 'Doing corrections on flat fields ...', 1
  ; If there is more than one dark field image then average them
  if (ndarks gt 1) then begin
    darks = total(darks, 3) / ndarks
  endif
  
  for i=0, nflats-1 do begin
    data_temp = flats[*,*,i] - darks
    if (min(data_temp) le 0) then data_temp = data_temp > 1
    flats[0,0,i] = data_temp
  endfor

  ; Remove zingers from flat fields.  If there is more than 1 flat field
  ; do it with double correlation, else do it with spatial filter
  if (nflats eq 1) then begin
    flats = remove_tomo_artifacts(flats, /zingers, threshold=threshold, debug=debug)
  endif else begin
    for i=0, nflats-2 do begin
      flats[0,0,i] = remove_tomo_artifacts(flats[*,*,i], image2=flats[*,*,i+1], $
          /double_correlation, threshold=double_threshold, debug=debug)
    endfor
  endelse
  flats = total(flats, 3)/nflats

  tFlats = systime(1)

  num_projections = self.nz
  self.data_offset = 0.
  self->display_status, 'Doing dark, flat, and zinger correction ...', 1
  for i=0, num_projections-1 do begin
    if (data_type eq 'UInt16') then begin
      data_temp = 10000. * (((*self.pvolume)[*,*,i] - darks) / flats)
      self.data_scale = 10000.
    endif else begin
      self.data_scale = 1.0
      data_temp = ((*self.pvolume)[*,*,i] - darks) / flats
    endelse
    if (min(data_temp) le 0) then data_temp = data_temp > 1
    data_temp = remove_tomo_artifacts(data_temp, /zingers, threshold=threshold, debug=0)
    self->display_status, 'Correcting projection ' + strtrim(i+1,2) + '/' + strtrim(num_projections,2), 2
    normalized[0,0,i] = data_temp
    if (self.check_abort()) then begin
      self->display_status, 'Preprocessing aborted', 1
      return
    endif
  endfor
  
  tNormalize = systime(1)

  self.image_type = 'NORMALIZED'
  if (keyword_set(write_output)) then begin
    self->display_status, 'Writing volume file ...', 1
    self->write_volume, output_file, normalized, netcdf=netcdf
  endif
  ptr_free, self.pvolume
  self.pvolume = ptr_new(normalized, /no_copy)
  status = self->write_setup(setup)
  tEnd = systime(1)
  self->display_status, 'Preprocessing complete', 1

  print, 'Preprocess execution times:'
  print, '                  Flat adjustments:', tFlats - tStart
  print, '  Dark, flat and zinger correction:', tNormalize - tFlats
  print, '                    Writing output:', tEnd - tNormalize
  print, '                             Total:', tEnd - tStart
 
end

pro tomo::set_tomo_params, _EXTRA=extra
  dimensions = [self.nx, self.ny, self.nz]
  ; Can't pass self.tomoParams because it will be a copy
  tp = self.tomoParams
  tomo_params_update, tp, dimensions=dimensions, _EXTRA=extra
  self.tomoParams = tp
end

pro tomo::optimize_center, slices, center, merit, width=width, step=step, method=method
  ; This routine calculates an array of image figure of merit as the rotation center is varied

  if (n_elements(method) eq 0) then method = 'Entropy'
  if (n_elements(width) eq 0) then width = 10
  if (n_elements(step) eq 0) then begin
    if (method eq 'Entropy') then step = 0.25 else step = 0.5
  endif

  t0 = systime(1)
  ncenter = fix(width/step)
  if (n_elements(center) eq 1) then begin
    center = center - width/2 + findgen(ncenter)*step
  endif
  merit = dblarr(ncenter, 2)
  max_angle = max(*self.angles)
  if (method eq 'Entropy' and (max_angle lt 181)) then begin
    self->optimize_center_entropy, slices, center, merit
  endif else  if (method eq 'Entropy' and (max_angle ge 181)) then begin
      self->optimize_center_entropy_360, slices, center, merit
  endif else begin
    self->optimize_center_mirror, slices, center, merit
  endelse

  ; Finds the minumum in the figure of merit for each slice
  t = min(merit[*,0], min_pos1)
  t = min(merit[*,1], min_pos2)
  center1 = center[min_pos1]
  center2 = center[min_pos2]
  if (method eq '0-180') then begin
    ; It appears that the center is 0.5 pixel larger than the center position used by gridrec
    center1 = center1 - 0.5
    center2 = center2 - 0.5
  endif
  self->set_rotation, slices[0], center1, slices[1], center2
  t1 = systime(1)
  print, 'optimize_center, time=', t1-t0
end

pro tomo::optimize_center_entropy, slices, center, merit
  ncenter = n_elements(center)
  s = size(*self.pvolume, /dimensions)
  input = fltarr(s[0], ncenter*2, s[2])
  ctr = fltarr(ncenter*2)
  for i=0, ncenter-1 do begin
    ctr[i*2] = center[i]
    ctr[i*2+1] = center[i]
    input[*, i*2, *] = (*self.pvolume)[*, slices[0], *]
    input[*, i*2+1, *] = (*self.pvolume)[*, slices[1], *]
  endfor
  self.tomoParams.numSlices = ncenter*2
  tomo_recon, self.tomoParams, input, recon, angles=*self.angles, center=ctr

  ; Use the slice in center of range to get min/max of reconstruction for histogram
  r1 = recon[*,*,ncenter]
  r2 = recon[*,*,ncenter+1]

  mn1 = min(r1)
  if ((mn1) lt 0) then mn1=2*mn1 else mn1=0.5*mn1
  mx1 = max(r1)
  if ((mx1) gt 0) then mx1=2*mx1 else mx1=0.5*mx1
  binsize1=(mx1-mn1)/1.e4

  mn2 = min(r2)
  if ((mn2) lt 0) then mn2=2*mn2 else mn2=0.5*mn2
  mx2 = max(r2)
  if ((mx2) gt 0) then mx2=2*mx2 else mx2=0.5*mx2
  binsize2=(mx2-mn2)/1.e4

  npixels = n_elements(r1)
  for i=0, ncenter-1 do begin
    r1 = recon[*,*,i*2]
    r2 = recon[*,*,i*2+1]
    h1 = histogram(r1, min=mn1, max=mx1, bin=binsize1) > 1
    h1 = float(h1) / npixels
    h2 = histogram(r2, min=mn2, max=mx2, bin=binsize2) > 1
    h2 = float(h2) / npixels
    merit[i,0] = -total(h1*alog(h1))
    merit[i,1] = -total(h2*alog(h2))
  endfor
end

pro tomo::optimize_center_entropy_360, slices, center, merit
  ncenter = n_elements(center)
  s = size(*self.pvolume, /dimensions)
  tomoParams = self.tomoParams
  for i=0, ncenter-1 do begin
    angles = *self.angles
    cent = round(center[i])
    input1 = self->convert_360_data((*self.pvolume)[*, slices[0], *], cent, angles)
    angles = *self.angles
    input2 = self->convert_360_data((*self.pvolume)[*, slices[1], *], cent, angles)
    input = [[input1], [input2]]
    ctr = [cent, cent]
    tomo_params_update, tomoParams, input, paddedsinogramwidth=0
    tomo_recon, tomoParams, input, recon, angles=angles, center=ctr
    if (i eq 0) then begin
      ; Use the first reconstuction to get min/max of reconstruction for histogram
      r1 = recon[*,*,0]
      r2 = recon[*,*,1]
      mn1 = min(r1)
      if ((mn1) lt 0) then mn1=2*mn1 else mn1=0.5*mn1
      mx1 = max(r1)
      if ((mx1) gt 0) then mx1=2*mx1 else mx1=0.5*mx1
      binsize1=(mx1-mn1)/1.e4
      mn2 = min(r2)
      if ((mn2) lt 0) then mn2=2*mn2 else mn2=0.5*mn2
      mx2 = max(r2)
      if ((mx2) gt 0) then mx2=2*mx2 else mx2=0.5*mx2
      binsize2=(mx2-mn2)/1.e4
    endif
    r1 = recon[*,*,0]
    r2 = recon[*,*,1]
    npixels = n_elements(r1)
    h1 = histogram(r1, min=mn1, max=mx1, bin=binsize1) > 1
    h1 = float(h1) / npixels
    h2 = histogram(r2, min=mn2, max=mx2, bin=binsize2) > 1
    h2 = float(h2) / npixels
    merit[i,0] = -total(h1*alog(h1))
    merit[i,1] = -total(h2*alog(h2))
  endfor
end

pro tomo::optimize_center_mirror, slices, center, merit
  ; Determines rotation center from 2 images that are close to 180 degrees apart
  numShift = n_elements(center)
  if (max(*self.angles) gt 181) then begin
    stripWidth = 101
    mid_center = center[numShift/2]
    overlap = min([mid_center, self.nx-1 - mid_center])
    xmin = mid_center - overlap
    xmax = mid_center + overlap
    proj180 = float((*self.pvolume)[xmin:xmax, *, self.nz/2])
    shft = round((center - mid_center) * 2)
    shft = (shft < overlap) > (-overlap)
  endif else begin
    stripWidth = 11
    xmin = 0
    xmax = self.nx-1
    proj180 = float((*self.pvolume)[xmin:xmax, *, self.nz-1])
    shft = round((center - self.nx/2) * 2)
  endelse
  proj0 = float((*self.pvolume)[xmin:xmax, *, 0])
  dims = size(proj0, /dimensions)
  nx = dims[0]
  max_shift = max(abs(shft))
  for j=0, 1 do begin
    ymin = slices[j] - stripWidth[0]/2 > 0
    ymax = slices[j] + stripWidth[0]/2 < (self.ny-1)
    p0 = reform(proj0[*, ymin:ymax])
    p180 = reform(proj180[*, ymin:ymax])
    p180 = rotate(p180, 5)
    for i=0, numShift-1 do begin
      diff = p0 - shift(p180, shft[i], 0)
      diff = diff[max_shift:nx-max_shift-1, *]
      d = diff^2
      merit[i, j] = total(d)
    endfor
  endfor
end

pro tomo::set_rotation, slice1, center1, slice2, center2
  slices = float([slice1, slice2])
  center = float([center1, center2])
  ; Do a linear fit of slice number and rotation center
  coeffs = poly_fit(slices, center, 1)
  self.rotation_center = coeffs[0]
  self.rotation_center_slope = coeffs[1]
  print, 'Rotation center, slope =', self.rotation_center, self.rotation_center_slope
end

pro tomo::correct_rotation_tilt, angle
  if (not ptr_valid(self.pvolume)) then begin
    t = dialog_message('Must read in volume file first.', /error)
    return
  endif
  for i=0, self.nz-1 do begin
    proj = (*self.pvolume)[*,*,i]
    r = rot(proj, angle, cubic=-0.5)
    (*self.pvolume)[0,0,i] = r
    self->display_status, 'Correcting projection ' + strtrim(i+1, 2) + '/' + strtrim(self.nz, 2)
  endfor
end

function tomo::convert_360_data, input, center, angles
  ; This procedure converts 360 degree data to 180
  ; It can accept a single slice or 3-D data
  dims = size(input, /dimensions)
  if (n_elements(dims) eq 2) then begin
    input = reform(input, dims[0], 1, dims[1])
  endif
  dims = size(input, /dimensions)
  nx = dims[0]
  ny = dims[1]
  nz = dims[2]
  datatype = size(input, /type)
  if (center gt nx/2) then begin
    nxtotal = 2*center
  endif else begin
    nxtotal = 2*(nx - center)
  endelse
  output = make_array(nxtotal, ny, nz/2, type=datatype, /nozero)
  if (center gt nx/2) then begin
    output[0,0,0] = input[0:center-1, *, 0:nz/2-1]
    for i=0, nz/2-1 do begin
      output[center, 0, i] = rotate(input[0:center-1, *, i+nz/2], 5)
    endfor
  endif else begin
    output[nxtotal/2,0,0] = input[center:nx-1, *, 0:nz/2-1]
    for i=0, nz/2-1 do begin
      output[0, 0, i] = rotate(input[center:nx-1, *, i+nz/2], 5)
    endfor
  endelse
  angles = angles[0:nz/2-1]
  return, output
end

  ;+
  ; NAME:
  ;   RECONSTRUCT_SLICE
  ;
  ; PURPOSE:
  ;   Reconstructs a single slice in a tomography volume array.
  ;
  ; CATEGORY:
  ;   Tomography data processing
  ;
  ; CALLING SEQUENCE:
  ;   Result = RECONSTRUCT_SLICE(tomoParams, Slice, Volume)
  ;
  ; INPUTS:
  ;   tomoParams: A tomo_params structure
  ;   Slice:      The number of the slice to be reconstructed.
  ;   Volume:     The 3-D volume array from which the slice is extracted
  ;
  ; KEYWORD PARAMETERS:
  ;   ANGLES:
  ;       An array of angles (in degrees) at which each projection was collected.
  ;       If this keyword is not specified then the routine assumes that the data was
  ;       collected in evenly spaced increments of 180/n_angles.
  ;   CENTER:
  ;       The rotation centers.  If this keyword is not specified then the
  ;       center is assumed to be the center pixel of the image
  ;
  ; OUTPUTS:
  ;   This function returns the reconstructed slice.  It is a floating point
  ;   array of dimensions NX x NX.
  ;
  ; PROCEDURE:
  ;   Does the following:  extracts the slice, computes the sinogram with
  ;   centering and optional center tweaking, removes ring artifacts, filters
  ;   with a Shepp-Logan filter and backprojects.  It also prints the time
  ;   required for each step at the end.
  ;
  ; EXAMPLE:
  ;   r = reconstruct_slice(tomoParams, 264, volume)
  ;
  ; MODIFICATION HISTORY:
  ;   Written by: Mark Rivers, May 13, 1998
  ;   Many changes over time, see CVS log.
  ;-
function tomo::reconstruct_slice, input, center=center, sinogram=singram, cog=cog

  time1 = systime(1)
  input = reform(input)
  dims = size(input, /dimensions)
  if (n_elements(dims) ne 2) then begin
    message, 'tomo::reconstruct_slice, input must be 2-D'
  endif
  nx = dims[0]
  nz = dims[1]
  angles = *self.angles
  tomoParams = self.tomoParams
  
  ; If this is 360 degree data we need to convert to 180
  if (max(angles) gt 181) then begin
    input = self->convert_360_data(input, center, angles)
    tomo_params_update, tomoParams, input, paddedsinogramwidth=0
  endif

  if (tomoParams.reconMethod eq tomoParams.reconMethodTomoRecon) then begin
    tomo_recon, tomoParams, input, r, center=center, angles=angles
  endif

  if (tomoParams.reconMethod eq tomoParams.reconMethodGridrec) then begin
    t1 = input
    t2 = input
    s1 = sinogram(tomoParams, t1, angles, cog=cog)
    singram = s1
    s2 = sinogram(tomoParams, t2, angles, cog=cog)
    time3 = systime(1)

    if (tomoParams.ringWidth eq 0) then begin
      g1 = s1
      g2 = s2
    endif else begin
      g1 = remove_tomo_artifacts(s1, /rings, width=tomoParams.ringWidth)
      g2 = remove_tomo_artifacts(s2, /rings, width=tomoParams.ringWidth)
    endelse

    ; pad sinogram
    pad = tomoParams.paddedSinogramWidth
    if (pad ne 0) then begin
      size = size(g1)
      if (size[1] gt pad) then begin
        t = dialog_message('Padded sinogram too small, proceed without padding')
        pad = 0
      endif else begin
        data_temp1 = temporary(g1)
        data_temp2 = temporary(g2)
        g1 = fltarr(pad-1, size[2])
        g2 = fltarr(pad-1, size[2])
        p = (pad - size[1] -1)/2
        g1[p:p+size[1]-1, *] = data_temp1
        g2[p:p+size[1]-1, *] = data_temp2
        center = center + p
      endelse
    endif

    time4 = systime(1)
    gridrec, tomoParams, g1, g2, angles, r, unused, center=center

    ; crop results if sinogram was padded
    if (pad ne 0 AND size[1] le pad) then begin
      r = r[p:p+size[1] - 1,p:p+size[1] - 1]
      center = center - p
    endif

    ; Scale results
    if ((tomoParams.reconScale ne 0.) and (tomoParams.reconScale ne 1.0)) then begin
      r = r * tomoParams.reconScale
    endif

    time5 = systime(1)
    print, 'Gridrec: center= ', center
    print, 'Time to compute sinogram: ', time3-time2
    print, 'Time to reconstruct:      ', time5-time4
  endif

  if (tomoParams.reconMethod eq tomoParams.reconMethodBackproject) then begin
    s1 = sinogram(tomoParams, t1, angles, cog=cog)
    cent = -1
    time3 = systime(1)
    if (tomoParams.ringWidth eq 0) then begin
      g1 = s1
    endif else begin
      g1 = remove_tomo_artifacts(s1, /rings, width=tomoParams.ringWidth)
    endelse

    time4 = systime(1)
    ss1 = tomo_filter(g1, filter_size=tomoParams.BP_filterSize, filter_name=string(tomoParams.BP_filterName))
    singram = ss1
    time5 = systime(1)
    if (center ge (nx-1)/2) then ctr = center else ctr = center + 2*abs(round(center - (nx-1)/2))
    r = backproject(tomoParams, ss1, angles, center=ctr)
    time6 = systime(1)
    ; Scale results
    if ((tomoParams.reconScale ne 0.) and (tomoParams.reconScale ne 1.0)) then begin
      r = r * tomoParams.reconScale
    endif
    print, 'Backproject: center= ', center
    print, 'Time to compute sinogram: ', time3-time2
    print, 'Time to filter sinogram:  ', time5-time4
    print, 'Time to backproject:      ', time6-time5
  endif

  return, r
end


;+
; NAME:
;   TOMO::RECONSTRUCT_VOLUME
;
; PURPOSE:
;   This procedure reconstructs a complete 3-D data set (X, Y, Theta) into a
;   3-D (X, Y, Z) volume.  It reads its input from disk and writes its output
;   back to disk.
;
; CATEGORY:
;   Tomography.
;
; CALLING SEQUENCE:
;   TOMO->RECONSTRUCT_VOLUME, Base_file
;
; INPUTS:
;   Base_file:
;       The base file name.  The input file is assumed to be named
;       base_file+'.volume', and the output file will be named
;       base_file+'_recon.volume'.  The input file is read with
;       READ_TOMO_VOLUME and the output file is written with WRITE_TOMO_VOLUME.
;
; KEYWORD PARAMETERS:
;   This procedure accepts all keywords accepted by READ_TOMO_VOLUME and
;   RECONSTRUCT_SLICE and simply passes them to those routines via keyword
;   inheritance.
;
;   CENTER
;       This keyword, which is passed to RECONSTRUCT_SLICE can either be a scaler (the
;       normal case) or a 2-element array.  If it is a 2-element array then the
;       center value passed to RECONSTRUCT_SLICE is interpolated between CENTER[0]
;       for the first slice of the volume file to CENTER[1] at the last slice of the
;       volume file.  This can be useful if the optimum center varies with slice
;       depth.
;   ANGLES
;       An optional array of angles (in degrees) at which each projection was taken.
;       This keyword is passed to RECONSTRUCT_SLICE.  If this keyword is missing
;       then RECONSTRUCT_SLICE assumes even spacing from 0 to 180-delta degrees.
;   SCALE
;       The scale factor by which the data should be multiplied before writing as
;       short integers to the output file.  The default is 1.e6.  Since the
;       attenuation values are per-pixel, and are typically 0.001, this leads to
;       integers in the range of 10,000.  If there are highly attenuating pixels the
;       scale factor may need to be decreased to 1-5e5 to avoid integer overflow.
;       The inverse of the SCALE is stored as the attribute volume:data_scale
;       in the netCDF file.
;;
; RESTRICTIONS:
;   This procedure assumes a naming convention for the input and output files.
;   The output is stored as 16 bit integers to save memory and disk space.
;   This can reduce the dynamic range of the reconstructed data.
;
; PROCEDURE:
;   This procedure simply does the following:
;       - Reads a corrected input volume (X, Y, Theta) which is typically
;         created with READ_TOMO_DATA
;       - Calls RECONSTRUCT_SLICE for each row (slice) in the input volume
;       - Scales the reconstructed data (floating poing) by 10000 and converts
;         to 16 bit integers
;       - Writes the reconstructed 3-D volume (X, Y, Z) back to disk with
;         WRITE_TOMO_VOLUME
;
; EXAMPLE:
;   reconstruct_volume, 'FOSSIL1', /AUTO_CENTER
;
; MODIFICATION HISTORY:
;   Written by:    Mark Rivers, April 23, 1999
;   30-APR-1999 MLR  Fixed bug introduced by new version of sinogram, need
;                    to get size of reconstructed slices after centering
;   18-MAY-1999 MLR  Changed formal parameter _extra to _ref_extra to allow
;                    CENTER keyword value to be returned from sinogram (via
;                    reconstruct_slice).
;   23-FEB-2000 MLR  Pass extra keywords to read_tomo_volume
;   7-MAR-2000  MLR  Added support for GRIDREC reconstruction, which reconstructs
;                    2 slices at once.
;   2-JAN-2001  MLR  Added CENTER keyword. If it is a 2-element array then the
;                    center is interpolated.
;   11-APR-2001 MLR  Incorporated the previous routine RECONSTRUCT_VOLUME into the
;                    TOMO class library.
;                    This procedure now updates the .SETUP file with the center
;                    value which was used for the reconstruction.
;   12-APR-2001 MLR  Added SCALE and angles keywords since we need to process them
;                    here.
;   11-APR-2002 MLR  Fixed bug introduced on 02-APR with center
;-

pro tomo::reconstruct_volume, data_type=data_type, debug=debug, $
                              write_output=write_output, netcdf=netcdf

  tStart = systime(1)
  self->display_status, 'Initializing reconstruction ...', 1
  if (keyword_set(netcdf)) then output_file = self.base_filename + 'recon.nc' $
                           else output_file = self.base_filename + 'recon.h5'
  ; Set write_output keyword by default
  if (n_elements(write_output) eq 0) then write_output=1
  
  if (n_elements(debug) ne 0) then self.debug_level = debug

  angles = *self.angles
  tomoParams = self.tomoParams
  dims = size(*self.pvolume, /dimensions)
  numPixels = dims[0]
  numSlices = dims[1]
  numProjections = dims[2]
  self.data_scale = 1./tomoParams.reconScale
  center = self.rotation_center + findgen(self.ny)*self.rotation_center_slope

  ; If this is 360 degree data we need to convert to 180
  if (max(angles) gt 181) then begin
    self->display_status, 'Converting 360 data ...', 1
    ; convert_360_data can only use a single center value. Use the one for the center slice.
    center_value =  round(center[numSlices/2])
    *self.pvolume = self->convert_360_data(*self.pvolume, center_value, angles)
    center = center_value + findgen(self.ny)*0
    tomo_params_update, tomoParams, *self.pvolume, paddedsinogramwidth=0
  endif
  tConvert360 = systime(1)
  if (size(*self.pvolume, /tname) ne 'FLOAT') then begin
    self->display_status, 'Converting to float ...', 1
    *self.pvolume = float(*self.pvolume)
  endif
  tConvertFloat = systime(1)
  recon = fltarr(numPixels, numPixels, numSlices, /nozero)
  self->display_status, 'Beginning reconstruction ...', 1
  if (tomoParams.reconMethod eq tomoParams.reconMethodTomoRecon) then begin
    ; Reconstruct volume
    tomo_recon, tomoParams, *self.pvolume, recon, angles=angles, wait=0, create=1, center=center
    repeat begin
      tomo_recon_poll, reconComplete, slicesRemaining
      wait, 0.1
      self->display_status, 'Reconstructing slice: ' $
        + strtrim(numSlices-slicesRemaining,2) + '/' + strtrim(numSlices,2), 2
    endrep until reconComplete
  endif else begin
    nrows = self.ny
    ; This procedure reconstructs all of the slices for a tomography data set
    ; If we are using GRIDREC to reconstruct then we get 2 slices at a time
    for i=0, nrows-1, 2 do begin
      self->display_status, 'Reconstructing slice ' + strtrim(i,2) + '/' + strtrim(nrows-1,2), 2
      r = reconstruct_slice((*self.pvolume)[*,[i,(i+1)<self.ny],*], r2, center=center[i])
      if (i eq 0) then begin
        ncols = n_elements(r[*,0])
        recon = intarr(ncols, ncols, nrows, /nozero)
      endif
      recon[0,0,i] = round(r)
      if ((n_elements(r2) ne 0) and (i ne nrows-1)) then begin
        recon[0,0,i+1] = round(r2)
      endif
      if (self.check_abort()) then begin
        self->display_status, 'Reconstruction aborted', 1
        return
      endif
    endfor
  endelse
  
  ptr_free, self.pvolume
  tReconDone = systime(1)
  status = self->write_setup(self.base_filename + '.setup')
  
  self->display_status, 'Converting to output data type ...', 1
  if (data_type eq 'Int16') then begin
    recon = fix(recon)
  endif
  if (data_type eq 'UInt16') then begin
    recon = uint(recon + 32767.)
  endif
  tConvertInt = systime(1)

  self.image_type = 'RECONSTRUCTED'
  dims = size(recon, /dimensions)
  self.nx = dims[0]
  self.ny = dims[1]
  self.nz = dims[2]

  if (write_output) then begin
    self->display_status, 'Writing reconstructed file ...', 1
    self->write_volume, output_file, recon, netcdf=netcdf
  endif
  self.pvolume = ptr_new(recon, /no_copy)
  tEnd = systime(1)
  print, '     Convert 360 to 180:', tConvert360 - tStart
  print, ' Convert input to float:', tConvertFloat - tConvert360
  print, '            Reconstruct:', tReconDone-tConvertFloat
  print, 'Convert output to ' + data_type + ':', tConvertInt - tReconDone
  print, '             Write file:', tEnd-tConvertInt
  print, '             Total time:', tEnd-tStart
  self->display_status, 'Reconstruction complete.', 1
end


  ;+
  ; NAME:
  ;   TOMO::WRITE_VOLUME
  ;
  ; PURPOSE:
  ;   Writes 3-D volume files to be read later by READ_TOMO_VOLUME.
  ;   There are currently 2 file formats supported:
  ;   1) The old APS-specific architecture-dependent binary format.
  ;      In general this format should no longer be used, since it does not
  ;      contain information on the dark current, centering, etc.  It is also
  ;      not nearly as portable as netCDF, since if the IDL routine
  ;      READ_TOMO_VOLUME is not used to read the files then user-code must
  ;      handle byte-swapping, etc.
  ;   2) netCDF format files.  This is the format which should generally be used,
  ;      since it supports additional information like the dark current and it is
  ;      very portable.  Many data-handling packages support netCDF and there are
  ;      netCDF libraries available on virtually all platforms.
  ;
  ; CATEGORY:
  ;   Tomography data processing
  ;
  ; CALLING SEQUENCE:
  ;   TOMO->WRITE_VOLUME, File, Volume
  ;
  ; INPUTS:
  ;   File:
  ;       The name of the volume file to be written.
  ;   Volume:
  ;       The 3-D volume data to be written.  This must be a 3-D 16-bit integer
  ;       array.  The dimensions are NX, NY, NANGLES or NX, NY, NZ
  ;
  ; KEYWORD PARAMETERS:
  ;   XOFFSET:
  ;   YOFFSET:
  ;   ZOFFSET:  The [X,Y,Z] offsets in the disk array to begin writing to.  Default
  ;             is [0,0,0]
  ;   XMAX:
  ;   YMAX:
  ;   ZMAX:     The maximum [X,Y,Z] size of the array on disk.  Valid only when the
  ;             file is first created, i.e. if APPEND is not specified.  Default
  ;             is the size of the Volume array in each dimension.
  ;   APPEND:   Open an existing file for appending or overwriting data.  Default is to
  ;             APPEND=0 which creates a new file.
  ;   NETCDF:
  ;       Set this keyword  to write files in netCDF file format.  This is the
  ;       default.  If NETCDF=0 then files are written in the old APS format.
  ;   RAW:      The data are raw projections (X,Y,THETA), not normalized for flat field
  ;   CORRECTED: The data are flat-field normalized projections (X,Y,THETA)
  ;   RECONSTRUCTED:  The data are reconstructed sections (X,Y,Z)
  ;
  ; RESTRICTIONS:
  ;   The old APS format files are written using little-endian byte order.
  ;   When this routine writes such files it swaps the byte order if it is
  ;   running on a big-endian machine.  Thus that file format
  ;   is most efficient on little-endian machines (Intel, DEC).
  ;
  ; EXAMPLE:
  ;   tomo = obj_new('tomo', 'test.setup')
  ;   tomo->WRITE_VOLUME, 'diamond2.volume', volume
  ;
  ; MODIFICATION HISTORY:
  ;   Written by:     Mark Rivers, May 13, 1998
  ;   26-JAN-2000  MLR  Added /swap_if_big_endian keyword to openw to allow
  ;                     files to be read on big-endian machines.
  ;   11-APR-2001  MLR  Added support for netCDF file format.  Added NETCDF keyword.
  ;   5-NOV-2001   MLR  Added XOFFSET, YOFFSET, ZOFFSET, XMAX, YMAX, ZMAX, and
  ;                     APPEND keywords
  ;   24-JUN-2002  MLR  Fixed bug if input volume was 2-D rather than 3-D.
  ;-
  ;-
pro tomo::write_volume, file, volume, netcdf=netcdf, append=append, $
  xoffset=xoffset, yoffset=yoffset, zoffset=zoffset, $
  xmax=xmax, ymax=ymax, zmax=zmax

   size = size(volume)
  ; If this is a 2-D array fake it out by setting third dimension to 1
  if (size[0] eq 2) then size[3]=1

  if (keyword_set(netcdf)) then begin
    ; netCDF file format
    if (keyword_set(append)) then begin
      ; If APPEND keyword is specifified then open an existing netCDF file
      file_id = ncdf_open(file, /write)
      ; Get the variable id
      vol_id   = ncdf_varid (file_id, 'VOLUME')
      if (vol_id eq -1) then begin
        ncdf_close, file_id
        message, 'No VOLUME variable in netCDF file'
      endif
    endif else begin
      ; else create a new netCDF file
      ; Create netCDF file
      file_id = ncdf_create(file, /clobber)
      ncdf_control, file_id, fill=0
      
      ; Create dimensions
      if (n_elements(xmax) eq 0) then xmax=size[1]
      if (n_elements(ymax) eq 0) then ymax=size[2]
      if (n_elements(zmax) eq 0) then zmax=size[3]
      nx_id = ncdf_dimdef(file_id, 'NX', xmax)
      ny_id = ncdf_dimdef(file_id, 'NY', ymax)
      nz_id = ncdf_dimdef(file_id, 'NZ', zmax)
      
      ; Create variables
      vol_id = ncdf_vardef(file_id, 'VOLUME', [nx_id, ny_id, nz_id], /SHORT)
      
      ; Create attributes.  Replace null strings with a blank.
      if (self.title ne '') then str=self.title else str=' '
      ncdf_attput, file_id, /GLOBAL, 'title', str
      if (self.operator ne '') then str=self.operator else str=' '
      ncdf_attput, file_id, /GLOBAL, 'operator', str
      if (self.camera ne '') then str=self.camera else str=' '
      ncdf_attput, file_id, /GLOBAL, 'camera', str
      if (self.sample ne '') then str=self.sample else str=' '
      ncdf_attput, file_id, /GLOBAL, 'sample', str
      if (self.image_type ne '') then str=self.image_type else str=' '
      ncdf_attput, file_id, /GLOBAL, 'image_type', str
      ncdf_attput, file_id, /GLOBAL, 'energy', self.energy
      ncdf_attput, file_id, /GLOBAL, 'dark_current', self.dark_current
      ncdf_attput, file_id, /GLOBAL, 'center', self.rotation_center
      ncdf_attput, file_id, /GLOBAL, 'x_pixel_size', self.x_pixel_size
      ncdf_attput, file_id, /GLOBAL, 'y_pixel_size', self.y_pixel_size
      ncdf_attput, file_id, /GLOBAL, 'z_pixel_size', self.z_pixel_size
      if (ptr_valid(self.angles)) then $
        ncdf_attput, file_id, /GLOBAL, 'angles', *(self.angles)
      if (self.data_scale ne 0) then scale=self.data_scale else scale=1.0
      ncdf_attput, file_id, vol_id,  'scale_factor',  scale
      ; Put the file into data mode.
      ncdf_control, file_id, /endef
    endelse
    
    ; Write volume data to the file
    offset = [0,0,0]
    if (n_elements(xoffset) ne 0) then offset[0]=xoffset
    if (n_elements(yoffset) ne 0) then offset[1]=yoffset
    if (n_elements(zoffset) ne 0) then offset[2]=zoffset
    count = [size[1], size[2], size[3]]
    stride=[1,1,1]
    error = 0
    catch, error
    if (error ne 0) then begin
      catch, /cancel
      ncdf_close, file_id
      print, 'Error calling ncdf_varput: ' + !error_state.msg
      help, file_id, vol_id, volume, offset, count, stride
      print, 'offset=', offset, ' count=', count, ' stride=', stride
      message, !error_state.msg
      return
    endif
    ncdf_varput, file_id, vol_id, volume, $
      offset=offset, count=count, stride=stride
      
    ; Close the file
    ncdf_close, file_id
  endif else begin
    ; Write HDF5 file
    file_id = h5f_create(file)
    group_id = h5g_create(file_id, 'exchange')
    self->write_hdf5_dataset, group_id, 'data', volume
    h5g_close, group_id
    group_id = h5g_create(file_id, 'process')
    self->write_hdf5_dataset, group_id, 'image_type', self.image_type
    self->write_hdf5_dataset, group_id, 'angles', *self.angles
    self->write_hdf5_dataset, group_id, 'x_pixel_size', self.x_pixel_size
    self->write_hdf5_dataset, group_id, 'y_pixel_size', self.y_pixel_size
    self->write_hdf5_dataset, group_id, 'z_pixel_size', self.z_pixel_size
    self->write_hdf5_dataset, group_id, 'rotation_center', self.rotation_center
    self->write_hdf5_dataset, group_id, 'rotation_center_slope', self.rotation_center_slope
    self->write_hdf5_dataset, group_id, 'data_scale', self.data_scale
    self->write_hdf5_dataset, group_id, 'data_offset', self.data_offset
    h5g_close, group_id
    h5f_close, file_id
  endelse
  
end

pro tomo::write_hdf5_dataset, group_id, dataset_name, data
  datatype_id = h5t_idl_create(data)
  dims = size(data, /dimensions)
  if (dims[0] eq 0) then dims = 1
  dataspace_id = h5s_create_simple(dims)
  ; Create dataset in the output file
  dataset_id = h5d_create(group_id, dataset_name, datatype_id, dataspace_id)
  ; Write data to dataset
  h5d_write, dataset_id, data
  ; Close things
  h5d_close, dataset_id
  h5s_close, dataspace_id
  h5t_close, datatype_id
end

  ;+
  ; NAME:
  ;   TOMO::READ_VOLUME
  ;
  ; PURPOSE:
  ;   Reads in 3-D volume files written by WRITE_TOMO_VOLUME.  These are binary
  ;   files written in little endian.  This file format is "temporary" until we
  ;   decide on a portable self-describing binary format, such as HDF or netCDF.
  ;   Both intermediate volume files (after preprocessing) and final
  ;   reconstructions are currently stored in this format.
  ;
  ; CATEGORY:
  ;   Tomography data processing
  ;
  ; CALLING SEQUENCE:
  ;   Result = READ_TOMO_VOLUME(File)
  ;
  ; INPUTS:
  ;   File:
  ;       The name of the volume file to be read.  If this is not specified then
  ;       the function will use DIALOG_PICKFILE to allow the user to select a
  ;       file.
  ; KEYWORD PARAMETERS:
  ;   XRANGE=[xstart, xstop]
  ;       The range of X values to read in.  The default is to read the entire
  ;       X range of the data
  ;   YRANGE=[ystart, ystop]
  ;       The range of Y values to read in.  The default is to read the entire
  ;       Y range of the data
  ;   ZRANGE=[zstart, zstop]
  ;       The range of Z values to read in.  The default is to read the entire
  ;       Z range of the data
  ;
  ; OUTPUTS:
  ;   This function returns a 3-D 16-bit integer array.  The dimensions are
  ;   NX, NY, NZ
  ;
  ; RESTRICTIONS:
  ;   These files are written using the little-endian byte order and
  ;   floating point format.  When this routine reads the files it swaps the
  ;   byte order if it is running on a big-endian machine.  Thus the file format
  ;   is most efficient on little-endian machines (Intel, DEC).
  ;
  ; EXAMPLE:
  ;   volume = READ_TOMO_VOLUME('diamond2.volume')
  ;
  ; MODIFICATION HISTORY:
  ;   Written by: Mark Rivers, May 13, 1998
  ;   06-APR-1999  MLR  Made file input optional, puts up dialog if it is not
  ;                     specified
  ;   25-JAN-2000  MLR  Added /swap_if_big_endian keyword to openr to allow
  ;                     files to be read on big-endian machines.
  ;   23-FEB-2000  MLR  Added xrange, yrange, zrange keywords
  ;   11-APR-2001  MLR  Added support for netCDF file format.
  ;-
 function tomo::read_volume, file, store=store, xrange=xrange, yrange=yrange, zrange=zrange
 
  if (n_elements(file) eq 0) then file = dialog_pickfile(/read, /must_exist)
  if file eq "" then return, 0
  t0 = systime(1)
  self->set_file_components, file
  
  if (keyword_set(store)) then begin
    ptr_free, self.pvolume
  endif

  if (ncdf_is_ncdf(file)) then begin
    ; This is a netCDF file
    file_id = ncdf_open(file, /nowrite)
    ; Process the global attributes
    status = ncdf_inquire(file_id)
    for i=0, status.ngatts-1 do begin
      name = ncdf_attname(file_id, /global, i)
      ncdf_attget, file_id, /global, name, value
      case name of
        'title':        self.title =        strtrim(value,2)
        'operator':     self.operator =     strtrim(value,2)
        'camera':       self.camera =       strtrim(value,2)
        'sample':       self.sample =       strtrim(value,2)
        'image_type':   self.image_type =   strtrim(value,2)
        'energy':       self.energy =       value
        'dark_current': self.dark_current = value
        'center':       self.rotation_center = value
        'x_pixel_size': self.x_pixel_size = value
        'y_pixel_size': self.y_pixel_size = value
        'z_pixel_size': self.z_pixel_size = value
        'angles':       begin
          ptr_free, self.angles
          self.angles = ptr_new(value)
        end
        else:
      endcase
    endfor
    ; Get the variable id
    vol_id   = ncdf_varid (file_id, 'VOLUME')
    
    if (vol_id eq -1) then begin
      ncdf_close, file_id
      message, 'No VOLUME variable in netCDF file'
    endif
    
    ; Get information about the volume variable
    vol_info = ncdf_varinq(file_id, vol_id)
    
    ; If we are to read the entire array things are simpler
    if (n_elements(zrange) eq 0) and (n_elements(yrange) eq 0) and $
      (n_elements(xrange) eq 0) then begin
      ncdf_varget, file_id, vol_id, volume
    endif else begin
      if (vol_info.ndims ne 3) then begin
        ncdf_close, file_id
        message, 'VOLUME variable does not have 3 dimensions in netCDF file'
      endif
      ncdf_diminq, file_id, vol_info.dim[0], name, nx
      ncdf_diminq, file_id, vol_info.dim[1], name, ny
      ncdf_diminq, file_id, vol_info.dim[2], name, nz
      if (n_elements(xrange) eq 0) then xrange = [0, nx-1]
      if (n_elements(yrange) eq 0) then yrange = [0, ny-1]
      if (n_elements(zrange) eq 0) then zrange = [0, nz-1]
      
      ; Make sure ranges are valid
      xrange = (xrange > 0) < (nx-1)
      yrange = (yrange > 0) < (ny-1)
      zrange = (zrange > 0) < (nz-1)
      ncdf_varget, file_id, vol_id, volume, $
        offset=[xrange[0], yrange[0], zrange[0]], $
        count=[xrange[1]-xrange[0]+1, $
        yrange[1]-yrange[0]+1, $
        zrange[1]-zrange[0]+1]
    endelse
    for i=0, vol_info.natts-1 do begin
      name = ncdf_attname(file_id, vol_id, i)
      ncdf_attget, file_id, vol_id, name, value
      case name of
        'scale_factor': self.data_scale = value
      endcase
    endfor
    ; Close the netCDF file
    ncdf_close, file_id

  endif else begin
    ; File must be HDF5
    volume                     = h5_getdata(file, '/exchange/data')
    self.image_type            = h5_getdata(file, '/process/image_type')
    angles                     = h5_getdata(file, '/process/angles')
    self.angles = ptr_new(angles)
    self.x_pixel_size          = h5_getdata(file, '/process/x_pixel_size')
    self.y_pixel_size          = h5_getdata(file, '/process/y_pixel_size')
    self.x_pixel_size          = h5_getdata(file, '/process/z_pixel_size')
    self.rotation_center       = h5_getdata(file, '/process/rotation_center')
    self.rotation_center_slope = h5_getdata(file, '/process/rotation_center_slope')
    self.data_scale            = h5_getdata(file, '/process/data_scale')
    self.data_offset           = h5_getdata(file, '/process/data_offset')
  endelse

  print, 'Time to read file=', systime(1)-t0
  if (keyword_set(store)) then begin
    dims = size(volume, /dimensions)
    self.pvolume = ptr_new(volume, /no_copy)
    self.nx = dims[0]
    self.ny = dims[1]
    self.nz = dims[2]
    return, 0
  endif else begin
    return, volume
  endelse
end

function tomo::get_angles
  if (ptr_valid(self.angles)) then begin
    return, *self.angles
  endif else begin
    return, 0
  endelse
end

pro tomo::set_angles, angles
  ptr_free, self.angles
  self.angles = ptr_new(angles)
end


;+
; NAME:
;   TOMO::WRITE_SETUP
;
; PURPOSE:
;   This function writes the setup information for a tomography data set to an
;   ASCII file.
;
; CATEGORY:
;   Tomography
;
; CALLING SEQUENCE:
;   result = TOMO->WRITE_SETUP(File)
;
; INPUTS:
;   File:
;       The name of the output file.
;
; OUTPUTS:
;   This function returns 0 if it was unable to write the file, 1 if it was
;   successful.
;
; EXAMPLE:
;       IDL>  status = TOMO->WRITE_SETUP('Sample1.setup')
;
; MODIFICATION HISTORY:
;   Written by: Mark Rivers, Aug. 2001?
;-

function tomo::write_setup, file
  openw, lun, file, error=error, /get_lun, width=1024
  if (error ne 0) then return, 0
  printf, lun, 'TITLE: ', self.title
  printf, lun, 'OPERATOR: ', self.operator
  printf, lun, 'CAMERA: ', self.camera
  printf, lun, 'SAMPLE: ', self.sample
  if (ptr_valid(self.comments)) then comments = *self.comments
  for i=0, n_elements(comments)-1 do begin
    printf, lun, 'COMMENT: ', comments[i]
  endfor
  printf, lun, 'DARK_CURRENT: ', self.dark_current
  printf, lun, 'CENTER: ', self.rotation_center
  printf, lun, 'ENERGY: ',  self.energy
  printf, lun, 'X_PIXEL_SIZE: ', self.x_pixel_size
  printf, lun, 'Y_PIXEL_SIZE: ', self.y_pixel_size
  printf, lun, 'Z_PIXEL_SIZE: ', self.z_pixel_size
  free_lun, lun
  return, 1
end


;+
; NAME:
;   TOMO::READ_SETUP
;
; PURPOSE:
;   This function reads the setup information for a tomography data set from an
;   ASCII file.
;
; CATEGORY:
;   Tomography
;
; CALLING SEQUENCE:
;   result = TOMO->READ_SETUP(File)
;
; INPUTS:
;   File:
;       The name of the input file.
;
; OUTPUTS:
;   This function returns 0 if it was unable to read the file, 1 if it was
;   successful.
;
; EXAMPLE:
;       IDL>  status = TOMO->READ_SETUP('Sample1.setup')
;
; MODIFICATION HISTORY:
;   Written by: Mark Rivers, Aug. 2001?
;-

function tomo::read_setup, file
  ; Clear any existing information
;  self->clear_setup
  ncomments = 0
  comment = strarr(100)
  line = ''
  openr, lun, file, error=error, /get_lun, width = 1024
  if (error ne 0) then return, 0
  while (not eof(lun)) do begin
    readf, lun, line
    pos = strpos(line, ' ')
    tag = strupcase(strmid(line, 0, pos))
    value = strtrim(strmid(line, pos, 1000), 2)
    case tag of
      'TITLE:'     :  self.title = value
      'OPERATOR:'  :  self.operator = value
      'CAMERA:'    :  self.camera = value
      'SAMPLE:'    :  self.sample = value
      'COMMENT:'   :  begin
        comment[ncomments] = value
        ncomments = ncomments + 1
      end
      'DARK_CURRENT:'  :  self.dark_current = value
      'CENTER:'        :  self.rotation_center = value
      'ENERGY:'        :  self.energy = value
      'X_PIXEL_SIZE:'  :  self.x_pixel_size = value
      'Y_PIXEL_SIZE:'  :  self.y_pixel_size = value
      'Z_PIXEL_SIZE:'  :  self.z_pixel_size = value
    endcase
  endwhile
  if (ncomments gt 0) then begin
    comment = comment[0:ncomments-1]
    ptr_free, self.comments
    self.comments =  ptr_new(comment)
  endif
  free_lun, lun
  return, 1
end

;+
; NAME:
;   TOMO::GET_SETUP
;
; PURPOSE:
;   This function returns the setup information for a tomography data set
;
; CATEGORY:
;   Tomography
;
; CALLING SEQUENCE:
;   setup = TOMO->GET_SETUP()
;
; OUTPUTS:
;   This function returns a structure of type {TOMO} containing the information
;   about the tomography dataset.  The current definition of the {TOMO}
;   structure is:
;       {tomo, $
;        title: " ", $
;        operator: " ", $
;        camera: " ", $
;        sample: " ", $
;        comments: ptr_new(), $
;        image_type: " ", $  ; "RAW", "CORRECTED" or "RECONSTRUCTED"
;        dark_current: 0., $
;        center: 0., $
;        energy: 0., $
;        x_pixel_size: 0., $
;        y_pixel_size: 0., $
;        z_pixel_size: 0., $
;        data_scale: 0., $
;        nx:     0L, $
;        ny:     0L, $
;        nz:     0L, $
;        angles: ptr_new() $
;    }
;   This definition is subject to change in the future, but the above fields
;   will not change, new fields may be added.
;
; EXAMPLE:
;       IDL>  tomo = obj_new('TOMO')
;       IDL>  status = TOMO->READ_SETUP('Sample1.setup')
;       IDL>  setup = TOMO->GET_SETUP()
;
; MODIFICATION HISTORY:
;   Written by: Mark Rivers, Nov. 18, 2001
;-

function tomo::get_struct
  t = {tomo}
  for i=0, n_tags(t)-1 do begin
    t.(i)=self.(i)
  endfor
  return, t
end


function tomo::init, file=file, status_widget=status_widget, abort_widget=abort_widget, debug_level=debug_level
  if (n_elements(file) ne 0) then status = self->read_setup(file)
  if (n_elements(status_widget) ne 0) then self.status_widget = status_widget
  if (n_elements(abort_widget) ne 0) then self.abort_widget = abort_widget
  if (n_elements(debug_level) ne 0) then self.debug_level = debug_level else self.debug_level=1
  self.tomoParams = tomo_params_create()
  return, 1
end

pro tomo::set_file_components, input_file
  path = file_dirname(input_file)
  file = file_basename(input_file)
  self.input_filename = input_file
  self.directory = path
  ; Construct the base file name
  base_file = file
  ; First remove any extension
  pos = base_file.LastIndexOf('.')
  if (pos ge 0) then base_file = base_file.remove(pos)
  ; Remove the string "norm"
  pos = base_file.LastIndexOf('norm', /fold_case)
  if (pos ge 0) then base_file = base_file.remove(pos)
  ; Remove the string "recon"
  pos = base_file.LastIndexOf('recon', /fold_case)
  if (pos ge 0) then base_file = base_file.remove(pos)
  self.base_filename = base_file
end

pro tomo::cleanup
  ptr_free, self.comments
  ptr_free, self.angles
  ptr_free, self.pvolume
  ptr_free, self.pflats
  ptr_free, self.pdarks
end

pro tomo::clear_setup
  self.title=""
  self.operator=""
  self.camera=""
  self.sample=""
  ptr_free, self.comments
  self.comments=ptr_new()
  self.image_type=""
  self.dark_current=0.
  self.rotation_center=0.
  self.energy=0.
  self.x_pixel_size=0.
  self.y_pixel_size=0.
  self.z_pixel_size=0.
  self.data_scale=0.
  self.nx=0L
  self.ny=0L
  self.nz=0L
  ptr_free, self.angles
  self.angles=ptr_new()
end


pro tomo__define
  tomo = $
   {tomo, $
    tomoParams: tomo_params_create(), $
    pvolume: ptr_new(), $
    pflats: ptr_new(), $
    pdarks: ptr_new(), $
    input_filename: " ", $
    base_filename: " ", $
    directory: " ", $
    title: " ", $
    operator: " ", $
    camera: " ", $
    sample: " ", $
    comments: ptr_new(), $
    image_type: " ", $  ; "RAW", "CORRECTED" or "RECONSTRUCTED"
    dark_current: 0., $
    rotation_center: 0., $
    rotation_center_slope: 0., $
    energy: 0., $
    x_pixel_size: 0., $
    y_pixel_size: 0., $
    z_pixel_size: 0., $
    data_scale: 0., $
    data_offset: 0., $
    nx:     0L, $
    ny:     0L, $
    nz:     0L, $
    rotation_start: 0., $
    rotation_step: 0., $
    angles: ptr_new(), $
    status_widget: 0L, $
    abort_widget: 0L, $
    debug_level: 0L $
  }
end
