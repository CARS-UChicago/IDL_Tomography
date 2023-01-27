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
  ptr_free, self.pVolume

  ; See if the file is netCDF
  if (ncdf_is_ncdf(filename)) then begin
    self.baseFilename = filename.remove(-4)
    file = self.baseFilename + '1.nc'
    self->display_status, 'Reading ' + file
    flat1       = read_nd_netcdf(file)
    file = self.baseFilename + '3.nc'
    self->display_status, 'Reading ' + file
    flat2       = read_nd_netcdf(file)
    file = self.baseFilename + '2.nc'
    self->display_status, 'Reading ' + file
    projections = read_nd_netcdf(file)
    dims = size(projections, /dimensions)
    self.nx = dims[0]
    self.ny = dims[1]
    self.nz = dims[2]
    num_projections = dims[2]
    flats = [[[flat1]], [[flat2]]]
    status = self.read_setup(self.baseFilename + '.setup')
    ; pDarks is set in read_setup
    rotation_start = 0.
    rotation_step = 180./num_projections
    self.rotationCenter = dims[0]/2.
    self.rotationCenterSlope = 0.
    return
  endif

  ; See if the file is HDF5
  if (h5f_is_hdf5(filename)) then begin
    flats = h5_getdata(filename, '/exchange/data_white')
    projections = h5_getdata(filename, '/exchange/data')
    dims = size(projections, /dimensions)
    num_projections = dims[2]
    ; For now we assume there are no dark field images
    dark_current = h5_getdata(filename, '/process/acquisition/dark_fields/dark_field_value')
    darks = fix(dark_current[0])
    rotation_start = (h5_getdata(filename, '/process/acquisition/rotation/rotation_start'))[0]
    rotation_step = (h5_getdata(filename, '/process/acquisition/rotation/rotation_step'))[0]
    camera = h5_getdata(filename, '/measurement/instrument/detector/model')
    self.xPixelSize = h5_getdata(filename, '/measurement/instrument/detection_system/objective/resolution')
    self.yPixelSize = self.xPixelSize
    self.zPixelSize = self.xPixelSize
    self.nx = dims[0]
    self.ny = dims[1]
    self.nz = dims[2]
    self->read_sample_config
    (*(self.pSampleConfig))['Camera'] = camera[0]
  endif
 
  self.imageType = 'RAW'
  angles = float(rotation_start + rotation_step * findgen(num_projections))
  self.pAngles = ptr_new(angles, /no_copy)
  self.pVolume = ptr_new(projections, /no_copy)
  self.pFlats = ptr_new(flats, /no_copy)
  self.pDarks = ptr_new(darks, /no_copy)

  print, 'Time to read camera file=', systime(1)-t0
end

function tomo::find_attribute, attributes, name
  for i=0, n_elements(attributes) do begin
    if (attributes[i].name eq name) then return, i
  endfor
  message, 'Could not find attribute ' + name
end

pro tomo::display_status, message, debugLevel
  if (n_elements(debugLevel) eq 0) then debugLevel = 0
  if (widget_info(self.statusWidget, /valid_id)) then begin
    widget_control, self.statusWidget, set_value=message
  endif
  if (self.debugLevel ge debugLevel) then print, systime() + ' ' + message
end

function tomo::check_abort
  if (widget_info(self.abortWidget, /valid_id)) then begin
    event = widget_event(/nowait, self.abortWidget)
    widget_control, self.abortWidget, get_uvalue=abort
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

pro tomo::preprocess

  tStart = systime(1)
  if (self.preprocessWriteFormat eq 'netCDF') then output_file = self.baseFilename + 'norm.nc' $
  else output_file = self.baseFilename + 'norm.h5'

  normalized = fltarr(self.nx, self.ny, self.nz, /nozero)

  ; Convert darks and flats to float
  darks = float(*self.pDarks)
  flats = float(*self.pFlats)
  if (n_elements(darks) eq 1) then begin
    darks = fltarr(self.nx, self.ny) + darks
  endif
  dims = size(darks, /dimensions)
  ndarks = 1
  if (n_elements(dims) eq 3) then ndarks = dims[2]
  dims = size(flats, /dimensions)
  nflats = 1
  if (n_elements(dims) eq 3) then nflats = dims[2]

  ; If there is more than one dark field image then average them
  if (ndarks gt 1) then begin
    darks = total(darks, 3) / ndarks
  endif

  self->display_status, 'Doing corrections on flat fields ...', 1
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

  self->display_status, 'Dark, flat, zinger correction ...', 1
  params = {preprocess_params, $
    numPixels:        self.nx, $
    numSlices:        self.ny, $
    numProjections:   self.nz, $
    numThreads:       self.preprocessThreads, $
    zingerWidth:      self.zingerWidth, $
    zingerThreshold:  self.zingerThreshold, $
    scaleFactor:      self.preprocessScale, $
    debug:            self.debug, $
    debugFile:        bytarr(256) $
  }
  params.debugFile = [byte(self.debugFile), 0B]

  t = call_external(self.tomoReconShareableLibrary, 'tomoPreprocessCreateIDL', $
                    params, darks, flats, *self.pVolume, normalized)

  preprocessComplete = 0L
  projectionsRemaining = 0L
  repeat begin
    t = call_external(self.tomoReconShareableLibrary, 'tomoPreprocessPollIDL', $
                      preprocessComplete, $
                      projectionsRemaining)
    self->display_status, 'Proprocessing projection: ' $
      + strtrim(self.nz-projectionsRemaining,2) + '/' + strtrim(self.nz,2), 2
    wait, 0.1
  endrep until preprocessComplete
  tNormalize = systime(1)

if (self.preprocessDataType eq 'UInt16') then begin
    self->display_status, 'Converting to UInt16 ...', 1
    normalized = fix(normalized)
  endif
  tConvertOut = systime(1)

  self.imageType = 'NORMALIZED'
  if (self.preprocessWriteOutput) then begin
    self->display_status, 'Writing normalized file ...', 1
    self->write_volume, output_file, normalized, netcdf=netcdf
  endif
  self->write_sample_config
  tWriteFile = systime(1)

  ptr_free, self.pVolume
  self.pVolume = ptr_new(normalized, /no_copy)
  tEnd = systime(1)
  self->display_status, 'Preprocessing complete', 1

  print, 'Preprocess execution times:'
  print, '                  Flat adjustments:', tFlats - tStart
  print, '  Dark, flat and zinger correction:', tNormalize - tFlats
  print, '                 Convert to UInt16:', tConvertOut - tNormalize
  print, '                    Writing output:', tWriteFile - tConvertOut
  print, '                    Freeing memory:', tEnd - tWriteFile
  print, '                             Total:', tEnd - tStart

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
  max_angle = max(*self.pAngles)
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
  s = size(*self.pVolume, /dimensions)
  input = fltarr(s[0], ncenter*2, s[2])
  ctr = fltarr(ncenter*2)
  for i=0, ncenter-1 do begin
    ctr[i*2] = center[i]
    ctr[i*2+1] = center[i]
    input[*, i*2, *] = (*self.pVolume)[*, slices[0], *]
    input[*, i*2+1, *] = (*self.pVolume)[*, slices[1], *]
  endfor
  self->tomo_recon, input, recon, angles=*self.pAngles, center=ctr

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
  s = size(*self.pVolume, /dimensions)
  for i=0, ncenter-1 do begin
    angles = *self.pAngles
    cent = round(center[i])
    input1 = self->convert_360_data((*self.pVolume)[*, slices[0], *], cent, angles)
    angles = *self.pAngles
    input2 = self->convert_360_data((*self.pVolume)[*, slices[1], *], cent, angles)
    input = [[input1], [input2]]
    ctr = [cent, cent]
    self->tomo_recon, input, recon, center=ctr, angles=angles
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
  if (max(*self.pAngles) gt 181) then begin
    stripWidth = 101
    mid_center = center[numShift/2]
    overlap = min([mid_center, self.nx-1 - mid_center])
    xmin = mid_center - overlap
    xmax = mid_center + overlap
    proj180 = float((*self.pVolume)[xmin:xmax, *, self.nz/2])
    shft = round((center - mid_center) * 2)
    shft = (shft < overlap) > (-overlap)
  endif else begin
    stripWidth = 11
    xmin = 0
    xmax = self.nx-1
    proj180 = float((*self.pVolume)[xmin:xmax, *, self.nz-1])
    shft = round((center - self.nx/2) * 2)
  endelse
  proj0 = float((*self.pVolume)[xmin:xmax, *, 0])
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
  self.rotationCenter = coeffs[0]
  self.rotationCenterSlope = coeffs[1]
  self.upperSlice = slice1
  self.lowerSlice = slice2
end

pro tomo::correct_rotation_tilt, angle
  if (not ptr_valid(self.pVolume)) then begin
    t = dialog_message('Must read in volume file first.', /error)
    return
  endif
  for i=0, self.nz-1 do begin
    proj = (*self.pVolume)[*,*,i]
    r = rot(proj, angle, cubic=-0.5)
    (*self.pVolume)[0,0,i] = r
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

pro tomo::tomo_recon, $
  input, $
  output, $
  create = create, $
  center=center, $
  angles=angles, $
  wait=wait

  t0 = systime(1)
  dims = long(size(input, /dimensions))
  numPixels = dims[0]
  if (n_elements(dims) eq 2) then begin
    numSlices      = 1L
    numProjections = dims[1]
  endif else if (n_elements(dims) eq 3) then begin
    numSlices      = dims[1]
    numProjections = dims[2]
  endif
  
  if (n_elements(create) eq 0) then create = 1
  if (n_elements(angles) eq 0) then angles = *self.pAngles
  if (n_elements(center) eq 0) then begin
    centerArr = fltarr(numSlices) + (numPixels)/2.
  endif else if (n_elements(center) eq 1) then begin
    centerArr = fltarr(numSlices) + float(center)
  endif else  begin
    if (n_elements(center) lt numSlices) then $
      message, 'Center size ='+ string(n_elements(center)) + ' must be '+ string(numSlices)
    centerArr = float(center)
  endelse
  if (n_elements(wait) eq 0) then wait = 1

  tp = {tomo_params,                                $
    ; Sinogram parameters
    numPixels:            numPixels,                $ ; Number of pixels in sinogram row before padding
    numProjections:       numProjections,           $ ; Number of angles
    numSlices:            numSlices,                $ ; Number of slices
    sinoScale:            self.preprocessScale,     $ ; Scale factor to multiply sinogram when airPixels=0;
    reconScale:           self.reconScale,          $ ; Scale factor to multiple reconstruction
    reconOffset:          self.reconOffset,         $ ; Offset factor to multiple reconstruction
    paddedSinogramWidth:  self.paddedSinogramWidth, $ ; Number of pixels in sinogram after padding
    paddingAverage:       self.paddingAverage,      $ ; Number of pixels to average on each side of sinogram to compute padding. 0 pixels pads with 0.0
    airPixels:            self.airPixels,           $ ; Number of pixels of air to average at each end of sinogram row
    ringWidth:            self.ringWidth,           $ ; Number of pixels to smooth by when removing ring artifacts
    fluorescence:         self.fluorescence,        $ ; 0=absorption data, 1=fluorescence
    ; tomoRecon parameters
    numThreads:           self.reconThreads,        $
    debug:                self.debug,               $
    debugFile:            bytarr(256),              $ ; Set below
    ; gridRec/tomoRecon parameters
    geom:                 self.geom,                $ ; 0 if array of angles provided; 1,2 if uniform in half,full circle
    pswfParam:            self.pswfParam,           $ ; PSWF parameter
    sampl:                self.sampl,               $ ; "Oversampling" ratio
    maxPixSize:           self.maxPixSize,          $ ; Max pixel size for reconstruction
    ROI:                  self.ROI,                 $ ; Region of interest (ROI) relative size
    X0:                   self.X0,                  $ ; (X0,Y0)=Offset of ROI from rotation axis
    Y0:                   self.Y0,                  $ ; in units of center-to-edge distance.
    ltbl:                 self.ltbl,                $ ; No. elements in convolvent lookup tables
    GR_filterName:        bytarr(16)                $ ; Name of filter function - set below
  }
  tp.debugFile = [byte(self.debugFile), 0B]
  tp.GR_filterName = [byte(self.GR_filterName), 0B]
  tp.paddedSinogramWidth = self->getPaddedSinogramWidth(numPixels)

  output = 0 ; Deallocate any existing array
  output = fltarr(numPixels, numPixels, numSlices, /nozero)

  ; Make sure input is a float array
  tname = size(input, /tname)
  if (tname ne 'FLOAT') then begin
    input = float(input)
  endif
  t1 = systime(1)

  if (create) then begin
    t = call_external(self.tomoReconShareableLibrary, 'tomoReconCreateIDL', tp, angles)
  endif
  t = call_external(self.tomoReconShareableLibrary, 'tomoReconRunIDL', $
    numSlices, $
    centerArr, $
    input, $
    output)

  if (wait) then begin
    reconComplete = 0L
    slicesRemaining = 0L
    repeat begin
      t = call_external(self.tomoReconShareableLibrary, 'tomoReconPollIDL', reconComplete, slicesRemaining)
      self->display_status, 'Reconstructing slice: ' + strtrim(numSlices-slicesRemaining,2) + '/' + strtrim(numSlices,2), 2
      wait, .01
    endrep until (reconComplete)
    t2 = systime(1)
    print, 'tomo_recon: time to convert to float:', t1-t0
    print, '                 time to reconstruct:', t2-t1
    print, '                          total time:', t2-t0
  endif
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
  angles = *self.pAngles
  
  ; If this is 360 degree data we need to convert to 180
  if (max(angles) gt 181) then begin
    input = self->convert_360_data(input, center, angles)
  endif

  if (self.reconMethod eq self.reconMethodTomoRecon) then begin
    self->tomo_recon, input, r, center=center, angles=angles
  endif

  if (self.reconMethod eq self.reconMethodGridrec) then begin
    t1 = input
    t2 = input
    s1 = sinogram(tomoParams, t1, angles, cog=cog)
    singram = s1
    s2 = sinogram(tomoParams, t2, angles, cog=cog)
    time3 = systime(1)

    if (self.ringWidth eq 0) then begin
      g1 = s1
      g2 = s2
    endif else begin
      g1 = remove_tomo_artifacts(s1, /rings, width=self.ringWidth)
      g2 = remove_tomo_artifacts(s2, /rings, width=self.ringWidth)
    endelse

    ; pad sinogram
    pad = self->getPaddedSinogramWidth(nx)
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
    if ((self.reconScale ne 0.) and (self.reconScale ne 1.0)) then begin
      r = r * self.reconScale
    endif
    if (self.reconOffset ne 0.) then begin
      r = r + self.reconOffset
    endif

    time5 = systime(1)
    print, 'Gridrec: center= ', center
    print, 'Time to compute sinogram: ', time3-time2
    print, 'Time to reconstruct:      ', time5-time4
  endif

  if (self.reconMethod eq self.reconMethodBackproject) then begin
    s1 = sinogram(tomoParams, t1, angles, cog=cog)
    cent = -1
    time3 = systime(1)
    if (self.ringWidth eq 0) then begin
      g1 = s1
    endif else begin
      g1 = remove_tomo_artifacts(s1, /rings, width=self.ringWidth)
    endelse

    time4 = systime(1)
    ss1 = tomo_filter(g1, filter_size=self.BP_filterSize, filter_name=self.BP_filterName)
    singram = ss1
    time5 = systime(1)
    if (center ge (nx-1)/2) then ctr = center else ctr = center + 2*abs(round(center - (nx-1)/2))
    r = backproject(tomoParams, ss1, angles, center=ctr)
    time6 = systime(1)
    ; Scale results
    if ((self.reconScale ne 0.) and (self.reconScale ne 1.0)) then begin
      r = r * self.reconScale
    endif
    if (self.reconOffset ne 0.) then begin
      r = r + self.reconOffset
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
;       The inverse of the SCALE is stored as the attribute volume:preprocessScale
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

pro tomo::reconstruct_volume

  tStart = systime(1)
  self->display_status, 'Initializing reconstruction ...', 1
  if (self.reconWriteFormat eq 'netCDF') then output_file = self.baseFilename + 'recon.nc' $
                                         else output_file = self.baseFilename + 'recon.h5'
  ; Set write_output keyword by default
  if (n_elements(write_output) eq 0) then write_output=1
  
  if (n_elements(debug) ne 0) then self.debugLevel = debug

  angles = *self.pAngles
  dims = size(*self.pVolume, /dimensions)
  numPixels = dims[0]
  numSlices = dims[1]
  numProjections = dims[2]
  center = self.rotationCenter + findgen(self.ny)*self.rotationCenterSlope

  ; If this is 360 degree data we need to convert to 180
  if (max(angles) gt 181) then begin
    self->display_status, 'Converting 360 data ...', 1
    ; convert_360_data can only use a single center value. Use the one for the center slice.
    center_value =  round(center[numSlices/2])
    *self.pVolume = self->convert_360_data(*self.pVolume, center_value, angles)
    center = center_value + findgen(self.ny)*0
  endif
  tConvert360 = systime(1)
  if (size(*self.pVolume, /tname) ne 'FLOAT') then begin
    self->display_status, 'Converting to float ...', 1
    *self.pVolume = float(*self.pVolume)
  endif
  tConvertFloat = systime(1)
  recon = fltarr(numPixels, numPixels, numSlices, /nozero)
  
  if (self.reconDataType eq 'UInt16') then begin
    self.reconOffset = 32767. 
  endif else begin
    self.reconOffset = 0.
  endelse

  self->display_status, 'Beginning reconstruction ...', 1
  if (self.reconMethod eq self.reconMethodTomoRecon) then begin
    ; Reconstruct volume
    self->tomo_recon, *self.pVolume, recon, angles=angles, center=center
  endif else begin
    nrows = self.ny
    ; This procedure reconstructs all of the slices for a tomography data set
    ; If we are using GRIDREC to reconstruct then we get 2 slices at a time
    for i=0, nrows-1, 2 do begin
      self->display_status, 'Reconstructing slice ' + strtrim(i,2) + '/' + strtrim(nrows-1,2), 2
      r = self->reconstruct_slice((*self.pVolume)[*,[i,(i+1)<self.ny],*], r2, center=center[i])
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
  
  ptr_free, self.pVolume
  tReconDone = systime(1)
  self->write_sample_config
  
  self->display_status, 'Converting to output data type ...', 1
  if (self.reconDataType eq 'Int16') then begin
    recon = fix(recon)
  endif
  if (self.reconDataType eq 'UInt16') then begin
    recon = uint(recon)
  endif
  tConvertInt = systime(1)

  self.imageType = 'RECONSTRUCTED'
  dims = size(recon, /dimensions)
  self.nx = dims[0]
  self.ny = dims[1]
  self.nz = dims[2]

  if (write_output) then begin
    self->display_status, 'Writing reconstructed file ...', 1
    self->write_volume, output_file, recon, netcdf=(self.reconWriteFormat eq 'netCDF')
  endif
  self.pVolume = ptr_new(recon, /no_copy)
  tEnd = systime(1)
  print, '     Convert 360 to 180:', tConvert360 - tStart
  print, ' Convert input to float:', tConvertFloat - tConvert360
  print, '            Reconstruct:', tReconDone-tConvertFloat
  print, 'Convert output to ' + self.reconDataType + ':', tConvertInt - tReconDone
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
      config = *self.pSampleConfig
      
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
      if (config.haskey('SampleDescription1')) then str=config['SampleDescription1'] else str=' '
      if (str eq '') then str = ' '
      ncdf_attput, file_id, /GLOBAL, 'title', str
      if (config.haskey('UserName')) then str=config['UserName'] else str=' '
      if (str eq '') then str = ' '
      ncdf_attput, file_id, /GLOBAL, 'operator', str
      if (config.haskey('Camera')) then str=config['Camera'] else str=' '
      if (str eq '') then str = ' '
      ncdf_attput, file_id, /GLOBAL, 'camera', str
      if (config.haskey('SampleName')) then str=config['SampleName'] else str=' '
      if (str eq '') then str = ' '
      ncdf_attput, file_id, /GLOBAL, 'sample', str
      if (self.imageType ne '') then str=self.imageType else str=' '
      if (str eq '') then str = ' '
      ncdf_attput, file_id, /GLOBAL, 'imageType', str
      if (config.haskey('Energy')) then energy =config['Energy'] else energy=0
      if (energy eq '') then energy = 0
      ncdf_attput, file_id, /GLOBAL, 'energy', energy
      ncdf_attput, file_id, /GLOBAL, 'dark_current', (*self.pDarks)[0]
      ncdf_attput, file_id, /GLOBAL, 'center', self.rotationCenter
      ncdf_attput, file_id, /GLOBAL, 'xPixelSize', self.xPixelSize
      ncdf_attput, file_id, /GLOBAL, 'yPixelSize', self.yPixelSize
      ncdf_attput, file_id, /GLOBAL, 'zPixelSize', self.zPixelSize
      if (ptr_valid(self.pAngles)) then $
        ncdf_attput, file_id, /GLOBAL, 'angles', *(self.pAngles)
      if (self.preprocessScale ne 0) then scale=self.preprocessScale else scale=1.0
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
    self->write_hdf5_dataset, group_id, 'imageType', self.imageType
    if (ptr_valid(self.pAngles)) then $
      self->write_hdf5_dataset, group_id, 'angles', *self.pAngles
    self->write_hdf5_dataset, group_id, 'xPixelSize', self.xPixelSize
    self->write_hdf5_dataset, group_id, 'yPixelSize', self.yPixelSize
    self->write_hdf5_dataset, group_id, 'zPixelSize', self.zPixelSize
    self->write_hdf5_dataset, group_id, 'rotationCenter', self.rotationCenter
    self->write_hdf5_dataset, group_id, 'rotationCenterSlope', self.rotationCenterSlope
    self->write_hdf5_dataset, group_id, 'preprocessScale', self.preprocessScale
    if (self.imageType eq 'NORMALIZED') then begin
      self->write_hdf5_dataset, group_id, 'dataOffset', self.preprocessOffset
      self->write_hdf5_dataset, group_id, 'dataScale', self.preprocessScale
    endif else if (self.imageType eq 'RECONSTRUCTED') then begin
      self->write_hdf5_dataset, group_id, 'dataOffset', self.reconOffset
      self->write_hdf5_dataset, group_id, 'dataScale', self.reconScale      
    endif
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
    ptr_free, self.pVolume
  endif

  if (ncdf_is_ncdf(file)) then begin
    config = *self.pSampleConfig
    ; This is a netCDF file
    file_id = ncdf_open(file, /nowrite)
    ; Process the global attributes
    status = ncdf_inquire(file_id)
    ; Set some reasonable defaults in case attributes are missing
    self.imageType = 'RECONSTRUCTED'
    self.xPixelSize = 1
    self.yPixelSize = 1
    self.zPixelSize = 1
    for i=0, status.ngatts-1 do begin
      name = ncdf_attname(file_id, /global, i)
      ncdf_attget, file_id, /global, name, value
      case name of
        'title':        config['SampleDescription1'] = strtrim(value,2)
        'operator':     config['UserName']           = strtrim(value,2)
        'camera':       config['Camera']             = strtrim(value,2)
        'sample':       config['SampleName']         = strtrim(value,2)
        'imageType':    self.imageType               = strtrim(value,2)
        'energy':       config['Energy']             = value
        'dark_current':  begin
          config['DarkFieldValue'] = value
          self.pDarks = ptr_new(value)
        end
        'center':       self.rotationCenter = value
        'xPixelSize':   self.xPixelSize     = value
        'yPixelSize':   self.yPixelSize     = value
        'zPixelSize':   self.zPixelSize     = value
        'angles':       begin
          ptr_free, self.pAngles
          self.pAngles = ptr_new(value)
        end
        else:
      endcase
    endfor
    self.pSampleConfig = ptr_new(config)
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
        'scale_factor': self.preprocessScale = value
      endcase
    endfor
    ; Close the netCDF file
    ncdf_close, file_id

  endif else begin
    ; File must be HDF5
    h5_list, file, output=contents
    h = hash(reform(contents[1,*]))
    volume                                                    = h5_getdata(file, '/exchange/data')
    ; This code should be improved to only read the selected subset of the data.
    ; For now we trim the data if any of the range keywords is set
    if (keyword_set(xrange) or keyword_set(yrange) or keyword_set(zrange)) then begin
      dims = size(volume, /dimensions)
      if (n_elements(xrange) eq 0) then xrange = [0, dims[0]-1]
      if (n_elements(yrange) eq 0) then yrange = [0, dims[1]-1]
      if (n_elements(zrange) eq 0) then zrange = [0, dims[2]-1]
      volume = volume[xrange[0]:xrange[1], yrange[0]:yrange[1], zrange[0]:zrange[1]]
    endif
    if (h.haskey('/process/imageType')) then self.imageType   = h5_getdata(file, '/process/imageType')
    if (self.imageType eq '') then self.imageType   = 'RECONSTRUCTED'
    if (h.haskey('/process/angles')) then begin
      angles                                                  = h5_getdata(file, '/process/angles')
      self.pAngles = ptr_new(angles)
    endif else begin
      dims = size(volume, /dimensions)
      self.pAngles = ptr_new(findgen(dims[2]))
    endelse
    if (h.haskey('/process/xPixelSize')) then self.xPixelSize = h5_getdata(file, '/process/xPixelSize')
    if (h.haskey('/process/yPixelSize')) then self.yPixelSize = h5_getdata(file, '/process/yPixelSize')
    if (h.haskey('/process/zPixelSize')) then self.zPixelSize = h5_getdata(file, '/process/zPixelSize')
  endelse

  self->read_sample_config

  print, 'Time to read file=', systime(1)-t0
  if (keyword_set(store)) then begin
    dims = size(volume, /dimensions)
    self.pVolume = ptr_new(volume, /no_copy)
    self.nx = dims[0]
    self.ny = dims[1]
    self.nz = dims[2]
    return, 0
  endif else begin
    return, volume
  endelse
end

function tomo::get_angles
  if (ptr_valid(self.pAngles)) then begin
    return, *self.pAngles
  endif else begin
    return, 0
  endelse
end

pro tomo::set_angles, angles
  ptr_free, self.pAngles
  self.pAngles = ptr_new(angles)
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
  line = ''
  openr, lun, file, error=error, /get_lun, width = 1024
  if (error ne 0) then return, 0
  sampleConfig = orderedhash()
  while (not eof(lun)) do begin
    readf, lun, line
    pos = strpos(line, ' ')
    tag = strupcase(strmid(line, 0, pos))
    value = strtrim(strmid(line, pos, 1000), 2)
    case tag of
      'TITLE:'         :  sampleConfig['SampleDescription1'] = value
      'OPERATOR:'      :  sampleConfig['UserName'] = value
      'CAMERA:'        :  sampleConfig['Camera'] = value
      'SAMPLE:'        :  sampleConfig['SampleName'] = value
      'DARK_CURRENT:'  :  begin
                            sampleConfig['DarkFieldValue'] = value
                            self.pDarks = ptrNew(value)
                          end
      'CENTER:'        :  self.rotationCenter = value
      'ENERGY:'        :  sampleConfig['Energy'] = value
      'X_PIXEL_SIZE:'  :  begin
                            sampleConfig['ImagePixelSize'] = value
                            self.xPixelSize = value
                          end
      'Y_PIXEL_SIZE:'  :  self.yPixelSize = value
      'Z_PIXEL_SIZE:'  :  self.zPixelSize = value
      else:
    endcase
  endwhile
  self.pSampleConfig = ptr_new(sampleConfig)
  free_lun, lun
  return, 1
end

pro tomo::write_sample_config
  (*self.pSampleConfig)['RotationCenter'] = self.rotationCenter
  (*self.pSampleConfig)['RotationSlope']   = self.rotationCenterSlope
  (*self.pSampleConfig)['UpperSlice'] = self.upperSlice
  (*self.pSampleConfig)['LowerSlice'] = self.lowerSlice
  openw, lun, /get_lun, self.baseFilename + '.config'
  printf, lun, /implied_print, *self.pSampleConfig
  close, lun
end

pro tomo::read_sample_config, file
  if (n_elements(file) eq 0) then file = self.directory + '/' + self.baseFilename + '.config'
  if ((file_info(file)).exists eq 0) then return
  config = json_parse(file)
  if (config.haskey('RotationCenter')) then self.rotationCenter = config['RotationCenter'] else self.rotationCenter = self.nx/2
  if (config.haskey('RotationSlope')) then self.rotationCenterSlope = config['RotationSlope'] else self.rotationCenterSlope = 0.
  if (config.haskey('UpperSlice')) then self.upperSlice = config['UpperSlice'] else self.upperSlice = self.ny*0.1
  if (config.haskey('LowerSlice')) then self.lowerSlice = config['LowerSlice'] else self.lowerSlice = self.ny*0.9
  self.pSampleConfig = ptr_new(config, /no_copy)
end

;+
function tomo::get_struct
  t = {tomo}
  for i=0, n_tags(t)-1 do begin
    t.(i)=self.(i)
  endfor
  return, t
end

function tomo::get_pvolume
  return, self.pVolume
end

pro tomo::set_file_components, input_file
  path = file_dirname(input_file)
  file = file_basename(input_file)
  self.inputFilename = input_file
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
  self.baseFilename = base_file
end

pro tomo::cleanup
  ptr_free, self.pAngles
  ptr_free, self.pVolume
  ptr_free, self.pFlats
  ptr_free, self.pDarks
end

function tomo::getPaddedSinogramWidth, numPixels
  if (self.paddedSinogramWidth eq 0) then begin
    ; Use the next largest power of 2 by default
    paddedSinogramWidth = 128
    repeat begin
      paddedSinogramWidth=paddedSinogramWidth * 2
    endrep until (paddedSinogramWidth ge numPixels)
  endif else if (paddedSinogramWidth eq 1) then begin
    paddedSinogramWidth = numPixels
  endif
  return, paddedSinogramWidth
end

pro tomo::set_recon_params, $
  rotationCenter = rotationCenter, $
  rotCenterSlope = rotCenterSlope, $
  reconMethod = reconMethod, $
  dataType = dataType, $
  writeOutput = writeOutput, $
  writeFormat = writeFormat, $
  sinoScale = sinoScale, $
  reconScale = reconScale, $
  reconOffset = reconOffset, $
  paddedSinogramWidth=paddedSinogramWidth, $
  paddingAverage=paddingAverage, $
  airPixels = airPixels, $
  ringWidth = ringWidth, $
  fluorescence = fluorescence, $
  threads = threads, $
  slicesPerChunk = slicesPerChunk, $
  debug = debug, $
  dbgFile = dbgFile, $
  geom=geom, $
  pswfParam=pswfParam, $
  sampl=sampl, $
  maxPixSize=maxPixSize, $
  ROI=ROI, $
  X0=X0, $
  Y0=Y0, $
  ltbl=ltbl, $
  GR_filterName=GR_filterName, $
  BP_method = BP_method, $
  BP_filterName = BP_filterName, $
  BP_filterSize = BP_filterSize, $
  RiemannInterpolation = RiemannInterpolation, $
  RadonInterpolation = RadonInterpolation

  if (n_elements(rotationCenter)       ne 0) then self.rotationCenter = rotationCenter
  if (n_elements(rotCenterSlope)       ne 0) then self.rotationCenterSlope = rotCenterSlope
  if (n_elements(reconMethod)          ne 0) then self.reconMethod = reconMethod
  if (n_elements(dataType)             ne 0) then self.reconDataType = dataType
  if (n_elements(writeOutput)          ne 0) then self.reconWriteOutput = writeOutput
  if (n_elements(writeFormat)          ne 0) then self.reconWriteFormat = writeFormat
  if (n_elements(sinoScale)            ne 0) then self.sinoScale = sinoScale
  if (n_elements(reconScale)           ne 0) then self.reconScale = reconScale
  if (n_elements(reconOffset)          ne 0) then self.reconOffset = reconOffset
  if (n_elements(paddedSinogramWidth)  ne 0) then self.paddedSinogramWidth = paddedSinogramWidth
  if (n_elements(paddingAverage)       ne 0) then self.paddingAverage = paddingAverage
  if (n_elements(airPixels)            ne 0) then self.airPixels = airPixels
  if (n_elements(ringWidth)            ne 0) then self.ringWidth = ringWidth
  if (n_elements(fluorescence)         ne 0) then self.fluorescence = fluorescence
  if (n_elements(threads)              ne 0) then self.reconThreads = threads
  if (n_elements(debug)                ne 0) then self.debug = debug
  if (n_elements(dbgFile)              ne 0) then self.debugFile = dbgFile
  if (n_elements(geom)                 ne 0) then self.geom = geom
  if (n_elements(pswfParam)            ne 0) then self.pswfParam = pswfParam
  if (n_elements(sampl)                ne 0) then self.sampl = sampl
  if (n_elements(maxPixSize)           ne 0) then self.maxPixSize = maxPixSize
  if (n_elements(ROI)                  ne 0) then self.ROI = ROI
  if (n_elements(X0)                   ne 0) then self.X0 = X0
  if (n_elements(Y0)                   ne 0) then self.Y0 = Y0
  if (n_elements(ltbl)                 ne 0) then self.ltbl = ltbl
  if (n_elements(GR_filterName)        ne 0) then self.GR_filterName = GR_filterName
  if (n_elements(slicesPerChunk)       ne 0) then self.reconSlicesPerChunk = slicesPerChunk
  if (n_elements(BP_method)            ne 0) then self.BP_method = BP_method
  if (n_elements(BP_filterName)        ne 0) then self.BP_filterName = BP_filterName
  if (n_elements(BP_filterSize)        ne 0) then self.BP_filterSize = BP_filterSize
  if (n_elements(RiemannInterpolation) ne 0) then self.RiemannInterpolation = RiemannInterpolation
  if (n_elements(RadonInterpolation)   ne 0) then self.RadonInterpolation = RadonInterpolation
end

pro tomo::set_preprocess_params, $
  zingerWidth = zingerWidth, $
  zingerThreshold = zingerThreshold, $
  zingerDoubleThreshold = zingerDoubleThreshold, $
  dataType = dataType, $
  writeOutput = writeOutput, $
  writeFormat = writeFormat, $
  scale = scale, $
  threads = threads
  
  if (n_elements(zingerWidth)           ne 0) then self.zingerWidth = zingerWidth
  if (n_elements(zingerThreshold)       ne 0) then self.zingerThreshold = zingerThreshold
  if (n_elements(zingerDoubleThreshold) ne 0) then self.zingerDoubleThreshold = zingerThreshold
  if (n_elements(dataType)              ne 0) then self.preprocessDataType = dataType
  if (n_elements(writeOutput)           ne 0) then self.preprocessWriteOutput = writeOutput
  if (n_elements(writeFormat)           ne 0) then self.preprocessWriteFormat = writeFormat
  if (n_elements(scale)                 ne 0) then self.preprocessScale = scale
  if (n_elements(threads)               ne 0) then self.preprocessThreads = threads
end

function tomo::get_saved_fields
  fields = ['zingerWidth', 'zingerThreshold', 'zingerDoubleThreshold',                                                                          $
            'preprocessScale', 'preprocessOffset', 'preprocessThreads', 'preprocessDataType', 'preprocessWriteOutput', 'preprocessWriteFormat', $
            'reconMethod', 'reconSlicesPerChunk', 'reconDataType', 'reconWriteOutput', 'reconWriteFormat',                                      $
            'reconScale', 'reconOffset', 'reconThreads',                                                                                        $
            'paddedSinogramWidth', 'paddingAverage', 'airPixels', 'ringWidth', 'fluorescence',                                                  $
            'debug', 'debugFile',                                                                                                               $
            'geom', 'pswfParam', 'sampl', 'maxPixSize', 'ROI', 'X0', 'Y0', 'ltbl', 'GR_filterName',                                             $
            'BP_Method', 'BP_filterName', 'BP_filterSize', 'RiemannInterpolation', 'RadonInterpolation'                                         $
           ]
  return, fields
end

function tomo::find_saved_field, field
  myFieldNames = tag_names(self)
  index = where(field.toUpper() eq myFieldNames)
  return, index[0]
end

function tomo::get_settings
  settings = orderedhash()
  savedFields = self->get_saved_fields()
  for i=0, n_elements(savedFields)-1 do begin
    field = savedFields[i]
    index = self->find_saved_field(field)
    if (index lt 0) then continue
    settings[field] = self.(index)
  endfor
  return, settings
end

pro tomo::save_settings, file
  settings = self->get_settings()
  openw, lun, /get_lun, file
  printf, lun, /implied_print, settings
  close, lun
end

pro tomo::restore_settings, file
  catch, ioerror
  if (ioerror ne 0) then begin
    t = dialog_message(/error, $
      ['Error reading settings file: ' + file + '.  Using defaults', $
      !err_string])
    return
  endif
  settings = json_parse(file)
  keys = settings.keys()
  values = settings.values()
  for i=0, n_elements(keys)-1 do begin
    key = keys[i]
    value = values[i]
    index = self->find_saved_field(key)
    if (index ge 0) then self.(index) = value
  endfor
end

function tomo::init, settingsFile=settingsFile, statusWidget=statusWidget, abortWidget=abortWidget, debugLevel=debugLevel
  if (n_elements(statusWidget)  ne 0) then self.statusWidget = statusWidget
  if (n_elements(abortWidget)   ne 0) then self.abortWidget = abortWidget
  if (n_elements(debugLevel)    ne 0) then self.debugLevel = debugLevel else self.debugLevel=1
  self.pSampleConfig                = ptr_new(orderedhash())
  self.zingerWidth                  = 3
  self.zingerThreshold              = 0.1
  self.zingerDoubleThreshold        = 1.05
  self.preprocessScale              = 10000. 
  self.preprocessOffset             = 0.
  self.preprocessThreads            = 8;
  self.preprocessDataType           = "Float32"
  self.preprocessWriteOutput        = 0 ; 0=don't write, 1=write
  self.preprocessWriteFormat        = "HDF5"
  self.reconMethod                  = 0 ; 0=tomoRecon, 1=Gridrec, 2=Backproject
  self.reconMethodTomoRecon         = 0
  self.reconMethodGridrec           = 1
  self.reconMethodBackproject       = 2
  self.reconSlicesPerChunk          = 256
  self.reconDataType                = "Int16"
  self.reconWriteOutput             = 1 ; 0=don't write, 1=write
  self.reconWriteFormat             = "HDF5"
  ; Sinogram parameters
  self.reconScale                   = 1.e6 ; Reconstruction scale
  self.reconOffset                  = 0.0 ; Reconstruction offset
  self.paddedSinogramWidth          = 0 ; Number of pixels in sinogram after padding
                                        ; There are 2 "special" values of paddedSinogramWidth
                                        ; 0 = automatically set the width to power of 2 that is >= actual width
                                        ; 1 = No padding, set the width to the actual width
  self.paddingAverage               = 10 ; Number of pixels to average on each side of sinogram to compute padding. 0 pixels pads with 0.0
  self.airPixels                    = 10 ; Number of pixels of air to average at each end of sinogram row
  self.ringWidth                    = 9 ; Number of pixels to smooth by when removing ring artifacts
  self.fluorescence                 = 0 ; 0=absorption data, 1=fluorescence
  ; tomoRecon parameters
  self.reconThreads                 = 8
  self.debug                        = 2
  self.debugFile                    = "tomoRecon_debug.txt"
  ; gridRec/tomoRecon parameters
  self.geom                         = 0 ; 0 if array of angles provided;
    ; 1,2 if uniform in half,full circle
  self.pswfParam                    = 6.0 ; PSWF parameter
  self.sampl                        = 1.0 ; "Oversampling" ratio
  self.maxPixSize                   = 1.0 ; Max pixel size for reconstruction
  self.ROI                          = 1.0 ; Region of interest (ROI) relative size
  self.X0                           = 0.0 ; (X0,Y0)=Offset of ROI from rotation axis
  self.Y0                           = 0.0 ; in units of center-to-edge distance.
  self.ltbl                         = 512 ; No. elements in convolvent lookup tables
  self.GR_filterName                = "shepp" ; Name of filter function
  self.BP_Method                    = 0 ; 0=Riemann, 1=Radon
  self.BP_MethodRiemann             = 0
  self.BP_MethodRadon               = 1L
  self.BP_filterName                = "SHEPP_LOGAN"
  self.BP_filterSize                = 0 ; Length of filter
  self.RiemannInterpolation         = 0 ; 0=none, 1=bilinear, 2=cubic
  self.RiemannInterpolationNone     = 0
  self.RiemannInterpolationBilinear = 1
  self.RiemannInterpolationCubic    = 2
  self.RadonInterpolation           = 0 ; 0=none, 1=linear
  self.RadonInterpolationNone       = 0
  self.RadonInterpolationLinear     = 1
  self.tomoReconShareableLibrary    = getenv('TOMO_RECON_SHARE')
  if (self.tomoReconShareableLibrary eq "") then begin
    file = 'tomoRecon_' + !version.os + '_' + !version.arch
    if (!version.os eq 'Win32') then file=file+'.dll' else file=file+'.so'
    self.tomoReconShareableLibrary = file_which(file)
  endif
  if (self.tomoReconShareableLibrary eq '') then message, 'tomoRecon shareable library not defined'
  if (n_elements(settingsFile) ne 0) then self->restore_settings, settings
  return, 1
end

pro tomo__define
  tomo =                              $
   {tomo,                             $
    ; Information about current dataset
    pVolume:               ptr_new(), $
    pFlats:                ptr_new(), $
    pDarks:                ptr_new(), $
    pAngles:               ptr_new(), $
    pSampleConfig:         ptr_new(), $
    nx:                           0L, $
    ny:                           0L, $
    nz:                           0L, $
    xPixelSize:                   0., $
    yPixelSize:                   0., $
    zPixelSize:                   0., $
    imageType:                    "", $  ; "RAW", "CORRECTED" or "RECONSTRUCTED"
    inputFilename:                "", $
    baseFilename:                 "", $
    directory:                    "", $
    ; Proprocessing parameters
    zingerWidth:                  0L, $
    zingerThreshold:              0., $
    zingerDoubleThreshold:        0., $
    preprocessScale:              0., $
    preprocessOffset:             0., $
    preprocessThreads:            0L, $
    preprocessDataType:           "", $ 'UInt16' or 'Float32'
    preprocessWriteOutput:        0L, $ 0: don't write, 1: write
    preprocessWriteFormat:        "", $ 'netCDF' or 'HDF5'
    ; Rotation center parameters
    upperSlice:                   0L, $
    lowerSlice:                   0L, $
    rotationCenter:               0., $
    rotationCenterSlope:          0., $
    ; Reconstruction parameters
    ; Reconstruction method
    reconMethod:                  0L, $ ; 0=tomoRecon, 1=Gridrec, 2=Backproject
    reconMethodTomoRecon:         0L, $
    reconMethodGridrec:           0L, $
    reconMethodBackproject:       0L, $
    reconSlicesPerChunk:          0L, $
    reconDataType:                "", $ 'UInt16', 'Int16' or 'Float32'
    reconWriteOutput:             0L, $ 0: don't write, 1: write
    reconWriteFormat:             "", $ 'netCDF' or 'HDF5'
    tomoReconShareableLibrary:    "", $
    ; tomoRecon parameters
    reconScale:                   0., $ ; Scale factor for reconstruction
    reconOffset:                  0., $ ; Offset factor for reconstruction
    paddedSinogramWidth:          0L, $ ; Number of pixels in sinogram after padding
    paddingAverage:               0L, $ ; Number of pixels to average on each side of sinogram to compute padding. 0 pixels pads with 0.0
    airPixels:                    0L, $ ; Number of pixels of air to average at each end of sinogram row
    ringWidth:                    0L, $ ; Number of pixels to smooth by when removing ring artifacts
    fluorescence:                 0L, $ ; 0=absorption data, 1=fluorescence
    ; tomoRecon parameters
    reconThreads:                 0L, $
    debug:                        0L, $
    debugFile:                    "", $
    ; gridRec/tomoRecon parameters
    geom:                         0L, $ ; 0 if array of angles provided; 1,2 if uniform in half,full circle
    pswfParam:                    0., $ ; PSWF parameter
    sampl:                        0., $ ; "Oversampling" ratio
    maxPixSize:                   0., $ ; Max pixel size for reconstruction
    ROI:                          0., $ ; Region of interest (ROI) relative size
    X0:                           0., $ ; (X0,Y0)=Offset of ROI from rotation axis
    Y0:                           0., $ ; in units of center-to-edge distance.
    ltbl:                         0L, $ ; No. elements in convolvent lookup tables
    GR_filterName:                "", $ ; Name of filter function
    ; Backproject parameters
    BP_Method:                    0L, $ ; 0=Riemann, 1=Radon
    BP_MethodRiemann:             0L, $
    BP_MethodRadon:               0L, $
    BP_filterName:                "", $ ; Name of filter function - initialized to "SHEPP_LOGAN" above
    BP_filterSize:                0L, $ ; Length of filter
    RiemannInterpolation:         0L, $ ; 0=none, 1=bilinear, 2=cubic
    RiemannInterpolationNone:     0L, $
    RiemannInterpolationBilinear: 0L, $
    RiemannInterpolationCubic:    0L, $
    RadonInterpolation:           0L, $ ; 0=none, 1=linear
    RadonInterpolationNone:       0L, $
    RadonInterpolationLinear:     0L, $
    ; Status and debugging parameters
    statusWidget:                 0L, $
    abortWidget:                  0L, $
    debugLevel:                   0L  $
  }
end
