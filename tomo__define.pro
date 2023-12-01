;+
; NAME:
;  TOMO::READ_CAMERA_FILE
;
; PURPOSE:
;   This procedure reads a tomography data set from the following types of data files:
;     - netCDF files.  These are files collected before July 2020.  There are 4 files
;       - base_1.nc contains flat fields collected at the beginning
;       - base_2.nc contains the projections
;       - base_3.nc contains the flat fields collected at the end
;       - base.setup ASCII file containing metadata including the pixel size and dark current.
;       
;     - HDF5 files.  These are files collected after July 2020.  There are 2 files:
;       - base.h5     A single file HDF5 that contains the flat fields, dark fields, projections, and metadata
;       - base.config An ASCII file containing metadata. 
;
; INPUT PARAMETERS:
;   FILENAME (required):
;     For netCDF this can be the name of any of the 3 netCDF files
;     For HDF5 this is the name of the HDF5 file
;
; PROCEDURE:
;   - Reads the data into the tomo object, including these member variables
;       nx, ny, nz
;       xPixelSize, yPixelSize, zPixelSize
;       pAngles
;       pFlats
;       pDarks
;       pData
;       rotationCenter       Initialized to nx/2
;       rotationCenterSlope  Initialize to 0
;  - For netCDF files reads the .setup file with TOMO::READ_SETUP
;  - For HDF5 files reads the .config file with TOMO::READ_SAMPLE_CONFIG
;-
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

pro tomo::read_nsls2_files, proj_dir, flat_dir

  t0 = systime(1)
  ptr_free, self.pVolume
  print, 'Time to free volume=', systime(1)-t0

  t1 = systime(1)
  darks       = h5_getdata(flat_dir + '/dark_00000.hdf', '/entry/data/data')
  print, 'Time to read darks=', systime(1)-t1
  t1 = systime(1)
  flats       = h5_getdata(flat_dir + '/flat_00000.hdf', '/entry/data/data')
  print, 'Time to read flats=', systime(1)-t1
  t1 = systime(1)
  ; We read the projections and then discard the last one.
  ; This is 5x faster than just reading a subset of the data with HDF5 functions
  projections = h5_getdata(proj_dir + '/proj_00000.hdf', '/entry/data/data/')
  dims = size(projections, /dimensions)
  num_projections = dims[2]
  print, 'Time to read projections=', systime(1)-t1
  self->set_file_components, proj_dir + '/proj_00000.hdf'
  temp        = file_search(proj_dir + '/scan*.nxs')
  filename = temp[0]
  angles      = h5_getdata(filename, '/entry/data/rotation_angle/')
  angles = float(angles)
  camera = 'Kinetix'
  self.xPixelSize = 1.4
  self.yPixelSize = 1.4
  self.zPixelSize = 1.4
  self.nx = dims[0]
  self.ny = dims[1]
  self.nz = dims[2]
  self.imageType = 'RAW'
  self.pAngles = ptr_new(angles, /no_copy)
  self.pVolume = ptr_new(projections, /no_copy)
  self.pFlats = ptr_new(flats, /no_copy)
  self.pDarks = ptr_new(darks, /no_copy)

  print, 'Time to read camera files=', systime(1)-t0
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
;   This procedure performs the preprocessing on a tomography dataset, producing normalized data that
;   is corrected for dark fields, flat fields, and zingers.
;   The data must have been read into the tomo object with TOMO::READ_CAMERA_FILE.
;   The preprocessing parameters can be specified before calling this function with TOMO::SET_PREPROCESS_PARAMS.
;
; CALLING SEQUENCE:
;   TOMO->PREPROCESS
;
; PROCEDURE:
;   This function performs the following steps:
;   - Averages the dark fields if there is more than 1.
;   - Subtracts the dark current from each flat field field frame,
;   - Removes zingers from flat field frames using SELF->REMOVE_ARTIFACTS with
;     /DOUBLE_CORRELATION if there is more than 1, or /ZINGERS if there is only 1.
;   - Averages the flat fields if there is more than 1.
;   - Calls the multithreaded C++ code in tomoRecon to do the following:
;     - Subtract the dark current from each projection.
;     - Divide each projection by the average flat field.
;     - Remove zingers using a median filter with self.zingerWidth and self.zingerThreshold.
;     - Scale the normalized projection by self.preprocessScale.
;   - Optionally converts the normalized projections to UInt16 if self.preprocessDataType = 'UInt16'.
;   - Optionally writes the normalized data frames to a single disk file if self.preprocessWriteOutput is true.
;     The file can be written in HDF5 or netCDF format.
;   - Replaces the raw data in self.pVolume with the normalized data.
;-
pro tomo::preprocess

  tStart = systime(1)
  if (self.preprocessWriteFormat eq 'netCDF') then output_file = self.baseFilename + 'norm.nc' $
  else output_file = self.baseFilename + 'norm.h5'

  if (self.preprocessDataType eq 'UInt16') then begin
    normalized = uintarr(self.nx, self.ny, self.nz, /nozero)
    outputDataType = 1L;
  endif else begin
    normalized = fltarr(self.nx, self.ny, self.nz, /nozero)
    outputDataType = 0L;
  endelse
   
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
    flats = self->remove_artifacts(flats, /zingers, threshold=threshold, debug=debug)
  endif else begin
    for i=0, nflats-2 do begin
      flats[0,0,i] = self->remove_artifacts(flats[*,*,i], image2=flats[*,*,i+1], $
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
    outputDataType:   outputDataType, $
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
  
  ; Free the volume array, no longer needed
  ptr_free, self.pVolume
  tFree = systime(1)

  self.imageType = 'NORMALIZED'
  if (self.preprocessWriteOutput) then begin
    self->display_status, 'Writing normalized file ...', 1
    self->write_volume, output_file, normalized, netcdf=netcdf
  endif
  self->write_sample_config
  tWriteFile = systime(1)

  self.pVolume = ptr_new(normalized, /no_copy)
  tEnd = systime(1)
  self->display_status, 'Preprocessing complete', 1

  print, 'Preprocess execution times:'
  print, '                  Flat adjustments:', tFlats - tStart
  print, '  Dark, flat and zinger correction:', tNormalize - tFlats
  print, '                    Freeing memory:', tFree - tNormalize
  print, '                    Writing output:', tWriteFile - tFree
  print, '                             Total:', tEnd - tStart

end

; *** *** Reconstruction Filters from IDL reconstruction demo *** ***
Function None, x, d
  return, [1.0]
end

Function RAMLAK, x, d
  zero = where(x eq 0.0, count)
  q = x
  if (count ne 0) then q(zero) = .01
  y = -sin(!pi*x/2)^2 / (!pi^2 * q^2 * d)
  if count ne 0 then y(zero) = 1./(4.*d)
  return, y
end

Function Shepp_logan, x, d
  d = !pi^2 * d * (1.-4.*x^2)
  zeros = where(abs(d) le 1.0e-6, count)
  if count ne 0 then d(zeros) = .001
  return, 2./d
end

Function lp_cosine, x, d
  return, 0.5 * (ramlak(x-.5,d) + ramlak(x+.5,d))
end

Function Gen_Hamming, x, d, alpha
  if n_elements(alpha) le 0 then alpha = 0.5
  return, alpha * ramlak(x,d) + ((1.-alpha)/2) * (ramlak(x-1,d) + ramlak(x+1,d))
end

;+
; NAME:
;   TOMO::FILTER
;
; PURPOSE:
;   Filters a sinogram before backprojection.  A selection of filters is
;   available.
;
; CALLING SEQUENCE:
;   Result = TOMO->FILTER(SINOGRAM, FILTER_SIZE, D)
;
; INPUTS:
;   SINOGRAM:
;     The unfiltered sinogram.  This must be a 2-D array.
;
; OPTIONAL INPUTS:
;   FILTER_SIZE:
;     The half-size of the filter in pixels.
;     The default is NX/4 where NX is the first dimension of the sinogram.
;
;   D:
;     An additional filter parameter.  The default is 1.0
;
; KEYWORD PARAMETERS:
;   FILTER_NAME:
;     A case-insensitive string specifying the filter to be used.
;     Allowed values are:
;       'GEN_HAMMING'
;       'LP_COSINE'
;       'SHEPP_LOGAN'
;       'RAMLAK'
;       'NONE'
;       The default is 'SHEPP_LOGAN'

; OUTPUTS:
;   This function returns the filtered sinogram.
;
; PROCEDURE:
;   For each row in the sinogram, this function simply does the following:
;       Pads the input sinogram
;       Does a convolution with the appropriate filter
;   The code for the filters was taken from the IDL tomography demo program
;   which is included in the IDL distribution.  It would be easy to add
;   additional filters in the future.
;-

function tomo::filter, image, filter_size=filter_size, filter_param=d, filter_name=filter_name
  dims = size(image, /dimensions)
  if (n_elements(filter_size) eq 0) then filter_size = 0
  if (filter_size eq 0) then filter_size = dims[0]/4
  nfilter = 2*filter_size+1
  x = findgen(nfilter)-filter_size
  if (n_elements(d) eq 0) then d=1.0
  if (n_elements(filter_name) eq 0) then filter_name = 'SHEPP_LOGAN'
  if (strlen(filter_name) eq 0) then filter_name = 'SHEPP_LOGAN'
  case strupcase(filter_name) of
    'GEN_HAMMING': filter = gen_hamming(x, d)
    'LP_COSINE':   filter = lp_cosine(x, d)
    'SHEPP_LOGAN': filter = shepp_logan(x, d)
    'RAMLAK':      filter = ramlak(x, d)
    'NONE':        filter = none(x, d)
    else:  message, 'Unknown filter in tomo::filter'
  endcase
  size = size(image)
  ncols = size[1]
  nrows = size[2]
  s = image
  temp = fltarr(ncols + 2*nfilter)
  for i=0, nrows-1 do begin
    ; Pad array with data from first and last columns
    temp[0:nfilter-1] = image[0,i]
    temp[nfilter+ncols-1:ncols+2*nfilter-1] = image[ncols-1,i]
    temp(nfilter) = image[*,i]
    temp = convol(temp, filter)
    s(0,i) = temp[nfilter : nfilter+ncols-1]
  endfor
  return, s
end

;+
; NAME:
;   TOMO::REMOVE_ARTIFACTS
;
; PURPOSE:
;   Removes artifacts from tomography images and sinograms.
;
; CALLING SEQUENCE:
;   Result = TOMO->REMOVE_ARTIFACTS(IMAGE)
;
; INPUTS:
;   IMAGE:  
;     The input array from which artifacts are to be removed.
;
; KEYWORD PARAMETERS:
;   IMAGE2:
;     A second input and output image when doing DOUBLE_CORRELATION
;   ZINGERS:
;     Set this keyword to remove zingers from the input image
;   DOUBLE_CORRELATION:
;     Set this keyword to remove zingers from the input using double
;     correlation rather than a spatial detection
;   RINGS:
;     Set this keyword to remove ring artifacts from a sinogram
;   DIFFRACTION:
;     Set this keyword to removed diffraction artifacts from a sinogram
;   WIDTH:
;     Set this keyword to adjust the size of the filter kernal used in the
;     artifact removal.  The default is 9 pixels.
;   THRESHOLD:
;     Set this keyword to adjust the threshold used in detecting zingers and
;     diffraction artifacts.  The defaults are 1.2 for /ZINGER and
;     /DOUBLE_CORRELATION and 0.8 for/DIFFRACTION
;   DEBUG:
;     Set this keyword to print debugging information
;
; OUTPUTS:
;   This function returns an image with the specified artifacts removed.
;
; PROCEDURE:
;   THIS STILL NEEDS TO BE DOCUMENTED.  For now, see the source code.
;-

function tomo::remove_artifacts, image, image2=image2, width=width, $
                                 threshold=threshold, $
                                 zingers=zingers, double_correlation=double_correlation, $
                                 rings=rings, diffraction=diffraction, debug=debug

  ; Copy input image to output
  output = image
  size = size(image)
  ncols = size[1]
  nrows = size[2]
  if (keyword_set(debug)) then debug=1 else debug=0

  if (keyword_set(zingers)) then begin
    ; Default kernal width for smoothing
    if (n_elements(width) eq 0) then width=9
    ; Default threshold for detecting zingers
    if (n_elements(threshold) eq 0) then threshold=1.2
    ; Compute ratio of unsmoothed image to smoothed image
    ratio = float(image)/smooth(image, width, /edge_truncate)
    ; Find indices of pixels which are above threshold
    zinger = where(ratio gt threshold, count)
    ; Replace pixels with zingers by the average of the pixels 2 to the left and
    ; 2 to the right.  We don't use nearest neighbor, since zingers sometimes hit 2
    ; adjacent pixels.
    if (count gt 0) then output[zinger] = $
      (output[zinger-2] + output[zinger+2])/2.
    if (debug) then print, 'Found ', count, ' zingers in image'
  endif

  if (keyword_set(double_correlation)) then begin
    ; Compute ratio of the two images
    ratio = float(image) / float(image2)
    ; Find the median value of the ratio
    m = median(ratio)
    ; Normalize the ratio to this value
    ratio = ratio / m
    if (n_elements(threshold) eq 0) then threshold=1.2
    zingers1 = where(ratio gt threshold, count1)
    zingers2 = where(ratio lt 1./threshold, count2)
    if (debug) then print, 'Found ', count1, ' zingers in image1 and ', $
      count2, ' zingers in image2'
    ; Copy the first image to the output to be returned in function value
    output = image
    ; Replace zinger pixels in the first image with pixels from the second
    ;   image scaled by median value of ratio
    if (count1 gt 0) then output[zingers1] = image2[zingers1] * m
    ; Replace zinger pixels in the second image with pixels from the first
    ;   image scaled by median value of ratio
    if (count2 gt 0) then image2[zingers2] = image[zingers2] / m
    ; Now deal with the case where there are zingers at the same pixel in
    ;   both images
    if ((count1 gt 0) and (count2 gt 0)) then begin
      both = [zingers1, zingers2]
      both = both[sort(both)]
      indices = where(both eq shift(both, -1), count)
      if (debug) then print, 'Found ', count, $
        ' common zingers in image1 and image2'
      if (count gt 0) then both = both[indices]
      for i=0, count-1 do begin
        output[both[i]] = (output[both[i]-2] + output[both[i]+2])/2.
        image2[both[i]] = (image2[both[i]-2] + image2[both[i]+2])/2.
      endfor
    endif
  endif

  if (keyword_set(rings)) then begin
    ; Default kernal width for smoothing
    if (n_elements(width) eq 0) then width=9
    ; Sum all of the rows in the image, get average
    ave = total(image, 2)/nrows
    ; Get the difference between the average row and smoothed version
    ; of average row.  These are the column deviations.
    diff = ave - smooth(ave, width, /edge_truncate)
    ; Subtract this difference from each row
    for i=0, nrows-1 do output[0,i] = output[*,i] - diff
  endif

  if (keyword_set(diffraction)) then begin
    ; Default kernal width for smoothing
    if (n_elements(width) eq 0) then width=9
    ; Default threshold for detecting diffraction peaks
    if (n_elements(threshold) eq 0) then threshold=.8
    ; Loop over each column in the image
    for i=0, ncols-1 do begin
      col = reform(output[i,*])
      ratio = col/smooth(col, width, /edge_truncate)
      bad = where(ratio lt threshold)
      col[bad] = (col[(bad-2)>0] + col[(bad+2)<(ncols-1)]) / 2.
      output[i,*] = col
    endfor
  endif
  return, output
end

;+
; NAME:
;  TOMO::OPTIMIZE_CENTER
;
; PURPOSE:
;   This procedure optimizes the rotation center on 2 different slices, typically one near the 
;   top of the projections and the other near the bottom.
;   TOMO::PREPROCESS must be called before calling this procudure. 
;
; CALLING SEQUENCE:
;   TOMO->OPTIMIZE_CENTER, SLICES, CENTER, MERIT, WIDTH=WIDTH, STEP=STEP, METHOD=METHOD
;
; INPUT PARAMETERS:
;   SLICES (required)
;     A 2-element array containing the slice numbers to use for optimization.
;     Normally one should be near the top of the projections and the other near the bottom.
;   CENTER (required)
;     The initial guess of the optimum rotation center.
;     
; OUTPUT PARAMETERS:
;   MERIT:
;     An array of [2, nCenter] where nCenter = WIDTH/STEP + 1. 
;     This array contains the figure of merit for each rotation center for the upper and lower slices.  
;     The minimum value is nominally the best rotation center.
;
; KEYWORD PARAMETERS
;   WIDTH:
;     The full-width of the rotation center search region in pixels. Default=10.
;   STEP:
;     The step size within the WIDTH for each trial rotation center.  
;     Default=0.5 if METHOD='0-180' and 0.25 if METHOD='Entropy'.
;   METHOD:
;     The optimization method.  Valid choices are '0-180' and 'Entropy'.
;     
; PROCEDURE:
;   The IDL tomography documentation (https://CARS-UChicago.github.io/IDL_tomography) explains
;   the optimization algorithms in detail.
;   
;   After the optimization the two slice numbers and the minimum values of MERIT are passed
;   to TOMO::SET_ROTATION.  That routine sets self.rotationCenter and self.rotationCenterSlope
;   which are used to set the rotation center of each slice during reconstruction.
;-
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
  print, 'optimize_center, time=', t1-t0, ' centers=', center1, center2
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
  if (slice1 eq slice2) then begin
    coeffs = [center1, 0.]
  endif else begin
    ; Do a linear fit of slice number and rotation center
    coeffs = poly_fit(slices, center, 1)
  endelse
  self.rotationCenter = coeffs[0]
  self.rotationCenterSlope = coeffs[1]
  self.upperSlice = slice1
  self.lowerSlice = slice2
end

;+
; NAME:
;  TOMO::CORRECT_ROTATION_TILT
;
; PURPOSE:
;   This procedure rotates each projection by the specified angle.
;   It can be used to correct for the rotation axis not being parallel to the columns of the camera.
;
; CALLING SEQUENCE:
;   TOMO->CORRECT_ROTATION_TILT, ANGLE
;
; INPUT PARAMETERS:
;   ANGLE (required)
;     The angle in degrees clockwise by which to rotate each projection.
;
;; PROCEDURE:
;   Uses the IDL ROT function.  self.pVolume is modified in place.
;-
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

pro sine_wave, x, a, f, pder

  ; Used to evaluate a sine wave with unit frequency. This routine is used by
  ; sinogram to fit the center-of-gravity data
  ; a(0) = rotation center
  ; a(1) = amplitude
  ; a(2) = phase
  f = a(0) + a(1)*sin(x + a(2))
  pder = fltarr(n_elements(x), n_elements(a))
  pder(*,0) = 1.
  pder(*,1) = sin(x + a(2))
  pder(*,2) = a(1)*cos(x + a(2))
  return
end


function tomo::sinogram, input, cog=cog
;+
; NAME:
;   TOMO::SINOGRAM
;
; PURPOSE:
;   To convert raw tomography data into a sinogram.
;
; CALLING SEQUENCE:
;   result = TOMO->SINOGRAM(Input)
;
; INPUTS:
;   Input
;     An array of raw tomography data. INPUT(I, J) is the intensity at
;     position I for view angle J. Each row is assumed to contain at least
;     one air value at each end for normalization.
;   COG=cog
;     This keyword is used to return the measured and fitted
;     center-of-gravity data for the sinogram. The center-of-gravity data are
;     very useful for diagnosing problems such as backlash, beam hardening,
;     detector saturation, etc. COG is dimensioned (n_angles, 2), where
;     n_angles is the number of angles in the input array. COG(*,0) is the
;     measured center-of-gravity data. COG(*,1) is the fitted data. The
;     following command can then be given after the SINOGRAM command
;     IDL> PLOT, COG(*,0)
;     IDL> OPLOT, COG(*,1)
;     to see is the sinogram data are reasonable.
; RETURN:
;   The output array containing the corrected sinogram. It is always of type FLOAT.
;
; PROCEDURE:
;   This routine creates a sinogram from raw tomography data. It does the
;   following:
;   -   Averages the air values for "airPixels" pixels on the left and
;       right hand sides of the input.
;   -   Logarithmation. output = -log(input/air). The air values are
;       interpolated between the averaged values for the left and right
;       hand edges of the image for each row.  This step is not performed
;       if the /FLUORESCENCE keyword is set.
;   -   The measured center-of-gravity is fitted to a sinusoidal curve
;       of the form Y = A(0) + A(1)*SIN(X + A(2)).
;           A(0) is the rotation axis
;           A(1) is the amplitude
;           A(2) is the phase
;       The fitting is done using routine CURVE_FIT in the User Library.
;       The shifting is done using routine POLY_2D which can shift by
;       fractional pixels.
;-

  ; Convert data to floating point
  output = float(input)
  nrows = n_elements(output(0,*))
  ncols = n_elements(output(*,0))

  cog = fltarr(nrows)                    ; Center-of-gravity array
  linear = findgen(ncols) / (ncols-1)
  no_air = fltarr(ncols) + self.preprocessScale
  lin2 = findgen(ncols) + 1.
  weight = fltarr(nrows) + 1.
  airPixels = self.airPixels
  for i=0, nrows-1 do begin
    if (airPixels gt 0) then begin
      air_left = total(output(0:airPixels-1,i)) / airPixels
      air_right = total(output(ncols-airPixels:ncols-1,i)) / airPixels
      air = air_left + linear*(air_right-air_left)
    endif else begin
      air = no_air
    endelse
    if (not self.fluorescence) then $
      output(0,i) = -alog(output(*,i)/air > 1.e-5)
    cog(i) = total(output(*,i) * lin2) / total(output(*,i))
  endfor
  x = *self.pAngles*!dtor
  a = [ncols/2., $               ; Initial estimate of rotation axis
    (max(cog) - min(cog))/2., $ ; Initial estimate of amplitude
    0.]                         ; Initial estimate of phase
  cog_fit = curvefit(x, cog, weight, a, sigmaa, $
    function_name='sine_wave')
  cog_mean = a(0)

  if (self.debug) then print, format='(a, f8.2, a, f8.2)', $
    'Fitted center of gravity = ', cog_mean, ' +-', sigmaa(0)
  cog = [[cog], [cog_fit]]

  return, output
end


;+
; NAME:
;   TOMO::TOMO_RECON
;
; PURPOSE:
;   Performs tomographic reconstruction using the "gridrec" algorithm written
;   by Bob Marr and Graham Campbell (not sure about authors) at BNL in 1997.
;
;   It uses CALL_EXTERNAL to call tomoReconIDL.cpp, which is a thin wrapper for tomoRecon.cpp.
;   The tomoRecon C++ code can be found at https://github.com/CARS-UChicago/tomoRecon.
;
; CALLING SEQUENCE:
;   TOMO->GRIDREC, INPUT, OUTPUT, KEYWORD=VALUE, ...
;
; INPUTS:
;   INPUT:
;     The input normalized volume [NZ, NY, NPROJECTIONS].
;     If this array is not of type Float it is first converted to Float.
;
; KEYWORD PARAMETERS:
;   CREATE:
;     Set this keyword to create the tomoRecon C++ object which must be done when changing any
;     reconstruction parameters.  
;     This should not be set when processing additional "chunks" in the same dataset.
;     
;   CENTER:
;     The column containing the rotation axis. 
;     - If this is not set then the center of the sinogram is used for all slices.
;     - If this is a single value then it is used as the rotation center for all slices.
;     - If this is an array then the number of elements must be the number of slices (NY) and
;       slice N will be reconstructed using the rotation center in CENTER[N].
;
;   ANGLES:
;     An array containing the projection angles in degrees.  Default is *self.pAngles.
;
;   WAIT:
;     Set this keyword to wait for the reconstruction to complete before returning.
;     When reconstructing in chunks this keyword can be omitted, which returns control
;     to IDL while the reconstruction continues in the background.
;     This allows overlapping I/O with reconstruction.
;
; OUTPUTS:
;   OUTPUT:
;       The reconstructed array [NX, NX, NY].  Type=Float.
; 
; PROCEDURE:
;   The tomoRecon code is multithreaded, with each thread processing 2 slices at a time.
;   The processing includes
;     - Optional secondary air normalization
;     - Computing the sinogram (logarithm)
;     - Optional ring artifact reduction on the sinogram
;     - Reconstruction
;   The reconstruction parameters are controlled by TOMO::SET_RECON_PARAMS.
;-
pro tomo::tomo_recon, input, $
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
  angles = float(angles)  ; Make sure angles array is type float
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

  tname = size(input, /tname)
  if (tname eq 'FLOAT') then begin
    inputDataType = 0L
  endif else begin
    inputDataType = 1L
  endelse

  output = 0 ; Deallocate any existing array
  if (self.reconDataType eq 'Float32') then begin
    output = fltarr(numPixels, numPixels, numSlices, /nozero)
    outputDataType = 0L
  endif else if (self.reconDataType eq 'UInt16') then begin
    output = uintarr(numPixels, numPixels, numSlices, /nozero)
    outputDataType = 1L
  endif else if (self.reconDataType eq 'Int16') then begin
    output = intarr(numPixels, numPixels, numSlices, /nozero)
    outputDataType = 2L
  endif

  tp = {tomo_params,                                $
    ; Sinogram parameters
    numPixels:            numPixels,                $ ; Number of pixels in sinogram row before padding
    numProjections:       numProjections,           $ ; Number of angles
    numSlices:            numSlices,                $ ; Number of slices
    inputDataType:        inputDataType,            $ ; Input data type (0=float32, 1=uint16)
    outputDataType:       outputDataType,           $ ; Output data type (0=float32, 1=uint16, 2=int16)
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
    t1 = systime(1)
    print, '                 time to reconstruct:', t1-t0
  endif
end

;+
; NAME:
;   TOMO::GRIDREC
;
; PURPOSE:
;   Performs tomographic reconstruction using the "gridrec" algorithm written
;   by Bob Marr and Graham Campbell (not sure about authors) at BNL in 1997.
;   The basic algorithm is based on FFTs.  It reconstructs 2 sinograms at once,
;   one in the real part of the FFT and one in the imaginary part.
;
;   This routine is 20-40 times faster than BACKPROJECT, and yields virtually
;   identical reconstructions. 
;   It uses CALL_EXTERNAL to call GridrecIDL.c which is a thin wrapper to grid.c
;
;   However, GridrecIDL.c only reconstructs 2 slices, and is single-threaded.
;   For fast reconstructions TOMO::TOMO_RECON should be used instead.
;   TOMO::TOMO_RECON is much faster bbecause it also computes the sinograms in C++ rather than in IDL
;   and it is multithreaded and so reconstructs many slices in parallel.
;
;   TOMO::GRIDREC is typically used only when reconstructing single slices when it is desireable to
;   visualize the sinogram and the center-of-gravity plot of the sinogram
;
; CALLING SEQUENCE:
;   TOMO->GRIDREC, SINOGRAM1, SINOGRAM2, IMAGE1, IMAGE2
;
; INPUTS:
;   SINOGRAM1:
;       The first input sinogram, dimensions NX x NANGLES.
;   SINOGRAM2:
;       The second input sinogram, dimensions NX x NANGLES.
;
; KEYWORD PARAMETERS:
;   CENTER: 
;     The column containing the rotation axis. The default is the center of the sinogram.
;
; OUTPUTS:
;   IMAGE1:
;       The reconstructed image from SINOGRAM1.
;   IMAGE2:
;       The reconstructed image from SINOGRAM2.
;
; PROCEDURE:
;   This function uses CALL_EXTERNAL to call the shareable library GridrecIDL,
;   which is written in C.
;-

pro tomo::gridrec, S1, S2, I1, I2, center=center

  image_size = 0L
  n_ang = n_elements(S1[0,*])
  n_det = n_elements(S1[*,0])
  angles = *self.pAngles
  if (n_elements(angles) ne n_ang) then message, 'Incorrect number of angles'

  if (n_elements(center) eq 0) then center=n_det/2.

  t = call_external(self.gridrecShareableLibrary, 'recon_init_IDL', $
    long(n_ang), $
    long(n_det), $
    self.geom, $
    float(angles), $
    float(center), $
    self.pswfParam, $
    self.sampl, $
    self.ROI, $
    self.maxPixSize, $
    self.X0, $
    self.Y0, $
    self.ltbl, $
    [byte(self.GR_filterName), 0B], $
    image_size)

  I1 = fltarr(image_size, image_size)
  I2 = fltarr(image_size, image_size)
  t = call_external(self.gridrecShareableLibrary, 'do_recon_IDL', $
    long(n_ang), $
    long(n_det), $
    long(image_size),$
    float(S1), $
    float(S2), $
    I1, $
    I2);
end

;+
; NAME:
;   TOMO::BACKPROJECT
;
; PURPOSE:
;   Reconstructs a sinogram into an image using backprojection.
;
; CALLING SEQUENCE:
;   Result = BACKPROJECT(SINOGRAM, CENTER=CENTER)
;
; INPUTS:
;   SINOGRAM:
;     The input sinogram, dimensions NX x NANGLES.  
;     This should have been filtered before calling this function.
;     It is typically created with TOMO::SINOGRAM
;
; KEYWORD PARAMETERS
;   CENTER:
;     The rotation center to use when reconstructing.  Default is the center of the sinogram.
;     
; OUTPUTS:
;   This function returns the reconstructed image.
;
; PROCEDURE:
;   This function calls either the IDL RIEMANN or RADON procedure depending on the value of self.BP_Method.
;   These procedures are called with the BACKPROJECT keyword.
;   With RIEMANN interpolation can be None, Bilinear or Cubic depending on the value of self.RiemannInterpolation.
;   With RADON interpolation can be None or Linear, depending on the value of self.RadonInterpolation.
;-

function tomo::backproject, sinogram, center=center
  nrho = self.nx
  nangles = self.nz
  angles = *self.pAngles

  if (self.BP_Method eq self.BP_MethodRiemann) then begin ; in case of Riemann backprojection
    b = fltarr(nrho, nrho)      ;Initial reconstructed image.
    if n_elements(center) then ctr = center
    for i=0,nangles-1 do begin
      riemann, sinogram, b, angles[i]*!dtor, row=i, /backproject, $
        center=ctr, $
        bilinear=(self.RiemannInterpolation eq self.RiemannInterpolationBilinear), $
        cubic=(self.RiemannInterpolation eq self.RiemannInterpolationCubic)
    endfor
    b = b*!pi/nangles

  endif else if(self.BP_Method eq self.BP_MethodRadon) then begin ; in case of Radon backprojection
    if (n_elements(center) eq 0) then center = (nrho-1)/2.
    rhos = (findgen(nrho) - center)
    rangles = angles*!dtor ; convert angles to radians
    b = radon(transpose(sinogram), /backproject, rho=rhos, theta=rangles, nx=nrho, ny=nrho, $
      linear=(self.RadonInterpolation eq self.RadonInterpolationLinear))
  endif

  mask = shift(dist(nrho), nrho/2, nrho/2)
  outside = where(mask gt nrho/2.)
  b(outside) = 0.
  return, b
end

;+
; NAME:
;   TOMO::RECONSTRUCT_SLICE
;
; PURPOSE:
;   Reconstructs a single slice in a tomography volume array.
;
; CALLING SEQUENCE:
;   Result = RECONSTRUCT_SLICE(INPUT, CENTER=CENTER, SINOGRAM=SINOGRAM, COG=COG)
;
; INPUTS:
;   INPUT:
;     The slice to be reconstructed, dimensions [NX, N_PROJECTIONS]
;
; INPUT KEYWORD PARAMETERS:
;   CENTER:
;     The rotation center.  If this keyword is not specified then the
;     center is assumed to be the center pixel of the slice.
;
; OUTPUT KEYWORD PARAMETERS:
;   SINOGRAM:
;     The computed sinogram. This can only be returned when the reconstruction method is
;     Gridrec or Backproject, not tomoRecon.
;   COG:
;     The center of gravity of the sinogram, 1-D array. This can only be returned when the reconstruction method is
;     Gridrec or Backproject, not tomoRecon.
;
; OUTPUTS:
;   This function returns the reconstructed slice.  It is a floating point array of dimensions [NX, NX]
;
; PROCEDURE:
;   Reconstructs the slice with the current reconstruction parameters set with TOMO::SET_RECON_PARAMS.
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
    time2 = systime(1)
    s1 = self->sinogram(t1, cog=cog)
    singram = s1
    s2 = self->sinogram(t2, cog=cog)
    time3 = systime(1)

    if (self.ringWidth eq 0) then begin
      g1 = s1
      g2 = s2
    endif else begin
      g1 = self->remove_artifacts(s1, /rings, width=self.ringWidth)
      g2 = self->remove_artifacts(s2, /rings, width=self.ringWidth)
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
    self->gridrec, g1, g2, r, unused, center=center

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
    time2 = systime(1)
    s1 = self->sinogram(input, cog=cog)
    cent = -1
    time3 = systime(1)
    if (self.ringWidth eq 0) then begin
      g1 = s1
    endif else begin
      g1 = self->remove_artifacts(s1, /rings, width=self.ringWidth)
    endelse

    time4 = systime(1)
    ss1 = self.filter(g1, filter_size=self.BP_filterSize, filter_name=self.BP_filterName)
    singram = ss1
    time5 = systime(1)
    ;if (center ge (nx-1)/2) then ctr = center else ctr = center + 2*abs(round(center - (nx-1)/2))
    r = self->backproject(ss1, center=center)
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
;   This procedure reconstructs a complete 3-D data set (X, Y, Theta) into a 3-D (X, Y, Z) volume.  
;
; CALLING SEQUENCE:
;   TOMO->RECONSTRUCT_VOLUME
;
; PROCEDURE:
;   - The input is normalized data in *self.pVolume.
;   - The reconstruction is done using the parameters set in TOMO::SET_RECON_PARAMS
;   - The rotation center for each slice is determined using self.rotationCenter and self.rotationCenterSlope
;   - If the maximum angle is larger then 180 degrees then it is assumes the data is 360 degrees and calls
;     TOMO::CONVERT_360_DATA to convert to 180.
;   - If *self.pVolume is not of type FLOAT it is converted to FLOAT.
;   - If self.reconMethod is tomoRecon then the reconstruction is done with TOMO::TOMO_RECON,
;     otherwise it is done by reconstructing each slice with TOMO::RECONSTRUCT_SLICE.
;   - The .config file is updated with TOMO::WRITE_SAMPLE_CONFIG.
;     This updates the upper and lower slice numbers and rotation centers used for the reconstruction.
;   - If self.reconDataType is not Float32 then the reconstructed data is converted to Int16 or UInt16.
;   - If self.reconWriteOutput is not 0 then the output is written to disk by TOMO::WRITE_VOLUME,
;     in the format selected by self.reconWriteFormat, i.e. 'HDF5' or 'netCDF'.
;   - The output is returned in *self.pVolume, replacing the normalized data.
;-

pro tomo::reconstruct_volume

  tStart = systime(1)
  self->display_status, 'Initializing reconstruction ...', 1
  if (self.reconWriteFormat eq 'netCDF') then output_file = self.baseFilename + 'recon.nc' $
                                         else output_file = self.baseFilename + 'recon.h5'
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
      r = self->reconstruct_slice(float((*self.pVolume)[*,[i,(i+1)<self.ny],*]), r2, center=center[i])
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

  self.imageType = 'RECONSTRUCTED'
  dims = size(recon, /dimensions)
  self.nx = dims[0]
  self.ny = dims[1]
  self.nz = dims[2]

  if (self.reconWriteOutput) then begin
    self->display_status, 'Writing reconstructed file ...', 1
    self->write_volume, output_file, recon, netcdf=(self.reconWriteFormat eq 'netCDF')
  endif
  self.pVolume = ptr_new(recon, /no_copy)
  tEnd = systime(1)
  print, '     Convert 360 to 180:', tConvert360 - tStart
  print, '            Reconstruct:', tReconDone-tConvert360
  print, '             Write file:', tEnd-tReconDone
  print, '             Total time:', tEnd-tStart
  self->display_status, 'Reconstruction complete.', 1
end


;+
; NAME:
;   TOMO::WRITE_VOLUME
;
; PURPOSE:
;   Writes 3-D arrays containing either normalized or reconstructed data to a disk file.
;   There are currently 2 file formats supported, HDF5 and netCDF.
;
; CALLING SEQUENCE:
;   TOMO->WRITE_VOLUME, FILE, VOLUME
;
; INPUTS:
;   FILE:
;     The name of the file to be written.
;   VOLUME:
;     The 3-D volume data to be written.  The dimensions are NX, NY, NANGLES or NX, NY, NZ
;
; KEYWORD PARAMETERS:
;   NETCDF:
;     Set this keyword  to write files in netCDF file format.  
;     If NETCDF is not set then files are written in HDF5 format.
;
;   The following keywords are currently only supported for netCDF files.
;   Support for HDF5 files may be added in the future.
;
;   [X,Y,Z]OFFSET:
;     The [X,Y,Z] offsets in the disk array to begin writing to.  Default is [0,0,0]
;
;   [X,Y,Z]MAX:
;     The maximum [X,Y,Z] size of the array on disk.
;     Valid only when the file is first created, i.e. if APPEND is not specified.
;     Default is the size of the Volume array in each dimension.
;
;   APPEND:
;     Open an existing file for appending or overwriting data.  Default is APPEND=0 which creates a new file.
;
; PROCEDURE:
;   HDF5 files will contain the following datasets
;     /exchange/data                The volume array
;     /process/imageType            self.imageType
;     /process/angles               *self.pAngles
;     /process/[x,y,z]PixelSize     self.[x,y,z]PixelSize
;     /process/rotationCenter       self.rotationCenter
;     /process/rotationCenterSlope  self.rotationCenter
;     /process/preprocessScale      self.rotationCenterSlope
;     /process/dataOffset           self.preprocessOffset or self.reconOffset depending on self.imageType
;     /process/dataScale            self.preprocessScale or self.reconScale depending on self.imageType
;  
;  netCDF files will contain the following:
;    Dimensions:  NX, NY, NZ
;    Variables:   VOLUME
;    Global attributes:
;      title
;      camera
;      operator
;      sample
;      imageType
;      energy
;      dark current
;      center
;      [x,y,z]PixelSize
;      angles
;      scale_factor 
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
;   Reads in 3-D volume files written by TOMO::WRITE_VOLUME.
;   These are either HDF5 or netCDF format.
;
; CALLING SEQUENCE:
;   Result = READ_TOMO_VOLUME(FILE, STORE=STORE, XRANGE=XRANGE, YRANGE=YRANGE, ZRANGE=ZRANGE)
;
; INPUTS:
;   FILE:
;     The name of the volume file to be read.  If this is not specified then
;     the function will use DIALOG_PICKFILE to allow the user to select a file.
;
; KEYWORD PARAMETERS:
;   STORE
;     If this keyword is set then the data is read into *self->pVolume, 
;     self.nx, self,ny, and self.nz are set appropriately, and the function returns the scalar value 0.
;     If this keyword is not set then the function returns the data read.
;     
;   The behavior of the following keywords depends on the file type:
;   netCDF: Only the specified subset of the data is read from disk.
;   HDF5:   The entire dataset is read from disk, but only the selected subset it returned.
;           In the future this may be improved to only read the selected subset. 
;   XRANGE=[xstart, xstop]
;       The range of X values to read in. The default is to read the entire X range of the data
;   YRANGE=[ystart, ystop]
;       The range of Y values to read in. The default is to read the entire Y range of the data
;   ZRANGE=[zstart, zstop]
;       The range of Z values to read in. The default is to read the entire Z range of the data
;
; OUTPUTS:
;   If the keyword STORE is not set then this function returns the data read.
;   If the keyword is set then the function puts the data in *self.pVolume and returns 0.
;
; PROCEDURE:
;   - Calls self->set_file_components to set self.inputFilename, self.directory, and self.baseFilename
;   - For HDF files the following datasets are read if they are present
;       /exchange/data              *self.pVolume or data returned by function
;       /process/imageType          self.imageType
;       /process/angles             *self.pAngles
;       /process/[x,y,z]PixelSize   self.[x,y,z]PixelSize
;   - For netCDF files the following variables and global attributes are read if present
;       VOLUME                      *self.pVolume or data returned by function
;       imageType                   self.imageType
;       angles                      *self.pAngles
;       scale_factor                self.preprocessScale
;       [x,y,z]PixelSize            self.[x,y,z]PixelSize
;       title                       self.pSampleConfig['SampleDescription1']
;       operator                    self.pSampleConfig['UserName']
;       camera                      self.pSampleConfig['Camera']
;       sample                      self.pSampleConfig['SampleName']
;       energy                      self.pSampleConfig['Energy']
;       dark_current                self.pSampleConfig['DarkFieldValue'], *self.pDarks
;       center                      self.rotationCenter
;   - Calls TOMO::READ_SAMPLE_CONFIG to read additional metadata that is stored in that file
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
;   This function reads the setup information for a tomography data set from an ASCII .setup file.
;   NOTE: These .setup files are present for pre-2020 data collected with raw data in netCDF format
;   They contain original metadata including the dark current.
;   In R1-0 the tomo class re-wrote these files with new metadata, including the rotation center.
;   In R2-0 it reads these .setup files, and writes the new metadata to the .config file.
;
; CALLING SEQUENCE:
;   result = TOMO->READ_SETUP(FILE)
;
; INPUTS:
;   FILE:
;     The name of the input .setup file.
;
; OUTPUTS:
;   The data read from the file is read into the following fields in the tomo object
;     *self.pDarks
;     self.rotationCenter
;     self.[x,y,z]PixelSize
;     self.pConfig[SampleDescription1, UserName, Camera, SampleName, DarkFieldValue, Energy, ImagePixelSize]
; 
; RETURN VALUE:
;   The function returns 0 if it was unable to read the file, 1 if it was successful.
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

;+
; NAME:
;   TOMO::WRITE_SAMPLE_CONFIG
;
; PURPOSE:
;   This writes some tomo object variables to fields in the JSON .config file which contains metadata.
;
; CALLING SEQUENCE:
;   TOMO->WRITE_SAMPLE_CONFIG
;
; PROCEDURE:
;   This is the list of JSON fields and TOMO object variables that are written to the .config file.
;   Other fields in the file are not changed.
;     RotationCenter  self.rotatinCenter
;     RotationSlose   self.rotationCenterSlope
;     UpperSlice      self.upperSlice
;     LowerSlice      self.lowerSlice
;-
pro tomo::write_sample_config
  (*self.pSampleConfig)['RotationCenter'] = self.rotationCenter
  (*self.pSampleConfig)['RotationSlope']   = self.rotationCenterSlope
  (*self.pSampleConfig)['UpperSlice'] = self.upperSlice
  (*self.pSampleConfig)['LowerSlice'] = self.lowerSlice
  openw, lun, /get_lun, self.baseFilename + '.config'
  printf, lun, /implied_print, *self.pSampleConfig
  free_lun, lun
end

;+
; NAME:
;   TOMO::READ_SAMPLE_CONFIG
;
; PURPOSE:
;   This procedure reads some of fields in the JSON .config file into tomo object variables.
;
; CALLING SEQUENCE:
;   TOMO->READ_SAMPLE_CONFIG, FILE
;   
; INPUTS:
;   FILE:  The name of the file to read. Optional, default is self.baseFileName + '.config'
;
; PROCEDURE:
;   This is the list of JSON fields and tomo object variables
;     RotationCenter  self.rotatinCenter
;     RotationSlope   self.rotationCenterSlope
;     UpperSlice      self.upperSlice
;     LowerSlice      self.lowerSlice
;-
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
; NAME:
;   TOMO::GET_STRUCT
;
; PURPOSE:
;   This function returns the member variables of the tomo class as a structure
;   It is provided because IDL class member variables are always private.
;
; CALLING SEQUENCE:
;   result = TOMO->GET_STRUCT()

; RETURN VALUE:
;   Returns the tomo object member variables as a structure.
;-

function tomo::get_struct
  t = {tomo}
  for i=0, n_tags(t)-1 do begin
    t.(i)=self.(i)
  endfor
  return, t
end

;+
; NAME:
;   TOMO::GET_VOLUME
;
; PURPOSE:
;   This function returns self.pVolume.
;   This is a pointer to raw, normalized, or reconstructed data, depending on the most recent operation performed.
;   It is an IDL pointer, so calling this function does not result in a copy operation.
;
; CALLING SEQUENCE:
;   result = TOMO->GET_VOLUME()
;
; RETURN VALUE:
;   Returns self.pVolume.
;-

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
  paddedSinogramWidth = max([numPixels, self.paddedSinogramWidth])
  if (self.paddedSinogramWidth eq 0) then begin
    ; Use the next largest power of 2 by default
    paddedSinogramWidth = 128
    repeat begin
      paddedSinogramWidth=paddedSinogramWidth * 2
    endrep until (paddedSinogramWidth ge numPixels)
  endif else if (self.paddedSinogramWidth eq 1) then begin
    paddedSinogramWidth = numPixels
  endif
  return, paddedSinogramWidth
end

;+
; NAME:
;   TOMO::SET_RECON_PARAMS
;
; PURPOSE:
;   This procedure sets the reconstruction parameters.
;   It uses keywords to set the appropriate TOMO member variable value.
;   It is typically called before calling TOMO::RECONSTRUCT_SLICE or TOMO::RECONSTRUCT_VOLUME.
;
; CALLING SEQUENCE:
;   TOMO->SET_RECON_PARAM, keyword=value, ...
;   
; KEYWORD PARAMETERS
;   Many of these value are explained in more depth in the IDL_Tomography documentation
;   at https://cars-uchicago.github.io/IDL_Tomography/processing_options.html
;   
;   rotationCenter        Rotation center for slice 0.
;   rotCenterSlope        Slope of the rotation in pixels/slice.
;   reconMethod           Reconstruction method: 'tomoRecon', 'GridRec', or 'Backproject'.
;   dataType              Output data type: 'Float32', 'Int16', or 'UInt16'.
;   writeOutput           1 to write the reconstruction to a file, 0 to not write.
;   writeFormat           Output file format: 'HDF5' or 'netCDF'.
;   sinoScale             Scale factor that was used when preprocessing.
;   reconScale            Scale factor for the output.  Typically 1e6 when dataType='Int16' or 'UInt16'.
;   reconOffset           Offset value for the output.  Typically 32767 when dataType='UInt16'.
;   paddedSinogramWidth   0=auto, 1=no padding, other values typically power of 2 and larger than NX.
;   paddingAverage        Number of pixel to average on the image edges for padding.
;   airPixels             Number of air pixels to average for secondary normalization.  0=disable.
;   ringWidth             Number of pixels for ring artifact reduction.  0=disable.
;   fluorescence          1 if this is fluorescence data, 0 if absorption (take logarithm of sinogram).
;   threads               Number of threads to use for reconstruction with tomoRecon.
;   slicesPerChunk        Number of slices per chunk for tomoRecon.  Not currently used.
;   debug                 Debugging print level.  Larger numbers are more verbose output.
;   dbgFile               File to write debugging output to.
;   geom                  Gridrec/tomoRecon geom parameter
;   pswfParam             Gridrec/tomoRecon pswf parameter
;   sampl                 Gridrec/tomoRecon sampl parameter
;   maxPixSize            Gridrec/tomoRecon maxPixSize parameter
;   ROI                   Gridrec/tomoRecon ROI parameter
;   X0                    Gridrec/tomoRecon X0 parameter
;   Y0                    Gridrec/tomoRecon Y0 parameter
;   ltbl                  Gridrec/tomoRecon ltbl parameter
;   GR_filterName         Gridrec/tomoRecon filter name
;   BP_method             Backproject method
;   BP_filterName         Backproject filter name
;   BP_filterSize         Backproject filter size
;   RiemannInterpolation  Riemann interpolation method
;   RadonInterpolation    Radon interpolation method
;-
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

;+
; NAME:
;   TOMO::SET_PREPROCESS_PARAMS
;
; PURPOSE:
;   This procedure sets the preprocess parameters.
;   It uses keywords to set the appropriate tomo member variable value.
;   It is typically called before calling TOMO::PREPROCESS.
;
; CALLING SEQUENCE:
;   TOMO->SET_PREPROCESS_PARAM, keyword=value, ...
;
; KEYWORD PARAMETERS
;   Many of these value are explained in more depth in the IDL_Tomography documentation
;   at https://cars-uchicago.github.io/IDL_Tomography/processing_options.html
;
;   zingerWidth           Number of pixels used for zinger removal. 0=disable.
;   zingerThreshold       Threshold for zinger removal when using median filter for projections.
;   zingerDoubleThreshold Threshold for zinger removal when using double correlation for flat fields.
;   dataType              Output data type: 'Float32' or 'UInt16'.
;   writeOutput           1 to write the preprocessed output data to a file, 0 to not write.
;   writeFormat           Output file format: 'HDF5' or 'netCDF'.
;   scale                 Output scale factor.  Typically 10000.  Needed when output='UInt16'.
;   threads               Number of threads to use for preprocessing.
;-
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

;+
; NAME:
;   TOMO::SAVE_SETTINGS
;
; PURPOSE:
;   This procedure saves the current tomo settings to a file.
;   The file is in JSON format.
;
; CALLING SEQUENCE:
;   TOMO->SAVE_SETTINGS, FILE
;
; INPUTS:
;  FILE: Name of the file to write to.
;-
pro tomo::save_settings, file
  settings = self->get_settings()
  openw, lun, /get_lun, file
  printf, lun, /implied_print, settings
  free_lun, lun
end

;+
; NAME:
;   TOMO::RESTORE_SETTINGS
;
; PURPOSE:
;   This procedure restores the tomo settings from a file that was written by TOMO::SAVE_SETTINGS.
;   The file can contain additional keys beyond those known to the tomo class, and these are ignored.
;   This is the case when the file is written by the TOMO_DISPLAY class, since such files include
;   TOMO_DISPLAY parameters as well.
;
; CALLING SEQUENCE:
;   TOMO->RESTORE_SETTINGS, FILE
;
; INPUTS:
;  FILE: Name of the file to read from to.
;-
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

;+
; NAME:
;   TOMO::INIT
;
; PURPOSE:
;   This function is called implicitly when a new TOMO object is created.
;   It initializes all member variables to reasonable defaults.
;
; CALLING SEQUENCE:
;   TOMO = OBJ_NEW('TOMO', KEYWORD=VALUE, ...)
;
; KEYWORD PARAMETERS:
;   settingsFile   Name of a settings file previously saved with TOMO::SAVE_SETTINGS
;   statusWidget   Widget ID of a status widget. If present, the TOMO methods will write status strings to this widget.
;   abortWidget    Widget ID of an abort width.  If present, the TOMO methods monitor this widget to know when to abort operations.
;   debugLevel     Debugging level for messages
;-
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
  self.gridrecShareableLibrary       = getenv('GRIDREC_SHARE')
  if (self.gridrecShareableLibrary eq "") then begin
    file = 'GridrecIDL_' + !version.os + '_' + !version.arch
    if (!version.os eq 'Win32') then file=file+'.dll' else file=file+'.so'
    self.gridrecShareableLibrary = file_which(file)
  endif
  if (self.gridrecShareableLibrary eq '') then message, 'Gridrec shareable library not defined'
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
    gridrecShareableLibrary:      "", $
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
