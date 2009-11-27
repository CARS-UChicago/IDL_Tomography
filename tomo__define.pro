;+
; NAME:
;  TOMO::PREPROCESS
;
; PURPOSE:
;   This procedure reads a tomography data set from individual Princeton
;   Instruments .SPE files.  It writes a 3-D volume file to disk.
;
; CATEGORY:
;   Tomography
;
; CALLING SEQUENCE:
;   TOMO->PREPROCESS, Base_file, First, Last
;
; INPUTS:
;   Base_file:
;       The base file name for the data set.  The actual file name are assumed
;       to be of the form Base_file + strtrim(file_number,2) + '.SPE'.
;   First:
;       The number of the first file, typically 1.
;   Last:
;       The number of the last file
;
; KEYWORD PARAMETERS:
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
;       Note that if the data set contains dark current frames (frame type =
;       DARK_FIELD) then this keyword is normally not used.
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
;   BUFF_ANGLES:
;       The size of the buffer (in # of angles) to be used when writing the normalized
;       data to the output file.  The default is no limit, so an array of size
;       (ncols, nrows, nangles) is allocated.  If BUFF_ANGLES is specified then the
;       output file is write in chunks of "BUFF_ANGLES" at a time, and an array
;       of only (ncols, nrows, BUFF_ANGLES) is needed.  This keyword is useful
;       when processing data sets with many angles.  The output file is identical
;       whether or not this keyword is used, it only affects how many times the
;       output file is opened and appended to.
;   WHITE_FIELD:
;       The white field value, either a scaler or a 2-D array.  If this is a
;       scaler value then each pixel in each data frame is normalized by this
;       constant value.  If this is a 2-D array then it must have the same
;       dimensions as each frame in the data set.  In this case then each data
;       frame in the data set is normalized by the specified 2-D array.
;       Note that if the data set contains white field frames (frame type =
;       FLAT_FIELD), which is typically the case, then this keyword is
;       normally not used.
;   WHITE_AVERAGE:
;       Set this flag to 1 to process the flat fields by averaging all of them
;       together.  The default (WHITE_AVERAGE=0) is to interpolate flat fields in time.
;       NOTE: The default value for this flag is 0 for backward compatibility.
;       However, in general setting WHITE_AVERAGE=1 greatly reduces ring artifacts
;       compared with the default interpolation method, so it's use is strongly
;       recommended.
;   WHITE_SMOOTH:
;       The size of the smoothing kernal for smoothing the white fields.  Set this
;       value to 2 or more to smooth the white fields before normalization.
;       Since white fields generally do not have much high frequency content,
;       smoothing can be used to reduce noise in the normalization.
;       Default=0 (no smoothing).
;   OUTPUT:
;       The name of the output file.  The default is Base_file + '.volume'
;   STATUS_WIDGET:
;       The widget ID of a text widget used to display the status of the
;       preprocessing operation.  If this is a valid widget ID then
;       informational messages will be written to this widget.
;   ABORT_WIDGET
;       The widget ID of a widget used to abort the preprocessing operation.
;       If this is a valid widget ID then the "uvalue" of this widget will be
;       checked periodically.  If it is 1 then this routine will clean up and
;       return immediately.
;   DEBUG:
;       A debugging flag.  Allowed values are:
;           0: No informational output
;           1: Prints each input filename as it is read, and prints limited
;              information on processing steps
;           2: Prints detailed information on processing steps
;
; OUTPUTS:
;   This function returns a 3-dimensional signed 16-bit integer volume array
;   of size [NCOLS, NROWS, NANGLES].  The data is the ratio of the input image
;   to the flat field, multiplied by 10,000.  The ratio of the data to the
;   flat field should be in the range 0 to 1 (with occasional values slightly
;   greater than 1).  Multiplying by 10000 should give sufficient resolution,
;   since even values with 99% absorption will be stored with a precision of
;   1%.
;
; RESTRICTIONS:
;   - There must not be any missing files between the numbers specified by the
;     First and Last parameters.
;   - The input files must follow the naming convention Base_file +
;     strtrim(number,2) + '.SPE', where number varies from First to Last.
;   - By storing the normalized data as 16-bit integers, there is a
;     possibility of loss of some information when using a true 16-bit camera.
;
; PROCEDURE:
;   This function performs the following steps:
;   - Reads each frame in the data set into a large 3-D data buffer.  Stores
;     a flag for each frame indicating if the frame is a dark current, a
;     white field or normal data.
;     Stores the rotation angle at which each frame was collected.
;   - Subtracts the dark current from each data frame and white field frame,
;     using dark current images in the data set if present, or the input dark
;     current if present.
;     If the data set contains multiple dark current frames, then the
;     correction is done as follows:
;         - Use the first dark current for all frames collected before the
;           first dark current
;         - Use the last dark current for all frames collected after the last
;           dark current
;         - Use the average of the closest preceeding and following dark
;           currents for all frames collected between two dark currents.
;   - Removes zingers from white field frames using REMOVE_TOMO_ARTIFACTS with
;     /DOUBLE_CORRELATION if possible, or /ZINGERS if not.
;   - Divides each data frame by the white field, using white field images in
;     the data set, or the input white field if present.  If the
;     data set contains multiple white field frames, and WHITE_AVERAGE=0,
;     then the correction is done as follows:
;         - Use the first white field for all frames collected before the
;           first white field
;         - Use the last white field for all frames collected after the last
;           white field
;         - Use the weighted average of the closest preceeding and following white
;           fields for all frames collected between two white fields.
;     If WHITE_AVERAGE=1 then all of the white fields in the data are averaged
;     before normalizing.  This is recommended.
;     The ratio of each frame to the white field is multiplied by 10,000 to be
;     able to use 16 bit integers, rather than floats to store the results,
;     saving a factor of 2 in memory, which is important for these large 3-D
;     data sets.
;   - Sorts the rotation angle array, to determine the order in which the
;     normalized data frames should be written back out to disk in the volume
;     file.
;   - Corrects for zingers in the white-field normalized data frames, using
;     REMOVE_TOMO_ARTIFACTS, /ZINGERS.
;   - Writes the normalized data frames to a single disk file.  The default
;     file name is Base_file + '.volume'.  This file is in little-endian binary
;     format, with the following data:
;       - NCOLS (long integer, number of columns in each frame)
;       - NROWS (long integer, number of rows in each frame)
;       - NANGLES (long integer, number of frames)
;       - DATA (short integer array [NCOLS, NROWS, NANGLES]
;       The volume file can be read back in to IDL with function
;       READ_TOMO_VOLUME
;
; EXAMPLE:
;   The following example will read files Mydata1.SPE through Mydata370.SPE,
;   using a constant dark current of 50 counts at each pixel.  This data set
;   is assumed to have white field frames in it.  The output file will be
;   Mydata.volume
;       IDL>  READ_TOMO_DATA, 'Mydata', 1, 370, dark=50
;       ; Now read the volume file back into IDL
;       IDL> vol = READ_TOMO_VOLUME('Mydata.volume')
;       ; Play the volume data as a movie, rotating the sample
;       IDL> window, 0
;       IDL> make_movie, vol, min=3000, max=12000
;
; MODIFICATION HISTORY:
;   Written by: Mark Rivers, March 27, 1999.
;   3-APR-1999 MLR  Changed white field normalization to use weighted average
;                   of white fields before and after, rather than simple
;                   average
;   4-APR-1999 MLR  Changed the default value of threshold from 1.20 to 1.05
;                   Switched to double correlation method of zinger removal
;                   for white field images when possible.
;   18-MAY-1999 MLR Added missing keyword DOUBLE_THRESHOLD to procedure line
;   07-JUL-1999 MLR Changed zinger removal for data frames so it is done after
;                   whitefield correction.  This makes the identification of
;                   zingers (versus high-frequency structure in the whitefield)
;                   much more robust.
;   13-SEP-1999 MLR Changed the dark current correction to use a loop, so that
;                   two large arrays are not required at the same time.
;   08-DEC-1999 MLR Added FIRST_ROW and LAST_ROW keywords for handling very
;                   large data sets.
;                   Added OUTPUT keyword
;   02-MAR-2000 MLR Added DEBUG keyword to calls to REMOVE_TOMO_ARTIFACTS
;                   large data sets.
;   02-MAR-2000 MLR Changed the default value of THRESHOLD from 1.05 to 1.25
;                   because the lower threshold was causing significant
;                   blurring of sharp edges.  Changed the default value of
;                   DOUBLE_THRESHOLD from 1.02 to 1.05, since it was finding
;                   many more zingers than physically plausible.
;   02-MAR-2000 MLR Added SWAP_IF_BIG_ENDIAN keyword when opening output file.
;   22-JAN-2001 MLR Added some default debugging when writing output file
;   11-APR-2001 MLR Changed the name of this routine from READ_TOMO_DATA to
;                   TOMO::PREPROCESS when it was incorporated in the TOMO class
;                   library.
;                   This routine now updates the .SETUP file with the dark current
;                   that was specified when running this procedure.
;                   The output file is now written with TOMO::WRITE_VOLUME rather
;                   than being incrementally written as the data are processed.  This
;                   requires more memory, but is necessary to allow use of netCDF
;                   and other file formats.
;   1-NOV-2001 MLR  Added BUFF_ANGLES keyword
;   20-NOV-2001 MLR  Added ABORT_WIDGET and STATUS_WIDGET keywords
;   25-APR-2002 MLR  Added support for reading 3-D .SPE files, created when doing
;                    fast scanning
;   18-DEC-2005 MLR  Added white_average and white_smooth keywords. 
;                    Renamed WHITE keyword to WHITE_FIELD 
;                    Setting white_average greatly reduces ring artifacts in many cases.
;-

pro tomo::read_data_file, base_file, file_number, data, type, angle, debug, $
                    status_widget
   file = base_file + strtrim(file_number, 2) + '.SPE'
   str = 'Reading ' + file
   if (widget_info(status_widget, /valid_id)) then $
        widget_control, status_widget, set_value=str
   read_princeton, file, data, header=header, comment=comment
   ndims = size(data, /n_dimensions)
   if (ndims eq 2) then begin
        angle=float(strmid(comment[0], 6))
        type=strmid(comment[1], 5)
        str = str + ' angle=' + string(angle, format='(f6.2)') + ' type=' + type
        if (widget_info(status_widget, /valid_id)) then $
                widget_control, status_widget, set_value=str
        if (debug ne 0) then print, str
    endif else if (ndims eq 3) then begin
        dims = size(data, /dimensions)
        n_views = dims[2]
        type = strarr(n_views)
        angle = fltarr(n_views)
        start_angle = float(strmid(comment[0], 12))
        angle_step = float(strmid(comment[1], 11))
        flat_fields = strmid(comment[2], 12)
        flat_fields = fix(strsplit(flat_fields, /extract))
        ang = start_angle
        for i=0, n_views-1 do begin
            angle[i] = ang
            if (max(i eq flat_fields)) then begin
                type[i] = 'FLAT_FIELD'
            endif else begin
                type[i] = 'NORMAL'
                ang = ang + angle_step
            endelse
        endfor
    endif
end


pro tomo::preprocess, base_file, start, stop, dark=input_dark, $
                    white_field=input_white, threshold=threshold, $
                    double_threshold=double_threshold, debug=debug, $
                    first_row=first_row, last_row=last_row, output=output, $
                    setup=setup, buff_angles=buff_angles, $
                    white_smooth=white_smooth, white_average=white_average, $
                    status_widget=status_widget, abort_widget=abort_widget

    nfiles = stop - start + 1
    if (n_elements(debug) eq 0) then debug=1
    if (n_elements(threshold) eq 0) then threshold=1.25
    if (n_elements(double_threshold) eq 0) then double_threshold=1.05
    if (n_elements(output) eq 0) then output=base_file + '.volume'
    if (n_elements(setup) eq 0) then setup=base_file + '.setup'
    if (n_elements(white_smooth) eq 0) then white_smooth=0
    if (n_elements(white_average) eq 0) then white_average=0
    if (n_elements(status_widget) eq 0) then status_widget = -1L
    if (n_elements(abort_widget) eq 0) then abort_widget = -1L

    status = self->read_setup(setup)

    ; Read one file to get the dimensions
    self->read_data_file, base_file, 1, data, type, angle, debug, status_widget
    ndims = size(data, /n_dimensions)
    dims = size(data, /dimensions)
    if (ndims eq 2) then begin
        ; This is data with 1 frame per file
        nframes = nfiles
        ncols = dims[0]
        nrows = dims[1]
        ystart = 0
        ystop = nrows-1
        if (n_elements(first_row) ne 0) then ystart = first_row
        if (n_elements(last_row) ne 0) then ystop = last_row
        ; The subset flag is for efficiency below
        if ((ystart eq 0) and (ystop eq nrows-1)) then subset=0 else subset=1
        nrows = ystop - ystart + 1
        if (debug ge 2) then print, 'ncols= ', ncols, ' nrows=', nrows

        data_buff = intarr(ncols, nrows, nframes, /nozero)
        angles = fltarr(nframes)
        image_type = strarr(nframes)
        for i=0, nframes-1 do begin
            self->read_data_file, base_file, start+i, data, type, angle, debug, $
                                status_widget
            if (subset) then begin
                data_buff[0,0,i]=data[*,ystart:ystop]
            endif  else begin
                data_buff[0,0,i]=data
            endelse
            angles[i]=angle
            image_type[i]=type
            if (widget_info(abort_widget, /valid_id)) then begin
                event = widget_event(/nowait, abort_widget)
                widget_control, abort_widget, get_uvalue=abort
                if (abort) then begin
                if (widget_info(status_widget, /valid_id)) then $
                    widget_control, status_widget, $
                                    set_value='Preprocessing aborted'
                return
                endif
            endif
        endfor
    endif else if (ndims eq 3) then begin
        ; This is a file with all of the data in it
        ncols = dims[0]
        nrows = dims[1]
        data_buff = temporary(data)
        nframes = n_elements(data_buff[0,0,*])
        image_type = type
        angles=angle
    endif

    ; Do dark current correction
    str = 'Doing dark current correction ...'
    if (widget_info(status_widget, /valid_id)) then $
                widget_control, status_widget, set_value=str
    if (debug ne 0) then print, str
    darks = where(image_type eq 'DARK_FIELD', ndarks)
    if (debug ge 2) then print, 'ndarks= ', ndarks, 'darks=', darks
    if (n_elements(input_dark) ne 0) then begin
        dark = input_dark
        s = size(dark)  ; Input dark current
        if (debug ge 2) then print, 'input_dark= ', input_dark, $
                              'size(input_dark)=', s
        case s[0] of
            0: begin ; Constant dark current
                for i=0, nframes-1 do data_buff[0,0,i]=data_buff[*,*,i] - dark
                self.dark_current = dark
            end
            2: begin  ; Dark current array
                if (s[1] ne ncols) or (s[2] ne nrows) then $
                            message, 'Wrong dims on dark'
                for i=0, nframes-1 do data_buff[0,0,i]=data_buff[*,*,i] - dark
            end
            else: message, 'Wrong dims on dark'
        endcase
    endif else if (ndarks gt 0) then begin
        ; File contains one or more dark current images
        ; Files up to the first dark current use the first dark current
        dark = data_buff[*, *, darks[0]]
        for i=0, darks[0]-1 do data_buff[0,0,i]=data_buff[*,*,i] - dark
            ; Files after the last dark current use the last dark current
            for i=darks[ndarks-1]+1, nframes-1 do $
                            data_buff[0,0,i]=data_buff[*,*,i] - dark
            ; Files in between the first dark and the last dark use the average of
            ; the dark current before and after
            nseries = ndarks-1
            for j=0, nseries-1 do begin
                dark = (data_buff[*,*,darks[j]] + data_buff[*,*,darks[j+1]]) / 2
                for i=darks[j]+1, darks[j+1]-1 do $
                            data_buff[0,0,i]=data_buff[*,*,i] - dark
            endfor
    endif else begin
        message, 'Must specify dark keyword since no dark frames in data files'
    endelse

    ; Do flat field current correction and zinger removal on flat field frames
    str = 'Doing white field correction ...'
    if (widget_info(status_widget, /valid_id)) then $
                widget_control, status_widget, set_value=str
    if (debug ne 0) then print, str
    whites = where(image_type eq 'FLAT_FIELD', nwhites)
    if (debug ge 2) then print, 'nwhites= ', nwhites, 'whites=', whites
    if (n_elements(input_white) ne 0) then begin
        white = input_white
        s = size(white)  ; Input white field
        case s[0] of
            0: data_buff[0,0,0] = 10000 * (data_buff/float(white))  ; Constant
            2: begin  ; White field array
                if (s[1] ne ncols) or (s[2] ne nrows) then $
                                message, 'Wrong dims on white'
                for i=0, nframes-1 do $
                    data_buff[0,0,i] = 10000 * (data_buff[*,*,i]/float(white))
            end
            else: message, 'Wrong dims on white'
        endcase
    endif else if (nwhites gt 0) then begin
        ; File contains one or more white field images
        ; Extract the white fields into their own array
        white_data = fltarr(ncols, nrows, nwhites)
        for i=0, nwhites-1 do white_data[0,0,i] = data_buff[*,*,whites[i]]
        ; Remove zingers from white fields.  If there is more than 1 white field
        ; do it with double correlation, else do it with spatial filter
        if (nwhites eq 1) then begin
            if (debug ge 1) then print, 'Single white field, correcting zingers with spatial'
            white_data[0,0,0] = remove_tomo_artifacts(white_data[*,*,0], /zingers, threshold=threshold, $
                               debug=debug)
        endif else begin
            if (debug ge 1) then print, 'Multiple white fields, correcting zingers with double correlation'
            for i=0, nwhites-2 do begin
                white_data[0,0,i] = remove_tomo_artifacts(white_data[*,*,i],   $
                                                   image2=white_data[*,*,i+1], $
                              /double_correlation, threshold=double_threshold, $
                              debug=debug)
            endfor
        endelse
        ; If we are smoothing white fields ...
        if (white_smooth gt 1) then begin
            if (debug ge 1) then print, 'Smoothing white fields, smooth width=', white_smooth
            for i=0, nwhites-1 do begin
                white_data[0,0,i] = smooth(white_data[*,*,i], white_smooth)
            endfor
        endif
        ; If we are averaging white fields ...
        if keyword_set(white_average) then begin
            if (debug ge 1) then print, 'Averaging white fields'
            white = total(white_data, 3)/nwhites
            for i=0, nframes-1 do begin
                data_buff[0,0,i] = 10000 * (data_buff[*,*,i]/white)
                if (widget_info(abort_widget, /valid_id)) then begin
                    event = widget_event(/nowait, abort_widget)
                    widget_control, abort_widget, get_uvalue=abort
                    if (abort) then begin
                        if (widget_info(status_widget, /valid_id)) then $
                            widget_control, status_widget, $
                            set_value='Preprocessing aborted'
                        return
                    endif
                endif
            endfor
        endif else begin
            if (debug ge 1) then print, 'Interpolating white fields'
            ; We are interpolating white fields
            ; Files up to the first white field use the first white field
            white = white_data[*,*,0]
            for i=0, whites[0]-1 do $
                data_buff[0,0,i] = 10000 * (data_buff[*,*,i]/white)
            ; Files after the last white field use the last white field
            white = white_data[*,*,[nwhites-1]]
            for i=whites[nwhites-1]+1, nframes-1 do $
                data_buff[0,0,i] = 10000 * (data_buff[*,*,i]/white)
            ; Files in between the first white field and the last white field use the
            ; weighted average of the white field before and after
            nseries = nwhites-1
            for j=0, nseries-1 do begin
                white1 = white_data[*,*,j]
                white2 = white_data[*,*,j+1]
                nf = whites[j+1] - whites[j] - 1
                for i=0, nf-1 do begin
                    k = i + whites[j]+1
                    ratio = float(i)/float(nf-1)
                    white = white1*(1.0-ratio) + white2*ratio
                    data_buff[0,0,k] = 10000 * (data_buff[*,*,k]/white)
                endfor
                if (widget_info(abort_widget, /valid_id)) then begin
                    event = widget_event(/nowait, abort_widget)
                    widget_control, abort_widget, get_uvalue=abort
                    if (abort) then begin
                        if (widget_info(status_widget, /valid_id)) then $
                            widget_control, status_widget, $
                            set_value='Preprocessing aborted'
                        return
                    endif
                endif
            endfor
        endelse  ; Interpolate
    endif else begin
        message, 'Must specify white keyword since no white frames in data files'
    endelse

    ; Now we have normalized data.
    ; Correct for zingers now that flat field normalization is done
    ; Write out to disk file, sorted by angle, arranged by slice
    str = 'Writing volume file ...'
    if (widget_info(status_widget, /valid_id)) then $
            widget_control, status_widget, set_value=str
    if (debug ne 0) then print, str
    data_images = where(image_type eq 'NORMAL', nangles)
    angles = angles[data_images]
    sorted_indices = sort(angles)
    angles = angles[sorted_indices]
    ptr_free, self.angles
    self.angles = ptr_new(angles)
    data_images = data_images[sorted_indices]
    ncols = long(ncols)
    nrows = long(nrows)
    nangles = long(nangles)
    if (n_elements(buff_angles) eq 0) then buff_angles = nangles
    dummy = intarr(2,2,2)
    self->write_volume, output, dummy, /corrected, xmax=ncols, ymax=nrows, $
                        zmax=nangles
    vol = intarr(ncols, nrows, buff_angles)
    angle_index = 0
    zoffset=0
    for i=0, nangles-1 do begin
        proj = reform(data_buff[*,*,data_images[i]])
        proj = remove_tomo_artifacts(proj, /zingers, threshold=threshold, $
                                debug=debug)
        str = 'Copying projection ' + strtrim(i+1,2) + '/' + $
                                      strtrim(nangles,2)
        if (widget_info(status_widget, /valid_id)) then $
                    widget_control, status_widget, set_value=str
        if (debug ne 0) then print, str
        vol[0,0,angle_index] = proj
        if (angle_index eq (buff_angles-1)) then begin
            str = 'Writing volume file ...'
            if (widget_info(status_widget, /valid_id)) then $
            widget_control, status_widget, set_value=str
            if (debug ne 0) then print, str
            write_tomo_volume, output, vol, /corrected, /append, zoffset=zoffset
            angle_index=0
            zoffset = zoffset + buff_angles
        endif else begin
            angle_index = angle_index+1
        endelse
        if (widget_info(abort_widget, /valid_id)) then begin
            event = widget_event(/nowait, abort_widget)
            widget_control, abort_widget, get_uvalue=abort
            if (abort) then begin
               if (widget_info(status_widget, /valid_id)) then $
                   widget_control, status_widget, $
                                   set_value='Preprocessing aborted'
               return
            endif
        endif
    endfor
    if (angle_index ne 0) then begin
        ; We have a partially filled buffer that needs to be written
        str = 'Writing volume file ...'
        if (widget_info(status_widget, /valid_id)) then $
                    widget_control, status_widget, set_value=str
        if (debug ne 0) then print, str
        write_tomo_volume, output, vol[*,*,0:angle_index-1], /corrected, /append, $
                           zoffset=zoffset
    endif
    status = self->write_setup(setup)
    if (widget_info(status_widget, /valid_id)) then $
        widget_control, status_widget, set_value='Preprocessing complete.'

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
;       The inverse of the SCALE is stored as the attribute volume:scale_factor
;       in the netCDF file.
;   STATUS_WIDGET:
;       The widget ID of a text widget used to display the status of the
;       preprocessing operation.  If this is a valid widget ID then
;       informational messages will be written to this widget.
;   ABORT_WIDGET
;       The widget ID of a widget used to abort the preprocessing operation.
;       If this is a valid widget ID then the "uvalue" of this widget will be
;       checked periodically.  If it is 1 then this routine will clean up and
;       return immediately.
;
; OUTPUTS:
;   This procedure writes its results to a file base_file+'_recon.volume'
;
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
;   22-NOV-2001 MLR  Added STATUS_WIDGET and ABORT_WIDGET keywords
;   02-APR-2002 MLR  Fixed bugs with STATUS_WIDGET and ABORT_WIDGET
;   11-APR-2002 MLR  Fixed bug introduced on 02-APR with center
;-

pro tomo::reconstruct_volume, base_file, center=center, scale=scale, $
                              angles=angles, abort_widget=abort_widget, $
                              status_widget=status_widget, $
                              _ref_extra=extra

    if (n_elements(status_widget) eq 0) then status_widget = -1L
    if (n_elements(abort_widget) eq 0) then abort_widget = -1L
    status = self->read_setup(base_file+'.setup')
    str = 'Reading volume file ...'
    if (widget_info(status_widget, /valid_id)) then $
        widget_control, status_widget, set_value=str
    print, str
    vol = self->read_volume(base_file + '.volume', _extra=extra)
    if (n_elements(center) ne 0) then self.center = center[0]
    if (n_elements(angles) ne 0) then self.angles = ptr_new(angles)
    if (n_elements(scale) eq 0) then scale=1.e6

    self.scale_factor = 1./scale
    ; This procedure reconstructs all of the slices for a tomography data set
    nrows = n_elements(vol[0,*,0])
    ; If we are using GRIDREC to reconstruct then we get 2 slices at a time
    if (keyword_set(back_project)) then step=1 else step=2
    for i=0, nrows-1, step do begin
        str = 'Reconstructing slice ' + strtrim(i,2) + '/' + $
                                        strtrim(nrows-1,2)
        if (widget_info(status_widget, /valid_id)) then $
            widget_control, status_widget, set_value=str
        print, str
        if (n_elements(center) eq 0) then cent=-1
        if (n_elements(center) eq 1) then cent=center
        if (n_elements(center) eq 2) then cent = $
                     center[0] + float(i) / (nrows-1) * (center[1]-center[0])
        r = reconstruct_slice(i, vol, r2, center=cent, scale=scale, angles=angles, $
                              _extra=extra)
        if (i eq 0) then begin
            ncols = n_elements(r[*,0])
            recon = intarr(ncols, ncols, nrows, /nozero)
        endif
        recon[0,0,i] = r
        if ((n_elements(r2) ne 0) and (i ne nrows-1)) then begin
            recon[0,0,i+1] = r2
        endif
        if (widget_info(abort_widget, /valid_id)) then begin
            event = widget_event(/nowait, abort_widget)
            widget_control, abort_widget, get_uvalue=abort
            if (abort) then begin
                if (widget_info(status_widget, /valid_id)) then $
                    widget_control, status_widget, $
                                    set_value='Reconstruction aborted.'
                return
            endif
        endif
    endfor
    ; If there was no input angle array copy the one that reconstruct_slice
    ; generated back into self
    if (not ptr_valid(self.angles)) then self.angles=ptr_new(angles)
    ; We are all done with the vol array, free it
    vol = 0
    if (widget_info(status_widget, /valid_id)) then $
        widget_control, status_widget, set_value='Writing volume file ...'
    self->write_volume, base_file + 'recon.volume', recon, /reconstructed
    status = self->write_setup(base_file + '.setup')
    if (widget_info(status_widget, /valid_id)) then $
        widget_control, status_widget, set_value='Reconstruction complete.'

end


pro tomo::write_volume, file, volume, netcdf=netcdf, append=append, $
                        raw=raw, corrected=corrected, reconstructed=reconstructed, $
                        xoffset=xoffset, yoffset=yoffset, zoffset=zoffset, $
                        xmax=xmax, ymax=ymax, zmax=zmax

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

    if (n_elements(netcdf) eq 0) then netcdf=1

    size = size(volume)
    ; If this is a 2-D array fake it out by setting third dimension to 1
    if (size[0] eq 2) then size[3]=1
    if (keyword_set(raw)) then self.image_type = "RAW"
    if (keyword_set(corrected)) then self.image_type = "CORRECTED"
    if (keyword_set(reconstructed)) then self.image_type = "RECONSTRUCTED"


    if (netcdf eq 0) then begin
        openw, lun, file, /get, /swap_if_big_endian
        ncols = size[1]
        nrows = size[2]
        nangles = size[3]
        writeu, lun, ncols, nrows, nangles, volume
        free_lun, lun
        return
    endif else begin

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
            ncdf_attput, file_id, /GLOBAL, 'center', self.center
            ncdf_attput, file_id, /GLOBAL, 'x_pixel_size', self.x_pixel_size
            ncdf_attput, file_id, /GLOBAL, 'y_pixel_size', self.y_pixel_size
            ncdf_attput, file_id, /GLOBAL, 'z_pixel_size', self.z_pixel_size
            if (ptr_valid(self.angles)) then $
                ncdf_attput, file_id, /GLOBAL, 'angles', *(self.angles)
            if (self.scale_factor ne 0) then scale=self.scale_factor else scale=1.0
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
        ncdf_varput, file_id, vol_id, volume, $
                     offset=offset, count=count, stride=stride

        ; Close the file
        ncdf_close, file_id
    endelse
end


function tomo::read_volume, file, $
         xrange=xrange, yrange=yrange, zrange=zrange

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

    if (n_elements(file) eq 0) then file = dialog_pickfile(/read, /must_exist)
    if file eq "" then return, 0

    on_ioerror, ignore_error
    ncdf_control, 0, /noverbose
    file_id = ncdf_open(file, /nowrite)
ignore_error:
    if (n_elements(file_id) eq 0) then begin
        on_ioerror, null
        ; This is not a netCDF file, it is an old APS format file
        openr, lun, file, error=error, /get, /swap_if_big_endian
        if (error ne 0) then message, 'Error opening file: ' + file
        nx = 0L
        ny = 0L
        nz = 0L
        header = 12  ; Size of header in bytes
        readu, lun, nx, ny, nz
        ; Simplest case is if no ranges are specified, no need to loop
        if (n_elements(zrange) eq 0) and (n_elements(yrange) eq 0) and $
            (n_elements(xrange) eq 0) then begin
            volume = intarr(nx, ny, nz, /nozero)
            readu, lun, volume
        endif else begin
            if (n_elements(xrange) eq 0) then xrange = [0, nx-1]
            if (n_elements(yrange) eq 0) then yrange = [0, ny-1]
            if (n_elements(zrange) eq 0) then zrange = [0, nz-1]
            ; Compute nx, ny, nz clip if user specified too large a range
            ix = (xrange[1] - xrange[0] + 1) < nx
            iy = (yrange[1] - yrange[0] + 1) < ny
            iz = (zrange[1] - zrange[0] + 1) < nz
            volume = intarr(ix, iy, iz, /nozero)
            slice = intarr(nx, ny, /nozero)
            point_lun, lun, header + zrange[0]*nx*ny*2L
            for i = 0, iz-1 do begin
                readu, lun, slice
                volume[0, 0, i] = $
                    slice[xrange[0]:xrange[1], yrange[0]:yrange[1]]
            endfor
            volume = reform(volume, /overwrite)
            free_lun, lun
        endelse
    endif else begin
        ; This is a netCDF file
        ; Clear any existing information
        self->clear_setup
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
                'center':       self.center =       value
                'x_pixel_size': self.x_pixel_size = value
                'y_pixel_size': self.y_pixel_size = value
                'z_pixel_size': self.z_pixel_size = value
                'angles':       begin
                                    ptr_free, self.angles
                                    self.angles = ptr_new(value)
                                end
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
                'scale_factor': self.scale_factor = value
            endcase
        endfor

        ; Close the netCDF file
        ncdf_close, file_id
    endelse  ; netCDF

    return, volume
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
    openw, lun, file, error=error, /get_lun
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
    printf, lun, 'CENTER: ', self.center
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
    self->clear_setup
    ncomments = 0
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
            'TITLE:'  :  self.title = value
            'OPERATOR:'  :  self.operator = value
            'CAMERA:'  :  self.camera = value
            'SAMPLE:'  :  self.sample = value
            'COMMENT:'  :  begin
                comment[ncomments] = value
                ncomments = ncomments + 1
            end
            'DARK_CURRENT:'  :  self.dark_current = value
            'CENTER:'  :  self.center = value
            'ENERGY:'  :  self.energy = value
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
;        scale_factor: 0., $
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

function tomo::get_setup
    t = {tomo}
    for i=0, n_tags(t)-1 do begin
        t.(i)=self.(i)
    endfor
    return, t
end


function tomo::init, file
    if (n_elements(file) ne 0) then status = self->read_setup(file)
    return, 1
end

pro tomo::cleanup
    ptr_free, self.comments
    ptr_free, self.angles
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
    self.center=0.
    self.energy=0.
    self.x_pixel_size=0.
    self.y_pixel_size=0.
    self.z_pixel_size=0.
    self.scale_factor=0.
    self.nx=0L
    self.ny=0L
    self.nz=0L
    ptr_free, self.angles
    self.angles=ptr_new()
end


pro tomo__define
    tomo = $
       {tomo, $
        title: " ", $
        operator: " ", $
        camera: " ", $
        sample: " ", $
        comments: ptr_new(), $
        image_type: " ", $  ; "RAW", "CORRECTED" or "RECONSTRUCTED"
        dark_current: 0., $
        center: 0., $
        energy: 0., $
        x_pixel_size: 0., $
        y_pixel_size: 0., $
        z_pixel_size: 0., $
        scale_factor: 0., $
        nx:     0L, $
        ny:     0L, $
        nz:     0L, $
        angles: ptr_new() $
    }
end
