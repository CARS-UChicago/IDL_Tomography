; This procedure converts the projection files from X2B at NSLS to
; the netCDF volume file formats used at GSECARS


function convert_x2b_netcdf_check_abort, status_widget, abort_widget
    if (widget_info(abort_widget, /valid_id)) then begin
        event = widget_event(/nowait, abort_widget)
        widget_control, abort_widget, get_uvalue=abort
        if (abort) then begin
            if (widget_info(status_widget, /valid_id)) then $
                widget_control, status_widget, $
                                set_value='Conversion aborted'
            return, 1
        endif else begin
            return, 0
        endelse
    endif else begin
        return, 0
    endelse
end


pro convert_x2b_netcdf, input_file, output_file, $
                        xrange=xrange, yrange=yrange, zrange=zrange, $
                        max_memory=max_memory, $
                        abort_widget=abort_widget, $
                        status_widget=status_widget

    if (n_elements(status_widget) eq 0) then status_widget = -1L
    if (n_elements(abort_widget) eq 0) then abort_widget = -1L
    input_basefile = file_basename(input_file)
    output_basefile = file_basename(output_file)
    status_widget_valid = widget_info(status_widget, /valid_id)

    ; Read the header and the first projection of the input file to get its 
    ; dimensions
    read_x2b_proj, input_file, header=header, data, zrange=[0,0]

    ; From the dimensions of the input file compute the amount of memory
    ; required for the input and output arrays.
    nz = header.nviews
    nx = header.ysize
    ny = header.end_slc - header.beg_slc + 1
    if (n_elements(xrange) ne 0) then begin
        xrange[0] = xrange[0] > 0
        xrange[1] = xrange[1] < (nx-1)
        nx = xrange[1] - xrange[0] + 1
    endif
    if (n_elements(yrange) ne 0) then begin
        yrange[0] = yrange[0] > 0
        yrange[1] = yrange[1] < (ny-1)
        ny = yrange[1] - yrange[0] + 1
    endif
    if (n_elements(zrange) ne 0) then begin
        zrange[0] = zrange[0] > 0
        zrange[1] = zrange[1] < (nz-1)
        nz = zrange[1] - zrange[0] + 1
    endif
    BYTES_PER_VOXEL = 2 ; Voxel size on input and output
    required_size = 1.d0*nx*ny*nz*(BYTES_PER_VOXEL)

    ; Create a netCDF file of the correct final size
    dummy = intarr(2,2,2)
    text = 'Creating netCDF file ' + output_basefile
    print, text
    if (status_widget_valid) then widget_control, status_widget, set_value=text
    write_tomo_volume, output_file, dummy, xmax=nx, ymax=ny, zmax=nz, /corrected
    if (n_elements(zrange) eq 0) then begin
        zstart = 0
        zstop = nz-1
    endif else begin
        zstart = zrange[0]
        zstop = zrange[1]
    endelse

    ; Compute the number of projections we can process at once
    if (n_elements(max_memory) eq 0) then begin
        zchunk = nz
    endif else begin
        zchunk = long(max_memory/(1.d0*nx*ny*(BYTES_PER_VOXEL)))
    endelse
    znext = zstart
    while (znext le zstop) do begin
        if (convert_x2b_netcdf_check_abort(status_widget, abort_widget)) then return
        zend = (znext + zchunk) < zstop
        zr = [znext, zend]
        if (convert_x2b_netcdf_check_abort(status_widget, abort_widget)) then return
        text = 'Reading X2B file: '+input_basefile+$
               ' projections '+strtrim(zr[0],2)+':'+strtrim(zr[1],2)
        print, text
        if (status_widget_valid) then widget_control, status_widget, set_value=text
        data = 0
        read_x2b_proj, input_file, data, $
                       xrange=xrange, yrange=yrange, zrange=zr
        n = zend-znext+1
        for i=0, n-1 do begin
            if (convert_x2b_netcdf_check_abort(status_widget, abort_widget)) then return
            text = 'Converting projection ' + strtrim(znext+i+1,2) + '/' +  $
                   strtrim(zstop+1, 2) + ' from log to linear ...'
            print, text
            if (status_widget_valid) then widget_control, status_widget, set_value=text
            projection = float(data[*,*,i])
            projection = 10000.*exp(temporary(projection)/(-6000.)) + 0.5
            data[0,0,i] = fix(projection)
        endfor
        if (convert_x2b_netcdf_check_abort(status_widget, abort_widget)) then return
        text = 'Writing netCDF file: '+output_basefile+$
               ' projections '+strtrim(zr[0],2)+':'+strtrim(zr[1],2)
        print, text
        if (status_widget_valid) then widget_control, status_widget, set_value=text
        write_tomo_volume, output_file, data, /append, zoffset=znext
        znext = zend + 1
    endwhile

    text = 'Completed conversion of ' + input_basefile + ' to ' + output_basefile
    print, text
    if (status_widget_valid) then widget_control, status_widget, set_value=text

end

