; This procedure tests the memory consumption with signed and unsigned shorts and
; longs
pro test_unsigned, volume
    dims = size(volume, /dimensions)
    type = size(volume, /type)
    file = 'test.nc'

    t0 = systime(1)
    ; Create netCDF file
    file_id = ncdf_create(file, /clobber)
    ncdf_control, file_id, fill=0
    
    ; Create dimensions
    nx_id = ncdf_dimdef(file_id, 'NX', dims[0])
    ny_id = ncdf_dimdef(file_id, 'NY', dims[1])
    nz_id = ncdf_dimdef(file_id, 'NZ', dims[2])

    ; Create variables
    if ((type eq 2) or (type eq 12)) then begin
        vol_id = ncdf_vardef(file_id, 'VOLUME', [nx_id, ny_id, nz_id], /SHORT)
    endif else if ((type eq 3) or (type eq 13)) then begin
        vol_id = ncdf_vardef(file_id, 'VOLUME', [nx_id, ny_id, nz_id], /LONG)
    endif else begin
        message, 'Invalid type'
    endelse
    
    ; Put the file into data mode.
    ncdf_control, file_id, /endef
    t1 = systime(1)
    
    ; Write volume data to the file
    ncdf_varput, file_id, vol_id, volume
    t2 = systime(1)

    ; Close the file
    ncdf_close, file_id
    t3 = systime(1)

    print, 'Time to initialize file:', t1-t0
    print, 'Time to write data:', t2-t1
    print, 'Time to close file:', t3-t2
    print, 'Total time:', t3-t0
end
