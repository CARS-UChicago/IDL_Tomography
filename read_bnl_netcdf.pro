function read_bnl_netcdf, file, center

; This procedure reads a BNL format netCDF file.  It will read
;   sinogram files which have sinogram(x, angle) 
;   reconstructed images which have slice(x, y)
;   3-D files which have volume(x, y, z)

    ; Open the netCDF file for reading
    ncid = ncdf_open (file, /NOWRITE);

    ; Get the variable id
    sinogram_id = ncdf_varid (ncid, "sinogram")
    slice_id    = ncdf_varid (ncid, "slice")
    volume_id   = ncdf_varid (ncid, "volume")

    ; Read the data
    if (sinogram_id ne -1) then begin
        ncdf_varget, ncid, sinogram_id, data
        centerid = ncdf_varid(ncid, "center")
        ncdf_varget1, ncid, centerid, center
    endif
    if (slice_id ne -1) then begin
        ncdf_varget, ncid, slice_id, data
    endif
    if (volume_id ne -1) then begin
        ncdf_varget, ncid, volume_id, data
    endif

    ; Close the netCDF file
    ncdf_close, ncid

    return, data
end
