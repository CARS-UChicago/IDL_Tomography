pro large_vol_diff, max_slices, file_above, file_below, file_diff, $
                    xshift=xshift, yshift=yshift, zshift=zshift

    if (n_elements(max_slices) eq 0) then max_slices=100
    if (n_elements(xshift) eq 0) then xshift = 0
    if (n_elements(yshift) eq 0) then yshift = 0
    if (n_elements(zshift) eq 0) then zshift = 0

    if (n_elements(file_above) eq 0) then file_above = dialog_pickfile(title='Select volume above edge...', filter='*.volume', /must_exist, get_path=get_path)
    if file_above eq '' then return
    if (n_elements(file_below) eq 0) then file_below = dialog_pickfile(title='Select volume below edge...', filter='*.volume', /must_exist)
    if file_below eq '' then return
    if (n_elements(file_diff) eq 0) then file_diff = dialog_pickfile(title='Select name for volume output...', filter='*.volume', /overwrite_prompt)
    if file_diff eq '' then return

    ; Read the first file, first 2 slices to get the X and Y dimensions
    print, 'Reading below edge file to get X, Y dimensions: ', file_below
    vol = read_tomo_volume(file_above, zrange=[0,1])
    dims = size(vol, /dimensions)
    nx = dims[0]
    ny = dims[1]
    ; Read the first file, first 2 Y to get the Z dimension
    print, 'Reading below edge file to get Z dimensions: ', file_below
    vol = read_tomo_volume(file_above, yrange=[0,1])
    dims = size(vol, /dimensions)
    nz = dims[2]
    last_slice = nz-1
    ; Create the output file
    zstart=0
    print, 'Creating output difference file: ', file_diff
    write_tomo_volume, file_diff, xmax=nx, ymax=ny, zmax=nz, vol
    while (zstart lt last_slice) do begin
        zend = zstart + max_slices - 1
        if (zend gt last_slice) then zend=last_slice
        print, 'Doing slice ', zstart, ' to ', zend, ' of ', last_slice
        print, '  Reading above edge file:', file_above
        vol_above = read_tomo_volume(file_above, zrange=[zstart, zend])
        print, '  Reading below edge file:', file_below
        vol_below = read_tomo_volume(file_below, zrange=[zstart, zend])
        print, '  Computing difference '
        vol_diff = shift(vol_above, xshift, yshift, zshift) - vol_below
        print, '  Writing difference file:', file_diff
        write_tomo_volume, file_diff, vol_diff, zoffset=zstart, /append
        zstart = zstart + max_slices
    endwhile

end
