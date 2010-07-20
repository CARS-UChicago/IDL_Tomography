pro convert_als_tiff, base_file

    ; This program reconstructs the data from the ALS

    ; The tiff files claim to be unsigned 16-bit integers, but they appear to have an
    ; offset of 32768
    offset = 32768
    
    ; Get the dimensions of the data
    data = read_tiff(base_file + '_1.tif')
    dims = size(data, /dimensions)
    nx = dims[0]
    ny = dims[1]

    ; Read all the flat fields and average them
    f = findfile(base_file + 'bak*.tif')
    nwhites = n_elements(f)
    whites = uintarr(nx, ny, nwhites)
    for i=0, nwhites-1 do begin
        file = base_file + 'bak_' + strtrim(i, 2) + '.tif'
        data = read_tiff(file) - offset
        whites[0,0,i] = data
    endfor

    ; Correct the flat fields for zingers using double correlation
    for i=0, nwhites-2 do begin
        whites[0,0,i] = remove_tomo_artifacts(whites[*,*,i],   $
                                              image2=whites[*,*,i+1], $
                      /double_correlation, threshold=1.05)
    endfor

    white = total(whites, 3)/nwhites

    ; Read all the dark fields and average them
    f = findfile(base_file + 'drk*.tif')
    ndarks = n_elements(f)
    darks = uintarr(nx, ny, ndarks)
    for i=0, ndarks-1 do begin
        file = base_file + 'drk_' + strtrim(i, 2) + '.tif'
        data = read_tiff(file) - offset
        darks[0,0,i] = data
    endfor

    ; Correct the dark fields for zingers using double correlation
    for i=0, ndarks-2 do begin
        darks[0,0,i] = remove_tomo_artifacts(darks[*,*,i],   $
                                             image2=darks[*,*,i+1], $
                      /double_correlation, threshold=1.05)
    endfor

    dark = total(darks, 3)/ndarks
    
    ; Subtract the dark field from the flat field
    white = white - dark

    f = findfile(base_file + '_*.tif')
    nprojections = n_elements(f)
    ; The ALS data goes from 0 to 180, but we don't want to include the 180 file
    nprojections = nprojections - 1
    vol = intarr(nx, ny, nprojections)
    for i=0, nprojections-1 do begin
        file = base_file + '_' + strtrim(i, 2) + '.tif'
        data = read_tiff(file) - offset
        data = ((float(data) - dark) / white)*10000. + 0.5
        data = remove_tomo_artifacts(data, /zingers, threshold=1.25)
        vol[0,0,i] = fix(data)
    endfor

    write_tomo_volume, base_file + '.volume', vol
end
