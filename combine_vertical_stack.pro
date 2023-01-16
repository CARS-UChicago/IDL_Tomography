; Procedure to read multiple volume files collected with vertical translation between them

pro combine_vertical_stack, base_file_name, num_volumes, pixel_overlap, zstart=zstart, extensions=extensions, suffix=suffix
    if (n_elements(zstart) eq 0) then zstart=0
    if (n_elements(extensions) eq 0) then extensions = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']
    if (n_elements(suffix) eq 0) then suffix='.h5'
    print, systime() + ' Reading volume ', 0
    v = read_tomo_volume(base_file_name + '_' + extensions[0] + 'recon' + suffix)
    dims = size(v, /dimensions)
    nx = dims[0]
    ny = dims[1]
    nz = dims[2]
    nz_total = nz + (num_volumes-1) * (nz - pixel_overlap)
    print, systime() + ' Combined dimensions = ', nx, ny, nz_total
    vol = intarr(nx, ny, nz_total, /nozero)
    vol[0,0,0] = v
    for i=1, num_volumes-1 do begin
       print, systime() + ' Reading volume ', i
       if (zstart ne 0) then begin
           zrange = [zstart, nz-1]
           vol[0, 0, (((nz-pixel_overlap)*i) + zstart)] = read_tomo_volume(base_file_name + '_' + extensions[i] + 'recon' + suffix, zrange=zrange)
       endif else begin
           vol[0, 0, (nz-pixel_overlap)*i] = read_tomo_volume(base_file_name + '_' + extensions[i] + 'recon' + suffix)      
       endelse
    endfor
    print, systime() + ' Writing combined volume '
    write_tomo_volume, base_file_name + 'combined_recon' + suffix, vol
    print, systime() + ' Done'
end

    
