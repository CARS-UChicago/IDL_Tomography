; Procedure to read multiple volume files collected with vertical translation between them

pro combine_vertical_stack, base_file_name, num_volumes, pixel_overlap, zstart=zstart
    if (n_elements(zstart) eq 0) then zstart=0
    extensions = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']
    print, 'Reading volume ', 0
    v = read_tomo_volume(base_file_name + '_' + extensions[0] + 'recon.volume')
    dims = size(v, /dimensions)
    nx = dims[0]
    ny = dims[1]
    nz = dims[2]
    nz_total = nz + (num_volumes-1) * (nz - pixel_overlap)
    print, 'Combined dimensions = ', nx, ny, nz_total
    vol = intarr(nx, ny, nz_total, /nozero)
    vol[0,0,0] = v
    for i=1, num_volumes-1 do begin
       print, 'Reading volume ', i
       if (zstart ne 0) then begin
           zrange = [zstart, nz-1]
           vol[0, 0, (((nz-pixel_overlap)*i) + zstart)] = read_tomo_volume(base_file_name + '_' + extensions[i] + 'recon.volume', zrange=zrange)
       endif else begin
           vol[0, 0, (nz-pixel_overlap)*i] = read_tomo_volume(base_file_name + '_' + extensions[i] + 'recon.volume')      
       endelse
    endfor
    print, 'Writing combined volume '
    write_tomo_volume, base_file_name + '_combined_recon.volume', vol
    print, 'Done'
end

    
