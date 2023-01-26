; Procedure to read multiple volume files collected with vertical translation between them

pro rebin_with_trim, vol, binning
  if (binning eq 1) then return
  dims = size(vol, /dimensions)
  new_dims = dims/binning
  new_scaled_dims = new_dims * binning
  if (not array_equal(dims, new_scaled_dims)) then begin
    vol = vol[0:new_scaled_dims[0]-1, 0:new_scaled_dims[1]-1, 0:new_scaled_dims[2]-1]
  endif
  vol = rebin(vol, new_dims)
end

pro combine_vertical_stack, base_file_name, num_volumes, pixel_overlap, $
                            zstart=zstart, extension=extension, suffix=suffix, binning=binning
    if (n_elements(zstart) eq 0) then zstart=0
    if (n_elements(suffix) eq 0) then suffix = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']
    if (n_elements(binning) eq 0) then binning = 1
    if (n_elements(extension) eq 0) then extension='.h5'
    is_netcdf = extension ne '.h5'
    file = base_file_name + '_' + suffix[0] + 'recon' + extension
    print, systime() + ' Reading volume 0 ' + file
    t = obj_new('tomo')
    v = t->read_volume(file)
    if (binning ne 1) then begin
      print, systime() + ' Rebinning volume 0'
      rebin_with_trim, v, binning
    endif
    dims = size(v, /dimensions)
    nx = dims[0]
    ny = dims[1]
    nz = dims[2]
    nz_total = nz + (num_volumes-1) * (nz - pixel_overlap/binning)
    print, systime() + ' Creating combined array dimensions = [' + strtrim(nx,2) + ',' + strtrim(ny,2) + ',' + strtrim(nz_total,2) + ']'
    vol = intarr(nx, ny, nz_total, /nozero)
    print, systime() + ' Copying volume 0 to combined array'
    vol[0,0,0] = v
    for i=1, num_volumes-1 do begin
       file = base_file_name + '_' + suffix[i] + 'recon' + extension
       print, systime() + ' Reading volume ' + strtrim(i,2) + ' ' + file
       if (zstart ne 0) then begin
           zrange = [zstart, nz-1]
           v = t->read_volume(file, zrange=zrange)
       endif else begin
           v = t->read_volume(file)      
       endelse
       if (binning ne 1) then begin
         print, systime() + ' Rebinning volume ' + strtrim(i,2)
         rebin_with_trim, v, binning
       endif
       print, systime() + ' Copying volume ' + strtrim(i,2) + ' to combined array'
       vol[0, 0, (((nz-pixel_overlap/binning)*i) + zstart)] = v
    endfor
    print, systime() + ' Writing combined volume '
    t->write_volume, base_file_name + 'combined_recon' + extension, vol, netcdf=is_netcdf
    print, systime() + ' Done'
end

    
