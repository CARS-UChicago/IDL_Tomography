; Procedure to read 2 volumes collected with rotation axes on opposite sides
; of the field of view and combine them, optimizing shift.

pro combine_rotation_offset, left, right, out, center_left, center_right, yrange=yrange
    print, 'Reading left volume ', left
    temp = read_tomo_volume(left, yrange=yrange)
    dims = size(temp, /dimensions)
    nx = dims[0]
    ny = dims[1]
    nz = dims[2]
    ; The total size is the distance from the rotation axis to the edge
    nxl = center_left + 1
    nxr = nx - center_right
    nxtotal = nxl + nxr
    vtotal = intarr(nxtotal, ny, nz, /nozero)
    vtotal[0,0,0] = temp[0:center_left,*,*]
    temp=0
    print, 'Reading right volume ', right
    temp = read_tomo_volume(right, yrange=yrange)
    vtotal[center_left+1,0,0] = temp[center_right:nx-1,*,*]
    print, 'Writing combined volume ', out
    write_tomo_volume, out, vtotal
end

    
