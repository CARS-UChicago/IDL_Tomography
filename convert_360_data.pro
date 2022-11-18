; Procedure to convert tomography data collected with 360 degree rotation and rotation axis on one side
; of the image to normal data with a wider sinogram and 180 degree rotation.
; It assumes that there is air on the left side of the images, i.e. that the first 180 degrees
; is the left side of the 180 degree sinogram and the second 180 is the right side of the 180 degree sinogram.

pro convert_360_data, input, output, center, yrange=yrange
    t0 = systime(1)
    print, 'Reading input volume ', input
    vol = read_tomo_volume(input, yrange=yrange)
    dims = size(vol, /dimensions)
    nx = dims[0]
    ny = dims[1]
    nz = dims[2]
    if (center gt nx/2) then begin
        nxtotal = 2*center
    endif else begin
        nxtotal = 2*(nx - center)
    endelse
    vtotal = intarr(nxtotal, ny, nz/2, /nozero)
    if (center gt nx/2) then begin
        print, 'Copying left side of image ...'
        vtotal[0,0,0] = vol[0:center-1, *, 0:nz/2-1]
        t1 = systime(1)
        print, 'Reversing and copying right side of image ...'
        for i=0, nz/2-1 do begin
          vtotal[center,0,i] = rotate(vol[0:center-1, *, i+nz/2], 5)
        endfor
        print, 'Time to reverse and copy = ', systime(1) - t1
   endif else begin
        print, 'Copying right side of image ...'
        vtotal[nxtotal/2,0,0] = vol[center:nx-1, *, 0:nz/2-1]
        t1 = systime(1)
        print, 'Reversing and copying right side of image ...'
        for i=0, nz/2-1 do begin
          vtotal[0,0,i] = rotate(vol[center:nx-1, *, i+nz/2], 5)
        endfor
        print, 'Time to reverse and copy = ', systime(1) - t1
    endelse
    print, 'Writing combined volume ', output
    write_tomo_volume, output, vtotal
    print, 'Elapsed time = ', systime(1) - t0
end

