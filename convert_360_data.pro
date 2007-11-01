; Procedure to convert tomography data collected with 360 degree rotation and rotation axis on one side
; of the image to normal data with a wider sinogram and 180 degree rotation.
; It assumes that there is air on the left side of the images, i.e. that the first 180 degrees
; is the left side of the 180 degree sinogram and the second 180 is the right side of the 180 degree sinogram.

pro convert_360_data, input, output, center, yrange=yrange
    print, 'Reading input volume ', input
    vol = read_tomo_volume(input, yrange=yrange)
    dims = size(vol, /dimensions)
    nx = dims[0]
    ny = dims[1]
    nz = dims[2]
    nxtotal = 2*center
    vtotal = intarr(nxtotal, ny, nz/2, /nozero)
    print, 'Copying left side of image ...'
    vtotal[0,0,0] = vol[0:center-1, *, 0:nz/2-1]
    print, 'Reversing and copying right side of image ...'
    vtotal[center,0,0] = reverse(vol[0:center-1, *, nz/2:*], 1, /overwrite)
    print, 'Writing combined volume ', output
    write_tomo_volume, output, vtotal
end

    
