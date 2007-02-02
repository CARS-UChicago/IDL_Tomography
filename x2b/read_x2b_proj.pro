pro read_x2b_proj, file, $
                data, $
                header=header, $
                nx=nx, $
                ny=ny, $
                nframes=nframes, $
                pixSize = pixSize, $
		energy = energy, $
                date=date, $
                xrange=xrange, $
                yrange=yrange, $
                zrange=zrange
;+
; NAME:
;   READ_X2B_PROJ
;
; PURPOSE:
;   This procedure reads data files written by the IPLab software at the
;   NSLS X2B tomographic facility
;
; CATEGORY:
;   File input.
;
; CALLING SEQUENCE:
;   READ_X2B_PROJ, File, Data, [header,nx,ny,nframes,pixSize,energy,date]
;
; INPUTS:
;   File:
;       The name of the data file to read.
;
; OUTPUTS:
;   Data[nx, ny, nframes]:
;   The output data array.  The array will be 1, 2 or 3 dimensions
;   (depending upon whether ny and nframes are 1) and can be integer,
;   long or float data type.
;
; KEYWORD OUTPUTS:
;   HEADER:
;       The 100 byte header from the file.  This header can be used to
;       extract additional information about the file.
;   NX:
;       Number of pixels in X direction in each projection
;   NY:
;       Number of pixels in Y direction in each projection
;   NFRAMES:
;       Number of projections
;   PIXSIZE:
;       Size of pixel in microns
;   ENERGY:
;	X-ray beam energy in kev
;   DATE:
;       A date string
;   XRANGE=[xstart, xstop]
;       The range of X values to read in.  The default is to read the entire
;       X range of the data
;   YRANGE=[ystart, ystop]
;       The range of Y values to read in.  The default is to read the entire
;       Y range of the data
;   ZRANGE=[zstart, zstop]
;       The range of Z values to read in.  The default is to read the entire
;       Z range of the data
;       
;
; RESTRICTIONS:
;   Byte swapping needs to be checked
;
; EXAMPLE:
;   Read a data file:
;
;       IDL> READ_X2B_PROJ, 'test.prj', data, header=header, nx=ny, ny=ny, nframes=nframes
;       IDL> print, 'Nx ', nx, ' Ny ', ny, ' Nproj ', nframes
;       IDL> energy = float(header, 24)
;       IDL> print, 'Beam energy (Kev) = ', energy
;
; MODIFICATION HISTORY:
;       Written by:     Brent Lindquist, 9/28/2006
;       Mark Rivers, Feb. 1, 2007
;          Added XRANGE, YRANGE, ZRANGE keywords.  Changed from uint to int.
;-

    openr, lun, /get, file

    header_len = 100
    header = bytarr(header_len)
    readu, lun, header

    ; Convert the header from a byte array to a structure
    header = convert_x2b_proj_header(header)

    ; Get the image size from the header
    nx = header.ysize
    ny = header.end_slc - header.beg_slc + 1
    nframes = header.nviews

    magnification = header.magnification
    pixSize = header.pixSize
    energy  = header.energy

    date = header.DateString
    date = string(date)

    ; Simplest case is if no ranges are specified, no need to loop
    if (n_elements(zrange) eq 0) and (n_elements(yrange) eq 0) and $
       (n_elements(xrange) eq 0) then begin
        ; Data is always signed short
        data = intarr(nx, ny, nz, /nozero)
        readu, lun, data
    endif else begin
        if (n_elements(xrange) eq 0) then xrange = [0, nx-1]
        if (n_elements(yrange) eq 0) then yrange = [0, ny-1]
        if (n_elements(zrange) eq 0) then zrange = [0, nframes-1]
        ; Compute nx, ny, nz clip if user specified too large a range
        ix = (xrange[1] - xrange[0] + 1) < nx
        iy = (yrange[1] - yrange[0] + 1) < ny
        iz = (zrange[1] - zrange[0] + 1) < nframes
        data = intarr(ix, iy, iz, /nozero)
        slice = intarr(nx, ny, /nozero)
        ; Compute the file offset, which needs to be a 64-bit integer
        offset = header_len + zrange[0]*nx*ny*2LL
        point_lun, lun, offset
        for i = 0, iz-1 do begin
            readu, lun, slice
            data[0, 0, i] = slice[xrange[0]:xrange[1], yrange[0]:yrange[1]]
        endfor
    endelse
    data = reform(data, /overwrite)
    byteorder, data, /sswap, /swap_if_big_endian
    free_lun, lun
end
