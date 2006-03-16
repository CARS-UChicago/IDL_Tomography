pro read_als_tomo, file, data

   openr, lun, /get, file+'.spr'
   dims = 0L
   readf, lun, dims
   nx = 0L
   readf, lun, nx
   xorigin = 0.
   readf, lun, xorigin
   xsize = 0.
   readf, lun, xsize
   ny = 0L
   readf, lun, ny
   yorigin = 0.
   readf, lun, yorigin
   ysize = 0.
   readf, lun, ysize
   data_type = 0L
   readf, lun, data_type
   free_lun, lun
   openr, lun, /get, file+'.sdt'
   case data_type of
      0: data = bytarr(nx, ny)
      1: data = uintarr(nx, ny)
      2: data = ulonarr(nx, ny)
      3: data = fltarr(nx, ny)
   endcase
   readu, lun, data
   free_lun, lun
end

