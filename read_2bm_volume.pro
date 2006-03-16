function read_2bm_volume, base_file, nprojections=nprojections, white_interval=white_interval, ndarks=ndarks, $
                          xrange=xrange, yrange=yrange
   if (n_elements(nprojections) eq 0) then nprojections = 721
   if (n_elements(white_interval) eq 0) then white_interval = 100
   if (n_elements(ndarks) eq 0) then ndarks = 1

   normal_type = 0
   white_type  = 1
   dark_type   = 2
   ignore_type = 3

   nwhites = 2 + nprojections/white_interval
   nfiles = nprojections + nwhites + ndarks
   file_types = intarr(nfiles)
   file_types[*] = normal_type
   file_types[0] = white_type
   for i=0, nwhites-2 do begin
      file_types[i*(white_interval+1)] = white_type
   endfor
   file_types[nfiles-ndarks-1] = white_type
   for i=0, ndarks-1 do begin
      file_types[nfiles-1-i] = dark_type
   endfor
   ; Ignore the last projection, it is 180
   nprojections = nprojections - 1
   file_types[nfiles-1-ndarks-1] = ignore_type

   nw = 0
   np = 0
   nd = 0
   ; Read first file to get dimensions
   read_2bm_hdf, base_file+'00001.hdf', data
   s = size(data, /dimensions)
   nx = s[0]
   ny = s[1]
   if (n_elements(xrange) eq 0) then xrange=[0,nx-1]
   if (n_elements(yrange) eq 0) then yrange=[0,ny-1]
   nx = xrange[1]-xrange[0]+1
   ny = yrange[1]-yrange[0]+1
   darks = uintarr(nx, ny, ndarks)
   whites = uintarr(nx, ny, nwhites)
   projections = uintarr(nx, ny, nprojections)
   for i=0, nfiles-1 do begin
      file = base_file + string(i+1, format='(i5.5)') + '.hdf'
      print, 'Reading file ', file
      read_2bm_hdf, file, data
      data = data[xrange[0]:xrange[1], yrange[0]:yrange[1]]
      case (file_types[i]) of
         normal_type: begin
            projections[0,0,np] = data
            np = np + 1
         end
         white_type: begin
            whites[0,0,nw] = data
            nw = nw + 1
         end
         dark_type: begin
            darks[0,0,nd] = data
            nd = nd + 1
         end
         ignore_type: begin
         end
      endcase
   endfor

   ; Average the darks
   if (ndarks gt 1) then begin
      dark = total(darks, 3)/ndarks
   endif else begin
      dark = float(darks)
   endelse

   ; Average the whites
   white = total(whites, 3)/nwhites

   white_minus_dark = white - dark

   ; Correct each projection
   for i=0, nprojections-1 do begin
      print, 'Correcting projection ', i
      projections[0,0,i] = 10000 * ((projections[*,*,i]-dark)/(white_minus_dark)) + 0.5
   endfor
   return, projections
end

