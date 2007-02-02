; This program corrects the ALS tomography data, producing a .volume file

xbin = 2
ybin = 2
file = 'lt-b0'
read_als_tomo, file+'drk', dark
read_als_tomo, file+'bak_0', white
nx = n_elements(white[*,0])
ny = n_elements(white[0,*])
nx = nx/xbin
ny = ny/ybin
nz = 360
dark = rebin(dark, nx, ny)
white = rebin(white, nx, ny)
vol = intarr(nx, ny, nz)
for i=0, 359 do begin
   print, 'correcting view', i
   read_als_tomo, file+'_'+strtrim(i,2), data
   data = rebin(data, nx, ny)
   norm = float(data - dark) / float(white - dark)
   vol[0,0,i] = 10000. * norm
endfor
write_tomo_volume, file+'.volume', vol
end
