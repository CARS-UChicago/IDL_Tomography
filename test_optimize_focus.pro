; Program to test optimizing rotation axis

nfiles = 10
files = 'T:\lvp_user\data\13-BM-D\2006\Nov_06\Focus test\focus_testx_' + $
        strtrim(2700+indgen(nfiles)*50,2) + '.SPE'
read_princeton, files[0], data
dims = size(data, /dimensions)
nx = dims[0]
ny = dims[1]
images = intarr(nx, ny, nfiles)
focus_motor = -2.7 - findgen(nfiles)*.05
for i=0, nfiles-1 do begin
   read_princeton, files[i], data
   images[0,0,i] = data
endfor
optimize_focus, images, focus_motor, entropy
end
