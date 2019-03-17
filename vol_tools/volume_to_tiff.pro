pro volume_to_tiff

	file_in = dialog_pickfile(title='Select volume...', filter='*.volume', /must_exist, get_path=get_path)
	if file_in eq '' then return

	root = strmid(file_in, 0, strlen(file_in)-7)

	cd, get_path

	vol_in = read_tomo_volume(file_in)

	s = size(vol_in)
	sx = s[1]
	sy = s[2]
	sz = s[3]
	img = intarr(sx, sy)

	msg = dialog_message('A total of '+strtrim(sz,2)+' TIFF files will be created. Choose a folder where they will be placed.')

    path_out = dialog_pickfile(/directory)
    if path_out eq '' then return
    cd, path_out

	window, 0,xsize=sx, ysize=sy

	for i = 0, sz-1 do begin
		img[*,*] = vol_in[*,*,i]
		img = float(img)
		img = img + 32768
		img = fix(img, type=12)
		tvscl, img
		str_i=string(i+1)
		case 1 of
			(i+1 lt 10)  : strput, str_i, '000', strlen(str_i)-4
			(i+1 lt 100) : strput, str_i, '00', strlen(str_i)-4
			(i+1 lt 1000): strput, str_i, '0', strlen(str_i)-4
		endcase

		file_out = root+'_'+strtrim(str_i,2)+'.tif'
		print, file_out
		write_tiff, file_out, img, /short
	endfor

	temp = dialog_message('Conversion completed! Press OK to exit...', /information)

end