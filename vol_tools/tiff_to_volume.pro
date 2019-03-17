pro tiff_to_volume

	repeat begin

		file = dialog_pickfile(filter = '*.tif', get_path = path, /must_exist, title = 'Select file 1...')
		if file eq '' then return
		cd, path

		underscore = strpos(file, '_', /reverse_search)
		root = strmid(file, 0, underscore+1)
		number = strmid(file, underscore+1, strlen(file)-underscore-5)

		files = FILE_SEARCH(root + '*.tif', count=count)

		img0 = read_image(files[0])
		s = size(img0)

		nlines = s[1]
		ncols = s[2]
		ntiffs = count

		vol = intarr(nlines, ncols, ntiffs)

		print, 'Reading tiffs...'

		for i=0, ntiffs-1 do begin
			tiffname = files[i]
   			slice = read_tiff(tiffname)
   			vol[*,*,i] = slice
   			print, 'Reading volume ' + strtrim(i+1,2) + ' of ' + strtrim(ntiffs,2)
		endfor

		print, 'Writing volume...'

		write_tomo_volume, strmid(file, 0, underscore) + '.volume',  vol

		print, 'END!'
		box = dialog_message('Volume created successfuly! Run tiff_to_volume again?', /question)
		if box eq 'No' then print, 'No' else print, 'Yes'

	endrep until box eq 'No'

end