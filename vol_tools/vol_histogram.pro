pro vol_histogram

	filein = dialog_pickfile(/read,filter = '*.volume',get_path = folder)
	if filein eq '' then return
	cd, folder
	volume = read_tomo_volume(filein)
	minimum = min(volume,max=maximum)
	histo = histogram(volume)
	histosize = size(histo)
	entries = histosize[1]
	data = lonarr(entries,2)
	for i = 0, entries-1 do begin
		data[i,0] = i+minimum
	endfor
	data[*,1] = histo[*]
	fileout = strmid(filein, 0, strlen(filein)-7)+'-histo'
	fileout = dialog_pickfile(file = fileout,/write,/overwrite_prompt,default_extension = 'slk',filter = '*.slk')
	if fileout eq '' then return
	success = write_sylk(fileout,data)
	if success eq 1 then begin
		box = dialog_message('The file was saved.',/information)
	endif else begin
		box = dialog_message('DIDNT SAVE',/error)
	endelse

end