
function gen_int_select, vol, value0, value1, color, decimate_percent_vertices, smooth_lambda;, conn=conn

    less_than_what = value0
    more_than_what = value1
    smooth_lambda = smooth_lambda/100.
    newvol = vol

    index = where(vol lt less_than_what, count)
    if count ne 0 then newvol[index] = 0
    index = where(vol gt more_than_what, count)
    if count ne 0 then newvol[index] = 0

    index = where(newvol ne 0)
    newvol[index] = 255

    interval_volume, newvol, 253, 255, intverts, intconn
    inttetra = tetra_surface(intverts, intconn)


	if decimate_percent_vertices ne 0 then begin
    	numberVertices = MESH_DECIMATE(intverts, inttetra, inttetra, VERTICES=intverts, PERCENT_VERTICES=decimate_percent_vertices)
    endif

    if smooth_lambda ne 0 then begin
           intverts = mesh_smooth(intverts, inttetra, lambda=smooth_lambda)
    endif

    int = obj_new('IDLgrPolygon', intverts, polygons=inttetra, color=color)

	return, int

end

function check_values, min_value, max_value, decim_value, smooth_value, vol_type

	print, min_value, max_value, decim_value, smooth_value, vol_type

	case vol_type of
		 1 : begin & type = 'BYTE'    & mini =       0. & maxi =    255. & end
		 2 : begin & type = 'INT'     & mini =  -32768. & maxi =  32767. & end
		12 : begin & type = 'UINT'    & mini =       0. & maxi =  65535. & end
		 3 : begin & type = 'LONG'    & mini =   -2^31. & maxi =   2^31. & end
		13 : begin & type = 'ULONG'   & mini =       0. & maxi =   2^32. & end
		14 : begin & type = 'LONG64'  & mini =   -2^63. & maxi =   2^63. & end
		15 : begin & type = 'ULONG64' & mini =       0. & maxi =   2^64. & end
	endcase

	print, 'Volume is of type: ' + type

	if min_value lt mini or min_value gt maxi then begin
		msg_text = 'Volume is of type: "'+type+'". Value of "Min" has to be between '+strtrim(mini,2)+' and '+strtrim(maxi,2)+'!'
		msg = dialog_message(msg_text, /error)
		return, -1
	endif
	if max_value lt mini or max_value gt maxi then begin
		msg_text = 'Volume is of type: "'+type+'". Value of "Max" has to be between '+strtrim(mini,2)+' and '+strtrim(maxi,2)+'!'
		msg = dialog_message(msg_text, /error)
		return, -1
	endif
	if min_value gt max_value then begin
		msg = dialog_message('Value of "Min" has to smaller than value of "Max"', /error)
		return, -1
	endif
	if decim_value lt 0 or decim_value gt 100 then begin
		msg = dialog_message('Value of "Decim" has to between 0 and 100', /error)
		return, -1
	endif
	if smooth_value lt 0 or decim_value gt 100 then begin
		msg = dialog_message('Value of "Decim" has to between 0 and 100', /error)
		return, -1
	endif

	return, 0

end

pro get_parameters, Event, zoom=zoom_value, xsize=xsize_value, ysize=ysize_value

    zoom_text = widget_info(Event.top, find_by_uname='Zoom_Text')
    widget_control, zoom_text, get_value=zoom_value
   	zoom_value = float(zoom_value[0])*1/sqrt(3)

    xsize_text = widget_info(Event.top, find_by_uname='XSize_Text')
    widget_control, xsize_text, get_value=xsize_value
   	xsize_value = fix(xsize_value[0])

    ysize_text = widget_info(Event.top, find_by_uname='YSize_Text')
    widget_control, ysize_text, get_value=ysize_value
   	ysize_value = fix(ysize_value[0])

	return

end


pro vol_animate_create_obj1, Event

    WIDGET_CONTROL, Event.TOP, GET_UVALUE=state, /NO_COPY
	text = state.text
	in_file = state.in_file
	vol = state.vol
	sx = state.sx &	sy = state.sy &	sz = state.sz
	obj1 = state.obj1 & obj2 = state.obj2 & obj3 = state.obj3 & obj4 = state.obj4 & obj_view=state.obj_view
	color1 = state.color1 & color2 = state.color2 & color3 = state.color3 & color4 = state.color4
	incl1 = state.incl1 & incl2 = state.incl2 & incl3 = state.incl3 & incl4 = state.incl4

    min_text = widget_info(Event.top, find_by_uname='Min1_Text')
    max_text = widget_info(Event.top, find_by_uname='Max1_Text')
    decim_text = widget_info(Event.top, find_by_uname='Decim1_Text')
    smooth_text = widget_info(Event.top, find_by_uname='Smooth1_Text')
    color_value = color1

    widget_control, min_text, get_value=min_value
    widget_control, max_text, get_value=max_value
    widget_control, decim_text, get_value=decim_value
    widget_control, smooth_text, get_value=smooth_value


	min_value = fix(min_value[0])
	max_value = fix(max_value[0])
	decim_value = fix(decim_value[0])
	smooth_value = fix(smooth_value[0])
	vol_type = size(vol) & vol_type = vol_type[4]

	pass = check_values(min_value, max_value, decim_value, smooth_value, vol_type)
	if pass eq -1 then begin
    	WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY
		return
	end

	obj1 = gen_int_select (vol, min_value, max_value, color_value, decim_value, smooth_value)
	xobjview, obj1, background=[0,0,0]

    state = {text:text, in_file:in_file, vol:vol, sx:sx, sy:sy, sz:sz, obj1:obj1, obj2:obj2, obj3:obj3, obj4:obj4, obj_view:obj_view, color1:color1, color2:color2, color3:color3, color4:color4, incl1:incl1, incl2:incl2, incl3:incl3, incl4:incl4}
    WIDGET_CONTROL, Event.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_animate_create_obj2, Event

    WIDGET_CONTROL, Event.TOP, GET_UVALUE=state, /NO_COPY
	text = state.text
	in_file = state.in_file
	vol = state.vol
	sx = state.sx &	sy = state.sy &	sz = state.sz
	obj1 = state.obj1 & obj2 = state.obj2 & obj3 = state.obj3 & obj4 = state.obj4 & obj_view=state.obj_view
	color1 = state.color1 & color2 = state.color2 & color3 = state.color3 & color4 = state.color4
	incl1 = state.incl1 & incl2 = state.incl2 & incl3 = state.incl3 & incl4 = state.incl4

    min_text = widget_info(Event.top, find_by_uname='Min2_Text')
    max_text = widget_info(Event.top, find_by_uname='Max2_Text')
    decim_text = widget_info(Event.top, find_by_uname='Decim2_Text')
    smooth_text = widget_info(Event.top, find_by_uname='Smooth2_Text')
    color_value = color2

    widget_control, min_text, get_value=min_value
    widget_control, max_text, get_value=max_value
    widget_control, decim_text, get_value=decim_value
    widget_control, smooth_text, get_value=smooth_value


	min_value = fix(min_value[0])
	max_value = fix(max_value[0])
	decim_value = fix(decim_value[0])
	smooth_value = fix(smooth_value[0])
	vol_type = size(vol) & vol_type = vol_type[4]

	pass = check_values(min_value, max_value, decim_value, smooth_value, vol_type)
	if pass eq -1 then begin
    	WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY
		return
	end

	obj2 = gen_int_select (vol, min_value, max_value, color_value, decim_value, smooth_value)
	xobjview, obj2, background=[0,0,0]

    state = {text:text, in_file:in_file, vol:vol, sx:sx, sy:sy, sz:sz, obj1:obj1, obj2:obj2, obj3:obj3, obj4:obj4, obj_view:obj_view, color1:color1, color2:color2, color3:color3, color4:color4, incl1:incl1, incl2:incl2, incl3:incl3, incl4:incl4}
    WIDGET_CONTROL, Event.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_animate_create_obj3, Event

    WIDGET_CONTROL, Event.TOP, GET_UVALUE=state, /NO_COPY
	text = state.text
	in_file = state.in_file
	vol = state.vol
	sx = state.sx &	sy = state.sy &	sz = state.sz
	obj1 = state.obj1 & obj2 = state.obj2 & obj3 = state.obj3 & obj4 = state.obj4 & obj_view=state.obj_view
	color1 = state.color1 & color2 = state.color2 & color3 = state.color3 & color4 = state.color4
	incl1 = state.incl1 & incl2 = state.incl2 & incl3 = state.incl3 & incl4 = state.incl4

    min_text = widget_info(Event.top, find_by_uname='Min3_Text')
    max_text = widget_info(Event.top, find_by_uname='Max3_Text')
    decim_text = widget_info(Event.top, find_by_uname='Decim3_Text')
    smooth_text = widget_info(Event.top, find_by_uname='Smooth3_Text')
    color_value = color3

    widget_control, min_text, get_value=min_value
    widget_control, max_text, get_value=max_value
    widget_control, decim_text, get_value=decim_value
    widget_control, smooth_text, get_value=smooth_value


	min_value = fix(min_value[0])
	max_value = fix(max_value[0])
	decim_value = fix(decim_value[0])
	smooth_value = fix(smooth_value[0])
	vol_type = size(vol) & vol_type = vol_type[4]

	pass = check_values(min_value, max_value, decim_value, smooth_value, vol_type)
	if pass eq -1 then begin
    	WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY
		return
	end

	obj3 = gen_int_select (vol, min_value, max_value, color_value, decim_value, smooth_value)
	xobjview, obj3, background=[0,0,0]

    state = {text:text, in_file:in_file, vol:vol, sx:sx, sy:sy, sz:sz, obj1:obj1, obj2:obj2, obj3:obj3, obj4:obj4, obj_view:obj_view, color1:color1, color2:color2, color3:color3, color4:color4, incl1:incl1, incl2:incl2, incl3:incl3, incl4:incl4}
    WIDGET_CONTROL, Event.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_animate_create_obj4, Event

    WIDGET_CONTROL, Event.TOP, GET_UVALUE=state, /NO_COPY
	text = state.text
	in_file = state.in_file
	vol = state.vol
	sx = state.sx &	sy = state.sy &	sz = state.sz
	obj1 = state.obj1 & obj2 = state.obj2 & obj3 = state.obj3 & obj4 = state.obj4 & obj_view=state.obj_view
	color1 = state.color1 & color2 = state.color2 & color3 = state.color3 & color4 = state.color4
	incl1 = state.incl1 & incl2 = state.incl2 & incl3 = state.incl3 & incl4 = state.incl4

    min_text = widget_info(Event.top, find_by_uname='Min4_Text')
    max_text = widget_info(Event.top, find_by_uname='Max4_Text')
    decim_text = widget_info(Event.top, find_by_uname='Decim4_Text')
    smooth_text = widget_info(Event.top, find_by_uname='Smooth4_Text')
    color_value = color4

    widget_control, min_text, get_value=min_value
    widget_control, max_text, get_value=max_value
    widget_control, decim_text, get_value=decim_value
    widget_control, smooth_text, get_value=smooth_value


	min_value = fix(min_value[0])
	max_value = fix(max_value[0])
	decim_value = fix(decim_value[0])
	smooth_value = fix(smooth_value[0])
	vol_type = size(vol) & vol_type = vol_type[4]

	pass = check_values(min_value, max_value, decim_value, smooth_value, vol_type)
	if pass eq -1 then begin
    	WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY
		return
	end

	obj4 = gen_int_select (vol, min_value, max_value, color_value, decim_value, smooth_value)
	xobjview, obj4, background=[0,0,0]

    state = {text:text, in_file:in_file, vol:vol, sx:sx, sy:sy, sz:sz, obj1:obj1, obj2:obj2, obj3:obj3, obj4:obj4, obj_view:obj_view, color1:color1, color2:color2, color3:color3, color4:color4, incl1:incl1, incl2:incl2, incl3:incl3, incl4:incl4}
    WIDGET_CONTROL, Event.TOP, SET_UVALUE=state, /NO_COPY

end

function vol_animate_set_color, ev

	case ev.str of
		'Red'     : color=[255,  0,  0]
		'Green'   : color=[  0,255,  0]
		'Blue'    : color=[  0,  0,255]
		'Yellow'  : color=[255,255,  0]
		'Cyan'	  : color=[  0,255,255]
		'Magenta' : color=[255,  0,255]
		'Orange'  : color=[255,127,  0]
		'Pink'    : color=[255,  0,127]
		'Purple'  : color=[127,  0,255]
		'L Red'   : color=[255,127,127]
		'L Green' : color=[127,255,127]
		'L Blue'  : color=[127,127,255]
		'L Yellow': color=[255,255,127]
		'Black'   : color=[  0,  0,  0]
		'Gray 75' : color=[ 63, 63, 63]
		'Gray 50' : color=[127,127,127]
		'Gray 25' : color=[191,191,191]
		'White'   : color=[255,255,255]
		;'Custom'  :	begin &	msg = dialog_message('Not implemented yet! Setting color to 50% Gray.') & color=[127,127,127] &	end
	endcase
	return, color

end

pro vol_animate_gen_view, Event

	get_parameters, Event, zoom=zoom_value, xsize=xsize_value, ysize=ysize_value

    WIDGET_CONTROL, Event.TOP, GET_UVALUE=state, /NO_COPY
	text = state.text
	in_file = state.in_file
	vol = state.vol
	sx = state.sx &	sy = state.sy &	sz = state.sz
	obj1 = state.obj1 & obj2 = state.obj2 & obj3 = state.obj3 & obj4 = state.obj4 & obj_view=state.obj_view
	color1 = state.color1 & color2 = state.color2 & color3 = state.color3 & color4 = state.color4
	incl1 = state.incl1 & incl2 = state.incl2 & incl3 = state.incl3 & incl4 = state.incl4

	size1 = size(obj1) & size2 = size(obj2) & size3 = size(obj3) & size4 = size(obj4)

	if size1[1] eq 11 then obj1_set = 1 else obj1_set = 0
	if size2[1] eq 11 then obj2_set = 1 else obj2_set = 0
	if size3[1] eq 11 then obj3_set = 1 else obj3_set = 0
	if size4[1] eq 11 then obj4_set = 1 else obj4_set = 0

	interrupt = 0
	if obj1_set eq 0 and incl1 eq 1 then interrupt = 1
	if obj2_set eq 0 and incl2 eq 1 then interrupt = 1
	if obj3_set eq 0 and incl3 eq 1 then interrupt = 1
	if obj4_set eq 0 and incl4 eq 1 then interrupt = 1
	if interrupt eq 1 then begin

		msg = dialog_message('Objects to be included need to be created first', /error)

	endif else begin

    	if obj1_set eq 1 then obj1->setproperty, color=color1
    	if obj2_set eq 1 then obj2->setproperty, color=color2
    	if obj3_set eq 1 then obj3->setproperty, color=color3
    	if obj4_set eq 1 then obj4->setproperty, color=color4

    	obj_view = OBJ_NEW('IDLgrModel')
    	if incl1 eq 1 then obj_view->Add, obj1, /alias
	    if incl2 eq 1 then obj_view->Add, obj2, /alias
    	if incl3 eq 1 then obj_view->Add, obj3, /alias
	    if incl4 eq 1 then obj_view->Add, obj4, /alias

    	xobjview, obj_view, background=[0,0,0], scale=zoom_value, xsize=xsize_value, ysize=ysize_value

	endelse

    state = {text:text, in_file:in_file, vol:vol, sx:sx, sy:sy, sz:sz, obj1:obj1, obj2:obj2, obj3:obj3, obj4:obj4, obj_view:obj_view, color1:color1, color2:color2, color3:color3, color4:color4, incl1:incl1, incl2:incl2, incl3:incl3, incl4:incl4}
    WIDGET_CONTROL, Event.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_animate_gen_tiffs, Event

	get_parameters, Event, zoom=zoom_value, xsize=xsize_value, ysize=ysize_value

	msg = dialog_message('A total of 720 TIFF files will be created. Choose a folder where they will be placed.')

    path_out = dialog_pickfile(/directory)
    if path_out eq '' then return
    cd, path_out

    WIDGET_CONTROL, Event.TOP, GET_UVALUE=state, /NO_COPY
    obj_view = state.obj_view

    zoom_text = widget_info(Event.top, find_by_uname='Zoom_Text')
    widget_control, zoom_text, get_value=zoom_value
   	zoom_value = float(zoom_value[0])*1/sqrt(3)

   	xobjview, obj_view, background=[0,0,0], scale=zoom_value, xsize=xsize_value, ysize=ysize_value

    xobjview_write_image, 'img0.tif', 'tiff'

    for i = 0, 359 do begin
       xobjview_rotate, [0,1,-.25], 1, /premultiply
       xobjview_write_image, 'img'+strcompress(i, /remove_all)+'.tif', 'tiff'
    endfor

    for i = 360, 719 do begin
       xobjview_rotate, [1,0.25,0], 1, /premultiply
       xobjview_write_image, 'img'+strcompress(i, /remove_all)+'.tif', 'tiff'
    endfor

    WIDGET_CONTROL, Event.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_animate_gen_movie, Event

    file_in = dialog_pickfile(FILTER = '*.tif', /read, /must_exist, get_path=source_path, title='Select file number 0...')
    if file_in eq '' then return
    cd, source_path

    finddot = strpos(file_in, '.tif', /reverse_search)
    findlen = strlen(file_in)
    basename_file_in = strmid(file_in, 0, finddot - 1)

    image = read_tiff(file_in)
    dim = size(image)

    mpegID = MPEG_OPEN([dim[2], dim[3]], quality=100, MOTION_VEC_LENGTH=1)

    for i = 0, 719 do begin
       image = read_tiff(basename_file_in + STRCOMPRESS(i, /REMOVE_ALL) + '.tif')
       MPEG_PUT, mpegID, IMAGE=image, FRAME=i
    endfor

    file_out = dialog_pickfile(file='movie.mpg', FILTER = '*.mpg', /write, /overwrite_prompt)
    if file_out eq '' then return

    MPEG_SAVE, mpegID, FILENAME=file_out

    MPEG_CLOSE, mpegID

end


pro vol_animate_open_volume, Event

    in_file = dialog_pickfile(filter='*.volume', /must_exist, /read, get_path=path)
    if in_file eq '' then return
    cd, path

    WIDGET_CONTROL, event.TOP, GET_UVALUE=state, /NO_COPY

    text = widget_info(Event.top, find_by_uname='File_Label')
    widget_control, text, set_value=' Reading volume...'
    WIDGET_CONTROL, /HOURGLASS
    vol = read_tomo_volume(in_file)
    widget_control, text, set_value=' Input volume: ' + in_file

    s = size(vol)
    sx = s[1]
    sy = s[2]
    sz = s[3]

    obj1button = widget_info(Event.top, find_by_uname='Obj1_Button')
    widget_control, obj1button, sensitive=1
    obj2button = widget_info(Event.top, find_by_uname='Obj2_Button')
    widget_control, obj2button, sensitive=1
    obj3button = widget_info(Event.top, find_by_uname='Obj3_Button')
    widget_control, obj3button, sensitive=1
    obj4button = widget_info(Event.top, find_by_uname='Obj4_Button')
    widget_control, obj4button, sensitive=1

    min1 = widget_info(Event.top, find_by_uname='Min1_Text')
    widget_control, min1, set_value=strtrim(0,2), sensitive=1
    min2 = widget_info(Event.top, find_by_uname='Min2_Text')
    widget_control, min2, set_value=strtrim(0,2), sensitive=1
    min3 = widget_info(Event.top, find_by_uname='Min3_Text')
    widget_control, min3, set_value=strtrim(0,2), sensitive=1
    min4 = widget_info(Event.top, find_by_uname='Min4_Text')
    widget_control, min4, set_value=strtrim(0,2), sensitive=1

    max1 = widget_info(Event.top, find_by_uname='Max1_Text')
    widget_control, max1, set_value=strtrim(255,2), sensitive=1
    max2 = widget_info(Event.top, find_by_uname='Max2_Text')
    widget_control, max2, set_value=strtrim(255,2), sensitive=1
    max3 = widget_info(Event.top, find_by_uname='Max3_Text')
    widget_control, max3, set_value=strtrim(255,2), sensitive=1
    max4 = widget_info(Event.top, find_by_uname='Max4_Text')
    widget_control, max4, set_value=strtrim(255,2), sensitive=1

    name1 = widget_info(Event.top, find_by_uname='Name1_Text')
    widget_control, name1, set_value='obj1', sensitive=1
    name2 = widget_info(Event.top, find_by_uname='Name2_Text')
    widget_control, name2, set_value='obj2', sensitive=1
    name3 = widget_info(Event.top, find_by_uname='Name3_Text')
    widget_control, name3, set_value='obj3', sensitive=1
    name4 = widget_info(Event.top, find_by_uname='Name4_Text')
    widget_control, name4, set_value='obj4', sensitive=1

    decim1 = widget_info(Event.top, find_by_uname='Decim1_Text')
    widget_control, decim1, set_value=strtrim(0,2), sensitive=1
    decim2 = widget_info(Event.top, find_by_uname='Decim2_Text')
    widget_control, decim2, set_value=strtrim(0,2), sensitive=1
    decim3 = widget_info(Event.top, find_by_uname='Decim3_Text')
    widget_control, decim3, set_value=strtrim(0,2), sensitive=1
    decim4 = widget_info(Event.top, find_by_uname='Decim4_Text')
    widget_control, decim4, set_value=strtrim(0,2), sensitive=1

    smooth1 = widget_info(Event.top, find_by_uname='Smooth1_Text')
    widget_control, smooth1, set_value=strtrim(10,2), sensitive=1
    smooth2 = widget_info(Event.top, find_by_uname='Smooth2_Text')
    widget_control, smooth2, set_value=strtrim(10,2), sensitive=1
    smooth3 = widget_info(Event.top, find_by_uname='Smooth3_Text')
    widget_control, smooth3, set_value=strtrim(10,2), sensitive=1
    smooth4 = widget_info(Event.top, find_by_uname='Smooth4_Text')
    widget_control, smooth4, set_value=strtrim(10,2), sensitive=1

	color_list = ['Red','Green','Blue','Yellow','Cyan','Magenta','Orange','Pink','Purple', $
					'L Red','L Green','L Blue','L Yellow','Black','Gray 75','Gray 50','Gray 25','White']
    color1 = widget_info(Event.top, find_by_uname='Color1_ComboBox')
    widget_control, color1, set_value=color_list, sensitive=1
    color2 = widget_info(Event.top, find_by_uname='Color2_ComboBox')
    widget_control, color2, set_value=color_list, sensitive=1
    color3 = widget_info(Event.top, find_by_uname='Color3_ComboBox')
    widget_control, color3, set_value=color_list, sensitive=1
    color4 = widget_info(Event.top, find_by_uname='Color4_ComboBox')
    widget_control, color4, set_value=color_list, sensitive=1

    include1 = widget_info(Event.top, find_by_uname='Include1_Button')
    widget_control, include1, sensitive=1
    include2 = widget_info(Event.top, find_by_uname='Include2_Button')
    widget_control, include2, sensitive=1
    include3 = widget_info(Event.top, find_by_uname='Include3_Button')
    widget_control, include3, sensitive=1
    include4 = widget_info(Event.top, find_by_uname='Include4_Button')
    widget_control, include4, sensitive=1

    genviewbutton = widget_info(Event.top, find_by_uname='GenView_Button')
    widget_control, genviewbutton, sensitive=1
    zoomtext = widget_info(Event.top, find_by_uname='Zoom_Text')
    widget_control, zoomtext, sensitive=1
    xsizetext = widget_info(Event.top, find_by_uname='XSize_Text')
    widget_control, xsizetext, sensitive=1
    ysizetext = widget_info(Event.top, find_by_uname='YSize_Text')
    widget_control, ysizetext, sensitive=1


    gentiffsbutton = widget_info(Event.top, find_by_uname='GenTiffs_Button')
    widget_control, gentiffsbutton, sensitive=1
    genmoviebutton = widget_info(Event.top, find_by_uname='GenMovie_Button')
    widget_control, genmoviebutton, sensitive=1

    ;state = {text:text, in_file:in_file, vol:vol, sx:sx, sy:sy, sz:sz, obj1:0, obj2:0, obj3:0, obj4:0}
    state = {text:text, in_file:in_file, vol:vol, sx:sx, sy:sy, sz:sz, obj1:0, obj2:0, obj3:0, obj4:0, obj_view:0, color1:[255,0,0], color2:[0,255,0], color3:[0,0,255], color4:[255,255,0], incl1:0, incl2:0, incl3:0, incl4:0}
    WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_animate_base_event, ev

  	WIDGET_CONTROL, ev.ID, GET_UVALUE=uval

  	case uval of

  		'open_button' : vol_animate_open_volume, ev
  		'obj1_button' : vol_animate_create_obj1, ev
  		'obj2_button' : vol_animate_create_obj2, ev
  		'obj3_button' : vol_animate_create_obj3, ev
  		'obj4_button' : vol_animate_create_obj4, ev
  		'genview_button' : vol_animate_gen_view, ev
  		'gentiffs_button' : vol_animate_gen_tiffs, ev
  		'genmovie_button' : vol_animate_gen_movie, ev
  		else :

  	endcase

    WIDGET_CONTROL, ev.TOP, GET_UVALUE=state, /NO_COPY

  	case uval of

  		'color1_combobox' : state.color1 = vol_animate_set_color(ev)
  		'color2_combobox' : state.color2 = vol_animate_set_color(ev)
  		'color3_combobox' : state.color3 = vol_animate_set_color(ev)
  		'color4_combobox' : state.color4 = vol_animate_set_color(ev)
  		'include1_button' : if state.incl1 eq 0 then state.incl1 = 1 else state.incl1 = 0
  		'include2_button' : if state.incl2 eq 0 then state.incl2 = 1 else state.incl2 = 0
  		'include3_button' : if state.incl3 eq 0 then state.incl3 = 1 else state.incl3 = 0
  		'include4_button' : if state.incl4 eq 0 then state.incl4 = 1 else state.incl4 = 0
  		else :

	endcase

    WIDGET_CONTROL, ev.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_animate_base, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  vol_animate_base = Widget_base(GROUP_LEADER=wGroup, UNAME='vol_animate_base' ,TITLE='vol_animate'  $
      ,/column , /align_left, SPACE=3 ,XPAD=3 ,YPAD=3, TAB_MODE=1)

  Open_Button = Widget_Button(vol_animate_base, UNAME='Open_Button' ,/ALIGN_left ,VALUE=' Open volume:', uvalue='open_button')

  File_Label = Widget_Text(vol_animate_base, UNAME='File_Label' ,FRAME=0 ,scr_xsize=660 ,/ALIGN_LEFT ,VALUE=' Click button to start.')

  Cols_base = widget_base(vol_animate_base, /row, space=10, /align_left)

  Lines_base = widget_base(Cols_base, /column, space=10, /align_center)
  Line0_base = widget_base(Lines_base, /row, space=10, /align_left)
  Line1_base = widget_base(Lines_base, /row, space=10, /align_left)
  Line2_base = widget_base(Lines_base, /row, space=10, /align_left)
  Line3_base = widget_base(Lines_base, /row, space=10, /align_left)
  Line4_base = widget_base(Lines_base, /row, space=10, /align_left)

  Empty_Label = Widget_Label(Line0_base, UNAME='Empty_Label' ,SCR_XSIZE=120 ,/ALIGN_CENTER ,VALUE='  ', frame=0)
  Name_Label = Widget_Label(Line0_base, UNAME='Name_Label' ,SCR_XSIZE=60 ,/ALIGN_CENTER ,VALUE='Name', frame=0)
  Min_Label = Widget_Label(Line0_base, UNAME='Min_Label' ,SCR_XSIZE=60 ,/ALIGN_CENTER ,VALUE='Min', frame=0)
  Max_Label = Widget_Label(Line0_base, UNAME='Max_Label' ,SCR_XSIZE=60 ,/ALIGN_CENTER ,VALUE='Max', frame=0)
  Decim_Label = Widget_Label(Line0_base, UNAME='Decim_Label' ,SCR_XSIZE=60 ,/ALIGN_CENTER ,VALUE='Decimate', frame=0)
  Smooth_Label = Widget_Label(Line0_base, UNAME='Smooth_Label' ,SCR_XSIZE=60 ,/ALIGN_CENTER ,VALUE='Smooth', frame=0)
  Color_Label = Widget_Label(Line0_base, UNAME='Color_Label' ,SCR_XSIZE=80 ,/ALIGN_CENTER ,VALUE='Color', frame=0)
  Include_Label = Widget_Label(Line0_base, UNAME='Include_Label' ,SCR_XSIZE=60 ,/ALIGN_CENTER ,VALUE='Include', frame=0)

  Obj1_Button = Widget_Button(Line1_base, UNAME='Obj1_Button' ,/ALIGN_CENTER ,SCR_XSIZE=120 ,VALUE='Create object 1', uvalue='obj1_button', sensitive=0)
  Name1_Text = Widget_Text(Line1_base, UNAME='Name1_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Min1_Text = Widget_Text(Line1_base, UNAME='Min1_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Max1_Text = Widget_Text(Line1_base, UNAME='Max1_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Decim1_Text = Widget_Text(Line1_base, UNAME='Decim1_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Smooth1_Text = Widget_Text(Line1_base, UNAME='Smooth1_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Color1_ComboBox = widget_combobox(Line1_base, uname='Color1_ComboBox', SCR_XSIZE=80, uvalue='color1_combobox', sensitive=0)
  Include1_Base = Widget_Base(Line1_base, UNAME='Include1_Base' ,/NONEXCLUSIVE, scr_xsize=60, /align_center, frame=0)
  Include1_Button = Widget_Button(Include1_Base, UNAME='Include1_Button' ,/ALIGN_CENTER ,VALUE='', xsize=1, ysize=15, uvalue='include1_button', sensitive=0)

  Obj2_Button = Widget_Button(Line2_base, UNAME='Obj2_Button' ,/ALIGN_CENTER ,SCR_XSIZE=120 ,VALUE='Create object 2', uvalue='obj2_button', sensitive=0)
  Name2_Text = Widget_Text(Line2_base, UNAME='Name2_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Min2_Text = Widget_Text(Line2_base, UNAME='Min2_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Max2_Text = Widget_Text(Line2_base, UNAME='Max2_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Decim2_Text = Widget_Text(Line2_base, UNAME='Decim2_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Smooth2_Text = Widget_Text(Line2_base, UNAME='Smooth2_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Color2_ComboBox = widget_combobox(Line2_base, uname='Color2_ComboBox', SCR_XSIZE=80, uvalue='color2_combobox', sensitive=0)
  Include2_Base = Widget_Base(Line2_base, UNAME='Include2_Base' ,/NONEXCLUSIVE, scr_xsize=60, /align_center, frame=0)
  Include2_Button = Widget_Button(Include2_Base, UNAME='Include2_Button' ,/ALIGN_CENTER ,VALUE='', xsize=1, ysize=15, uvalue='include2_button', sensitive=0)

  Obj3_Button = Widget_Button(Line3_base, UNAME='Obj3_Button' ,/ALIGN_CENTER ,SCR_XSIZE=120 ,VALUE='Create object 3', uvalue='obj3_button', sensitive=0)
  Name3_Text = Widget_Text(Line3_base, UNAME='Name3_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Min3_Text = Widget_Text(Line3_base, UNAME='Min3_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Max3_Text = Widget_Text(Line3_base, UNAME='Max3_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Decim3_Text = Widget_Text(Line3_base, UNAME='Decim3_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Smooth3_Text = Widget_Text(Line3_base, UNAME='Smooth3_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Color3_ComboBox = widget_combobox(Line3_base, uname='Color3_ComboBox', SCR_XSIZE=80, uvalue='color3_combobox', sensitive=0)
  Include3_Base = Widget_Base(Line3_base, UNAME='Include3_Base' ,/NONEXCLUSIVE, scr_xsize=60, /align_center, frame=0)
  Include3_Button = Widget_Button(Include3_Base, UNAME='Include3_Button' ,/ALIGN_CENTER ,VALUE='', xsize=1, ysize=15, uvalue='include3_button', sensitive=0)

  Obj4_Button = Widget_Button(Line4_base, UNAME='Obj4_Button' ,/ALIGN_CENTER ,SCR_XSIZE=120 ,VALUE='Create object 4', uvalue='obj4_button', sensitive=0)
  Name4_Text = Widget_Text(Line4_base, UNAME='Name4_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Min4_Text = Widget_Text(Line4_base, UNAME='Min4_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Max4_Text = Widget_Text(Line4_base, UNAME='Max4_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Decim4_Text = Widget_Text(Line4_base, UNAME='Decim4_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Smooth4_Text = Widget_Text(Line4_base, UNAME='Smooth4_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  Color4_ComboBox = widget_combobox(Line4_base, uname='Color4_ComboBox', SCR_XSIZE=80, uvalue='color4_combobox', sensitive=0)
  Include4_Base = Widget_Base(Line4_base, UNAME='Include4_Base' ,/NONEXCLUSIVE, scr_xsize=60, /align_center, frame=0)
  Include4_Button = Widget_Button(Include4_Base, UNAME='Include4_Button' ,/ALIGN_CENTER ,VALUE='', xsize=1, ysize=15, uvalue='include4_button', sensitive=0)

  GenView_Base = widget_base(vol_animate_base, /row, /align_center, space=20)
  GenView_Button = Widget_Button(GenView_Base, UNAME='GenView_Button' ,SCR_XSIZE=120 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Generate View', uvalue='genview_button', sensitive=0)
  Zoom_Label = Widget_Label(GenView_Base, UNAME='Zoom_Label' ,SCR_XSIZE=40 ,/ALIGN_right ,VALUE='Zoom:', frame=0)
  Zoom_Text = Widget_Text(GenView_Base, UNAME='Zoom_Text' ,FRAME=1 ,SCR_XSIZE=40 ,scr_ysize=15 ,/EDITABLE ,sensitive=0, value=strtrim(1,2))
  XSize_Label = Widget_Label(GenView_Base, UNAME='XSize_Label' ,SCR_XSIZE=40 ,/ALIGN_right ,VALUE='X Size:', frame=0)
  XSize_Text = Widget_Text(GenView_Base, UNAME='XSize_Text' ,FRAME=1 ,SCR_XSIZE=40 ,scr_ysize=15 ,/EDITABLE ,sensitive=0, value=strtrim(800,2))
  YSize_Label = Widget_Label(GenView_Base, UNAME='YSize_Label' ,SCR_XSIZE=40 ,/ALIGN_right ,VALUE='Y Size:', frame=0)
  YSize_Text = Widget_Text(GenView_Base, UNAME='YSize_Text' ,FRAME=1 ,SCR_XSIZE=40 ,scr_ysize=15 ,/EDITABLE ,sensitive=0, value=strtrim(800,2))
  ;Space_Label = Widget_Label(GenView_Base, UNAME='Space_Label' ,SCR_XSIZE=60 ,/ALIGN_right ,VALUE='     ', frame=0)

  Gen_Button_Base = widget_base(vol_animate_base, /row, /align_center, space=20)

;  Zoom_Label = Widget_Label(Gen_Button_Base, UNAME='Zoom_Label' ,SCR_XSIZE=60 ,/ALIGN_right ,VALUE='Zoom:', frame=0)
;  Zoom_Text = Widget_Text(Gen_Button_Base, UNAME='Zoom_Text' ,FRAME=1 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0, value=strtrim(1,2))

  GenTiffs_Button = Widget_Button(Gen_Button_Base, UNAME='GenTiffs_Button' ,SCR_XSIZE=120 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Generate Tiffs', uvalue='gentiffs_button', sensitive=0)
  GenMovie_Button = Widget_Button(Gen_Button_Base, UNAME='GenMovie_Button' ,SCR_XSIZE=120 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Generate Movie', uvalue='genmovie_button', sensitive=1)


  Widget_Control, /REALIZE, vol_animate_base

;  state = {initialized:0}
  state = {text:0, in_file:0, vol:0, sx:0, sy:0, sz:0, obj1:0, obj2:0, obj3:0, obj4:0, obj_view:0, color1:[0,0,0], color2:[0,0,0], color3:[0,0,0], color4:[0,0,0], incl1:0, incl2:0, incl3:0, incl4:0}
  WIDGET_CONTROL, vol_animate_base, SET_UVALUE=state

  XManager, 'vol_animate_base', vol_animate_base, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro vol_animate, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  vol_animate_base, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end


