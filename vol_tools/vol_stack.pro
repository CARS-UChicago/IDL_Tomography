
function patch_one, x_displacement, x_offset, y_displacement, y_offset, sy, sz, n, img_array

	x_size_tv = (n-1)*abs(x_offset) + sy
	y_size_tv = (n-1)*abs(y_offset) + sz
	patched = intarr(3, x_size_tv, y_size_tv)

	for i = 0, n-1 do begin
		if x_offset ge 0 then x_begin = i*x_offset
		if x_offset lt 0 then x_begin = (i-n+1)*x_offset

		if y_offset ge 0 then y_begin = i*y_offset
		if y_offset lt 0 then y_begin = (i-n+1)*y_offset

		x_end = x_begin+sy-1
		y_end = y_begin+sz-1

		x_begin = fix(x_begin) & x_end = fix(x_end)
		y_begin = fix(y_begin) & y_end = fix(y_end)

		patched[(i mod 2)+1, x_begin:x_end, y_begin:y_end] = img_array[i,*,*]
		patched[0, x_begin:x_end, y_begin:y_end] = img_array[i,*,*]
	endfor

	return, patched

end


function good_offsets, x_offset, y_offset, sy, sz

    if abs(x_offset) ne 0 then begin
    	error_msg_text = 'X offset not implemented. Choose X=0!'
    	error_msg = DIALOG_MESSAGE(error_msg_text, /CENTER, /ERROR)
    	return, 0
    endif

    if abs(y_offset) gt sz then begin
    	error_msg_text = 'Choose an Y Offset smaller than the image size (' + strtrim(sz,2) + ')!'
    	error_msg = DIALOG_MESSAGE(error_msg_text, /CENTER, /ERROR)
    	return, 0
    endif

	return, 1

end


function write_log, in_file, out_file, log_file, x_offset, y_offset
    openw, lun, log_file, error=error, /get_lun
    if (error ne 0) then return, 0
    printf, lun, 'Input files: ', in_file
    printf, lun, 'Output file: ', out_file
    printf, lun, 'X offset: ', strtrim(x_offset,2)
    printf, lun, 'Y offset: ', strtrim(y_offset,2)
    free_lun, lun
    return, 1
end


pro vol_stack_event, ev

  	WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
  	WIDGET_CONTROL, ev.TOP, GET_UVALUE=state, /no_copy

	open_button = widget_info(ev.top, find_by_uname='Open_Button')
	stack_button = widget_info(ev.top, find_by_uname='Stack_Button')
	save_button = widget_info(ev.top, find_by_uname='Save_Button')
	frame_slider = widget_info(ev.top, find_by_uname='Frame_Slider')

    frame_slider_label = widget_info(ev.top, find_by_uname='frame_slider_label')
	x_displacement = widget_info(ev.top, find_by_uname='X_Text')
	y_displacement = widget_info(ev.top, find_by_uname='Y_Text')

	draw_window = widget_info(ev.top, find_by_uname='Draw_Window')

	if ev.id ne open_button then begin
		sx = state.sx
		sy = state.sy
		sz = state.sz
		n = state.n
		img_array = state.img_array
		frame = state.frame

		widget_control, x_displacement, get_value=x_offset
		x_offset = x_offset[0]
		widget_control, y_displacement, get_value=y_offset
		y_offset = y_offset[0]

		if good_offsets(x_offset, y_offset, state.sy, state.sz) eq 0 then goto, end_statement
	endif


  	case 1 of
    	(ev.id eq open_button):	begin
									file = dialog_pickfile(filter = '*.volume', get_path=get_path, /must_exist, /multiple_files, title = 'Select volume files to stack')
									if file[0] eq '' then return
									cd, get_path

									n = n_elements(file)

									vol_temp = read_tomo_volume(file[0])
									s = size(vol_temp)
									sx = s[1]
									sy = s[2]
									sz = s[3]

									vol_array = intarr(n, sx, sy, sz, /nozero)
									vol_array[0,*,*,*] = vol_temp[*,*,*]
									vol_temp = 0

									img_array = intarr(n, sy, sz)

									for i = 1, n-1 do vol_array[i,*,*,*]=read_tomo_volume(file[i])

									x_size_tv = sy
									y_size_tv = n*sz

									patched = intarr(3, x_size_tv, y_size_tv)

    								drawwindow = widget_info(ev.top, find_by_uname='Draw_Window')
    								widget_control, drawwindow, sensitive=1, draw_xsize=x_size_tv, draw_ysize=y_size_tv, GET_VALUE=drawID
									tvscl, patched, /true

									for i = 0, n-1 do begin
										img_array[i,*,*] = vol_array[i,sx/2,*,*]
										patched[0,0:sy-1,i*sz:(i+1)*sz-1] = img_array[i,*,*]
									endfor

									tvscl, patched[0,*,*];, /true

	    							widget_control, x_displacement, set_value=strtrim(0,2), sensitive=1
    								widget_control, y_displacement, set_value=strtrim(sz,2), sensitive=1

    								widget_control, frame_slider, /sensitive, set_slider_max=sx-1, set_slider_min=0, set_value=sx/2
    								widget_control, frame_slider_label, set_value='Frame: '+strtrim(sx/2,2), scr_xsize=100

    								widget_control, open_button, /input_focus
    								widget_control, stack_button, sensitive=1
    								widget_control, save_button, sensitive=1

									state = {file:file, n:n, sx:sx, sy:sy, sz:sz, patched:patched, img_array:img_array, vol_array:vol_array, frame:sx/2, file_out:'', file_log:''}
    							end
    	(ev.id eq stack_button)	or $
    	(ev.id eq frame_slider):begin
							  		WIDGET_CONTROL, draw_window, GET_VALUE=drawID
									WSET, drawID

    								widget_control, frame_slider, get_value=frame
    								widget_control, frame_slider_label, set_value='Frame: '+strtrim(frame,2), scr_xsize=100

									for i = 0, n-1 do img_array[i,*,*] = state.vol_array[i,frame,*,*]

									patched = patch_one(x_displacement, x_offset, y_displacement, y_offset, sy, sz, n, img_array)

									s = size(patched)
    								widget_control, draw_window, draw_xsize=s[2], draw_ysize=s[3]
									tvscl, patched, /true

									state.frame = frame
									state.patched = patched
    							end
    	(ev.id eq save_button):	begin
	   								for j = 0, sx-1 do begin
										for i = 0, n-1 do img_array[i,*,*] = state.vol_array[i,j,*,*]
										patched = patch_one(x_displacement, x_offset, y_displacement, y_offset, sy, sz, n, img_array)
										if j eq 0 then begin
											s = size(patched)
											vol = intarr(sx, s[2], s[3])
										endif
										vol[j,*,*]=patched[0,*,*]
									endfor

									repeat begin
										file_out = dialog_pickfile(filter = '*.volume', /write, default_extension='volume', /overwrite_prompt)
										if file_out eq '' then begin
											temp = dialog_message('Are you sure you want to quit without saving?', /question)
											if temp eq 'Yes' then return
										endif
									endrep until file_out ne ''

									write_tomo_volume, file_out, vol

									file_log = strmid(file_out, 0, strpos(file_out, '.', /reverse_search)) + '.log'
									temp = write_log(state.file, file_out, file_log, x_offset, y_offset)

									state.file_log = file_log
									state.file_out = file_out
									state.patched = patched
								end
			else:
	endcase

end_statement:
    WIDGET_CONTROL, ev.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_stack, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	vol_stack_base = Widget_base(GROUP_LEADER=wGroup, UNAME='vol_stack_base', TITLE='vol_stack' ,/row , /align_left, $
		TAB_MODE=1, SPACE=7 ,XPAD=5 ,YPAD=5, xoffset=100, yoffset=100)

	col1_base = widget_base(vol_stack_base, /column, space=10, /align_left, frame=0, xpad=0, ypad=0)

	Open_Button = Widget_Button(col1_base, UNAME='Open_Button' ,/ALIGN_center ,VALUE='Select files:', uvalue='open_button', ysize=25, scr_xsize=100)
	Empty_Label_1 = Widget_Label(col1_base, UNAME='Empty_Label_1' ,/ALIGN_left ,VALUE='', frame=0)

	frame_slider_label = widget_label(col1_base, /align_center, uname='frame_slider_label', value='Frame:      ')
	frame_slider = widget_slider(col1_base, maximum=sz, xsize = 100, sensitive=0, uname='Frame_Slider', uvalue='frame_slider', /suppress_value)
	Empty_Label_2 = Widget_Label(col1_base, UNAME='Empty_Label_2' ,/ALIGN_left ,VALUE='', frame=0)

	Displacement_Label = Widget_Label(col1_base, UNAME='Displacement_Label' ,/ALIGN_left ,VALUE='Displacement:', frame=0)

	subline1_base =widget_base(col1_base, /row, space=5, /align_center, frame=0, xpad=0, ypad=0)
  	X_Label = Widget_Label(subline1_base, UNAME='X_Label', /ALIGN_left,VALUE='X:')
  	X_Text = Widget_Text(subline1_base, UNAME='X_Text', FRAME=1, SCR_XSIZE=60, /EDITABLE ,XSIZE=20 ,YSIZE=1, sensitive=0)

	subline2_base =widget_base(col1_base, /row, space=5, /align_center, frame=0, xpad=0, ypad=0)
  	Y_Label = Widget_Label(subline2_base, UNAME='Y_Label', /ALIGN_left,VALUE='Y:')
  	Y_Text = Widget_Text(subline2_base, UNAME='Y_Text', FRAME=1, SCR_XSIZE=60, /EDITABLE ,XSIZE=20 ,YSIZE=1, sensitive=0)

	Empty_Label_3 = Widget_Label(col1_base, UNAME='Empty_Label_3' ,/ALIGN_left ,VALUE='', frame=0)

	Stack_Button = Widget_Button(col1_base, UNAME='Stack_Button', SCR_XSIZE=100, SCR_YSIZE=25, /ALIGN_CENTER ,VALUE='Stack', sensitive=0)
	Empty_Label_4 = Widget_Label(col1_base, UNAME='Empty_Label_4' ,/ALIGN_left ,VALUE='', frame=0)

  	Save_Button = Widget_Button(col1_base, UNAME='Save_Button', SCR_XSIZE=100, SCR_YSIZE=25, /ALIGN_CENTER ,VALUE='Save', sensitive=0)
	Empty_Label_5 = Widget_Label(col1_base, UNAME='Empty_Label_5' ,/ALIGN_left ,VALUE='', frame=0)

	Draw_Window = Widget_Draw(vol_stack_base, uname='Draw_Window', /scroll, X_SCROLL_SIZE=846, Y_SCROLL_SIZE=682, xsize=847, ysize=683, sensitive=0)

	Widget_Control, /REALIZE, vol_stack_base

  	state = {initialized:0}
  	WIDGET_CONTROL, vol_stack_base, SET_UVALUE=state

  	XManager, 'vol_stack', vol_stack_base, /NO_BLOCK

end
