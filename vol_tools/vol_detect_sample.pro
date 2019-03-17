
function detect_sample, img, level, gamma, struct_size

	img = LEEFILT(img , 2)

	level = level/255.
	gamma_level = level^(1./gamma)
	level = gamma_level*255
	print, 'Final level is: ' + strtrim(level,2)

	newimg = img
	s = size(newimg)
	if s[0] eq 2 then newimg[*,*] = 0 else newimg[*,*,*] = 0

	index = where(img gt level, count)
	if count ne 0 then begin
		newimg[index] = 255

		if struct_size ne 0 then begin
			if s[0] eq 2 then newimg = morph_close(newimg, REPLICATE(1, struct_size, struct_size))*255 $
				else newimg = morph_close(newimg, REPLICATE(1, struct_size, struct_size, struct_size))*255
		endif
	endif

	return, newimg

end



pro vol_detect_sample_event, ev

  	WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
  	WIDGET_CONTROL, ev.TOP, GET_UVALUE=state

	vol = state.vol & img = state.img & img_bw = state.img_bw & img_window = state.img_window
	gamma = state.gamma & level = state.level & struct_size = state.struct_size
	sx = state.sx & sy = state.sy & sz = state.sz & file_in = state.file_in

	label_file = widget_info(ev.top, find_by_uname='label_file')

	draw_1 = widget_info(ev.top, find_by_uname='draw_1')
	widget_control, draw_1, get_value=draw_1_ID

	label_slider_1 = widget_info(ev.top, find_by_uname='label_slider_1')
	slider_1 = widget_info(ev.top, find_by_uname='slider_1')
	label_slider_2 = widget_info(ev.top, find_by_uname='label_slider_2')
	slider_2 = widget_info(ev.top, find_by_uname='slider_2')
	label_slider_3 = widget_info(ev.top, find_by_uname='label_slider_3')
	slider_3 = widget_info(ev.top, find_by_uname='slider_3')
	label_slider_4 = widget_info(ev.top, find_by_uname='label_slider_4')
	slider_4 = widget_info(ev.top, find_by_uname='slider_4')

	button_process = widget_info(ev.top, find_by_uname='button_process')

  	case uval of
    	'button_open' : begin

							file = dialog_pickfile(title='Choose volume to be opened', /read, /must_exist, filter='*.volume', get_path=path)
							if file eq '' then return
							cd, path

							vol = read_tomo_volume(file)
							text = ' Volume file: ' + file
							widget_control, label_file, set_value=text

							s = size(vol)

							sx = s[1] & sy = s[2] & sz = s[3]
							widget_control, draw_1, draw_xsize=2*sx, draw_ysize=sy
							img_window = intarr(2*sx,sy)
    						img_bw = intarr(sx,sy)

							position = fix(sz/2.)

    						vol = bytscl(vol)

   							img = vol[*,*,position]
    						img_window[0:sx-1,0:sy-1] = img

							img_bw = detect_sample(img, level, gamma, struct_size)
    						img_window[sx:2*sx-1,0:sy-1] = img_bw

							wset, draw_1_ID & tv, img_window


							widget_control, slider_1, /sensitive
							widget_control, slider_2, /sensitive
							widget_control, label_slider_3, set_value='Z frame: '+strtrim(position,2)
							widget_control, slider_3, /sensitive, set_slider_max=sz-1, set_slider_min=0, set_value=curr_z
							widget_control, label_slider_4, set_value='Close window size: '+strtrim(struct_size,2)
							widget_control, slider_4, /sensitive, set_value=struct_size
							widget_control, button_process, /sensitive

    						state = {vol:vol, img:img, img_bw:img_bw, img_window:img_window, level:127, gamma:1., struct_size:10, sx:sx, sy:sy, sz:sz, file_in:file}

    				  end

    	'slider_1' : begin

    					widget_control, slider_1, get_value=level
    					widget_control, slider_2, get_value=gamma
    					gamma = gamma/1000.
    					widget_control, slider_3, get_value=position
    					widget_control, slider_4, get_value=struct_size
						widget_control, label_slider_1, set_value='Threshold value: '+strtrim(level,2)

   						img = vol[*,*,position]
    					img_window[0:sx-1,0:sy-1] = img

						img_bw = detect_sample(img, level, gamma, struct_size)
    					img_window[sx:2*sx-1,0:sy-1] = img_bw

						wset, draw_1_ID & tv, img_window

					 end
     	'slider_2' : begin

    					widget_control, slider_1, get_value=level
    					widget_control, slider_2, get_value=gamma
    					gamma = gamma/1000.
    					widget_control, slider_3, get_value=position
    					widget_control, slider_4, get_value=struct_size
						widget_control, label_slider_2, set_value='Gamma value: '+strtrim(gamma,2)

   						img = vol[*,*,position]
    					img_window[0:sx-1,0:sy-1] = img

						img_bw = detect_sample(img, level, gamma, struct_size)
    					img_window[sx:2*sx-1,0:sy-1] = img_bw

						wset, draw_1_ID & tv, img_window

					 end
      	'slider_3' : begin

    					widget_control, slider_1, get_value=level
    					widget_control, slider_2, get_value=gamma
    					gamma = gamma/1000.
    					widget_control, slider_3, get_value=position
    					widget_control, slider_4, get_value=struct_size
						widget_control, label_slider_3, set_value='Z frame: '+strtrim(position,2)

   						img = vol[*,*,position]
    					img_window[0:sx-1,0:sy-1] = img

						img_bw = detect_sample(img, level, gamma, struct_size)
    					img_window[sx:2*sx-1,0:sy-1] = img_bw

						wset, draw_1_ID & tv, img_window

    				 end
      	'slider_4' : begin

    					widget_control, slider_1, get_value=level
    					widget_control, slider_2, get_value=gamma
    					gamma = gamma/1000.
    					widget_control, slider_3, get_value=position
    					widget_control, slider_4, get_value=struct_size
						widget_control, label_slider_4, set_value='Close window size: '+strtrim(struct_size,2)

   						img = vol[*,*,position]
    					img_window[0:sx-1,0:sy-1] = img

						img_bw = detect_sample(img, level, gamma, struct_size)
    					img_window[sx:2*sx-1,0:sy-1] = img_bw

						wset, draw_1_ID & tv, img_window

    				 end
    	'button_process' :  begin

								file_out = dialog_pickfile(/write, filter='*.volume')
								if file_out eq '' then return
								if strmid(file_out, 6, /reverse_offset) ne '.volume' then file_out = file_out+'.volume'
								file_slk = strmid(file_out, 0, strlen(file_out)-7)+'.slk'

    							widget_control, slider_1, get_value=level
		    					widget_control, slider_2, get_value=gamma
    							gamma = gamma/1000.
    							widget_control, slider_3, get_value=position
    							widget_control, slider_4, get_value=struct_size

								final_level = ((level/255.)^(1./gamma))*255

								data = [['Input_file:', 'Output_file:', 'Threshold_value:', 'Gamma_value:', 'Final_level:', 'Close_window_size:','','Sample_pixels:', 'Air_pixels:', 'Total_pixels'],$
									[file_in, file_out, strtrim(level,2), strtrim(gamma,2), strtrim(final_level,2), strtrim(struct_size,2),'','Not calculated','Not calculated','Not calculated']]
								result = write_sylk(file_slk, data)

								vol_bw = detect_sample(vol, level, gamma, struct_size)

								index = where(vol_bw eq 255, sample_pixels)
								index = where(vol_bw eq 0, air_pixels)
								total_pixels = long(sx)*sy*sz

								data = [['Input_file:', 'Output_file:', 'Threshold_value:', 'Gamma_value:', 'Final_level:', 'Close_window_size:','','Sample_pixels:', 'Air_pixels:', 'Total_pixels'],$
									[file_in, file_out, strtrim(level,2), strtrim(gamma,2), strtrim(final_level,2), strtrim(struct_size,2),'', strtrim(sample_pixels,2), strtrim(air_pixels,2), strtrim(total_pixels,2)]]
								result = write_sylk(file_slk, data)

								write_tomo_volume, file_out, vol_bw

    						end
     	else :

    endcase

	WIDGET_CONTROL, ev.TOP, SET_UVALUE=state ;, /no_copy

end




pro vol_detect_sample

	DEVICE, DECOMPOSED = 0
	LOADCT, 0

	sx = 650 & sy = 650

	base = WIDGET_BASE(title='Detect Sample', /column, space=5)
  	Widget_Control, /REALIZE, base

	rowA_base = widget_base(base, /column, space=10, /align_left)
	rowB_base = widget_base(base, /row, space=10, /align_left)
	rowC_base = widget_base(base, /row, space=10, /align_left)
	rowD_base = widget_base(base, /row, space=10, /align_left)
	rowE_base = widget_base(base, /row, space=10, /align_left)

	button_open = widget_button(rowA_base, /align_left, uvalue='button_open', value=' Open volume:')

	label_file = widget_label(rowA_base, /align_left, scr_xsize=625, uname='label_file', value='Press button to open volume...')

	column1_base = widget_base(rowD_base, /column, space=20, /align_left)
	slider_base_1 = widget_base(column1_base, /column, space=2, /align_center)
	label_slider_1 = widget_label(slider_base_1, /align_center, uname='label_slider_1', value='Threshold value: 127')
	slider_1 = widget_slider(slider_base_1, minimum=0, maximum=255, xsize = 150, sensitive=0, value=127, uname='slider_1', uvalue='slider_1', /suppress_value)
	slider_base_2 = widget_base(column1_base, /column, space=2, /align_center)
	label_slider_2 = widget_label(slider_base_2, /align_center, uname='label_slider_2', value='Gamma correction: 1.000')
	slider_2 = widget_slider(slider_base_2, minimum=0, maximum=5000, xsize = 150, sensitive=0, value=1000, uname='slider_2', uvalue='slider_2', /suppress_value)
	slider_base_3 = widget_base(column1_base, /column, space=2, /align_center)
	label_slider_3 = widget_label(slider_base_3, /align_center, uname='label_slider_3', value='Z frame:      ')
	slider_3 = widget_slider(slider_base_3, maximum=sz, xsize = 150, sensitive=0, uname='slider_3', uvalue='slider_3', /suppress_value)
	slider_base_4 = widget_base(column1_base, /column, space=2, /align_center)
	label_slider_4 = widget_label(slider_base_4, /align_center, uname='label_slider_4', value='Close window size:      ')
	slider_4 = widget_slider(slider_base_4, maximum=50, xsize = 150, sensitive=0, uname='slider_4', uvalue='slider_4', /suppress_value)
	button_process = widget_button(column1_base, /align_center, uvalue='button_process', uname='button_process', value='Process whole volume', sensitive=0)

	draw_1 = widget_draw(rowD_base, xsize=2*sx, ysize=sy, uname='draw_1') ; $ , /button_events, /motion_events, /wheel_events, uvalue='draw', frame=1)

    state = {vol:-1, img:-1, img_bw:-1, img_window:-1, level:127, gamma:1., struct_size:10, sx:-1, sy:-1, sz:-1, file_in:-1}

	WIDGET_CONTROL, base, SET_UVALUE=state

	XManager, 'vol_detect_sample', base, /NO_BLOCK

end
