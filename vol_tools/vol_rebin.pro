
function write_log, in_file, out_file, log_file, mag
    openw, lun, log_file, error=error, /get_lun
    if (error ne 0) then return, 0
    printf, lun, 'Input file: ', in_file
    printf, lun, 'Output file: ', out_file
    printf, lun, 'Magnification factor: ', strtrim(mag,2)
    free_lun, lun
    return, 1
end


function set_mag, mag, new_label, sx, sy, sz

	temp = strtrim(string(long(sx*sy*sz*mag^3)),2)
	widget_control, new_label, set_value=' New size: '+temp+' pixels'
	return, mag

end


pro vol_rebin_event, ev

  	WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
  	WIDGET_CONTROL, ev.TOP, GET_UVALUE=state, /no_copy

	text = state.text
	in_file = state.in_file
	out_file = state.out_file
	log_file = state.log_file
	;vol = state.vol
	sx = state.sx
	sy = state.sy
	sz = state.sz
	mag = state.mag

	Open_Button = widget_info(ev.top, find_by_uname='Open_Button')

	File_Label = widget_info(ev.top, find_by_uname='File_Label')

	Radio_Button_1 = widget_info(ev.top, find_by_uname='Radio_Button_1')
	Radio_Button_2 = widget_info(ev.top, find_by_uname='Radio_Button_2')
	Radio_Button_3 = widget_info(ev.top, find_by_uname='Radio_Button_3')
	Radio_Button_4 = widget_info(ev.top, find_by_uname='Radio_Button_4')
	Radio_Button_5 = widget_info(ev.top, find_by_uname='Radio_Button_5')
	Radio_Button_6 = widget_info(ev.top, find_by_uname='Radio_Button_6')
	Radio_Button_7 = widget_info(ev.top, find_by_uname='Radio_Button_7')
	Radio_Button_8 = widget_info(ev.top, find_by_uname='Radio_Button_8')

	mid_base = widget_info(ev.top, find_by_uname='mid_base')
	widget_control, mid_base, sensitive=1
	bottom_base = widget_info(ev.top, find_by_uname='bottom_base')
	widget_control, bottom_base, sensitive=1

	orig_label = widget_info(ev.top, find_by_uname='orig_label')
	new_label = widget_info(ev.top, find_by_uname='new_label')

	Rebin_Button = widget_info(ev.top, find_by_uname='Rebin_Button')

    print, "Event ID: " + strtrim(ev.id,2)
  	case ev.id of
    	open_button 	: 	begin
							    in_file = dialog_pickfile(filter='*.volume', /must_exist, /read, get_path=path)
    							if in_file eq '' then return
    							cd, path
	    						out_file = strmid(in_file, 0, strpos(in_file, '.', /reverse_search)) + '-rebin.volume'

    							widget_control, file_label, set_value=' Reading volume...'
    							WIDGET_CONTROL, /HOURGLASS
    							vol = read_tomo_volume(in_file)
    							widget_control, file_label, set_value=' Input volume: ' + in_file

    							s = size(vol)
    							sx = s[1]
	    						sy = s[2]
    							sz = s[3]

								temp = strtrim(string(sx*sy*sz),2)
								widget_control, orig_label, set_value=' Original size: '+temp+' pixels'

    							widget_control, Rebin_Button, sensitive=1
    							state = {text:text, in_file:in_file, out_file:out_file, log_file:log_file, vol:temporary(vol), sx:sx, sy:sy, sz:sz, mag:mag}
    						end
    	Radio_Button_1 	: 	state.mag = set_mag(1/5., new_label, sx, sy, sz)
      Radio_Button_2 	: 	state.mag = set_mag(1/4., new_label, sx, sy, sz)
    	Radio_Button_3 	: 	state.mag = set_mag(1/3., new_label, sx, sy, sz)
    	Radio_Button_4 	: 	state.mag = set_mag(1/2., new_label, sx, sy, sz)
    	Radio_Button_5 	: 	state.mag = set_mag(2, new_label, sx, sy, sz)
    	Radio_Button_6 	: 	state.mag = set_mag(3, new_label, sx, sy, sz)
    	Radio_Button_7 	: 	state.mag = set_mag(4, new_label, sx, sy, sz)
    	Radio_Button_8 	: 	state.mag = set_mag(5, new_label, sx, sy, sz)

		Rebin_Button	:	begin
								vol = state.vol
								state = {text:text, in_file:in_file, out_file:out_file, log_file:log_file, vol:-1, sx:sx, sy:sy, sz:sz, mag:mag}
								
								temp = dialog_pickfile(/write, /overwrite_prompt, filter='*.volume', default_extension='volume', file=out_file)
								if temp eq '' then return
								out_file = temp

								max_x = fix(sx*mag)*(1/mag)
								max_y = fix(sy*mag)*(1/mag)
								max_z = fix(sz*mag)*(1/mag)

								if (max_x ne sx or max_y ne sy or max_z ne sz) then vol = temporary(vol[0:max_x-1,0:max_y-1,0:max_z-1])
								vol = rebin(temporary(vol), fix(sx*mag), fix(sy*mag), fix(sz*mag)) ;, /sample

								write_tomo_volume, out_file, vol

								log_file = strmid(out_file, 0, strpos(out_file, '.', /reverse_search)) + '.log'
								temp = write_log(in_file, out_file, log_file, mag)

                state = {text:text, in_file:in_file, out_file:out_file, log_file:log_file, vol:-1, sx:sx, sy:sy, sz:sz, mag:mag}
                
                widget_control, file_label, set_value=' Click button to start.'
                widget_control, orig_label, set_value=' Original size: - '
                widget_control, new_label, set_value=' New size: - '
                widget_control, Rebin_Button, sensitive=0
								
							end
	endcase

	;state = {text:text, in_file:in_file, out_file:out_file, log_file:log_file, vol:vol, sx:sx, sy:sy, sz:sz, mag:mag}
    WIDGET_CONTROL, ev.TOP, SET_UVALUE=state, /NO_COPY

end


pro vol_rebin, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  	vol_rebin_base = Widget_Base( GROUP_LEADER=wGroup, UNAME='vol_rebin_base', TITLE='vol_rebin'  $
      ,/column , /align_center, TAB_MODE=1, SPACE=7 ,XPAD=5 ,YPAD=5, xoffset=100, yoffset=100)

	Open_Button = Widget_Button(vol_rebin_base, UNAME='Open_Button' ,/ALIGN_left ,VALUE=' Open volume:', uvalue='open_button', ysize=25)

  	File_Label = Widget_Text(vol_rebin_base, UNAME='File_Label' ,FRAME=0 ,scr_xsize=410 ,/ALIGN_LEFT ,VALUE=' Click button to start.')

  	mid_base = widget_base(vol_rebin_base, uname='mid_base', /align_center, /column, yoffset=0, sensitive=0)
  	Mag_Label = Widget_Label(mid_base, UNAME='Mag_Label', /ALIGN_CENTER ,VALUE='Magnification:', frame=0)

  	Radio_Base = widget_base(mid_base, /row, space=10, /align_center, frame=0, xpad=0, /exclusive)
  	Radio_Button_1 = widget_button(Radio_Base, uname='Radio_Button_1', scr_xsize=25, /align_center, value='1/5', frame=1)
  	Radio_Button_2 = widget_button(Radio_Base, uname='Radio_Button_2', scr_xsize=25, /align_center, value='1/4', frame=1)
  	Radio_Button_3 = widget_button(Radio_Base, uname='Radio_Button_3', scr_xsize=25, /align_center, value='1/3', frame=1)
  	Radio_Button_4 = widget_button(Radio_Base, uname='Radio_Button_4', scr_xsize=25, /align_center, value='1/2', frame=1)
  	Radio_Button_5 = widget_button(Radio_Base, uname='Radio_Button_5', scr_xsize=25, /align_center, value=' 2 ', frame=1)
  	Radio_Button_6 = widget_button(Radio_Base, uname='Radio_Button_6', scr_xsize=25, /align_center, value=' 3 ', frame=1)
  	Radio_Button_7 = widget_button(Radio_Base, uname='Radio_Button_7', scr_xsize=25, /align_center, value=' 4 ', frame=1)
  	Radio_Button_8 = widget_button(Radio_Base, uname='Radio_Button_8', scr_xsize=25, /align_center, value=' 5 ', frame=1)

	bottom_base = widget_base(vol_rebin_base, uname='bottom_base', /align_left, /row, sensitive=0)

	bottom_subbase = widget_base(bottom_base, uname='bottom_subbase', /align_left, /column)
	orig_label = widget_label(bottom_subbase, uname='orig_label', /align_left, value=' Original size: - ', scr_xsize=270, scr_ysize=20)
	new_label = widget_label(bottom_subbase, uname='new_label', /align_left, value=' New size: - ', scr_xsize=200, scr_ysize=20)

  	Rebin_Button = Widget_Button(bottom_base, UNAME='Rebin_Button' ,SCR_XSIZE=120 ,SCR_YSIZE=25, /ALIGN_CENTER, VALUE='Rebin', uvalue='rebin', sensitive=0)

  	Widget_Control, /REALIZE, vol_rebin_base

  	state = {text:'', in_file:'', out_file:'', log_file:'', vol:-1, sx:-1, sy:-1, sz:-1, mag:-1.}
  	WIDGET_CONTROL, vol_rebin_base, SET_UVALUE=state

  	XManager, 'vol_rebin', vol_rebin_base, /NO_BLOCK

end
