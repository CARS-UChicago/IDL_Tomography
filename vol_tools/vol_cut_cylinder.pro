pro vol_cut_cylinder_open_volume, Event

    in_file = dialog_pickfile(filter='*.volume', /must_exist, /read, get_path=path)
    if in_file eq '' then return
    cd, path
    out_file = strmid(in_file, 0, strpos(in_file, '.', /reverse_search)) + '-cylinder.volume'

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

    maxx = widget_info(Event.top, find_by_uname='MaxX_Text')
    widget_control, maxx, set_value=strtrim(sx-1,2), sensitive=1
    minx = widget_info(Event.top, find_by_uname='MinX_Text')
    widget_control, minx, set_value=strtrim(0,2), sensitive=1
    maxy = widget_info(Event.top, find_by_uname='MaxY_Text')
    widget_control, maxy, set_value=strtrim(sy-1,2), sensitive=1
    miny = widget_info(Event.top, find_by_uname='MinY_Text')
    widget_control, miny, set_value=strtrim(0,2), sensitive=1
    maxz = widget_info(Event.top, find_by_uname='MaxZ_Text')
    widget_control, maxz, set_value=strtrim(sz-1,2), sensitive=1
    minz = widget_info(Event.top, find_by_uname='MinZ_Text')
    widget_control, minz, set_value=strtrim(0,2), sensitive=1

    xybutton = widget_info(Event.top, find_by_uname='FindXY_Button')
    widget_control, xybutton, sensitive=1
    xzbutton = widget_info(Event.top, find_by_uname='FindXZ_Button')
    widget_control, xzbutton, sensitive=1
    yzbutton = widget_info(Event.top, find_by_uname='FindYZ_Button')
    widget_control, yzbutton, sensitive=1
    directionlist = widget_info(Event.top, find_by_uname='Direction_List')
    widget_control, directionlist, sensitive=1
    trimbutton = widget_info(Event.top, find_by_uname='Trim_Button')
    widget_control, trimbutton, sensitive=1

	state = {text:text, in_file:in_file, out_file:out_file, vol:vol, sx:sx, sy:sy, sz:sz, direction:'Z'}
    WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY

end


pro vol_cut_cylinder_find_xy, Event

  	WIDGET_CONTROL, event.TOP, GET_UVALUE=state, /NO_COPY

    text = widget_info(Event.top, find_by_uname='File_Label')
	vol = state.vol
	sx = state.sx
	sy = state.sy
	sz = state.sz

    img = dblarr(sx, sy)
    for i = 0, sz-1 do begin
       img = vol[*, *, i] + img
       if i mod 10 eq 0 then widget_control, text, set_value=' Adding '+strtrim(i, 2)+'/'+strtrim(sz,2)
    endfor
    widget_control, text, set_value=' Input volume: ' + state.in_file

    img_max = max(img, min=img_min)
    img = (img - img_min)/(img_max - img_min) * 255
    img = byte(img)
    image_display, img

    WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_cut_cylinder_find_yz, Event

  	WIDGET_CONTROL, event.TOP, GET_UVALUE=state, /NO_COPY

    text = widget_info(Event.top, find_by_uname='File_Label')
	vol = state.vol
	sx = state.sx
	sy = state.sy
	sz = state.sz

    img = dblarr(sy, sz)
    for i = 0, sx-1 do begin
       img = vol[i, *, *] + img
       if i mod 10 eq 0 then widget_control, text, set_value=' Adding '+strtrim(i, 2)+'/'+strtrim(sx,2)
    endfor
    widget_control, text, set_value=' Input volume: ' + state.in_file

    img_max = max(img, min=img_min)
    img = (img - img_min)/(img_max - img_min) * 255
    img = byte(img)
    image_display, img

    WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_cut_cylinder_find_xz, Event

  	WIDGET_CONTROL, event.TOP, GET_UVALUE=state, /NO_COPY

    text = widget_info(Event.top, find_by_uname='File_Label')
	vol = state.vol
	sx = state.sx
	sy = state.sy
	sz = state.sz

    img = dblarr(sx, sz)
    for i = 0, sy-1 do begin
       img = vol[*, i, *] + img
       if i mod 10 eq 0 then widget_control, text, set_value=' Adding '+strtrim(i, 2)+'/'+strtrim(sy,2)
    endfor
    widget_control, text, set_value=' Input volume: ' + state.in_file

    img_max = max(img, min=img_min)
    img = (img - img_min)/(img_max - img_min) * 255
    img = byte(img)
    image_display, img

    WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY

end

function write_log, file, array, direction
    openw, lun, file, error=error, /get_lun
    if (error ne 0) then return, 0
    printf, lun, 'X: ', strtrim(array[0],2), '-', strtrim(array[1],2)
    printf, lun, 'Y: ', strtrim(array[2],2), '-', strtrim(array[3],2)
    printf, lun, 'Z: ', strtrim(array[4],2), '-', strtrim(array[5],2)
    printf, lun, 'Direction: ', direction
    free_lun, lun
    return, 1
end

pro vol_cut_cylinder_trim, Event

  	WIDGET_CONTROL, event.TOP, GET_UVALUE=state, /NO_COPY

    text = widget_info(Event.top, find_by_uname='File_Label')
	vol = state.vol
	sx = state.sx
	sy = state.sy
	sz = state.sz
	out_file = state.out_file
	direction = state.direction

    minx = widget_info(Event.top, find_by_uname='MinX_Text')
    widget_control, minx, get_value=x_lo
    maxx = widget_info(Event.top, find_by_uname='MaxX_Text')
    widget_control, maxx, get_value=x_up
    maxy = widget_info(Event.top, find_by_uname='MaxY_Text')
    widget_control, maxy, get_value=y_up
    miny = widget_info(Event.top, find_by_uname='MinY_Text')
    widget_control, miny, get_value=y_lo
    maxz = widget_info(Event.top, find_by_uname='MaxZ_Text')
    widget_control, maxz, get_value=z_up
    minz = widget_info(Event.top, find_by_uname='MinZ_Text')
    widget_control, minz, get_value=z_lo
;    directionlist = widget_info(Event.top, find_by_uname='Direction_List')
;    widget_control, directionlist, get_value=direction

	x_lo = fix(x_lo[0]) & x_up = fix(x_up[0]) & y_lo = fix(y_lo[0]) & y_up = fix(y_up[0]) & z_lo = fix(z_lo[0]) & z_up = fix(z_up[0])
;	direction = direction[0]
	xproblems = 0 & yproblems = 0 & zproblems = 0
    if (x_lo lt 0) xor (x_up gt sx-1) xor (x_lo ge x_up) then xproblems = 1
    if (y_lo lt 0) xor (y_up gt sy-1) xor (y_lo ge y_up) then yproblems = 1
    if (z_lo lt 0) xor (z_up gt sz-1) xor (z_lo ge z_up) then zproblems = 1

	if xproblems eq 1 or yproblems eq 1 or zproblems eq 1 then begin
		my_message = 'Problems found! Please fix values of '
		if xproblems eq 1 then my_message = my_message + 'X, '
		if yproblems eq 1 then my_message = my_message + 'Y, '
		if zproblems eq 1 then my_message = my_message + 'Z, '
		my_message = STRMID(my_message, 0, STRLEN(my_message)-2)
   		my_dialog = DIALOG_MESSAGE(my_message, /CENTER)
   		WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY
   		return
   	endif

	case direction of
		'Z': begin
				a_lo = float(x_lo)
				a_up = float(x_up)
				b_lo = float(y_lo)
				b_up = float(y_up)
				sa = sx
				sb = sy
				sc = sz
			 end
		'Y': begin
				a_lo = float(x_lo)
				a_up = float(x_up)
				b_lo = float(z_lo)
				b_up = float(z_up)
				sa = sx
				sb = sz
				sc = sy
			 end
		'X': begin
				a_lo = float(y_lo)
				a_up = float(y_up)
				b_lo = float(z_lo)
				b_up = float(z_up)
				sa = sy
				sb = sz
				sc = sx
			 end
	endcase

	ca = (a_lo + a_up)/2
	cb = (b_lo + b_up)/2
	r = min([(a_up - a_lo)/2, (b_up - b_lo)/2])
	img = bytarr(sa, sb)
    cyl = bytarr(sx, sy, sz)
	for i = 0, sa-1 do begin
       	for j = 0, sb-1 do begin
         	d = fix(sqrt((i-ca)^2+(j-cb)^2))
			if d lt r then img[i,j] = 1
		endfor
	endfor

	case direction of
		'Z': begin
				for k = 0, sc-1 do cyl[*,*,k] = img
				vol = vol * cyl
    			vol = vol[ca-r:ca+r, cb-r:cb+r, z_lo:z_up]
			 end
		'Y': begin
				for k = 0, sc-1 do cyl[*,k,*] = img
				vol = vol * cyl
    			vol = vol[ca-r:ca+r, y_lo:y_up, cb-r:cb+r]
			 end
		'X': begin
				for k = 0, sc-1 do cyl[k,*,*] = img
				vol = vol * cyl
    			vol = vol[x_lo:x_up, ca-r:ca+r, cb-r:cb+r]
			 end
	endcase

	temp = dialog_pickfile(/write, filter='*.volume', default_extension='volume', file=out_file, /overwrite_prompt)

	if temp ne '' then begin
		out_file = temp

    	widget_control, text, set_value=' Writing volume...'
    	WIDGET_CONTROL, /HOURGLASS
    	write_tomo_volume, out_file, vol
    	widget_control, text, set_value=' Output volume: ' + out_file

    	nvoxels = long(x_up - x_lo + 1) * (y_up - y_lo + 1) * (z_up - z_lo + 1)
    	reduction = 100 - float(nvoxels)/(sx*sy*sz)*100

    	my_message = strtrim(nvoxels,2)+' voxels in volume! New file is '+strtrim(reduction, 2)+'% smaller than original.'
    	my_dialog = DIALOG_MESSAGE(my_message, /CENTER, /INFORMATION)

		success = write_log(strmid(out_file, 0, strlen(out_file)-6)+'log', [x_lo, x_up, y_lo, y_up, z_lo, z_up], direction)

	endif

    WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY

end

pro vol_cut_cylinder_list, Event

  	WIDGET_CONTROL, event.TOP, GET_UVALUE=state, /NO_COPY

	selection = event.index
	case selection of
		0: direction = 'Z'
		1: direction = 'Y'
		2: direction = 'X'
		else:
	endcase

	state.direction = direction
    WIDGET_CONTROL, event.TOP, SET_UVALUE=state, /NO_COPY


end


pro vol_cut_cylinder_base_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='Open_Button'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        vol_cut_cylinder_open_volume, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='FindXY_Button'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        vol_cut_cylinder_find_xy, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='FindYZ_Button'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        vol_cut_cylinder_find_yz, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='FindXZ_Button'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        vol_cut_cylinder_find_xz, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Trim_Button'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        vol_cut_cylinder_trim, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Direction_List'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_DROPLIST' )then $
        vol_cut_cylinder_list, Event
    end
    else:
  endcase

end

pro vol_cut_cylinder_base, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  vol_cut_cylinder_base = Widget_base( GROUP_LEADER=wGroup, UNAME='vol_cut_cylinder_base', TITLE='vol_cut_cylinder'  $
      ,/column , /align_left, TAB_MODE=1, SPACE=7 ,XPAD=5 ,YPAD=5, xoffset=100, yoffset=100)

  Open_Button = Widget_Button(vol_cut_cylinder_base, UNAME='Open_Button' ,/ALIGN_left ,VALUE=' Open volume:', uvalue='open_button', ysize=25)

  File_Label = Widget_Text(vol_cut_cylinder_base, UNAME='File_Label' ,FRAME=0 ,scr_xsize=310 ,/ALIGN_LEFT ,VALUE=' Click button to start.')

  Cols_base = widget_base(vol_cut_cylinder_base, /row, space=10, /align_left, frame=0, xpad=0, ypad=0)
  Lines_base = widget_base(Cols_base, /column, space=5, /align_center, frame=0, xpad=0, ypad=0)

  Line0_base = widget_base(Lines_base, /row, space=10, /align_center, frame=0, xpad=0)
  Line1_base = widget_base(Lines_base, /row, space=10, /align_center, frame=0, xpad=0)
  Line2_base = widget_base(Lines_base, /row, space=10, /align_center, frame=0, xpad=0)
  Line3_base = widget_base(Lines_base, /row, space=10, /align_center, frame=0, xpad=0)

  Empty_Label_1 = Widget_Label(Line0_base, UNAME='Empty_Label_1' ,SCR_XSIZE=90 ,/ALIGN_CENTER ,VALUE='  ', frame=0)
  Empty_Label_2 = Widget_Label(Line0_base, UNAME='Empty_Label_2' ,SCR_XSIZE=60 ,/ALIGN_CENTER ,VALUE='  ', frame=0)
  Min_Label = Widget_Label(Line0_base, UNAME='Min_Label' ,SCR_XSIZE=60 ,/ALIGN_CENTER ,VALUE='Min', frame=0)
  Max_Label = Widget_Label(Line0_base, UNAME='Max_Label' ,SCR_XSIZE=60 ,/ALIGN_CENTER ,VALUE='Max', frame=0)

  FindXY_Button = Widget_Button(Line1_base, UNAME='FindXY_Button' ,/ALIGN_CENTER ,SCR_XSIZE=90 ,VALUE='Find X, Y', uvalue='findxy_button', sensitive=0)
  X_Label = Widget_Label(Line1_base, UNAME='X_Label' ,frame=0 ,SCR_XSIZE=60 ,/align_right ,sensitive=1 ,VALUE='X:')
  MinX_Text = Widget_Text(Line1_base, UNAME='MinX_Text' ,frame=0 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  MaxX_Text = Widget_Text(Line1_base, UNAME='MaxX_Text' ,frame=0 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)

  FindYZ_Button = Widget_Button(Line2_base, UNAME='FindYZ_Button' ,/ALIGN_CENTER ,SCR_XSIZE=90 ,VALUE='Find Y, Z', uvalue='findyz_button', sensitive=0)
  Y_Label = Widget_Label(Line2_base, UNAME='Y_Label' ,frame=0 ,SCR_XSIZE=60 ,/align_right ,sensitive=1 ,VALUE='Y:')
  MinY_Text = Widget_Text(Line2_base, UNAME='MinY_Text' ,frame=0 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  MaxY_Text = Widget_Text(Line2_base, UNAME='MaxY_Text' ,frame=0 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)

  FindXZ_Button = Widget_Button(Line3_base, UNAME='FindXZ_Button' ,/ALIGN_CENTER ,SCR_XSIZE=90 ,VALUE='Find X, Z', uvalue='findxz_button', sensitive=0)
  Z_Label = Widget_Label(Line3_base, UNAME='Z_Label' ,frame=0 ,SCR_XSIZE=60 ,/align_right ,sensitive=1 ,VALUE='Z:')
  MinZ_Text = Widget_Text(Line3_base, UNAME='MinZ_Text' ,frame=0 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)
  MaxZ_Text = Widget_Text(Line3_base, UNAME='MaxZ_Text' ,frame=0 ,SCR_XSIZE=60 ,/EDITABLE ,sensitive=0)


  Trim_Button_Base = widget_base(vol_cut_cylinder_base, /row, /align_left, space=20, frame=0, xpad=0, ypad=0)

  Direction_Sub_Base = widget_base(Trim_Button_Base, /row, /align_left, frame=0, xpad=0, ypad=0)

  Direction_Label = Widget_Label(Direction_Sub_Base, UNAME='Direction_Label' ,/ALIGN_LEFT ,VALUE='Direction:', frame=0)
  Direction_List = Widget_DropList(Direction_Sub_Base, UNAME='Direction_List' ,FRAME=0, XSIZE=40 , sensitive=0, value=['Z', 'Y', 'X'])
  Empty_Label_3 = Widget_Label(Trim_Button_Base, UNAME='Empty_Label_3' ,SCR_XSIZE=45 ,/ALIGN_CENTER ,VALUE='  ', frame=0)
  Trim_Button = Widget_Button(Trim_Button_Base, UNAME='Trim_Button' ,SCR_XSIZE=120 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Cut cylinder!', uvalue='trim', sensitive=0)

  Widget_Control, /REALIZE, vol_cut_cylinder_base

  state = {initialized:0}
  WIDGET_CONTROL, vol_cut_cylinder_base, SET_UVALUE=state

  XManager, 'vol_cut_cylinder_base', vol_cut_cylinder_base, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro vol_cut_cylinder, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  vol_cut_cylinder_base, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
