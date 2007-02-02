pro convert_x2b_netcdf_display_event, event
    widget_control, event.top, get_uvalue=convert_x2b_netcdf_display
    convert_x2b_netcdf_display->event, event
end

pro convert_x2b_netcdf_display::event, event
    if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
        widget_control, event.top, /destroy
        obj_destroy, self
        return
    endif

    catch, err
    if (err ne 0) then begin
        t = dialog_message(!error_state.msg, /error)
        widget_control, self.widgets.status, set_value=!error_state.msg
        return
    endif

    case event.id of
        self.widgets.go: begin
            widget_control, self.widgets.abort, sensitive=1
            widget_control, self.widgets.abort, set_uvalue=0
            input_file = dialog_pickfile(/read, filter='*.prj', /must_exist, get_path=path)
            if (input_file eq '') then break
            cd, path
            ; Read the header of the file (and first projection)
            read_x2b_proj, input_file, zrange=[0,0], data, nx=nx, ny=ny, nframes=nz
            widget_control, /hourglass
            widget_control, self.widgets.xauto, get_value=xauto
            widget_control, self.widgets.xmin, get_value=xmin
            widget_control, self.widgets.xmax, get_value=xmax
            if (xauto eq 0) then begin
                xrange = [xmin, xmax]
            endif else begin
                xrange = [0, nx-1]
            endelse
            widget_control, self.widgets.yauto, get_value=yauto
            widget_control, self.widgets.ymin, get_value=ymin
            widget_control, self.widgets.ymax, get_value=ymax
            if (yauto eq 0) then begin
                yrange = [ymin, ymax]
            endif else begin
                yrange = [0, ny-1]
            endelse
            widget_control, self.widgets.zauto, get_value=zauto
            widget_control, self.widgets.zmin, get_value=zmin
            widget_control, self.widgets.zmax, get_value=zmax
            if (zauto eq 0) then begin
                zrange = [zmin, zmax]
            endif else begin
                zrange = [0, nz-1]
            endelse
            widget_control, self.widgets.max_memory, get_value=max_memory
            ; Convert from GB to bytes
            max_memory = max_memory * 1.e9
            output_file = dialog_pickfile(/write, filter='*.volume')
            if (output_file eq '') then return
            convert_x2b_netcdf, input_file, output_file, $
                                xrange=xrange, yrange=yrange, zrange=zrange, $
                                max_memory=max_memory, $
                                status_widget=self.widgets.status, $
                                abort_widget=self.widgets.abort
            widget_control, self.widgets.abort, sensitive=0
        end

        self.widgets.display_projection: begin
            input_file = dialog_pickfile(/read, filter='*.prj', /must_exist, get_path=path)
            if (input_file eq '') then break;
            cd, path
            widget_control, /hourglass
            widget_control, self.widgets.zmin, get_value=zmin
            read_x2b_proj, input_file, zrange=[zmin, zmin], data, nx=nx, ny=ny, nframes=nz
            widget_control, self.widgets.xauto, get_value=xauto
            widget_control, self.widgets.yauto, get_value=yauto
            widget_control, self.widgets.zauto, get_value=zauto
            if (xauto eq 1) then widget_control, self.widgets.xmax, set_value=nx-1
            if (yauto eq 1) then widget_control, self.widgets.ymax, set_value=ny-1
            if (zauto eq 1) then widget_control, self.widgets.zmax, set_value=nz-1
            iimage, data
        end
        
        self.widgets.xauto: begin
            ; Nothing to do
        end

        self.widgets.yauto: begin
            ; Nothing to do
        end

        self.widgets.zauto: begin
            ; Nothing to do
        end

        self.widgets.exit: begin
            widget_control, event.top, /destroy
            obj_destroy, self
            return
        end

        else:  t = dialog_message('Unknown event')
    endcase
end

pro convert_x2b_netcdf_display_abort_event, event
    ; This procedure is called when an abort event is received.
    widget_control, event.id, set_uvalue=1
end



function convert_x2b_netcdf_display::init

    self.fonts.normal = get_font_name(/helvetica)
    self.fonts.heading1 = get_font_name(/large, /bold)
    self.fonts.heading2 = get_font_name(/bold)

    self.widgets.base= widget_base(column=1, $
                                   title='Convert X2B to netCDF', mbar=mbar)

    file = widget_button(mbar, /menu, value = 'File')
    self.widgets.display_projection = widget_button(file, value='Display projection @zmin')
    self.widgets.go = widget_button(file, value='Convert to netCDF')
    self.widgets.exit = widget_button(file, value = 'Exit')
    col = widget_base(self.widgets.base, /column, /frame)
    row = widget_base(col, /row)
    self.widgets.status = cw_field(row, title="Status:", $
                                        xsize=50, /noedit, $
                                        fieldfont=self.fonts.heading2)

    row = widget_base(col, /row, /base_align_center)
    t = widget_label(row, value='X (horizontal) range:')
    self.widgets.xauto = cw_bgroup(row, ['Manual', 'Auto'], $
                                             row=1, set_value=1, /exclusive)
    self.widgets.xmin = cw_field(row, title='Min.', /long, $
                                        /column, xsize=10, value=0)
    self.widgets.xmax = cw_field(row, title='Max.', /long, $
                                        /column, xsize=10, value=0)
    row = widget_base(col, /row, /base_align_center)
    t = widget_label(row, value='Y (vertical) range:')
    self.widgets.yauto = cw_bgroup(row, ['Manual', 'Auto'], $
                                             row=1, set_value=1, /exclusive)
    self.widgets.ymin = cw_field(row, title='Min.', /long, $
                                        /column, xsize=10, value=0)
    self.widgets.ymax = cw_field(row, title='Max.', /long, $
                                        /column, xsize=10, value=0)
    row = widget_base(col, /row, /base_align_center)
    t = widget_label(row, value='Z (angle) range:')
    self.widgets.zauto = cw_bgroup(row, ['Manual', 'Auto'], $
                                             row=1, set_value=1, /exclusive)
    self.widgets.zmin = cw_field(row, title='Min.', /long, $
                                        /column, xsize=10, value=0)
    self.widgets.zmax = cw_field(row, title='Max.', /long, $
                                        /column, xsize=10, value=0)
    row = widget_base(col, /row, /base_align_center)
    self.widgets.max_memory = cw_field(row, title='Max. memory (GB)', /float, $
                                        /row, xsize=10, value=1.0)
    self.widgets.abort = widget_button(row, value='Abort', font=self.fonts.heading2, $
                                       event_pro='convert_x2b_netcdf_display_abort_event')
    widget_control, self.widgets.abort, sensitive=0

    widget_control, self.widgets.base, set_uvalue=self
    ; Make all of the base widgets the same size so they line up nicely
    g = widget_info(self.widgets.base, /geometry)
    widget_control, self.widgets.base, /realize

    xmanager, 'convert_x2b_netcdf_display', self.widgets.base, /no_block
    return, 1
end


pro convert_x2b_netcdf_display__define

    widgets={convert_x2b_netcdf_display_widgets, $
        base: 0L, $
        xauto: 0L, $
        xmin: 0L, $
        xmax: 0L, $
        yauto: 0L, $
        ymin: 0L, $
        ymax: 0L, $
        zauto: 0L, $
        zmin: 0L, $
        zmax: 0L, $
        max_memory: 0L, $
        go: 0L, $
        status: 0L, $
        abort: 0L, $
        display_projection: 0L, $
        exit: 0L $
    }

    fonts = {convert_x2b_netcdf_display_fonts, $
        normal: '', $
        heading1: '', $
        heading2: '' $
    }

    convert_x2b_netcdf_display = {convert_x2b_netcdf_display, $
        widgets: widgets, $
        fonts: fonts $
    }
end
