pro tomo_display::set_base_file, base_file
    widget_control, self.widgets.base_file, set_value=base_file
    widget_control, self.widgets.first_file, set_value=1
    files = findfile(base_file + '*.SPE', count=count)
    widget_control, self.widgets.last_file, set_value=count
    ; Read the setup file.  If there are non-zero values for the dark current
    ; or rotation axis then use them.
    tomo = obj_new('tomo')
    s = tomo->read_setup(base_file + '.setup')
    setup = tomo->get_setup()
    if (setup.dark_current ne 0.) then begin
        widget_control, self.widgets.dark_current, set_value=setup.dark_current
    endif
    if (setup.center ne 0.) then begin
        widget_control, self.widgets.rotation_center, $
                        set_value=setup.center
    endif
end

pro tomo_display::set_directory
    cd, current=current
    widget_control, self.widgets.directory, set_value=current
    ; See if there are any .setup files in this directory
    ; If so, make the first one the default base file
    files = findfile('*.setup', count=count)
    if (count ge 1) then begin
        file = files[0]
        index = strpos(file, '.setup')
        file = strmid(file, 0, index)
        self->set_base_file, file
    endif
end

pro tomo_display::set_movie_limits
    widget_control, self.widgets.direction, get_value=direction
    case direction of
        0: widget_control, self.widgets.last_slice, set_value=self.nx-1
        1: widget_control, self.widgets.last_slice, set_value=self.ny-1
        2: widget_control, self.widgets.last_slice, set_value=self.nz-1
    endcase
end

pro tomo_display::free_memory
    ptr_free, self.pvolume
    widget_control, self.widgets.volume_file, set_value=''
end

pro tomo_display_event, event
    widget_control, event.top, get_uvalue=tomo_display
    tomo_display->event, event
end

pro tomo_display::event, event
    if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
        widget_control, event.top, /destroy
        obj_destroy, self
        return
    endif
    case event.id of
        self.widgets.change_directory: begin
            f = dialog_pickfile(/directory, get_path=p)
            if (p ne "") then begin
                cd, p
                self->set_directory
            endif
        end

        self.widgets.read_file: begin
            file = dialog_pickfile(filter='*.volume', get_path=path)
            if (file ne '') then begin
                ptr_free, self.pvolume
                widget_control, /hourglass
                vol = read_tomo_volume(file)
                dims = size(vol, /dimensions)
                if (n_elements(dims) eq 3) then begin
                    self.pvolume = ptr_new(vol, /no_copy)
                    ; Set the volume filename
                    widget_control, self.widgets.volume_file, set_value=file
                    widget_control, self.widgets.directory, set_value=path
                    widget_control, self.widgets.nx, set_value=dims[0]
                    self.nx = dims[0]
                    widget_control, self.widgets.ny, set_value=dims[1]
                    self.ny = dims[1]
                    widget_control, self.widgets.nz, set_value=dims[2]
                    self.nz = dims[2]
                    min = min(*self.pvolume, max=max)
                    widget_control, self.widgets.display_min, set_value=min
                    widget_control, self.widgets.display_max, set_value=max
                    self->set_movie_limits
                endif
                cd, path
                self->set_directory
            endif
        end

        self.widgets.exit: begin
            widget_control, event.top, /destroy
            obj_destroy, self
            return
        end

        self.widgets.base_file: begin
            widget_control, self.widgets.base_file, get_value=base_file
            self->set_base_file, base_file[0]
        end

        self.widgets.free_memory: begin
            self->free_memory
        end

        self.widgets.preprocess_go: begin
            widget_control, self.widgets.base_file, get_value=base_file
            base_file = base_file[0]
            widget_control, self.widgets.first_file, get_value=first_file
            widget_control, self.widgets.last_file, get_value=last_file
            widget_control, self.widgets.dark_current, get_value=dark_current
            widget_control, /hourglass
            read_tomo_data, base_file, first_file, last_file, $
                            dark=dark_current
        end

        self.widgets.reconstruct_slice: begin
            if (ptr_valid(self.pvolume)) then begin
                widget_control, self.widgets.rotation_center, get_value=center
                widget_control, self.widgets.scale, get_value=scale
                widget_control, self.widgets.recon_slice, get_value=slice
                widget_control, /hourglass
                r = reconstruct_slice(slice, *self.pvolume, center=center, $
                                      scale=scale)
                widget_control, self.widgets.auto_intensity, get_value=auto
                if (auto) then begin
                    min=min(r, max=max) 
                endif else begin
                    widget_control, self.widgets.display_min, get_value=min
                    widget_control, self.widgets.display_max, get_value=max
                endelse
                widget_control, self.widgets.volume_file, get_value=file
                title = file + '    Center='+strtrim(string(center),2)
                image_display, r, min=min, max=max, title=title
            endif else begin
                t = dialog_message('Must read in volume file first.', /error)
            endelse
        end

        self.widgets.reconstruct_all: begin
            ; Delete any volume array to free up memory
            self->free_memory
            widget_control, self.widgets.rotation_center, get_value=center
            widget_control, self.widgets.scale, get_value=scale
            widget_control, self.widgets.base_file, get_value=base_file
            base_file = base_file[0]
            widget_control, /hourglass
            reconstruct_volume, base_file, center=center, scale=scale
        end

        self.widgets.direction: begin
            self->set_movie_limits
        end

        self.widgets.order: begin
            widget_control, self.widgets.order, get_value=order
            !order=order
        end

        self.widgets.auto_intensity: begin
            ; Nothing to do
        end

        self.widgets.display_slice: begin
            if (ptr_valid(self.pvolume)) then begin
                widget_control, self.widgets.disp_slice, get_value=slice
                widget_control, self.widgets.display_min, get_value=min
                widget_control, self.widgets.display_max, get_value=max
                widget_control, self.widgets.direction, get_value=direction
                widget_control, self.widgets.volume_file, get_value=file
                widget_control, self.widgets.rotation_center, get_value=center
                axes = ['X', 'Y', 'Z']
                title = file + '    Center='+strtrim(string(center),2) + $
                        '     '+axes[direction]+'='+strtrim(string(slice),2)
                case direction of
                    0: begin
                        slice = (slice > 0) < (self.nx-1)
                        r = (*(self.pvolume))[slice, *, *]
                    end
                    1: begin
                        slice = (slice > 0) < (self.ny-1)
                        r = (*(self.pvolume))[*, slice, *]
                    end
                    2: begin
                        slice = (slice > 0) < (self.nz-1)
                        r = (*(self.pvolume))[*, *, slice]
                    end
                endcase
                widget_control, self.widgets.auto_intensity, get_value=auto
                if (auto) then begin
                    min=min(r, max=max) 
                endif else begin
                    widget_control, self.widgets.display_min, get_value=min
                    widget_control, self.widgets.display_max, get_value=max
                endelse
                image_display, r, min=min, max=max, title=title
            endif else begin
                t = dialog_message('Must read in volume file first.', /error)
            endelse
        end

        self.widgets.movie_output: begin
            ; Nothing to do
        end

        self.widgets.zoom: begin
            ; Nothing to do
        end

        self.widgets.make_movie: begin
            widget_control, self.widgets.disp_slice, get_value=slice
            if (ptr_valid(self.pvolume)) then begin
                widget_control, self.widgets.movie_output, get_value=output
                widget_control, self.widgets.display_min, get_value=min
                widget_control, self.widgets.display_max, get_value=max
                widget_control, self.widgets.direction, get_value=direction
                widget_control, self.widgets.movie_file, get_value=file
                widget_control, self.widgets.zoom, get_value=zoom
                widget_control, self.widgets.first_slice, get_value=start
                widget_control, self.widgets.last_slice, get_value=stop
                widget_control, self.widgets.slice_step, get_value=step
                all_zooms = [-4, -2, 1, 2, 4]
                scale = all_zooms[zoom]
                label=0
                case output of
                    0: label=1
                    1: widget_control, self.widgets.movie_file, $
                                       get_value=jpeg_file
                    2: widget_control, self.widgets.movie_file, $
                                       get_value=mpeg_file
                endcase
                make_movie, index=direction+1, scale=scale, *self.pvolume, $
                            jpeg_file=jpeg_file, mpeg_file=mpeg_file, min=min, $
                            max=max, start=start, stop=stop, step=step, $
                            label=label
            endif else begin
                t = dialog_message('Must read in volume file first.', /error)
            endelse
        end

        else:  t = dialog_message('Unknown event')
    endcase

    ; If there is a valid volume array make the visualize base sensitive
    sensitive = ptr_valid(self.pvolume)
    widget_control, self.widgets.visualize_base, sensitive=sensitive
    
end


function tomo_display::init
;+
; NAME:
;       TOMO_DISPLAY::INIT
;
; PURPOSE:
;       This function initializes an object of class TOMO_DISPLAY.  It is
;       not called directly, but is called indirectly when a new object of
;       class TOMO_DISPLAY is created via OBJ_NEW('TOMO_DISPLAY')
;       
;       The TOMO_DISPLAY object is a GUI display which provides control
;       for preprocessing, reconstructing and visualizing tomographic data
;
; CATEGORY:
;       Imaging
;
; CALLING SEQUENCE:
;       obj = OBJ_NEW('TOMO_DISPLAY')
;
; EXAMPLE:
;       IDL> obj = OBJ_NEW('TOMO_DISPLAY')
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers (16-Nov-2001)
;
;-

    maintitle= 'IDL Tomo Processing'
    stmp     = 'image'
    if (keyword_set(title))    then begin
        maintitle=title
    endif
    if (keyword_set(subtitle)) then  begin
        maintitle= maintitle+ ': '+subtitle
    endif

    self.fonts.normal = get_font_name(/helvetica)
    self.fonts.heading1 = get_font_name(/large, /bold)
    self.fonts.heading2 = get_font_name(/bold)

    self.widgets.base= widget_base(column=1, /tlb_kill_request_events, $
                                   title=maintitle, mbar=mbar)

    file           = widget_button(mbar, /menu, value = 'File      ')
    self.widgets.change_directory = widget_button(file, $
                                            value = 'Change directory ...')
    self.widgets.read_file = widget_button(file, $
                                            value = 'Read volume file ...')
    self.widgets.free_memory = widget_button(file, value='Free volume array')
    self.widgets.exit = widget_button(file, $
                                            value = 'Exit')


    col = widget_base(self.widgets.base, /column, /frame)
    self.widgets.base_file = cw_field(col, title="Base file name:", $
                                        xsize=50, /return_events)
    self.widgets.directory = cw_field(col, title="Working directory:", $
                                        xsize=50, /noedit)
    self.widgets.volume_file = cw_field(col, title="Volume file name:", $
                                        xsize=50, /noedit)


    ; Preprocessing
    col = widget_base(self.widgets.base, /column, /frame)
    t = widget_label(col, value='Preprocess', font=self.fonts.heading1)
    row = widget_base(col, /row)
    self.widgets.first_file = cw_field(row, /column, title='First file', $
                                      /integer, value=1)
    self.widgets.last_file = cw_field(row, /column, title='Last file', $
                                      /integer, value=1)
    self.widgets.dark_current = cw_field(row, /column, title='Dark current', $
                                      /integer, value=100)
    self.widgets.preprocess_go = widget_button(row, value=' Preprocess! ')

    ; Reconstruction
    col = widget_base(self.widgets.base, /column, /frame)
    self.widgets.reconstruct_base = col
    t = widget_label(col, value='Reconstruct', font=self.fonts.heading1)
    row = widget_base(col, /row)
    self.widgets.rotation_center = cw_field(row, /column, $
                                            title='Rot. Center', /integer, $
                                            value=329)
    self.widgets.scale = cw_field(row, /column, title='Scale', $
                                      /float, value=1e6)
    self.widgets.recon_slice = cw_field(row, /column, title='Slice', $
                                      /integer, value=100)
    col = widget_base(row, /column)
    self.widgets.reconstruct_slice = widget_button(col, $
                                                   value='Reconstruct slice')
    self.widgets.reconstruct_all = widget_button(col, $
                                                   value='Reconstruct all')

    ; Visualization
    col = widget_base(self.widgets.base, /column, /frame)
    self.widgets.visualize_base = col
    widget_control, col, sensitive=0
    t = widget_label(col, value='Visualize', font=self.fonts.heading1)

    row = widget_base(col, /row)
    t = widget_label(row, value='Volume array size:')
    self.widgets.nx = cw_field(row, title='NX', /integer, /noedit, /column, $
                                value=0)
    self.widgets.ny = cw_field(row, title='NY', /integer, /noedit, /column, $
                                value=0)
    self.widgets.nz = cw_field(row, title='NZ', /integer, /noedit, /column, $
                                value=0)

    row = widget_base(col, /row)
    self.widgets.direction = cw_bgroup(row, ['X', 'Y', 'Z'], $
                                            label_left='Direction:', $
                                            row=1, set_value=2, /exclusive)
    !order=1
    self.widgets.order = cw_bgroup(row, ['Top to bottom', 'Bottom to top'], $
                                            label_left='Order:', row=1, $
                                            set_value=1, /exclusive)

    row = widget_base(col, /row)
    t = widget_label(row, value='Intensity range:')
    self.widgets.display_min = cw_field(row, title='Min.', /float, $
                                        /column, value=0)
    self.widgets.display_max = cw_field(row, title='Max.', /float, $
                                        /column, value=5000)
    self.widgets.auto_intensity = cw_bgroup(row, ['Manual', 'Auto'], $
                                             column=1, set_value=1, /exclusive)


    row = widget_base(col, /row)
    t = widget_label(row, value='Display slice:')
    self.widgets.disp_slice = cw_field(row, title='Slice', /integer, value=100)
    self.widgets.display_slice = widget_button(row, $
                                                   value='Display slice')

    t = widget_label(col, value='Movies', font=self.fonts.heading1)

    row = widget_base(col, /row)
    self.widgets.movie_output = cw_bgroup(row, ['Screen', 'JPEGs', 'MPEG'], $
                                            label_left='Output:', row=1, $
                                            set_value=0, /exclusive)

    row = widget_base(col, /row)
    self.widgets.first_slice = cw_field(row, title='First slice', /integer, $
                                        /column, value=0)
    self.widgets.last_slice = cw_field(row, title='Last slice', /integer, $
                                        /column, value=0)
    self.widgets.slice_step = cw_field(row, title='Step', /integer, $
                                        /column, value=1)

    row = widget_base(col, /row)
    self.widgets.zoom = cw_bgroup(row, ['1/4', '1/2', '1', '2', '4'], $
                                    label_left='Zoom:', row=1, $
                                    set_value=2, /exclusive)

    row = widget_base(col, /row)
    self.widgets.movie_file = cw_field(row, title="JPEG/MPEG file name:", $
                                        xsize=40)
    self.widgets.make_movie = widget_button(row, value='Make movie')

    widget_control, self.widgets.base, set_uvalue=self
    widget_control, self.widgets.base, /realize

    self->set_directory

    xmanager, 'tomo_display', self.widgets.base, /no_block
    return, 1
end

pro tomo_display::cleanup
    ptr_free, self.pvolume
end

pro tomo_display__define

    widgets={ tomo_display_widgets, $
        base: 0L, $
        change_directory: 0L, $
        read_file: 0L, $
        exit: 0L, $
        base_file: 0L, $
        directory: 0L, $
        volume_file: 0L, $
        free_memory: 0L, $
        first_file: 0L, $
        last_file: 0L, $
        dark_current: 0L, $
        preprocess_go: 0L, $
        reconstruct_base: 0L, $
        rotation_center: 0L, $
        scale: 0L, $
        recon_slice: 0L, $
        reconstruct_slice: 0L, $
        reconstruct_all: 0L, $
        visualize_base: 0L, $
        nx: 0L, $
        ny: 0L, $
        nz: 0L, $
        direction: 0L, $
        order: 0L, $
        display_min: 0L, $
        display_max: 0L, $
        auto_intensity: 0L, $
        disp_slice: 0L, $
        display_slice: 0L, $
        movie_output: 0L, $
        first_slice: 0L, $
        last_slice: 0L, $
        slice_step: 0L, $
        zoom: 0L, $
        movie_file: 0L, $
        make_movie: 0L $
    }

    fonts = {tomo_fonts, $
        normal: '', $
        heading1: '', $
        heading2: '' $
    }

    tomo_display = {tomo_display, $
        widgets: widgets, $
        pvolume: ptr_new(), $
        nx: 0, $
        ny: 0, $
        nz: 0, $
        fonts: fonts $
    }
end
