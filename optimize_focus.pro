function compute_entropy, data
    npixels = n_elements(data)
    h = histogram(data, min=0, max=4095, bin=1) > 1
    entropy = -total(h*alog(h))/npixels
    return, entropy
end

pro optimize_focus, images, focus_motor, entropy
    ; This procedure reads a sequence of images and computes the entropy of
    ; the images to optimize the focus
    s = size(images, /dimensions)
    nimages = s[2]
    full_entropy = dblarr(nimages)
    dims = size(images, /dimensions)
    nx = dims[0]
    ny = dims[1]
    quarter_entropy = dblarr(4, nimages)
    quarter_start_x = [0,      nx/2,   0,      nx/2]
    quarter_stop_x  = [nx/2-1, nx-1,   nx/2-1, nx-1]
    quarter_start_y = [0,      0,      ny/2,   ny/2]
    quarter_stop_y =  [ny/2-1, ny/2-1, ny-1,   ny-1]
    for i=0, nimages-1 do begin
       data = images[*,*,i]
       full_entropy[i] = compute_entropy(data)
       for j=0,3 do begin
           quarter_entropy[j,i] = compute_entropy(data[quarter_start_x[j]:quarter_stop_x[j], $
                                                       quarter_start_y[j]:quarter_stop_y[j]])
       endfor
    endfor
    ; Normalize the entropy values so minimum = 1
    full_entropy = full_entropy - min(full_entropy)
    colors = [[0,0,0], $    ;  Black
              [255,0,0], $  ;  Red
              [0,255,0], $  ;  Green
              [0,0,255], $  ;  Blue
              [0,255,255], $ ; Cyan
              [255,255,0], $ ; Yellow
              [255,0,255]]   ; Magenta
    quadrant_names = ['Lower left', $
                      'Lower right', $
                      'Upper left', $
                      'Upper right']
    for j=0, 3 do begin
        quarter_entropy[j,*] = quarter_entropy[j,*] - min(quarter_entropy[j,*])
    endfor
    iplot, focus_motor, full_entropy, sym_index=1, xtitle='Focus motor position', ytitle='Image entropy', $
           color=colors[*,0], name='Entire image'
    for j=0, 3 do begin
        iplot, focus_motor, quarter_entropy[j,*], identifier=id, /overplot, $
               color=colors[*,j+1], name=quadrant_names[j], sym_index=j+2

    endfor
    ny = fix(sqrt(nimages))
    nx = nimages/ny
    if (nx*ny lt nimages) then nx = nx + 1
    view_grid = [nx, ny]
    view_next = 1
    overplot = 0

    iimage, images[*,*,0], identifier=id, view_grid=view_grid, $
            view_title='Focus motor='+strtrim(focus_motor[0],2)
    for i=1, nimages-1 do begin
        title='Focus motor='+strtrim(focus_motor[i],2)
        iimage, images[*,*,i], identifier=id, /view_next, view_title=title, name=title
    endfor
end
