pro write_rotation_errors, errs, filename
  openw, lun, /get_lun, filename
  printf, lun, n_elements(errs)
  for i=0, n_elements(errs)-1 do begin
    printf, lun, errs[i]
  endfor
  close, lun
end
