pro vol_diff, zrange=zrange, shift2=shift2, first_file=first_file, second_file=second_file, diff_file=diff_file

  if (n_elements(first_file) eq 0) then first_file = dialog_pickfile(title='Select first file...', filter='*.volume', /must_exist, get_path=get_path)
  if first_file eq '' then return
  if (n_elements(second_file) eq 0) then second_file = dialog_pickfile(title='Select second file...', filter='*.volume', /must_exist)
  if second_file eq '' then return
  if (n_elements(diff_file) eq 0) then diff_file = dialog_pickfile(title='Select name for volume output...', filter='*.volume', /overwrite_prompt)
  if diff_file eq '' then return

  print, 'Reading first file', first_file
  vol1 = read_tomo_volume(first_file, zrange=zrange)
  dims = size(vol1, /dimensions)
  nx = dims[0]
  ny = dims[1]
  nz = dims[2]

  print, 'Reading second file', second_file
  vol2 = read_tomo_volume(second_file, zrange=zrange)
  
  if (n_elements(shift2) ne 0) then begin
    print, 'Shifting volume 2'
    vol2 = shift(vol2, shift2[0], shift2[1], shift2[2])
  endif
  
  print, 'Computing difference'
  diff = vol1 - vol2 
  
  print, 'Writing difference file'
  write_tomo_volume, diff_file, diff
  
  print, 'Finished'
  
  end
