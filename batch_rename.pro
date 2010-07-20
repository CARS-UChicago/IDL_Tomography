pro batch_rename, base_file, first, last, offset

; Renames a series of files
  
  if (n_elements(offset) eq 0) then offset = 1
  
  for i = first, last do begin
    source_file = base_file + strtrim(i,2)+'.SPE'
    dest_file = base_file + strtrim(i-offset,2)+'.SPE'
    file_move, source_file, dest_file
   endfor
end