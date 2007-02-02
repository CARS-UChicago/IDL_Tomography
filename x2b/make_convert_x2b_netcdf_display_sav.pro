pro make_convert_x2b_netcdf_display_sav
   ; This IDL procedure makes convert_x2b_netcdf_display.sav, to run under IDL Virtual Machine
   filenames = ['convert_x2b_netcdf_display']
   classnames = ['tomo', 'convert_x2b_netcdf_display', 'x2b_proj_header']
   resolve_routine, filenames, /either, /compile_full_file, /no_recompile
   resolve_all, class=classnames
   ; For IDL 6.2 and above use the following line (I am not sure about IDL 6.1)
   itresolve
   ; For IDL 6.0 use the following line (I am not sure about IDL 6.1)
   ;idlitresolveitools
   save, /routine, file='convert_x2b_netcdf_display.sav'
end

