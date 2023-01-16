pro make_tomo_display_sav
   ; This IDL procedure makes tomo_display.sav, to run under IDL Virtual Machine
   filenames = ['tomo_display']
   classnames = ['tomo', 'tomo_display', 'image_display', 'idlexrotator', 'trackball']
   resolve_routine, filenames, /either, /compile_full_file, /no_recompile
   resolve_all, class=classnames
   itresolve
   save, /routine, file='tomo_display.sav'
end
