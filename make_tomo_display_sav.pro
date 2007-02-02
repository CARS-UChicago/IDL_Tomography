pro make_tomo_display_sav
   ; This IDL procedure makes tomo_display.sav, to run under IDL Virtual Machine
   filenames = ['tomo_display']
   classnames = ['tomo', 'tomo_display', 'image_display', 'princeton_header', 'idlexrotator', 'trackball']
   resolve_routine, filenames, /either, /compile_full_file, /no_recompile
   resolve_all, class=classnames
   ; For IDL 6.2 and above use the following line (I am not sure about IDL 6.1)
   itresolve
   ; For IDL 6.0 use the following line (I am not sure about IDL 6.1)
   ;idlitresolveitools
    save, /routine, file='tomo_display.sav'
end
