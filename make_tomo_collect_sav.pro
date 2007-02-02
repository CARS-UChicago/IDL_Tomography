pro make_tomo_collect_sav
   ; This IDL procedure makes tomo_collect.sav, to run under IDL Virtual Machine
   filenames = ['tomo_collect', 'ezcaIDL']
   classnames = ['tomo_collect', 'winx32_ccd', 'epics_motor']
   resolve_routine, filenames, /either, /compile_full_file, /no_recompile
   resolve_all, class=classnames
   save, /routine, file='tomo_collect.sav'
end
