; This script makes tomo_display.sav, to run under IDL Virtual Machine
.compile tomo_display
.compile tomo_display__define
.compile tomo__define
.compile image_display__define
.compile princeton_header__define
.compile idlexrotator__define
.compile trackball__define
resolve_all
save, /routine, file='tomo_display.sav'