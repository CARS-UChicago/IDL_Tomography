pro x2b_proj_header__define

; NOTE: the following values are fixed
DATELEN = 32    ; length of projDateString
PADLEN  = 40    ; length of header terminal padding

x2b_proj_header = {x2b_proj_header, $
  nviews:               0L, $               ;    0  Number of projections
  ysize:                0L, $               ;    4  y dimension of projection
  beg_slc:              0L, $               ;    8  starting slc number
  end_slc:              0L, $               ;   12  ending slc number
  magnification:        0L, $               ;   16  lens magnification (dummy)
  pixSize:              0., $               ;   20  pixel size, microns
  energy:               0L, $               ;   24  x-ray energy, kev
  DateString:           "", $               ;   28  data sample scanned
  pad:                  "" $                ;   60  empty
}                                           ;   100 Bytes Total Header Size

end
