function convert_x2b_proj_header, header

; NOTE: the following values are fixed
DATELEN = 32    ; length of projDateString
PADLEN  = 40    ; length of header terminal padding


str = {x2b_proj_header}

str.nviews         =  long(header, 0)           ;    0  number of projections
str.ysize          =  long(header, 4)           ;    4  projection image width
str.beg_slc        =  long(header, 8)           ;    8  start slc number
str.end_slc        =  long(header, 12)          ;   12  end slc number
str.magnification  =  long(header, 16)          ;   16  lens magnification (dummy)
str.pixSize        = float(header, 20)          ;   20  pixel size (microns)
str.energy         =  long(header, 24)          ;   24  x-ray energy
temp               =  byte(header, 28, DATELEN) ;   28
str.DateString     =  string(temp)              ;   28  date sample scanned
temp               =  byte(header, 60, PADLEN)  ;   60
str.pad            =  string(temp)              ;   60  header terminal padding
                                                ; 100 Total Header Size

; If this is a big-endian machine swap the byte order
; I don't know of a built-in IDL test for endianness, do it ourselves
t1 = 1
t2 = 1
byteorder, t2, /sswap, /swap_if_big_endian
big_endian = (t1 ne t2)
if (big_endian) then str = swap_endian(str)

return, str
end
