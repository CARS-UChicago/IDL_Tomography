function read_tomo_volume, file, _EXTRA=extra

;+
; NAME:
;   READ_TOMO_VOLUME
;
; PURPOSE:
;   Reads in 3-D volume files written by WRITE_TOMO_VOLUME or TOMO::WRITE_VOLUME.
;   This function is a simple wrapper around TOMO::READ_VOLUME.
;   See the documentation for <A HREF="#TOMO::READ_VOLUME">TOMO::READ_VOLUME </A>
;   for information on the input parameters and other details.
;
; PROCEDURE:
;   This function simply creates a TOMO object and calls TOMO::READ_VOLUME.
;
; EXAMPLE:
;   volume = READ_TOMO_VOLUME('diamond2.volume')
;
; MODIFICATION HISTORY:
;   Written by: Mark Rivers, May 13, 1998
;   06-APR-1999  MLR  Made file input optional, puts up dialog if it is not 
;                     specified
;   25-JAN-2000  MLR  Added /swap_if_big_endian keyword to openr to allow
;                     files to be read on big-endian machines.
;   23-FEB-2000  MLR  Added xrange, yrange, zrange keywords
;   11-APR-2001  MLR  Put the original version in the TOMO class library, made this
;                     function simply create a TOMO object and call TOMO::READ_VOLUME.
;-

    tomo = obj_new('tomo')
    vol = tomo->read_volume(file, _EXTRA=extra)
    obj_destroy, tomo
    return, vol
end
