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
;-

    tomo = obj_new('tomo')
    vol = tomo->read_volume(file, _EXTRA=extra)
    obj_destroy, tomo
    return, vol
end
