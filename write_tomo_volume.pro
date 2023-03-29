pro write_tomo_volume, file, volume, _EXTRA=extra

;+
; NAME:
;   WRITE_TOMO_VOLUME
;
; PURPOSE:
;   Writes 3-D volume files to be read later by READ_TOMO_VOLUME or TOMO::READ-VOLUME.
;
;   This function is a simple wrapper around TOMO::WRITE_VOLUME.
;   See the documentation for <A HREF="#TOMO::WRITE_VOLUME">TOMO::WRITE_VOLUME </A>
;   for more information.
;
; CALLING SEQUENCE:
;   WRITE_TOMO_VOLUME, File, Volume
;
; PROCEDURE:
;   This function simply creates a TOMO object and calls TOMO::WRITE_VOLUME.
;-
    tomo = obj_new('tomo')
    tomo->write_volume, file, volume, _EXTRA=extra
    obj_destroy, tomo
end
