pro write_tomo_volume, file, volume, _EXTRA=extra

;+
; NAME:
;   WRITE_TOMO_VOLUME
;
; PURPOSE:
;   Writes 3-D volume files to be read later by READ_TOMO_VOLUME or TOMO::
;   READ-VOLUME.
;
;   This function is a simple wrapper around TOMO::WRITE_VOLUME.
;   See the documentation for <A HREF="#TOMO::WRITE_VOLUME">TOMO::WRITE_VOLUME </A>
;   for more information.
;
; CATEGORY:
;   Tomography data processing
;
; CALLING SEQUENCE:
;   WRITE_TOMO_VOLUME, File, Volume
;
; PROCEDURE:
;   This function simply creates a TOMO object and calls TOMO::WRITE_VOLUME.
;
; EXAMPLE:
;   WRITE_TOMO_VOLUME, 'diamond2.volume', volume
;
; MODIFICATION HISTORY:
;   Written by:     Mark Rivers, May 13, 1998
;   26-JAN-2000  MLR  Added /swap_if_big_endian keyword to openw to allow
;                     files to be read on big-endian machines.
;   04-MAR-2001  MLR  Added support for netCDF file format.  Added NETCDF
;                     and SETUP keywords
;   11-APR-2001  MLR  Put the original version in the TOMO class library, made this
;                     function simply create a TOMO object and call TOMO::WRITE_VOLUME.
;-
    tomo = obj_new('tomo')
    tomo->write_volume, file, volume, _EXTRA=extra
    obj_destroy, tomo
end
