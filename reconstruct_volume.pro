;+
; NAME:
;   RECONSTRUCT_VOLUME
;
; PURPOSE:
;   This procedure reconstructs a complete 3-D data set (X, Y, Theta) into a 
;   3-D (X, Y, Z) volume.  It reads its input from disk and writes its output
;   back to disk.
;
;   This procedure is a simple wrapper around TOMO::RECONSTRUCT_VOLUME().
;   See the documentation for <A HREF="#TOMO::RECONSTRUCT_VOLUME">TOMO::RECONSTRUCT_VOLUME </A>
;   for information on the input parameters and other details.
;
; EXAMPLE:
;   reconstruct_volume, 'FOSSIL1', CENTER=329
;
; MODIFICATION HISTORY:
;   Written by:    Mark Rivers, April 23, 1999
;   30-APR-1999 MLR  Fixed bug introduced by new version of sinogram, need
;                    to get size of reconstructed slices after centering
;   18-MAY-1999 MLR  Changed formal parameter _extra to _ref_extra to allow
;                    CENTER keyword value to be returned from sinogram (via
;                    reconstruct_slice).
;   23-FEB-2000 MLR  Pass extra keywords to read_tomo_volume
;   7-MAR-2000  MLR  Added support for GRIDREC reconstruction, which reconstructs
;                    2 slices at once.
;   2-JAN-2001  MLR  Added CENTER keyword. If it is a 2-element array then the
;                    center is interpolated.
;   11-APR-2001 MLR  Put the original version in the TOMO class library, made this
;                    procedure simply create a TOMO object and call 
;                    TOMO::RECONSTRUCT_VOLUME.
;-

pro reconstruct_volume, base_file, _ref_extra=extra
    tomo = obj_new('tomo')
    tomo->reconstruct_volume, base_file, _extra=extra
    obj_destroy, tomo
end
