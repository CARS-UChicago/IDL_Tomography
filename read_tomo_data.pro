;+
; NAME:
;  READ_TOMO_DATA
;
; PURPOSE:
;   This procedure reads a tomography data set from individual Princeton
;   Instruments .SPE files.  It writes a 3-D volume file to disk.
;
;   This function is a simple wrapper around TOMO::PREPROCESS.  
;   See the documentation for <A HREF="#TOMO::PREPROCESS">TOMO::PREPROCESS</A>
;   for information on the input parameters and other details.
;
; PROCEDURE:
;   This function simply creates a TOMO object and calls TOMO::PREPROCESS.
;
; EXAMPLE:
;   The following example will read files Mydata1.SPE through Mydata370.SPE,
;   using a constant dark current of 50 counts at each pixel.  This data set
;   is assumed to have white field frames in it.  The output file will be
;   Mydata.volume
;       IDL>  READ_TOMO_DATA, 'Mydata', 1, 370, dark=50
;       ; Now read the volume file back into IDL
;       IDL> vol = READ_TOMO_VOLUME('Mydata.volume')
;       ; Play the volume data as a movie, rotating the sample
;       IDL> window, 0
;       IDL> make_movie, vol, min=3000, max=12000
;
; MODIFICATION HISTORY:
;   Written by: Mark Rivers, March 27, 1999.
;   3-APR-1999 MLR  Changed white field normalization to use weighted average
;                   of white fields before and after, rather than simple
;                   average
;   4-APR-1999 MLR  Changed the default value of threshold from 1.20 to 1.05
;                   Switched to double correlation method of zinger removal
;                   for white field images when possible.
;   18-MAY-1999 MLR Added missing keyword DOUBLE_THRESHOLD to procedure line
;   07-JUL-1999 MLR Changed zinger removal for data frames so it is done after
;                   whitefield correction.  This makes the identification of
;                   zingers (versus high-frequency structure in the whitefield)
;                   much more robust.
;   13-SEP-1999 MLR Changed the dark current correction to use a loop, so that
;                   two large arrays are not required at the same time.
;   08-DEC-1999 MLR Added FIRST_ROW and LAST_ROW keywords for handling very
;                   large data sets.
;                   Added OUTPUT keyword
;   02-MAR-2000 MLR Added DEBUG keyword to calls to REMOVE_TOMO_ARTIFACTS
;                   large data sets.
;   02-MAR-2000 MLR Changed the default value of THRESHOLD from 1.05 to 1.25
;                   because the lower threshold was causing significant
;                   blurring of sharp edges.  Changed the default value of
;                   DOUBLE_THRESHOLD from 1.02 to 1.05, since it was finding
;                   many more zingers than physically plausible.
;   02-MAR-2000 MLR Added SWAP_IF_BIG_ENDIAN keyword when opening output file.
;   22-JAN-2001 MLR Added some default debugging when writing output file
;   11-APR-2001 MLR Put the original version in the TOMO class library and renamed
;                   the procedure there PREPROCESS rather than READ_TOMO_DATA.
;                   Made this procedure simply create a TOMO object and call 
;                   TOMO::PREPROCESS.
;-

pro read_tomo_data, base_file, start, stop, _EXTRA=extra
    tomo = obj_new('tomo')
    tomo->preprocess, base_file, start, stop, _EXTRA=extra
    obj_destroy, tomo
end
