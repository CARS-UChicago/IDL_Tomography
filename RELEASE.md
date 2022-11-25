# IDL_Tomography Release notes

## R2-0 (December XXX, 2022)
 - Continues support for the following:
   - Reconstruction of on-the-fly netCDF data files
   - Reading and writing netCDF .volume files
 - Adds support for the following:
   - Reading and preprocessing raw data files in HDF5 format
   - Writing normalized data in HDF5 format
     - This step will now be optional
   - Leaving normalized data after proprocessing in memory to be reconstructed immediately 
   - Writing reconstructed data in HDF5 format. It can then be quickly read into ImageJ

## R1-0 (November 25, 2022)
- This is the first formal release.
- This is the last release that will support the following:
  - IDL based tomography data collection (tomo_collect, tomo_collect_ad2)
  - Princeton Instruments SPE data files.
  - "Slow scan" data, i.e. step-scan with one projection per netCDF data file
  - "Fast scan" data, i.e. step-scan multiple projections per data file

