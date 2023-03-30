# IDL_Tomography Release notes

## R2-0 (March 30, 2023)
 - Continues support for the following:
   - Reconstruction of on-the-fly netCDF data files.
   - Reading and writing netCDF normalized and reconstructed files.
 - New features:
   - Reads and preprocesses raw data files in HDF5 format.
   - Preprocessing is done in C++ as part of tomoRecon package.
     - Improves performance more than 10X compared to previous tomopy preprocessing step.
   - Now handles 360 degree projection data
   - Optionally writes normalized data in HDF5 or netCDF format. 
   - Leaves normalized data after proprocessing in memory to be reconstructed immediately.
   - Writes reconstructed data in netCDF or HDF5 format. HDF5 has these advantages:
     - 3X faster to write than netCDF.
     - It can be very quickly read into ImageJ.
   - Leaves reconstructed data in memory for visualization immediately.
 - Only 6 mouse clicks from reading raw data file to visualizing reconstructed slices in X, Y, or Z.
 - Overall performance by factor of 3.8-5.6 compared to R1-0 depending on dataset size.
 - "tomo" IDL class is much cleaner and more complete.
   - Preprocessing and reconstruction can be easily scripted for batch processing.
 - Uses new version image_display which use IDL Object Graphics and has many enhancements. 
 - Adds Sphinx-generated documentation (https://cars-uchicago.github.io/IDL_Tomography).

## R1-0 (November 25, 2022)
- This is the first formal release.
- This is the last release that will support the following:
  - IDL based tomography data collection (tomo_collect, tomo_collect_ad2)
  - Princeton Instruments SPE data files.
  - "Slow scan" data, i.e. step-scan with one projection per netCDF data file
  - "Fast scan" data, i.e. step-scan multiple projections per data file

