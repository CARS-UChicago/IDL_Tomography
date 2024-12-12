File formats
------------
Raw data
~~~~~~~~
Beginning in July 2020 the raw data is stored in HDF5 files with the extension “.h5”.  
There is one HDF5 file per dataset, and it contains the flat field images and the projections.
It also contains metadata including the positions of many motors on the beamline, the ring current,
the tomography data collection parameters, the sample and optics information, and more.
An ASCII text file with the extension “.config” contains the tomography data collection parameters,
the sample and optics information, and more.

From May 2014 to July 2020 the raw data is stored in netCDF files with the extension “.nc”.  
There are three netCDF files per dataset.  Two are the flat field images collected at 
the beginning and end of the scan, the third contains all of the projections.
An ASCII text file with the extension “.setup” contains the metadata, 
including the sample information, x-ray energy, and pixel size.

Prior to May 2014 the raw data is stored in Princeton Instruments SPE files with the extension ".spe".

Preprocessed (normalized) data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Beginning with the R2-0 version of *tomo_display* in January 2023 the
preprocessed data is generally not written to disk at all, it is kept in memory after preprocessing.
This significantly improves the reconstruction speed.  
*tomo_display* can optionally save the preprocessed data either as a netCDF file whose
name ends in "norm.nc" or an HDF5 file whose name ends in "norm.h5".

Prior to R2-0 preprocessed data was written to disk as netCDF files with a file name that ends in ".volume".
Prior to July 2020 these files were written by *tomo_display*, while from July 2020 to January 2023 they were written
by a Python script called *preprocess_13bm.py*.

Reconstructed data
~~~~~~~~~~~~~~~~~~
Beginning with the R2-0 version of *tomo_display* it can store reconstructed files in
either netCDF format with a file name that ends in "recon.nc", or in HDF5 format with a file name that ends in "recon.h5".
HDF5 is 2-3 times faster to write, and is generally recommended.

Prior to R2-0 the reconstructed files were stored in netCDF format with a file name that ends
in "recon.volume".

