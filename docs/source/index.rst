===========================
GSECARS Tomography Software
===========================

:author: Mark Rivers, University of Chicago

.. contents:: Contents

.. _IDL_Tomography:      https://github.com/CARS-UChicago/IDL_tomography
.. _IDL_Imaging:         https://github.com/CARS-UChicago/IDL_tomography
.. _tomo_display.sav:    https://cars.uchicago.edu/data/tomography/tomo_display/tomo_display.sav
.. _IDL:                 https://www.l3harrisgeospatial.com/Software-Technology/IDL
.. _IDL_Register:        https://www.l3harrisgeospatial.com/Company/Create-Account?returnurl=https%3a%2f%2fwww.l3harrisgeospatial.com%2fSoftware-Technology%2fIDL

Overview
--------

This document describes how to install the required software to reconstruct and 
visualize tomography data from the 13-BM-D beamline.  
This includes data collected both on the upstream tomography table, and that collected in the 250-ton press.
This software can be installed on Linux and Windows.  
A computer with at least 32 GB of RAM, at least 8 cores, and a fast solid-state hard disk for the data
is recommended.

The software consists of the following components:

- C++ library containing optimized code for preprocessing and reconstruction
- IDL_ class library called ``tomo`` that does the following:

  - Read raw data files
  - Preprocess using the C++ library
  - Optimize rotation center
  - Reconstruct using the C++ library
  - Save reconstructed data
  - Save and restore preprocessing and reconstruction parameters
- IDL visualization class called ``image_display`` that can display 2-D and 3-D data
- IDL GUI program called ``tomo_display`` that calls the ``tomo`` and ``image_display`` classes

``tomo`` and ``tomo_display`` are provided in source code form in the IDL_Tomography_ repository. 
That repository contains DLLs (Windows) and shareable libraries (Linux) to run
the IDL software without compiling any code.  ``tomo_display`` is also available as as an IDL .sav
file, tomo_display.sav_, that can be run for free in the IDL Virtual Machine. ``image_display`` is available in a
separate IDL_imaging_ repository.

Installation
------------
The following instructions are for installation for sites who have an IDL license which will use the full version of IDL
and for those without a license which will use the IDL Virtual Machine.

Install IDL
~~~~~~~~~~~
All sites must install IDL_, whether they will use the licensed version or the Virtual Machine. 
You should `register on the Harris site 
<https://www.l3harrisgeospatial.com/Company/Create-Account?returnurl=https%3a%2f%2fwww.l3harrisgeospatial.com%2fSoftware-Technology%2fIDL>`_ 
and then you can download the software and follow the installation
instructions. If you want to get started immediately with the IDL Virtual Machine you can 
`download IDL here <https://cars.uchicago.edu/data/idl>`__,
but please also register with Harris.

Install IDL_Tomography
~~~~~~~~~~~~~~~~~~~~~~
- Create a directory that will contain the IDL tomography routines. This might be C:\GSECARS_tomography on Windows
  or /home/youraccount/GSECARS_tomography on Linux.
- Clone the IDL_Tomography_ and IDL_Imaging_ repositories into that directory. This must be done even if you
  are using the IDL Virtual Machine because that repository contains the required DLLs for Windows and shareable
  libraries for Linux.
- If you are running the licensed version of IDL clone the IDL_Imaging_ repository into the same directory.
- If you are running the IDL Virtual Machine download tomo_display.sav_ to a folder like C:\GSECARS_tomography.
- If you are using the licensed version of IDL you need to add the directory you created to the IDL_PATH. 
  In the IDLDE use Window/Preferences/IDL/Paths to add that directory to the IDL Path, and check the box 
  to also search subdirectories.
  Alternatively you can set the IDL_PATH environment variable to e.g. "<IDL_DEFAULT>:+/home/myaccount/GSECARS_tomography".
  The + symbol means to search subdirectories.
- On Windows add the IDL_tomography folder to your PATH environment variable.
  This can be done with Control Panel/System/Advanced/Environment Variables.
  This is necessary for both the licensed and Virtual Machine versions of IDL to find libfftw3f-3.dll.
- If you are using the IDL Virtual machine you must to the following to define the location of the tomoRecon
  shareable library:

  - On Linux define the environment variable TOMO_RECON_SHARE to point to the complete path to tomoRecon_linux_x86_64.so, 
    which is contained in the IDL_Tomography software downloaded above.
    Example: ``export TOMO_RECON_SHARE=/usr/local/tomography/idl/tomoRecon_linux_x86_64.so``
  - On Windows define the environment variable TOMO_RECON_SHARE to point to the complete path to tomoRecon_Win32_x86_64.dll.
    Example: ``set TOMO_RECON_SHARE=C:\tomography\idl\tomoRecon_Win32_x86.dll``. This can be done in Control Panel/System as well.
- On Linux make sure the fftw-devel package is installed.  This is required for reconstruction.

Starting tomo_display
---------------------
If you are running the licensed version of IDL then start IDL and type the IDL command *tomo_display*.
If you are running the IDL Virtual Machine on Windows then double-click on the tomo_display.sav file,
or open the IDL Virtual Machine from the Windows Start menu and browse for the file.
On Linux type the command ``idl -vm=tomo_display.sav`` 

File formats
------------
Raw data
~~~~~~~~
From May 2014 to July 2020 the raw data is stored in netCDF files with the extension “.nc”.  
There are three netCDF files per dataset.  Two are the flat field images collected at 
the beginning and end of the scan, the third contains all of the projections.
An ASCII text file with the extension “.setup” contains the metadata, 
including the sample information, x-ray energy, and pixel size.

Beginning in July 2020 the raw data is stored in HDF5 files with the extension “.h5”.  
There is one HDF5 file per dataset, and it contains the flat field images and the projections.
It also contains metadata including the positions of many motors on the beamline, the ring current,
the tomography data collection parameters, the sample and optics information, and more.
An ASCII text file with the extension “.config” contains the tomography data collection parameters,
the sample and optics information, and more.

Preprocessed (normalized) data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Prior to January 2023 preprocessed data was written to disk as netCDF files with a file name that ends in ".volume".
Prior to July 2020 these files were written by *tomo_display*, while from July 2020 to January 2023 they were written
by a Python script called preprocess_13bm.py.  Beginning with the new version of *tomo_display* in January 2023 the
preprocessed data generally not written to disk at all, it is kept in memory.  This significantly improves
the reconstruction speed.  *tomo_display* can optionally save the preprocessed data either as a netCDF file whose
name ends in "norm.nc" or an HDF5 file whose name ends in "norm.h5".

Reconstructed data
~~~~~~~~~~~~~~~~~~
Prior to January 2023 the reconstructed files were stored in netCDF format with a file name that ends
in "recon.volume".  tomo_display can now stored reconstructed files in either netCDF format with a file
name that ends in "recon.nc", or in HDF5 format with a file name that ends in "recon.h5".  HDF5 is 2-3 times
faster to write, and is generally recommended.  However, there are some limitations in reading HDF5 files into
ImageJ which are discussed below.

Using tomo_display
------------------

Visualizing results
-------------------

Visualize with tomo_display
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Visualize with ImageJ
~~~~~~~~~~~~~~~~~~~~~

Limitations in ImageJ
_____________________


Scripting and batch processing
------------------------------

Benchmarks
----------
