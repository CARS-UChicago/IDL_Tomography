.. _IDL_Tomography:      https://github.com/CARS-UChicago/IDL_tomography
.. _IDL_Imaging:         https://github.com/CARS-UChicago/IDL_tomography
.. _IDL:                 https://www.l3harrisgeospatial.com/Software-Technology/IDL
.. _tomo_display.sav:    https://cars.uchicago.edu/data/tomography/tomo_display/tomo_display.sav
.. _IDL_Register:        https://www.l3harrisgeospatial.com/Company/Create-Account?returnurl=https%3a%2f%2fwww.l3harrisgeospatial.com%2fSoftware-Technology%2fIDL

Installation
============
The following instructions are for installation for sites who have an IDL license which will use the full version of IDL
and for those without a license which will use the IDL Virtual Machine.

This software can be installed on Linux and Windows.  
A computer with at least 32 GB of RAM, at least 8 cores, and a fast solid-state hard disk for the data
is recommended.

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
