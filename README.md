Displaying and Analyzing GSECARS Tomography Data

The IDL and ImageJ software for GSECARS tomography data is contained in this tar file:
http://cars.uchicago.edu/software/idl/pub/idl_tomography.tar

Untar that file into some directory on your system.   For example, it might
be/usr/local/idl_user/tomography on Linux or C:\idl_user\tomography  on Windows. In the rest
of this document that location will be referred to as <PATH_TO_TOMO_DISPLAY>.  


                               ImageJ

To use ImageJ to read the netCDF .volume and recon.volume files:

- Download the ImageJ netCDF plugin from:
http://lmb.informatik.uni-freiburg.de/lmbsoft/imagej_plugins/netcdf.html

Copy that NetCDF_.jar file to a location in the ImageJ/plugins folder directories, such as in
plugins/Input-Output/

- Take the GSETomo_.ijm file from the idl_tomography.tar file described above and place it in
the ImageJ/plugins folder.

- Start ImageJ click on Plugins/GSETomo.  Browse to open .volume or recon.volume file.


                                IDL

On Windows you must add the directory <PATH_TO_TOMO_DISPLAY>\win32dll or 
<PATH_TO_TOMO_DISPLAY>\win64dll to your Windows environment variable "Path".
This will allow Windows to find the correct version of libfftw3f-3.dll.


                        If you have an IDL license

To use the IDL tomo_display software with an IDL license simply add the directory 
<PATH_TO_TOMO_DISPLAY> to your IDL_PATH, using the IDL Window/Preferences/IDL/Paths menu.
You do NOT need to define the environment variable TOMO_RECON_SHARE discussed below.


                        If you do not have an IDL license

Download the IDL Virtual Machine from  http://www.exelisvis.com/ProductsServices/IDL.aspx

I believe you need to create an account to be able to download the IDL, which includes the
Virtual Machine.

You can run tomo_display.sav in the Virtual Machine by double-clicking on its icon or
by opening the IDL Virtual Machine from the Windows start menu.  On Linux type the command
"idl -vm <PATH_TO_TOMO_DISPLAY>/tomo_display.sav"

To use the tomoRecon C library to reconstruct in tomo_display do the following:

On Linux:

- Define the environment variable TOMO_RECON_SHARE to point to the complete path to
tomoRecon_linux_x86.so or tomoRecon_linux_x86_64.so depending on whether you are running 32 or
64 bit IDL.

On Windows:

- Define the environment variable TOMO_RECON_SHARE to point to the complete path to
tomoRecon_Win32_x86.dll or tomoRecon_Win32_x86_64.dll depending on whether you are running 32
or 64 bit IDL.
- Add the win32dll (for 32-bit) or win64dll (for 64-bit) directory to your PATH environment variable.
This is needed because those directories contain the fftw library needed by tomoRecon.

