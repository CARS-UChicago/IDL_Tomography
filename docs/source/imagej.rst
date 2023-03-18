.. _IDL Tomography:       https://github.com/CARS-UChicago/IDL_tomography
.. _netCDF jar file:      https://lmb.informatik.uni-freiburg.de/resources/opensource/imagej_plugins/07.12.12/NetCDF\_.jar
.. _Freiburg HDF5 plugin: https://lmb.informatik.uni-freiburg.de/resources/opensource/imagej_plugins/hdf5.html
.. _PSI HDF5 plugin:      https://github.com/paulscherrerinstitute/ch.psi.imagej.hdf5
.. _PSI HDF5 releases:    https://github.com/paulscherrerinstitute/ch.psi.imagej.hdf5/releases

Visualize with ImageJ
---------------------
ImageJ is an excellent free program for displaying and analyzing tomography data.
ImageJ for Linux, Windows, and Mac can be downloaded from the
`ImageJ Website <https://imagej.nih.gov/ij/download.html>`__.

The download is a ZIP file that can be extracted to a location like ``C:\\ImageJ`` on Windows
or ``/home/user/ImageJ`` or ``/usr/local/ImageJ`` on Linux.

netCDF plugin
~~~~~~~~~~~~~
The netCDF files written by **tomo_display** or the Python **preprocess_13bm.py** routines can be read into ImageJ
by copying the following files into the ImageJ/plugins folder

- `netCDF jar file`_ from the University of Frieburg.
- GSETomo\_.ijm from the `IDL Tomography`_ repository.  See the instructions in :ref:`Installation` for how to download this.

Open ImageJ and select ``Plugins/GSETomo``.  This will open a file browser to select a netCDF file.
The 3-D dataset in the file
will be opened as a stack in ImageJ

HDF5 plugin
~~~~~~~~~~~
There are 2 ImageJ plugins for reading HDF5 files.

- The first one is the `Freiburg HDF5 plugin`_.
  This plugin can only read datasets smaller than 2 GB, so it is not useful for reading most tomography datasets.
- The second one is `PSI HDF5 plugin`_.
  This version is also limited to reading datasets smaller than 2 GB into "real" ImageJ stacks.
  However, it supports "virtual" ImageJ stacks, which can be any size.  Virtual stacks are not read into memory
  in their entirety, only the currently selected slice is actually in memory.  

Virtual datasets have these 2 advantages:

- The required memory is small
- It is very fast to open files and browse through them

They also have a major disadvantage:

- The datasets are read-only. 
 
  - This means that they cannot be modified and scaled to display unsigned 16-bit data, the way that GSETomo\_.ijm does for
    netCDF datasets.
    
To install the PSI plugin copy the latest jar file from the `PSI HDF5 releases`_ to the ImageJ/plugins folder.
NOTE: If this is being installed in FIJI, rather than ImageJ it is first necessary to uninstall the Freiburg HDF5 plugin.
This is explained in the `PSI HDF5 plugin`_ README.md file.

Open ImageJ and select ``File/Import/HDF5...``.  This will open a file browser to select an HDF5 file.
After selecting the HDF file it will open the following window to select which dataset to read:

.. figure:: ImageJ_HDF5_select.png
    :align: center

    **ImageJ HDF5 plugin dataset selection window**

This example is for reading an HDF5 raw data file, which contains 4 datasets.  
/exchange/data is the one with the projections, /exchange/data_white contains the flat fields.
Reconstruction and normalized HDF5 only have a single dataset, /exchange/data.

The 3-D dataset in the file will be opened by default as a virtual stack in ImageJ.  Unchecking the ``Virtual Stack``
box in the selection window will generate an error because the dataset is larger than 2 GB.

This is an example of a reconstructed dataset read into a virtual stack:

.. figure:: ImageJ_virtual_stack.png
    :align: center

    **ImageJ virtual stack for HDF5 file with reconstructed data**

It is possible to copy the virtual stack into a real stack in ImageJ. This is as follows:

- Right click on the image in the virtual stack window
- Select ``Duplicate``
- In the next window check the ``Duplicate stack`` box.

The copy operation takes some time.  The new window will be a real stack which can be modified.

Incompatibility of netCDF and HDF5 plugins
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unfortunately the netCDF and PSI HDF5 plugins referenced above are incompatible, meaning that they cannot both be
installed in the same ImageJ installation.
This is because they both use a Java logging utility, but they use incompatible versions.
The netCDF plugin uses a much older version.  
In principle this could be fixed by changing the Java source code of the netCDF plugin 
to use the more recent version of the logging utility.
This is not possible, however, because the source code for that plugin appears to have been lost.
I have contacted both the author, and the chair of the department at the University of Freiburg where the
author used to work, and neither are able to find it.

If both plugins are needed then the workaround is to have 2 separate installations of ImageJ or FIJI.
One will have the netCDF plugin and the other the HDF5 plugin.
