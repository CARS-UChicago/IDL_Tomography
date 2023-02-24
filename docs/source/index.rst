
:author: Mark Rivers, University of Chicago

.. _IDL_Tomography:      https://github.com/CARS-UChicago/IDL_tomography
.. _IDL_Imaging:         https://github.com/CARS-UChicago/IDL_tomography
.. _tomo_display.sav:    https://cars.uchicago.edu/data/tomography/tomo_display/tomo_display.sav
.. _IDL:                 https://www.l3harrisgeospatial.com/Software-Technology/IDL
.. _IDL_Register:        https://www.l3harrisgeospatial.com/Company/Create-Account?returnurl=https%3a%2f%2fwww.l3harrisgeospatial.com%2fSoftware-Technology%2fIDL

GSECARS Tomography Software
===========================

This document describes how to install and use the software to reconstruct and 
visualize tomography data from the 13-BM-D beamline.  
This includes data collected both on the upstream tomography table, and that collected in the 250-ton press.

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

Table of Contents
-----------------

.. toctree::
  :maxdepth: 3
  
  installation
  file_formats
  tomo_display_quick_start
  processing_options
  imagej
  scripting
  benchmarks
