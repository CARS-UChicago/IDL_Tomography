===========================
GSECARS Tomography Software
===========================

:author: Mark Rivers, University of Chicago

.. contents:: Contents

.. _IDL_Tomography: https://github.com/CARS-UChicago/IDL_tomography


Overview
--------

This document describes the software used for reconstructing and viewing tomography
data at GSECARS. The software consists of the following components:
- C++ library containing optimized code for preprocessing and reconstruction
- IDL class library called "tomo" that does the following:

  - Read raw data files
  - Preprocess using the C++ library
  - Optimize rotation center
  - Reconstruct using the C++ library
  - Save reconstructed data
  - Save and restore preprocessing and reconstruction parameters

- IDL visualization class called image_display that can display 2-D and 3-D data
- IDL Graphical User Interface program that calls the "tomo" and "image_display" classes

All of these IDL routines are available from IDL_Tomography_.
The repository contains DLLs (Windows) and shareable libraries (Linux) to run
the IDL software without compiling any code.  

tomo_display is provided both in source code form in the repository and as an IDL .save
file, tomo_display.sav, that can be run for free in the IDL Virtual Machine.

Installation
------------

Number formats
~~~~~~~~~~~~~~

It can be convenient to specify the modbusStartAddress and modbusLength
in octal, rather than decimal, because this is the convention on most
PLCs. In the iocsh and vxWorks shells this is done by using a leading 0
on the number, i.e. 040400 is an octal number.
