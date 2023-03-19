Scripting and batch processing
------------------------------
The ``tomo`` class can be called from user-written IDL routines to perform all steps in the reconstruction process.
This can be used to write scripts to do batch processing of data without using the tomo_display GUI.

Processing a single dataset
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following IDL procedure will reconstruct a single dataset.

.. code-block:: IDL

  pro process_dataset, camera_file
    tomo = obj_new('tomo')
    tomo->restore_settings, 'tomo_settings.txt'
    tomo->read_camera_file, camera_file
    tomo->preprocess
    tomo->optimize_center, [200, 1080], 954, method='0-180'
    tomo->reconstruct_volume
  end

This procedure does the following:

- Creates a ``tomo`` object
- Restores a settings file.
  It is a good idea to save a settings file with the preprocessing, center optimization, and reconstruction parameters
  that work well for similar datasets.  This can be done with the ``File\Save settings ...`` menu in ``tomo_display``.
  This is the contents of that tomo_settings.txt file:
  ::
 
    {
      "zingerWidth": 3,
      "zingerThreshold": 0.10000000,
      "zingerDoubleThreshold": 0.10000000,
      "preprocessScale": 10000.000,
      "preprocessOffset": 0.00000000,
      "preprocessThreads": 8,
      "preprocessDataType": "Float32",
      "preprocessWriteOutput": 0,
      "preprocessWriteFormat": "HDF5",
      "reconMethod": 0,
      "reconSlicesPerChunk": 256,
      "reconDataType": "Int16",
      "reconWriteOutput": 1,
      "reconWriteFormat": "HDF5",
      "reconScale": 1000000.0,
      "reconOffset": 0.00000000,
      "reconThreads": 8,
      "paddedSinogramWidth": 0,
      "paddingAverage": 10,
      "airPixels": 10,
      "ringWidth": 9,
      "fluorescence": 0,
      "debug": 2,
      "debugFile": "tomoRecon_debug.txt",
      "geom": 0,
      "pswfParam": 6.0000000,
      "sampl": 1.0000000,
      "maxPixSize": 1.0000000,
      "ROI": 1.0000000,
      "X0": 0.00000000,
      "Y0": 0.00000000,
      "ltbl": 512,
      "GR_filterName": "hann",
      "BP_Method": 0,
      "BP_filterName": "Gen_Hamming",
      "BP_filterSize": 1920,
      "RiemannInterpolation": 0,
      "RadonInterpolation": 0,
      "optimizeMethod": 1,
      "optimizeRange": 10.000000,
      "optimizeStep": 0.50000000,
      "movieZoom": 2,
      "displayOrder": 1,
      "displayDirection": 2,
      "displayAuto": 0,
      "displayMin": -1000.0000,
      "displayMax": 3500.0000

- Reads the camera file
- Preprocesses the data
- Optimizes the rotation center. The upper and lower slices, initial guess of rotation center, and
  optimization method were specified on the command line. 
- Does the reconstruction

It can be run at the IDL command line with the following commands:

.. code-block:: IDL

  IDL> cd, 'C:\Data\Pamukcu'
  IDL> .compile -v 'C:\Data\process_dataset.pro'
  IDL> process_dataset, 'Ka5_RunD_A.h5'

That produces this output:
::

  Time to read camera file=       5.9350002
  Sun Mar 19 10:01:17 2023 Doing corrections on flat fields ...
  Sun Mar 19 10:01:19 2023 Dark, flat, zinger correction ...
  Sun Mar 19 10:01:27 2023 Preprocessing complete
  Preprocess execution times:
                    Flat adjustments:       2.5039999
    Dark, flat and zinger correction:       7.4550002
                  Convert to UInt16:      0.00000000
                      Writing output:    0.0019998550
                      Freeing memory:      0.58599997
                              Total:       10.547000
  optimize_center, time=     0.093000174
  Sun Mar 19 10:01:27 2023 Initializing reconstruction ...
  Sun Mar 19 10:01:27 2023 Beginning reconstruction ...
  tomo_recon: time to convert to float:      0.12100005
                  time to reconstruct:       36.282000
                            total time:       36.403000
  Sun Mar 19 10:02:05 2023 Converting to output data type ...
  Sun Mar 19 10:02:13 2023 Writing reconstructed file ...
      Convert 360 to 180:      0.00000000
  Convert input to float:      0.00000000
              Reconstruct:       37.346000
  Convert output to Int16:       8.6210001
              Write file:       35.560000
              Total time:       81.527000
  Sun Mar 19 10:02:49 2023 Reconstruction complete.
  Time to read file=       11.945000

Processing a mulitple datasets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following IDL procedure will reconstruct a multiple datasets collected with the IDL ``collect_tomoscan_stack`` procedure.
Each file has a base name followed by _A, _B, etc.

.. code-block:: IDL

  pro process_stack, base_file, num_files
    tomo = obj_new('tomo')
    suffixes = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M']
    tomo->restore_settings, 'tomo_settings.txt'
    for i=0, num_files-1 do begin
      camera_file = base_file + '_' + suffixes[i] + '.h5'
      print, 'Processing file: ' + camera_file
      tomo->read_camera_file, camera_file
      tomo->preprocess
      tomo->optimize_center, [200, 1080], 954, method='0-180'
      tomo->reconstruct_volume
      print, 'Done processing file: ' + camera_file
    endfor
  end

This is similar to the ``process_dataset`` procedure above, but it loops over a set of datasets.
Note that the ``tomo`` object is created outside the loop, which is more efficient.
The settings file is also read outside the loop.  
The procedure prints status information before and after processing so the user can see what dataset
is currently being processed.

It can be run at the IDL command line with the following commands:

.. code-block:: IDL

  IDL> cd, 'C:\Data\Pamukcu'
  IDL> .compile -v 'C:\Data\process_stack.pro'
  IDL> process_stack, 'Ka5_RunD', 6

Ka5_RunD is the base file name, and there are 6 datasets in this stack.

