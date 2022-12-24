pro tomo_params_set_dimensions, tp, volume

    size = size(volume, /dimensions)
    if (n_elements(size) eq 2) then begin
        tp.numPixels      = size[0]
        tp.numSlices      = 1
        tp.numProjections = size[1]
    endif else if (n_elements(size) eq 3) then begin
        tp.numPixels      = size[0]
        tp.numSlices      = size[1]
        tp.numProjections = size[2]
    endif
end

pro tomo_params_update, tp, volume, dimensions=dimensions, $
            sinoScale = sinoScale, $
            reconScale = reconScale, $
            paddedSinogramWidth=paddedSinogramWidth, $
            paddingAverage=paddingAverage, $
            airPixels = airPixels, $
            ringWidth = ringWidth, $
            fluorescence = fluorescence, $
            reconMethod = reconMethod, $
            numThreads = numThreads, $
            slicesPerChunk = slicesPerChunk, $
            debug = debug, $
            dbgFile = dbgFile, $
            geom=geom, $
            pswfParam=pswfParam, $
            sampl=sampl, $
            maxPixSize=maxPixSize, $
            ROI=ROI, $
            X0=X0, $
            Y0=Y0, $
            ltbl=ltbl, $
            GR_filterName=GR_filterName, $
            BP_method = BP_method, $
            BP_filterName = BP_filterName, $
            BP_filterSize = BP_filterSize, $
            RiemannInterpolation = RiemannInterpolation, $
            RadonInterpolation = RadonInterpolation

    if (n_elements(volume) ne 0) then begin
        tomo_params_set_dimensions, tp, volume
    endif

    if (n_elements(dimensions) ne 0) then begin
        tp.numPixels      = dimensions[0]
        tp.numSlices      = dimensions[1]
        tp.numProjections = dimensions[2]
    endif

    ; If air normalization is not done then divide by 10000.
    if (n_elements(sinoScale) ne 0) then tp.sinoScale = sinoScale

    ; Reconstuction files can be 16-bit integers.
    ; The voxel values are multiplied typically by 1e6 to convert to integers.
    if (n_elements(reconScale) ne 0) then tp.reconScale = reconScale

    ; There are 2 "special" values of paddedSinogramWidth
    ; 0 = automatically set the width to power of 2 that is >= actual width
    ; 1 = No padding, set the width to the actual width
    if (n_elements(paddedSinogramWidth) ne 0) then begin
      if (paddedSinogramWidth eq 0) then begin
        ; Use the next largest power of 2 by default
        paddedSinogramWidth = 128
        repeat begin
            paddedSinogramWidth=paddedSinogramWidth * 2
        endrep until (paddedSinogramWidth ge tp.numPixels)
      endif else if (paddedSinogramWidth eq 1) then begin
        paddedSinogramWidth = tp.numPixels
      endif
      tp.paddedSinogramWidth = paddedSinogramWidth
    endif
    
    if (n_elements(paddingAverage)       ne 0) then tp.paddingAverage = paddingAverage
    if (n_elements(airPixels)            ne 0) then tp.airPixels = airPixels
    if (n_elements(ringWidth)            ne 0) then tp.ringWidth = ringWidth
    if (n_elements(fluorescence)         ne 0) then tp.fluorescence = fluorescence
    if (n_elements(reconMethod)          ne 0) then tp.reconMethod = reconMethod
    if (n_elements(numThreads)           ne 0) then tp.numThreads = numThreads
    if (n_elements(slicesPerChunk)       ne 0) then tp.slicesPerChunk = slicesPerChunk
    if (n_elements(debug)                ne 0) then p.debug = debug
    if (n_elements(dbgFile)              ne 0) then tp.debugFile = [byte(dbgFile), 0B]
    if (n_elements(geom)                 ne 0) then tp.geom = geom
    if (n_elements(pswfParam)            ne 0) then tp.pswfParam = pswfParam
    if (n_elements(sampl)                ne 0) then tp.sampl = sampl
    if (n_elements(maxPixSize)           ne 0) then tp.maxPixSize = maxPixSize
    if (n_elements(ROI)                  ne 0) then tp.ROI = ROI
    if (n_elements(X0)                   ne 0) then tp.X0 = X0
    if (n_elements(Y0)                   ne 0) then tp.Y0 = Y0
    if (n_elements(ltbl)                 ne 0) then tp.ltbl = ltbl
    if (n_elements(GR_filterName)        ne 0) then tp.GR_filterName = [byte(GR_filterName),0B]
    if (n_elements(BP_method)            ne 0) then tp.BP_method = BP_method
    if (n_elements(BP_filterName)        ne 0) then tp.BP_filterName = [byte(BP_filterName),0B]
    if (n_elements(BP_filterSize)        ne 0) then tp.BP_filterSize = BP_filterSize
    if (n_elements(RiemannInterpolation) ne 0) then tp.RiemannInterpolation = RiemannInterpolation
    if (n_elements(RadonInterpolation)   ne 0) then tp.RadonInterpolation = RadonInterpolation
end

