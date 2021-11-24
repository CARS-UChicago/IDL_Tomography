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

function tomo_params_init, volume, dimensions=dimensions, $
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

    tp = {tomo_params}
    if (n_elements(volume) ne 0) then begin
        tomo_params_set_dimensions, tp, volume
    endif

    if (n_elements(dimensions) ne 0) then begin
        tp.numPixels      = dimensions[0]
        tp.numSlices      = dimensions[1]
        tp.numProjections = dimensions[2]
    endif

    ; GSECARS normalized .volume files are 16-bit integers with I/IO*10000.
    ; If air normalization is not done then divide by 10000.
    if (n_elements(sinoScale) eq 0) then sinoScale = 10000.
    tp.sinoScale = sinoScale

    ; GSECARS recon.volume files are 16-bit integers.
    ; The voxel values are multiplied typically by 1e6 to convert to integers.
    if (n_elements(reconScale) eq 0) then reconScale = 1.e6
    tp.reconScale = reconScale

    ; There are 2 "special" values of paddedSinogramWidth
    ; 0 = automatically set the width to power of 2 that is >= actual width
    ; 1 = No padding, set the width to the actual width
    if (n_elements(paddedSinogramWidth) eq 0) then paddedSinogramWidth = 0
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
    
    if (n_elements(paddingAverage) eq 0) then paddingAverage = 10
    tp.paddingAverage = paddingAverage

    if (n_elements(airPixels) eq 0) then airPixels = 10
    tp.airPixels = airPixels

    if (n_elements(ringWidth) eq 0) then ringWidth = 9
    tp.ringWidth = ringWidth

    if (n_elements(fluorescence) eq 0) then fluorescence = 0
    tp.fluorescence = fluorescence

    tp.reconMethodTomoRecon   = 0
    tp.reconMethodGridrec     = 1
    tp.reconMethodBackproject = 2
    if (n_elements(reconMethod) eq 0) then reconMethod = tp.reconMethodTomoRecon
    tp.reconMethod = reconMethod

    if (n_elements(numThreads) eq 0) then numThreads = 8
    tp.numThreads = numThreads

    if (n_elements(slicesPerChunk) eq 0) then slicesPerChunk = 128
    tp.slicesPerChunk = slicesPerChunk
    
    if (n_elements(debug) eq 0) then debug = 0
    tp.debug = debug

    if (n_elements(dbgFile) eq 0) then dbgFile = ""
    tp.debugFile = [byte(dbgFile), 0B]
    
    if (n_elements(geom) eq 0) then geom=0
    tp.geom = geom

    if (n_elements(pswfParam) eq 0) then pswfParam = 6.0
    tp.pswfParam = pswfParam

    if (n_elements(sampl) eq 0) then sampl = 1.0
    tp.sampl = sampl
    
    if (n_elements(maxPixSize) eq 0) then maxPixSize = 1.0
    tp.maxPixSize = maxPixSize

    if (n_elements(ROI) eq 0) then ROI = 1.0
    tp.ROI = ROI

    if (n_elements(X0) eq 0) then X0 = 0.
    tp.X0 = X0

    if (n_elements(Y0) eq 0) then Y0 = 0.
    tp.Y0 = Y0
    
    if (n_elements(ltbl) eq 0) then ltbl=512
    tp.ltbl = ltbl
    
    if (n_elements(ltbl) eq 0) then ltbl=512
    tp.ltbl = ltbl

    if (n_elements(GR_filterName) eq 0) then GR_filterName="shepp"
    tp.GR_filterName = [byte(GR_filterName),0B]
    
    tp.BP_MethodRiemann = 0
    tp.BP_methodRadon = 1
    if (n_elements(BP_method) eq 0) then BP_method = tp.BP_methodRiemann
    tp.BP_method = BP_method

    if (n_elements(BP_filterName) eq 0) then BP_filterName="SHEPP_LOGAN"
    tp.BP_filterName = [byte(BP_filterName),0B]

    if (n_elements(BP_filterSize) eq 0) then BP_filterSize = tp.numPixels/4
    tp.BP_filterSize = BP_filterSize
    
    tp.RiemannInterpolationNone     = 0
    tp.RiemannInterpolationBilinear = 1
    tp.RiemannInterpolationCubic    = 2
    if (n_elements(RiemannInterpolation) eq 0) then RiemannInterpolation = tp.RiemannInterpolationNone
    tp.RiemannInterpolation = RiemannInterpolation

    tp.RadonInterpolationNone     = 0
    tp.RadonInterpolationLinear   = 1
    if (n_elements(RadonInterpolation) eq 0) then RadonInterpolation = tp.RadonInterpolationNone
    tp.RadonInterpolation = RadonInterpolation

    return, tp
end

