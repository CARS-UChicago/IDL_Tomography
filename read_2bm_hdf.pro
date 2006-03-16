pro read_2bm_hdf, file, data
   sd_id=HDF_SD_START(file)
   sds_idx=HDF_SD_NAMETOINDEX(sd_id, 'data')
   sds_id=HDF_SD_SELECT(sd_id, sds_idx)
   HDF_SD_GETDATA,sds_id,data
   HDF_SD_ENDACCESS,sds_id
   HDF_SD_END,sd_id
end
