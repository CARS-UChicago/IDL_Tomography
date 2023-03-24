dest = '/usr/local/idl_user/tomography/docs/source/_static/IDL_help/'
mk_html_help, ['backproject.pro', $
               'gridrec.pro', $
               'read_tomo_volume.pro', $
               'remove_tomo_artifacts.pro', $
               'sinogram.pro', $
               'tomo__define.pro', $
               'tomo_filter.pro', $
               'tomo_recon_netcdf.pro', $
               'write_tomo_volume.pro'], $
              dest+'tomography_routines.html', $
    title = 'CARS Tomography Routines'
end
