function candels_fir_counterparts, raj2000, decj2000, mips24_unc, pacs100_unc, pacs160_unc, spire250_unc, $
                                   catalogue_match_radius=catalogue_match_radius, files_dir=files_dir, $
                                   counterpart_match_radius=counterpart_match_radius, frac_total_flux=frac_total_flux
;+
; Name
; ----
;   CANDELS_FIR_COUNTERPARTS
;
; Purpose
; -------
;   Calculated the number for FIR counterparts (PACS 100 and 160 um, and SPIRE 250 um)
;   associated with each HST/WFC/F160W source using their corresponding MIPS 24um flux.
;   The number of counterparts is determined as MIPS 24um sources within the optionally
;   input counterpart match radius. Additionally, the total fraction the source
;   contributes to the total MIPS flux within the counterpart match radius can be output.
;
; Calling Sequence
; ----------------
;   ::
;
;       ncounterparts = candels_fir_counterparts(raj2000, decj2000, mips24_unc, pacs100_unc, $
;                                                pacs160_unc, spire250_unc [, catalogue_match_radius = , $
;                                                counterpart_match_radius = , files_dir = , $
;                                                frac_total_flux=frac_total_flux])
;
; Inputs
; ------
;   ``raj2000`` : int, float, or double array(Nsources)
;       The Right Ascension of each source in J2000 [degrees].
;   ``decj2000`` : int, float, or double array(Nsources)
;       The Declination of each source in J2000 [degrees].
;   ``mips24_unc`` : int, float, or double array(Nsources)
;       The MIPS 24um uncertainty of each source.
;   ``pacs100_unc`` : int, float, or double array(Nsources)
;       The PACS 100um uncertainty of each source.
;   ``pacs160_unc`` : int, float, or double array(Nsources)
;       The PACS 160um uncertainty of each source.
;   ``spire250_unc`` : int, float, or double array(Nsources)
;       The SPIRE 250um uncertainty of each source.
;
; Optional Inputs
; ---------------
;   ``catalogue_match_radius`` : int, float, or double scalar
;       The maximum radius at which to match sources between the inputs and
;       the catalogues [arcseconds]. (Default = ``0.5``)
;   ``counterpart_match_radius`` : int, float, or double array(3)
;       The maximum search radius at which sources will be considered counterparts
;       to the input source for the PACS100, PACS160, and SPIRE250 bands,
;       respectively [arcseconds]. (Default = ``[7, 11.2, 18]``)
;   ``files_dir`` : string scalar
;       The path to the CANDELS photometry and Barro+2019 MIPS flag catalogues
;       (see note below). (Default = Current working directory)
;
; Output
; ------
;   ``ncounterparts`` : int array(3, Nsources)
;       The number of sources within the counterpart match radius for the PACS100,
;       PACS160, and SPIRE250 bands, respectively. A value of -1 means that there
;       is either no MIPS 24um or no respective bandpass detection for the source.
;       A value of 1 means the source is the only MIPS 24um source within the
;       counterpart match radius for the respective band. A value > 1 means there
;       are that many minus 1 additional sources within the counterpart match radius
;       for the respective band.
;
; Optional Output
; ---------------
;   ``frac_total_flux`` : double array(3, Nsources)
;       The total fraction the source contributes to the total MIPS flux that is 
;       within the counterpart match radius for the PACS100, PACS160, and SPIRE250 
;       bands, respectively. A value of -1 means that there is either no MIPS 24um 
;       or no respective bandpass detection for the source.
;
; Note
; ----
;   Requires the CANDELS photometry and Barro+2019 MIPS flag catalogues. They can be
;   downloaded from:
;      GOODS-N: https://archive.stsci.edu/missions/hlsp/candels/goods-n/catalogs/v1/
;              (hlsp_candels_hst_wfc3_goodsn-barro19_multi_v1-1_photometry-cat.fits and
;               hlsp_candels_hst_wfc3_goodsn-barro19_multi_v1_sfr-flag-mips-gdn-cat.fits)
;      GOODS-S: https://archive.stsci.edu/missions/hlsp/candels/goods-s/catalogs/v1/
;              (hlsp_candels_hst_wfc3_goodss-tot-multiband_f160w_v1_cat.fits and
;               hlsp_candels_hst_wfc3_goodss-barro19_multi_v1_sfr-flag-mips-gds-cat.fits)
;      COSMOS: https://archive.stsci.edu/missions/hlsp/candels/cosmos/catalogs/v1/
;              (hlsp_candels_hst_wfc3_cos-tot-multiband_f160w_v1-1photom_cat.fits and
;               hlsp_candels_hst_wfc3_cosmos-barro19_multi_v1_sfr-flag-mips-cos-cat.fits)
;      EGS: https://archive.stsci.edu/missions/hlsp/candels/egs/catalogs/v1/
;              (hlsp_candels_hst_wfc3_egs-tot-multiband_f160w_v1-1photom_cat.fits and 
;               hlsp_candels_hst_wfc3_egs-barro19_multi_v1_sfr-flag-mips-egs-cat.fits)
;      UDS: https://archive.stsci.edu/missions/hlsp/candels/uds/catalogs/v1/
;              (hlsp_candels_hst_wfc3_uds-tot-multiband_f160w_v1_cat.fits and 
;               hlsp_candels_hst_wfc3_uds-barro19_multi_v1_sfr-flag-mips-uds-cat.fits)
;   Each file must then be gunzipped (.gz) and saved to a common directory.
;
; Modification History
; --------------------
;   - 2022/08/18: Created (Keith Doore)
;-

; Error Handling
 if n_elements(raj2000) eq 0 then message, 'Variable is undefined: RAJ2000.'
 if size(raj2000, /type) lt 2 or size(raj2000, /type) gt 5 then $
   message, 'RAJ2000 is not of type int, float, or double.'
 if size(reform(raj2000), /n_dim) ne 1 then $
   message, 'RAJ2000 must be a scalar or 1-D array.'
 if max(raj2000) ge 360 or min(raj2000) lt 0 then $
   message, 'RAJ2000 must only contain values between 0 and 360.'
 Nsources = n_elements(raj2000)

 if n_elements(decj2000) eq 0 then message, 'Variable is undefined: DECJ2000.'
 if size(decj2000, /type) lt 2 or size(decj2000, /type) gt 5 then $
   message, 'DECJ2000 is not of type int, float, or double.'
 if size(reform(decj2000), /n_dim) ne 1 then $
   message, 'DECJ2000 must be a scalar or 1-D array.'
 if max(decj2000) gt 90 or min(decj2000) lt -90 then $
   message, 'DECJ2000 must only contain values between -90 and 90.'
 if n_elements(decj2000) ne Nsources then $
   message, 'DECJ2000 must have Nsources elements.'
 
 if n_elements(mips24_unc) eq 0 then message, 'Variable is undefined: MIPS24_UNC.'
 if size(mips24_unc, /type) lt 2 or size(mips24_unc, /type) gt 5 then $
   message, 'MIPS24_UNC is not of type int, float, or double.'
 if size(reform(mips24_unc), /n_dim) ne 1 then $
   message, 'MIPS24_UNC must be a scalar or 1-D array.'
 if n_elements(mips24_unc) ne Nsources then $
   message, 'MIPS24_UNC must have Nsources elements.'

 if n_elements(pacs100_unc) eq 0 then message, 'Variable is undefined: PACS100_UNC.'
 if size(pacs100_unc, /type) lt 2 or size(pacs100_unc, /type) gt 5 then $
   message, 'PACS100_UNC is not of type int, float, or double.'
 if size(reform(pacs100_unc), /n_dim) ne 1 then $
   message, 'PACS100_UNC must be a scalar or 1-D array.'
 if n_elements(pacs100_unc) ne Nsources then $
   message, 'PACS100_UNC must have Nsources elements.'

 if n_elements(pacs160_unc) eq 0 then message, 'Variable is undefined: PACS160_UNC.'
 if size(pacs160_unc, /type) lt 2 or size(pacs160_unc, /type) gt 5 then $
   message, 'PACS160_UNC is not of type int, float, or double.'
 if size(reform(pacs160_unc), /n_dim) ne 1 then $
   message, 'PACS160_UNC must be a scalar or 1-D array.'
 if n_elements(pacs160_unc) ne Nsources then $
   message, 'PACS160_UNC must have Nsources elements.'

 if n_elements(spire250_unc) eq 0 then message, 'Variable is undefined: SPIRE250_UNC.'
 if size(spire250_unc, /type) lt 2 or size(spire250_unc, /type) gt 5 then $
   message, 'SPIRE250_UNC is not of type int, float, or double.'
 if size(reform(spire250_unc), /n_dim) ne 1 then $
   message, 'SPIRE250_UNC must be a scalar or 1-D array.'
 if n_elements(spire250_unc) ne Nsources then $
   message, 'SPIRE250_UNC must have Nsources elements.'

 if n_elements(catalogue_match_radius) ne 0 then begin
   if size(catalogue_match_radius, /type) lt 2 or size(catalogue_match_radius, /type) gt 5 then $
     message, 'CATALOGUE_MATCH_RADIUS is not of type int, float, or double.'
   if size(catalogue_match_radius, /n_dim) ne 0 then $
     message, 'CATALOGUE_MATCH_RADIUS must be a scalar.'
   if catalogue_match_radius le 0 then $
     message, 'CATALOGUE_MATCH_RADIUS must be a positive value.'
 endif else catalogue_match_radius = 0.5

 if n_elements(counterpart_match_radius) ne 0 then begin
   if size(counterpart_match_radius, /type) lt 2 or size(counterpart_match_radius, /type) gt 5 then $
     message, 'COUNTERPART_MATCH_RADIUS is not of type int, float, or double.'
   if size(counterpart_match_radius, /n_dim) ne 1 then $
     message, 'COUNTERPART_MATCH_RADIUS must be a 1-D array.'
   if n_elements(counterpart_match_radius) ne 3 then $
     message, 'COUNTERPART_MATCH_RADIUS must only have three elements.'
   if min(counterpart_match_radius) le 0 then $
     message, 'COUNTERPART_MATCH_RADIUS must be a positive value.'
 endif else counterpart_match_radius = [7.d, 11.2d, 18.d]

 if n_elements(files_dir) ne 0 then begin
   if size(files_dir, /type) ne 7 then $
     message, 'FILES_DIR is not of type string.'
   if size(files_dir, /n_dim) ne 0 then $
     message, 'FILES_DIR must be a scalar.'
   if ~file_test(files_dir, /directory) then $
     message, 'FILES_DIR is not a valid directory.'
 endif else begin
   cd, current=files_dir
   files_dir = files_dir+'/'
 endelse


; Combine all inputs into an array of structures for easy indexing
 input = {raj2000:0.d, decj2000:0.d, $
          mips24:0.d0, pacs100:0.d, $
          pacs160:0.d, spire250:0.d}
 input = replicate(input, Nsources)
 input.raj2000  = raj2000
 input.decj2000 = decj2000
 input.mips24   = mips24_unc
 input.pacs100  = pacs100_unc
 input.pacs160  = pacs160_unc
 input.spire250 = spire250_unc


; Determine approximately what field the galaxies are located
 ; Center location of each field
 goodsn = [189.228621, 62.238572]
 goodss = [53.122751, -27.805089]
 cosmos = [150.116321, 2.2009731]
 egs = [214.825000, 52.825000]
 uds = [34.406250, -5.2000000]

 ; All fields are smaller than +/- 1 deg from the center
 field = replicate('none', Nsources)
 field[where(input.raj2000 gt goodsn[0]-1 and input.raj2000 lt goodsn[0]+1 and $
             input.decj2000 gt goodsn[1]-1 and input.decj2000 lt goodsn[1]+1, /null)] = 'goodsn'
 field[where(input.raj2000 gt goodss[0]-1 and input.raj2000 lt goodss[0]+1 and $
             input.decj2000 gt goodss[1]-1 and input.decj2000 lt goodss[1]+1, /null)] = 'goodss'
 field[where(input.raj2000 gt cosmos[0]-1 and input.raj2000 lt cosmos[0]+1 and $
             input.decj2000 gt cosmos[1]-1 and input.decj2000 lt cosmos[1]+1, /null)] = 'cosmos'
 field[where(input.raj2000 gt egs[0]-1 and input.raj2000 lt egs[0]+1 and $
             input.decj2000 gt egs[1]-1 and input.decj2000 lt egs[1]+1, /null)] = 'egs'
 field[where(input.raj2000 gt uds[0]-1 and input.raj2000 lt uds[0]+1 and $
             input.decj2000 gt uds[1]-1 and input.decj2000 lt uds[1]+1, /null)] = 'uds'
 goodsn_idc = where(field eq 'goodsn', /null)
 goodss_idc = where(field eq 'goodss', /null)
 cosmos_idc = where(field eq 'cosmos', /null)
 egs_idc = where(field eq 'egs', /null)
 uds_idc = where(field eq 'uds', /null)
 none_idc = where(field eq 'none', comp=candels_idc, /null)
 input_candels = input[candels_idc]
 field_candels = field[candels_idc]



; Read in GOODS catalogue data
 photo_n=mrdfits(files_dir+'hlsp_candels_hst_wfc3_goodsn-barro19_multi_v1-1_photometry-cat.fits.gz',1)
 photo_s=mrdfits(files_dir+'hlsp_candels_hst_wfc3_goodss-tot-multiband_f160w_v1_cat.fits.gz',1)
 photo_c=mrdfits(files_dir+'hlsp_candels_hst_wfc3_cos-tot-multiband_f160w_v1-1photom_cat.fits.gz',1)
 photo_e=mrdfits(files_dir+'hlsp_candels_hst_wfc3_egs-tot-multiband_f160w_v1-1photom_cat.fits.gz',1)
 photo_u=mrdfits(files_dir+'hlsp_candels_hst_wfc3_uds-tot-multiband_f160w_v1_cat.fits.gz',1)

 mips_n= mrdfits(files_dir+'hlsp_candels_hst_wfc3_goodsn-barro19_multi_v1_sfr-flag-mips-gdn-cat.fits.gz',1)
 mips_s= mrdfits(files_dir+'hlsp_candels_hst_wfc3_goodss-barro19_multi_v1_sfr-flag-mips-gds-cat.fits.gz',1)
 mips_c= mrdfits(files_dir+'hlsp_candels_hst_wfc3_cosmos-barro19_multi_v1_sfr-flag-mips-cos-cat.fits.gz',1)
 mips_e= mrdfits(files_dir+'hlsp_candels_hst_wfc3_egs-barro19_multi_v1_sfr-flag-mips-egs-cat.fits.gz',1)
 mips_u= mrdfits(files_dir+'hlsp_candels_hst_wfc3_uds-barro19_multi_v1_sfr-flag-mips-uds-cat.fits.gz',1)


; Match input sources to catalogue sources
 loc = !null
 angle = !null
 for i=0, n_elements(input_candels)-1 do begin
   case field_candels[i] of
     'goodsn': gcirc, 2, input_candels[i].RAJ2000, input_candels[i].DECJ2000, photo_n.ra, photo_n.dec, dis
     'goodss': gcirc, 2, input_candels[i].RAJ2000, input_candels[i].DECJ2000, photo_s.ra, photo_s.dec, dis
     'cosmos': gcirc, 2, input_candels[i].RAJ2000, input_candels[i].DECJ2000, photo_c.ra, photo_c.dec, dis
     'egs': gcirc, 2, input_candels[i].RAJ2000, input_candels[i].DECJ2000, photo_e.ra, photo_e.dec, dis
     'uds': gcirc, 2, input_candels[i].RAJ2000, input_candels[i].DECJ2000, photo_u.ra, photo_u.dec, dis
   endcase
   close_match = where(dis le catalogue_match_radius, no, /null)
   if no eq 1 then begin $
     loc = [loc, close_match]
     angle = [angle, dis[close_match]]
   endif
   if no ne 1 then begin
     print,'Number of matches for galaxy '+string(i, f='(I0)')+': '+string(no, f='(I0)')
   endif
 endfor
 mips  = [mips_n[loc[goodsn_idc]], mips_s[loc[goodss_idc]], mips_c[loc[cosmos_idc]], mips_e[loc[egs_idc]], mips_u[loc[uds_idc]]]
 ; Reorder to match input_candels ordering
 mips  = mips[sort([goodsn_idc, goodss_idc, cosmos_idc, egs_idc, uds_idc])]  
 

; Counterpart numbers and fraction of flux
 pacs100_psf = counterpart_match_radius[0]
 pacs160_psf = counterpart_match_radius[1]
 spire250_psf = counterpart_match_radius[2]
 psf = [pacs100_psf, pacs160_psf, spire250_psf]

 frac_total_flux_candels = replicate(1.0d, 3, n_elements(input_candels))
 ncounterparts_candels = replicate(0, 3, n_elements(input_candels))

 for i=0, n_elements(input_candels)-1 do begin
   case field_candels[i] of
     'goodsn': gcirc, 2, input_candels[i].RAJ2000, input_candels[i].DECJ2000, photo_n.ra, photo_n.dec, dis
     'goodss': gcirc, 2, input_candels[i].RAJ2000, input_candels[i].DECJ2000, photo_s.ra, photo_s.dec, dis
     'cosmos': gcirc, 2, input_candels[i].RAJ2000, input_candels[i].DECJ2000, photo_c.ra, photo_c.dec, dis
     'egs': gcirc, 2, input_candels[i].RAJ2000, input_candels[i].DECJ2000, photo_e.ra, photo_e.dec, dis
     'uds': gcirc, 2, input_candels[i].RAJ2000, input_candels[i].DECJ2000, photo_u.ra, photo_u.dec, dis
   endcase
   for j=0, n_elements(psf)-1 do begin
     in_psf = where(dis le psf[j], num, /null)
     case field_candels[i] of
       'goodsn': mips_flux = mips_n[in_psf].mips_FMIPS24
       'goodss': mips_flux = mips_s[in_psf].mips_FMIPS24
       'cosmos': mips_flux = mips_c[in_psf].mips_FMIPS24
       'egs': mips_flux = mips_e[in_psf].mips_FMIPS24
       'uds': mips_flux = mips_u[in_psf].mips_FMIPS24
     endcase
     mips_flux[where(mips_flux eq '--', /null)] = '0.0'
     mips_flux = float(mips_flux)
     mips_flux[where(mips_flux lt 0,/null)] = 0.0
     sort_flux = mips_flux[uniq(mips_flux, sort(mips_flux))]
     source_mips = mips[i].mips_FMIPS24
     if source_mips eq '--' then source_mips = '0.0'
     source_mips = float(source_mips)
     if source_mips lt 0 then source_mips = 0.0
     if n_elements(sort_flux) gt 1 then frac_total_flux_candels[j, i] = source_mips/total(sort_flux)
     ncounterparts_candels[j, i] = n_elements(where(sort_flux ne 0, /null))
   endfor
 endfor

 frac_total_flux_candels[0, where(input_candels.pacs100 le 0)] = -1
 frac_total_flux_candels[1, where(input_candels.pacs160 le 0)] = -1
 frac_total_flux_candels[2, where(input_candels.spire250 le 0)] = -1
 frac_total_flux_candels[*, where(input_candels.mips24 le 0)] = -1
 ncounterparts_candels[0, where(input_candels.pacs100 le 0)] = -1
 ncounterparts_candels[1, where(input_candels.pacs160 le 0)] = -1
 ncounterparts_candels[2, where(input_candels.spire250 le 0)] = -1
 ncounterparts_candels[*, where(input_candels.mips24 le 0)] = -1

 frac_total_flux = replicate(-1.0d, 3, Nsources)
 ncounterparts = replicate(-1, 3, Nsources)
 frac_total_flux[*, candels_idc] = frac_total_flux_candels
 ncounterparts[*, candels_idc] = ncounterparts_candels

 return, ncounterparts

end