pro candels_postage_stamps, ra, dec, redshift, outfile_loc, img_file_loc,$
                            phys_size=phys_size, name=name
;+
; NAME:
;	CANDELS_POSTAGE_STAMPS
; PURPOSE:
;	To create HST/ACS color postage stamp images of galaxies in the CANDELS fields
; EXPLANATION: 
;   Uses input Right Ascension (ra), Declination (dec), and redshift to create
;    a color postage stamp image of a galaxy in the CANDELS fields. The field is 
;    identified from the ra and dec. Multiple input locations can be 
;    supplied and each images will be consecutively created. The resulting
;    postage stamps are saved as a square RGB matrix (3xNxN) to the output directory
;    as FITS files with the name of 'JXXXXXX.XX+XXXXXX.X_postagestamp', where 
;    JXXXXXX.XX+XXXXXX.X is the J2000 name. The color images use the HST/ACS F814W
;    as red, F606W as green, and F435W as blue, for the GOODS fields. For the COSMOS,
;    EGS, and UDS fields, only F814W and F606W data available. Therefore, the average
;    of the two is used for green, and F606W is used for blue. If a source is not within
;    a field, a warning will be printed for the source.
;
;    Requires the HST/ACS section drizzled images to be downloaded from:
;       GOODS-N: https://archive.stsci.edu/missions/hlsp/goods/v2/
;               (e.g., h_nz_sect13_v2.0_drz_img.fits, h_nv_sect13_v2.0_drz_img.fits and
;                      h_nb_sect13_v2.0_drz_img.fits for section 13)
;       GOODS-S: https://archive.stsci.edu/missions/hlsp/goods/v2/
;               (e.g., h_sz_sect11_v2.0_drz_img.fits, h_sv_sect11_v2.0_drz_img.fits and
;                      h_sb_sect11_v2.0_drz_img.fits for section 11)
;       COSMOS: https://archive.stsci.edu/missions/hlsp/candels/cosmos/cos-tot/v1.0/
;               (e.g., hlsp_candels_hst_acs_cos-tot-sect11_f606w_v1.0_drz.fits and 
;                      hlsp_candels_hst_acs_cos-tot-sect11_f814w_v1.0_drz.fits for section 11)
;       EGS: help,
;               (e.g., hlsp_candels_hst_acs_egs-tot-30mas-section11_f606w_v1.0_drz.fits and 
;                      hlsp_candels_hst_acs_egs-tot-30mas-section11_f814w_v1.0_drz.fits for section 11)
;       UDS: https://archive.stsci.edu/missions/hlsp/candels/uds/uds-tot/v1.0/
;               (e.g., hlsp_candels_hst_acs_uds-tot-sect11_f606w_v1.0_drz.fits and 
;                      hlsp_candels_hst_acs_uds-tot-sect11_f814w_v1.0_drz.fits for section 11)
;    Each file must then be gunzipped (.gz) and saved in a unique directory for each field.
;
; CALLING SEQUENCE:
;	candels_postage_stamps, ra, dec, redshift, outfile_loc, img_file_loc, $
;      [PHYS_SIZE=PHYS_SIZE]
;
; INPUTS:
;   ra           - a vector of length M containing the Right Ascension of each galaxy
;   dec          - a vector of length M containing the Declination of each galaxy
;   redshift     - a vector of length M containing the redshift of each galaxy
;   outfile_loc  - a string containing the path of where to save the output FITS files
;   img_file_loc - a 5 element string array containing the path to each of the HST/ACS 
;                  section drizzled images in index order of 0) GOODS-N, 1) GOODS-S
;                  2) COSMOS, 3) EGS, 4) UDS
;
; OPTIONAL INPUTS:
;   PHYS_SIZE    - a scalar containing the assumed physical radius of the galaxies in kpc
;                    (Default = 15kpc)
;   NAME         - a string vector of length M containing the name of each galaxy to be used
;                    for naming the output files. If not specified, then the name will be the
;                    J2000 name generated from the RA and DEC 
;
; NOTES:
;   Requires the IDL Astronomy User's Library and Red IDL cosmology package,
;     which can be downloaded from
;       IDL Astronomy User's Library: https://idlastro.gsfc.nasa.gov
;       Red IDL cosmology package:    https://github.com/jlfischer/red-idl-cosmology
;
; REVISON HISTORY:
;	Written by K. Doore, 9/16/2021
;   Revised by K. Doore, 9/23/2021
;     - Allowed for the input of user specified names
;   Revised by K. Doore, 4/6/2022
;     - Included header in output file
;-
 compile_opt idl2
 On_error,2

; Compile the IDL cosmology package
 red


; Check for allowable type and size of inputs
;ra, dec, redshift, outfile_loc, img_file_loc
 if size(ra, /type) lt 2 or size(ra, /type) gt 5 then begin
   print, 'Right Ascension is incorrect data type'
   return
 endif 
 if size(ra, /n_dim) ne 1 then begin
   print, 'Right Ascension must be a vector'
   return
 endif 
 if size(dec, /type) lt 2 or size(dec, /type) gt 5 then begin
   print, 'Declination is incorrect data type'
   return
 endif 
 if size(dec, /n_dim) ne 1 then begin
   print, 'Declination must be a vector'
   return
 endif 
 if size(redshift, /type) lt 2 or size(redshift, /type) gt 5 then begin
   print, 'Redshift is incorrect data type'
   return
 endif
 if size(redshift, /n_dim) ne 1 then begin
   print, 'Redshift must be a vector'
   return
 endif
 if n_elements(where(redshift le 0, /null)) ne 0 then begin
   print, 'Redshift must be a positive non-zero value'
   return
 endif
 if n_elements(ra) ne n_elements(dec) or n_elements(ra) ne n_elements(redshift) then begin
   print, 'Right Ascension, Declination, and Redshift must be the same length'
   return
 endif
 if size(outfile_loc,/type) ne 7 then begin
   print, 'outfile_loc is not of type string'
   return
 endif
 if size(img_file_loc,/type) ne 7 then begin
   print, 'img_file_loc is not of type string'
   return
 endif 
 if n_elements(img_file_loc) ne 5 then begin
   print, 'img_file_loc must be a 5 element string vector'
   return
 endif
 if n_elements(phys_size) gt 0 then begin
  if size(phys_size, /type) lt 2 or size(phys_size, /type) gt 5 then begin
    print, 'phys_size is incorrect data type'
    return
  endif
  if n_elements(phys_size) ne 1 then begin
    print, 'phys_size must be a scalar'
    return
  endif
  if phys_size le 0 then begin
    print, 'phys_size must be a positive non-zero value'
    return
  endif
 endif else begin
   phys_size = 15.
 endelse
 if n_elements(name) gt 0 then begin
  if size(name,/type) ne 7 then begin
    print, 'Galaxy names are not of type string'
    return
  endif
  if size(name, /n_dim) ne 1 then begin
    print, 'Galaxy names must be a vector'
    return
  endif
  if n_elements(name) ne n_elements(ra) then begin
    print, 'Galaxy name vector must be the same length as the Right Ascension, Declination, and Redshift'
    return
  endif
 endif else begin
   ; Generate the J2000 name for each galaxy
   ra_hh = floor(ra/15.)
   ra_mm = floor(4.*ra-60.*ra_hh)
   ra_ss =  3600.d0*ra/15.d0-3600.d0*ra_hh-60.d0*ra_mm
   rastr = string(ra_hh, format='(I02)') + string(ra_mm, format='(I02)') + string(ra_ss, format='(F05.2)')
   
   sign = replicate('+',n_elements(ra))
   sign[where(dec lt 0,/null)] = '-'
   dec_dd = floor(abs(dec))
   dec_mm = floor(60.*(abs(dec)-dec_dd))
   dec_ss = 3600.d0*abs(dec)-3600.d0*dec_dd-60.d0*dec_mm
   decstr = sign+string(dec_dd, format='(I02)')+string(dec_mm, format='(I02)')+string(dec_ss, format='(F04.1)')
   name = 'J' + rastr + decstr
 endelse


; Determine approximately what field the galaxies are located
; Center location of each field
 goodsn = [189.228621, 62.238572]
 goodss = [53.122751, -27.805089]
 cosmos = [150.116321, 2.2009731]
 egs = [214.825000, 52.825000]
 uds = [34.406250, -5.2000000]


; All fields are smaller than +/- 1 deg from the center
 field = replicate('none',n_elements(ra))
 field[where(ra gt goodsn[0]-1 and ra lt goodsn[0]+1 and dec gt goodsn[1]-1 and dec lt goodsn[1]+1, /null)] = 'goodsn'
 field[where(ra gt goodss[0]-1 and ra lt goodss[0]+1 and dec gt goodss[1]-1 and dec lt goodss[1]+1, /null)] = 'goodss'
 field[where(ra gt cosmos[0]-1 and ra lt cosmos[0]+1 and dec gt cosmos[1]-1 and dec lt cosmos[1]+1, /null)] = 'cosmos'
 field[where(ra gt egs[0]-1 and ra lt egs[0]+1 and dec gt egs[1]-1 and dec lt egs[1]+1, /null)] = 'egs'
 field[where(ra gt uds[0]-1 and ra lt uds[0]+1 and dec gt uds[1]-1 and dec lt uds[1]+1, /null)] = 'uds'


; Determine what section of each field the galaxies are located
; Section values of 0 indicate source is not within the image region
 section = intarr(n_elements(ra))
 if n_elements(where(field eq 'goodsn', /null)) gt 0 then begin
   ; Read in astrometry
   extast, headfits(img_file_loc[0]+'h_nv_sect13_v2.0_drz_img.fits.gz', /silent), astr13 
   extast, headfits(img_file_loc[0]+'h_nv_sect14_v2.0_drz_img.fits.gz', /silent), astr14 
   extast, headfits(img_file_loc[0]+'h_nv_sect22_v2.0_drz_img.fits.gz', /silent), astr22 
   extast, headfits(img_file_loc[0]+'h_nv_sect23_v2.0_drz_img.fits.gz', /silent), astr23 
   extast, headfits(img_file_loc[0]+'h_nv_sect24_v2.0_drz_img.fits.gz', /silent), astr24 
   extast, headfits(img_file_loc[0]+'h_nv_sect25_v2.0_drz_img.fits.gz', /silent), astr25 
   extast, headfits(img_file_loc[0]+'h_nv_sect31_v2.0_drz_img.fits.gz', /silent), astr31 
   extast, headfits(img_file_loc[0]+'h_nv_sect32_v2.0_drz_img.fits.gz', /silent), astr32 
   extast, headfits(img_file_loc[0]+'h_nv_sect33_v2.0_drz_img.fits.gz', /silent), astr33 
   extast, headfits(img_file_loc[0]+'h_nv_sect34_v2.0_drz_img.fits.gz', /silent), astr34 
   extast, headfits(img_file_loc[0]+'h_nv_sect35_v2.0_drz_img.fits.gz', /silent), astr35 
   extast, headfits(img_file_loc[0]+'h_nv_sect41_v2.0_drz_img.fits.gz', /silent), astr41 
   extast, headfits(img_file_loc[0]+'h_nv_sect42_v2.0_drz_img.fits.gz', /silent), astr42 
   extast, headfits(img_file_loc[0]+'h_nv_sect43_v2.0_drz_img.fits.gz', /silent), astr43 
   extast, headfits(img_file_loc[0]+'h_nv_sect44_v2.0_drz_img.fits.gz', /silent), astr44 
   extast, headfits(img_file_loc[0]+'h_nv_sect52_v2.0_drz_img.fits.gz', /silent), astr52 
   extast, headfits(img_file_loc[0]+'h_nv_sect53_v2.0_drz_img.fits.gz', /silent), astr53

   ; All images are perfect cuts, meaning no blank overlapping regions between sections
   ; Check the pixel location of each ra and dec, and if it is between 0 and astrX.NAXIS,
   ;    then it is in that section
   ad2xy, ra, dec, astr13, x13, y13
   ad2xy, ra, dec, astr14, x14, y14
   ad2xy, ra, dec, astr22, x22, y22
   ad2xy, ra, dec, astr23, x23, y23
   ad2xy, ra, dec, astr24, x24, y24
   ad2xy, ra, dec, astr25, x25, y25
   ad2xy, ra, dec, astr31, x31, y31
   ad2xy, ra, dec, astr32, x32, y32
   ad2xy, ra, dec, astr33, x33, y33
   ad2xy, ra, dec, astr34, x34, y34
   ad2xy, ra, dec, astr35, x35, y35
   ad2xy, ra, dec, astr41, x41, y41
   ad2xy, ra, dec, astr42, x42, y42
   ad2xy, ra, dec, astr43, x43, y43
   ad2xy, ra, dec, astr44, x44, y44
   ad2xy, ra, dec, astr52, x52, y52
   ad2xy, ra, dec, astr53, x53, y53

   section[where(field eq 'goodsn' and x13 ge 0 and x13 lt astr13.naxis[0] and y13 ge 0 and y13 lt astr13.naxis[1], /null)] = 13
   section[where(field eq 'goodsn' and x14 ge 0 and x14 lt astr14.naxis[0] and y14 ge 0 and y14 lt astr14.naxis[1], /null)] = 14
   section[where(field eq 'goodsn' and x22 ge 0 and x22 lt astr22.naxis[0] and y22 ge 0 and y22 lt astr22.naxis[1], /null)] = 22
   section[where(field eq 'goodsn' and x23 ge 0 and x23 lt astr23.naxis[0] and y23 ge 0 and y23 lt astr23.naxis[1], /null)] = 23
   section[where(field eq 'goodsn' and x24 ge 0 and x24 lt astr24.naxis[0] and y24 ge 0 and y24 lt astr24.naxis[1], /null)] = 24
   section[where(field eq 'goodsn' and x25 ge 0 and x25 lt astr25.naxis[0] and y25 ge 0 and y25 lt astr25.naxis[1], /null)] = 25
   section[where(field eq 'goodsn' and x31 ge 0 and x31 lt astr31.naxis[0] and y31 ge 0 and y31 lt astr31.naxis[1], /null)] = 31
   section[where(field eq 'goodsn' and x32 ge 0 and x32 lt astr32.naxis[0] and y32 ge 0 and y32 lt astr32.naxis[1], /null)] = 32
   section[where(field eq 'goodsn' and x33 ge 0 and x33 lt astr33.naxis[0] and y33 ge 0 and y33 lt astr33.naxis[1], /null)] = 33
   section[where(field eq 'goodsn' and x34 ge 0 and x34 lt astr34.naxis[0] and y34 ge 0 and y34 lt astr34.naxis[1], /null)] = 34
   section[where(field eq 'goodsn' and x35 ge 0 and x35 lt astr35.naxis[0] and y35 ge 0 and y35 lt astr35.naxis[1], /null)] = 35
   section[where(field eq 'goodsn' and x41 ge 0 and x41 lt astr41.naxis[0] and y41 ge 0 and y41 lt astr41.naxis[1], /null)] = 41
   section[where(field eq 'goodsn' and x42 ge 0 and x42 lt astr42.naxis[0] and y42 ge 0 and y42 lt astr42.naxis[1], /null)] = 42
   section[where(field eq 'goodsn' and x43 ge 0 and x43 lt astr43.naxis[0] and y43 ge 0 and y43 lt astr43.naxis[1], /null)] = 43
   section[where(field eq 'goodsn' and x44 ge 0 and x44 lt astr44.naxis[0] and y44 ge 0 and y44 lt astr44.naxis[1], /null)] = 44
   section[where(field eq 'goodsn' and x52 ge 0 and x52 lt astr52.naxis[0] and y52 ge 0 and y52 lt astr52.naxis[1], /null)] = 52
   section[where(field eq 'goodsn' and x53 ge 0 and x53 lt astr53.naxis[0] and y53 ge 0 and y53 lt astr53.naxis[1], /null)] = 53
 endif
 if n_elements(where(field eq 'goodss', /null)) gt 0 then begin
   ; Read in astrometry
   extast, headfits(img_file_loc[1]+'h_sv_sect11_v2.0_drz_img.fits.gz', /silent), astr11
   extast, headfits(img_file_loc[1]+'h_sv_sect12_v2.0_drz_img.fits.gz', /silent), astr12
   extast, headfits(img_file_loc[1]+'h_sv_sect13_v2.0_drz_img.fits.gz', /silent), astr13
   extast, headfits(img_file_loc[1]+'h_sv_sect14_v2.0_drz_img.fits.gz', /silent), astr14
   extast, headfits(img_file_loc[1]+'h_sv_sect21_v2.0_drz_img.fits.gz', /silent), astr21
   extast, headfits(img_file_loc[1]+'h_sv_sect22_v2.0_drz_img.fits.gz', /silent), astr22
   extast, headfits(img_file_loc[1]+'h_sv_sect23_v2.0_drz_img.fits.gz', /silent), astr23
   extast, headfits(img_file_loc[1]+'h_sv_sect24_v2.0_drz_img.fits.gz', /silent), astr24
   extast, headfits(img_file_loc[1]+'h_sv_sect25_v2.0_drz_img.fits.gz', /silent), astr25
   extast, headfits(img_file_loc[1]+'h_sv_sect31_v2.0_drz_img.fits.gz', /silent), astr31
   extast, headfits(img_file_loc[1]+'h_sv_sect32_v2.0_drz_img.fits.gz', /silent), astr32
   extast, headfits(img_file_loc[1]+'h_sv_sect33_v2.0_drz_img.fits.gz', /silent), astr33
   extast, headfits(img_file_loc[1]+'h_sv_sect34_v2.0_drz_img.fits.gz', /silent), astr34
   extast, headfits(img_file_loc[1]+'h_sv_sect35_v2.0_drz_img.fits.gz', /silent), astr35
   extast, headfits(img_file_loc[1]+'h_sv_sect42_v2.0_drz_img.fits.gz', /silent), astr42
   extast, headfits(img_file_loc[1]+'h_sv_sect43_v2.0_drz_img.fits.gz', /silent), astr43
   extast, headfits(img_file_loc[1]+'h_sv_sect44_v2.0_drz_img.fits.gz', /silent), astr44
   extast, headfits(img_file_loc[1]+'h_sv_sect45_v2.0_drz_img.fits.gz', /silent), astr45

   ; All images are perfect cuts, meaning no blank overlapping regions between sections
   ; Check the pixel location of each ra and dec, and if it is between 0 and astrX.NAXIS,
   ;    then it is in that section
   ad2xy, ra, dec, astr11, x11, y11
   ad2xy, ra, dec, astr12, x12, y12
   ad2xy, ra, dec, astr13, x13, y13
   ad2xy, ra, dec, astr14, x14, y14
   ad2xy, ra, dec, astr21, x21, y21
   ad2xy, ra, dec, astr22, x22, y22
   ad2xy, ra, dec, astr23, x23, y23
   ad2xy, ra, dec, astr24, x24, y24
   ad2xy, ra, dec, astr25, x25, y25
   ad2xy, ra, dec, astr31, x31, y31
   ad2xy, ra, dec, astr32, x32, y32
   ad2xy, ra, dec, astr33, x33, y33
   ad2xy, ra, dec, astr34, x34, y34
   ad2xy, ra, dec, astr35, x35, y35
   ad2xy, ra, dec, astr42, x42, y42
   ad2xy, ra, dec, astr43, x43, y43
   ad2xy, ra, dec, astr44, x44, y44
   ad2xy, ra, dec, astr45, x45, y45

   section[where(field eq 'goodss' and x11 ge 0 and x11 lt astr11.naxis[0] and y11 ge 0 and y11 lt astr11.naxis[1], /null)] = 11
   section[where(field eq 'goodss' and x12 ge 0 and x12 lt astr12.naxis[0] and y12 ge 0 and y12 lt astr12.naxis[1], /null)] = 12
   section[where(field eq 'goodss' and x13 ge 0 and x13 lt astr13.naxis[0] and y13 ge 0 and y13 lt astr13.naxis[1], /null)] = 13
   section[where(field eq 'goodss' and x14 ge 0 and x14 lt astr14.naxis[0] and y14 ge 0 and y14 lt astr14.naxis[1], /null)] = 14
   section[where(field eq 'goodss' and x21 ge 0 and x21 lt astr21.naxis[0] and y21 ge 0 and y21 lt astr21.naxis[1], /null)] = 21
   section[where(field eq 'goodss' and x22 ge 0 and x22 lt astr22.naxis[0] and y22 ge 0 and y22 lt astr22.naxis[1], /null)] = 22
   section[where(field eq 'goodss' and x23 ge 0 and x23 lt astr23.naxis[0] and y23 ge 0 and y23 lt astr23.naxis[1], /null)] = 23
   section[where(field eq 'goodss' and x24 ge 0 and x24 lt astr24.naxis[0] and y24 ge 0 and y24 lt astr24.naxis[1], /null)] = 24
   section[where(field eq 'goodss' and x25 ge 0 and x25 lt astr25.naxis[0] and y25 ge 0 and y25 lt astr25.naxis[1], /null)] = 25
   section[where(field eq 'goodss' and x31 ge 0 and x31 lt astr31.naxis[0] and y31 ge 0 and y31 lt astr31.naxis[1], /null)] = 31
   section[where(field eq 'goodss' and x32 ge 0 and x32 lt astr32.naxis[0] and y32 ge 0 and y32 lt astr32.naxis[1], /null)] = 32
   section[where(field eq 'goodss' and x33 ge 0 and x33 lt astr33.naxis[0] and y33 ge 0 and y33 lt astr33.naxis[1], /null)] = 33
   section[where(field eq 'goodss' and x34 ge 0 and x34 lt astr34.naxis[0] and y34 ge 0 and y34 lt astr34.naxis[1], /null)] = 34
   section[where(field eq 'goodss' and x35 ge 0 and x35 lt astr35.naxis[0] and y35 ge 0 and y35 lt astr35.naxis[1], /null)] = 35
   section[where(field eq 'goodss' and x42 ge 0 and x42 lt astr42.naxis[0] and y42 ge 0 and y42 lt astr42.naxis[1], /null)] = 42
   section[where(field eq 'goodss' and x43 ge 0 and x43 lt astr43.naxis[0] and y43 ge 0 and y43 lt astr43.naxis[1], /null)] = 43
   section[where(field eq 'goodss' and x44 ge 0 and x44 lt astr44.naxis[0] and y44 ge 0 and y44 lt astr44.naxis[1], /null)] = 44
   section[where(field eq 'goodss' and x45 ge 0 and x45 lt astr45.naxis[0] and y45 ge 0 and y45 lt astr45.naxis[1], /null)] = 45
 endif
 if n_elements(where(field eq 'cosmos', /null)) gt 0 then begin
   ; Read in astrometry
   extast, headfits(img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect11_f606w_v1.0_drz.fits.gz', /silent), astr11 
   extast, headfits(img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect12_f606w_v1.0_drz.fits.gz', /silent), astr12 
   extast, headfits(img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect13_f606w_v1.0_drz.fits.gz', /silent), astr13 
   extast, headfits(img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect14_f606w_v1.0_drz.fits.gz', /silent), astr14 
   extast, headfits(img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect21_f606w_v1.0_drz.fits.gz', /silent), astr21 
   extast, headfits(img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect22_f606w_v1.0_drz.fits.gz', /silent), astr22 
   extast, headfits(img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect23_f606w_v1.0_drz.fits.gz', /silent), astr23 
   extast, headfits(img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect24_f606w_v1.0_drz.fits.gz', /silent), astr24 
   extast, headfits(img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect25_f606w_v1.0_drz.fits.gz', /silent), astr25 

   ; All images are perfect cuts, meaning no blank overlapping regions between sections
   ; Check the pixel location of each ra and dec, and if it is between 0 and astrX.NAXIS,
   ;    then it is in that section
   ad2xy, ra, dec, astr11, x11, y11
   ad2xy, ra, dec, astr12, x12, y12
   ad2xy, ra, dec, astr13, x13, y13
   ad2xy, ra, dec, astr14, x14, y14
   ad2xy, ra, dec, astr21, x21, y21
   ad2xy, ra, dec, astr22, x22, y22
   ad2xy, ra, dec, astr23, x23, y23
   ad2xy, ra, dec, astr24, x24, y24
   ad2xy, ra, dec, astr25, x25, y25

   section[where(field eq 'cosmos' and x11 ge 0 and x11 lt astr11.naxis[0] and y11 ge 0 and y11 lt astr11.naxis[1], /null)] = 11
   section[where(field eq 'cosmos' and x12 ge 0 and x12 lt astr12.naxis[0] and y12 ge 0 and y12 lt astr12.naxis[1], /null)] = 12
   section[where(field eq 'cosmos' and x13 ge 0 and x13 lt astr13.naxis[0] and y13 ge 0 and y13 lt astr13.naxis[1], /null)] = 13
   section[where(field eq 'cosmos' and x14 ge 0 and x14 lt astr14.naxis[0] and y14 ge 0 and y14 lt astr14.naxis[1], /null)] = 14
   section[where(field eq 'cosmos' and x21 ge 0 and x21 lt astr21.naxis[0] and y21 ge 0 and y21 lt astr21.naxis[1], /null)] = 21
   section[where(field eq 'cosmos' and x22 ge 0 and x22 lt astr22.naxis[0] and y22 ge 0 and y22 lt astr22.naxis[1], /null)] = 22
   section[where(field eq 'cosmos' and x23 ge 0 and x23 lt astr23.naxis[0] and y23 ge 0 and y23 lt astr23.naxis[1], /null)] = 23
   section[where(field eq 'cosmos' and x24 ge 0 and x24 lt astr24.naxis[0] and y24 ge 0 and y24 lt astr24.naxis[1], /null)] = 24
   section[where(field eq 'cosmos' and x25 ge 0 and x25 lt astr25.naxis[0] and y25 ge 0 and y25 lt astr25.naxis[1], /null)] = 25
 endif
 if n_elements(where(field eq 'egs', /null)) gt 0 then begin
   ; Read in astrometry
   extast, headfits(img_file_loc[3]+'hlsp_candels_hst_acs_egs-tot-30mas-section11_f606w_v1.0_drz.fits.gz', /silent), astr11 
   extast, headfits(img_file_loc[3]+'hlsp_candels_hst_acs_egs-tot-30mas-section12_f606w_v1.0_drz.fits.gz', /silent), astr12 
   extast, headfits(img_file_loc[3]+'hlsp_candels_hst_acs_egs-tot-30mas-section21_f606w_v1.0_drz.fits.gz', /silent), astr21 
   extast, headfits(img_file_loc[3]+'hlsp_candels_hst_acs_egs-tot-30mas-section22_f606w_v1.0_drz.fits.gz', /silent), astr22 

   ; All images are perfect cuts, meaning no blank overlapping regions between sections
   ; Check the pixel location of each ra and dec, and if it is between 0 and astrX.NAXIS,
   ;    then it is in that section
   ad2xy, ra, dec, astr11, x11, y11
   ad2xy, ra, dec, astr12, x12, y12
   ad2xy, ra, dec, astr21, x21, y21
   ad2xy, ra, dec, astr22, x22, y22

   section[where(field eq 'egs' and x11 ge 0 and x11 lt astr11.naxis[0] and y11 ge 0 and y11 lt astr11.naxis[1], /null)] = 11
   section[where(field eq 'egs' and x12 ge 0 and x12 lt astr12.naxis[0] and y12 ge 0 and y12 lt astr12.naxis[1], /null)] = 12
   section[where(field eq 'egs' and x21 ge 0 and x21 lt astr21.naxis[0] and y21 ge 0 and y21 lt astr21.naxis[1], /null)] = 21
   section[where(field eq 'egs' and x22 ge 0 and x22 lt astr22.naxis[0] and y22 ge 0 and y22 lt astr22.naxis[1], /null)] = 22
 endif
 if n_elements(where(field eq 'uds', /null)) gt 0 then begin
   ; Read in astrometry
   extast, headfits(img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect11_f606w_v1.0_drz.fits.gz', /silent), astr11 
   extast, headfits(img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect12_f606w_v1.0_drz.fits.gz', /silent), astr12 
   extast, headfits(img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect13_f606w_v1.0_drz.fits.gz', /silent), astr13 
   extast, headfits(img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect14_f606w_v1.0_drz.fits.gz', /silent), astr14 
   extast, headfits(img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect15_f606w_v1.0_drz.fits.gz', /silent), astr15 
   extast, headfits(img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect21_f606w_v1.0_drz.fits.gz', /silent), astr21 
   extast, headfits(img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect22_f606w_v1.0_drz.fits.gz', /silent), astr22 
   extast, headfits(img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect23_f606w_v1.0_drz.fits.gz', /silent), astr23 
   extast, headfits(img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect24_f606w_v1.0_drz.fits.gz', /silent), astr24 
   extast, headfits(img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect25_f606w_v1.0_drz.fits.gz', /silent), astr25 

   ; All images are perfect cuts, meaning no blank overlapping regions between sections
   ; Check the pixel location of each ra and dec, and if it is between 0 and astrX.NAXIS,
   ;    then it is in that section
   ad2xy, ra, dec, astr11, x11, y11
   ad2xy, ra, dec, astr12, x12, y12
   ad2xy, ra, dec, astr13, x13, y13
   ad2xy, ra, dec, astr14, x14, y14
   ad2xy, ra, dec, astr15, x15, y15
   ad2xy, ra, dec, astr21, x21, y21
   ad2xy, ra, dec, astr22, x22, y22
   ad2xy, ra, dec, astr23, x23, y23
   ad2xy, ra, dec, astr24, x24, y24
   ad2xy, ra, dec, astr25, x25, y25

   section[where(field eq 'uds' and x11 ge 0 and x11 lt astr11.naxis[0] and y11 ge 0 and y11 lt astr11.naxis[1], /null)] = 11
   section[where(field eq 'uds' and x12 ge 0 and x12 lt astr12.naxis[0] and y12 ge 0 and y12 lt astr12.naxis[1], /null)] = 12
   section[where(field eq 'uds' and x13 ge 0 and x13 lt astr13.naxis[0] and y13 ge 0 and y13 lt astr13.naxis[1], /null)] = 13
   section[where(field eq 'uds' and x14 ge 0 and x14 lt astr14.naxis[0] and y14 ge 0 and y14 lt astr14.naxis[1], /null)] = 14
   section[where(field eq 'uds' and x15 ge 0 and x15 lt astr15.naxis[0] and y15 ge 0 and y15 lt astr15.naxis[1], /null)] = 15
   section[where(field eq 'uds' and x21 ge 0 and x21 lt astr21.naxis[0] and y21 ge 0 and y21 lt astr21.naxis[1], /null)] = 21
   section[where(field eq 'uds' and x22 ge 0 and x22 lt astr22.naxis[0] and y22 ge 0 and y22 lt astr22.naxis[1], /null)] = 22
   section[where(field eq 'uds' and x23 ge 0 and x23 lt astr23.naxis[0] and y23 ge 0 and y23 lt astr23.naxis[1], /null)] = 23
   section[where(field eq 'uds' and x24 ge 0 and x24 lt astr24.naxis[0] and y24 ge 0 and y24 lt astr24.naxis[1], /null)] = 24
   section[where(field eq 'uds' and x25 ge 0 and x25 lt astr25.naxis[0] and y25 ge 0 and y25 lt astr25.naxis[1], /null)] = 25
 endif


; Remove galaxies that are not within a field or section of a field and print the J2000 name of the galaxy
 outside = where(field eq 'none' or section eq 0, nout, /null, comp = inside)
 for i = 0, nout-1 do print, name[outside[i]]+' is not contained within any CANDELS HST/ACS images'
 ra = ra[inside]
 dec = dec[inside]
 redshift = redshift[inside]
 name = name[inside]
 field = field[inside]
 section = section[inside]


; Sort based on field, then based on section. Prevents reopening of image fits files 
;   (as they can be large)
 field_sort = sort(field)
 ra = ra[field_sort]
 dec = dec[field_sort]
 redshift = redshift[field_sort]
 name = name[field_sort]
 field = field[field_sort]
 section = section[field_sort]
 
; Since already sorted by field, add the number of galaxies already sorted by section
 n_cosmos = n_elements(where(field eq 'cosmos', /null))
 n_egs = n_elements(where(field eq 'egs', /null))
 n_goodsn = n_elements(where(field eq 'goodsn', /null))
 n_goodss = n_elements(where(field eq 'goodss', /null))
 n_uds = n_elements(where(field eq 'uds', /null))

 section_sort = lonarr(n_elements(ra))
 section_sort[where(field eq 'cosmos', /null)] = sort(section[where(field eq 'cosmos', /null)])
 section_sort[where(field eq 'egs', /null)] = sort(section[where(field eq 'egs', /null)])+n_cosmos
 section_sort[where(field eq 'goodsn', /null)] = sort(section[where(field eq 'goodsn', /null)])+n_cosmos+n_egs
 section_sort[where(field eq 'goodss', /null)] = sort(section[where(field eq 'goodss', /null)])+n_cosmos+n_egs+n_goodsn
 section_sort[where(field eq 'uds', /null)] = sort(section[where(field eq 'uds', /null)])+n_cosmos+n_egs+n_goodsn+n_goodss

 ra = ra[section_sort]
 dec = dec[section_sort]
 redshift = redshift[section_sort]
 name = name[section_sort]
 field = field[section_sort]
 section = section[section_sort]


; Pixel scale of HST/ACS
 pixscl = 0.03 ; arcsec pixel-1

; Print message stating number of postage stamps to generate
 print,'Number of Galaxies without HST Postage Stamps: '+strtrim(string(nout),2)
 print,'Number of Galaxies with HST Postage Stamps: '+strtrim(string(n_elements(ra)),2)

; Loop Through All Galaxies and Save Postage Stamps
 for i = 0, n_elements(ra)-1 do begin
   
   ; Determine pixel size of galaxy
   scale = dangular(redshift[i], /kpc)/206265.  ; kpc arcsec-1 (dangular is in kpc/rad, 206265 arcsec = 1 rad)
   ang_size = round(phys_size/scale)
   pix_size = round(ang_size/pixscl)

   ; Only read in images if it is different from the previous one
   if i gt 0 and field[i] eq field[i-1] and section[i] eq section[i-1] then new_section = 0 else new_section  = 1

   if new_section eq 1 then begin
     sctstr = string(section[i],f='(i2)')

     if field[i] eq 'cosmos' then begin
       r_image = img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect'+sctstr+'_f814w_v1.0_drz.fits.gz'
       b_image = img_file_loc[2]+'hlsp_candels_hst_acs_cos-tot-sect'+sctstr+'_f606w_v1.0_drz.fits.gz'
     endif
     if field[i] eq 'egs' then begin
       r_image = img_file_loc[3]+'hlsp_candels_hst_acs_egs-tot-30mas-section'+sctstr+'_f814w_v1.0_drz.fits.gz'
       b_image = img_file_loc[3]+'hlsp_candels_hst_acs_egs-tot-30mas-section'+sctstr+'_f606w_v1.0_drz.fits.gz'
     endif
     if field[i] eq 'goodsn' then begin
       r_image = img_file_loc[0]+'h_nz_sect'+sctstr+'_v2.0_drz_img.fits.gz'
       g_image = img_file_loc[0]+'h_nv_sect'+sctstr+'_v2.0_drz_img.fits.gz'
       b_image = img_file_loc[0]+'h_nb_sect'+sctstr+'_v2.0_drz_img.fits.gz'
     endif
     if field[i] eq 'goodss' then begin
       r_image = img_file_loc[1]+'h_sz_sect'+sctstr+'_v2.0_drz_img.fits.gz'
       g_image = img_file_loc[1]+'h_sv_sect'+sctstr+'_v2.0_drz_img.fits.gz'
       b_image = img_file_loc[1]+'h_sb_sect'+sctstr+'_v2.0_drz_img.fits.gz'
     endif
     if field[i] eq 'uds' then begin
       r_image = img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect'+sctstr+'_f814w_v1.0_drz.fits.gz'
       b_image = img_file_loc[4]+'hlsp_candels_hst_acs_uds-tot-sect'+sctstr+'_f606w_v1.0_drz.fits.gz'
     endif

     rim = mrdfits(r_image,0,rhdr,/silent)
     bim = mrdfits(b_image,0,bhdr,/silent)

     if field[i] eq 'goodsn' or field[i] eq 'goodss' then begin
       gim = mrdfits(g_image,0,ghdr,/silent)
     endif else begin
       gim = (rim+bim)/2.
       ghdr = rhdr
     endelse
   endif

   ; Determine image center from RA and DEC for each image   
   adxy, rhdr, ra[i], dec[i], tempx, tempy
   tempxpix_r = round(tempx)
   tempypix_r = round(tempy)
   adxy, ghdr, ra[i], dec[i], tempx, tempy
   tempxpix_g = round(tempx)
   tempypix_g = round(tempy)
   adxy, bhdr, ra[i], dec[i], tempx, tempy
   tempxpix_b = round(tempx)
   tempypix_b = round(tempy)

   ; Check if object is near edge of image. If so, print warning stating that postage 
   ;   stamp is near edge of image and lower pix_size if it exceeds image size.
   ;   Do not lower pix_size below 50. If this occurs, print name of galaxy do not
   ;   generate postage stamp.
   if tempxpix_r-pix_size lt 0 or tempxpix_r+pix_size ge (size(rim,/dim))[0] or $
      tempypix_r-pix_size lt 0 or tempypix_r+pix_size ge (size(rim,/dim))[1] then begin
     print, 'Warning: '+name[i]+' is near edge of image tile. Reducing physical extent of image.'
     if tempxpix_r-pix_size lt 0 then pix_size = tempxpix_r
     if tempxpix_r+pix_size ge (size(rim,/dim))[0] then pix_size = (size(rim,/dim))[0]-tempxpix_r-1
     if tempypix_r-pix_size lt 0 then pix_size = tempypix_r
     if tempypix_r+pix_size ge (size(rim,/dim))[1] then pix_size = (size(rim,/dim))[1]-tempypix_r-1
     if pix_size lt 50 then begin
       print, 'Warning: '+name[i]+' is too close to edge of image tile for postage stamp. Skipping.'
       continue
     endif
   endif

   ; Extract the postage stamp images
   ; R-image
   hextract, rim, rhdr, subim_r, subhdr_r, $
     tempxpix_r-pix_size, tempxpix_r+pix_size, tempypix_r-pix_size, tempypix_r+pix_size, /silent
   ; G-image
   hextract, gim, ghdr, subim_g, subhdr_g, $
     tempxpix_g-pix_size, tempxpix_g+pix_size, tempypix_g-pix_size, tempypix_g+pix_size, /silent
   ; B-image
   hextract, bim, bhdr, subim_b, subhdr_b, $
     tempxpix_b-pix_size, tempxpix_b+pix_size, tempypix_b-pix_size, tempypix_b+pix_size, /silent


   ; Combine images into 3D RGB array   
   subim_size = size(subim_r, /dimen)
   color_image = dblarr(subim_size[0], subim_size[1], 3)
   color_image[*,*,0] = subim_r
   color_image[*,*,1] = subim_g
   color_image[*,*,2] = subim_b


   ; Save the postage stamp to FITS file
   mwrfits, color_image, outfile_loc+name[i]+'_postage_stamp.fits', subhdr_r, /create
   print,'Postage Stamp '+strtrim(string(i+1),2)+' of '+strtrim(string(n_elements(ra)),2)+' generated'
 endfor

end