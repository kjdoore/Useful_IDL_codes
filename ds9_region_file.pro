pro ds9_region_file,outfile,ra,dec,radius,color=color
;+
; NAME:
;	DS9_REGION_FILE
; PURPOSE:
;	To create a circular ds9 region file for input RA and DECs
; EXPLANATION: 
;	A simple function that creates a circular DS9 region file for a set of 
;     input RA and DEC, and radii.
;
; CALLING SEQUENCE:
;	ds9_region_file, outfile, ra, dec, radius, [color=color]
;
; INPUTS:
;	outfile - A string containing the path and file name of the region
;               file. File type ust be .reg.
;	ra      - A vector containing the RA for each region in decimal degrees
;	dec     - A vector containing the DEC for each region in decimal degrees
;	radius  - A vector containing the radius for each region in arcseconds
;
; OPTIONAL INPUTS:
;	color   - A string containing the single color for all the regions.
;               If not specified, the color will be green.
;
; REVISON HISTORY:
;	Written by K. Doore, 8/24/2021
;-
  Compile_opt idl2
  On_error,2

; Check arguments
  if (N_params() LT 4) then begin
    print,'Syntax - ds9_region_file, outfile, ra, dec, radius, [color=color]'
    return
  endif

  if size(outfile,/type) ne 7 then begin
    print,'outfile must be a string'
    return
  endif 
  if size(ra,/type) lt 2 and size(ra,/type) gt 5 then begin
    print,'ra is incorrect data type'
    return
  endif
  if size(dec,/type) lt 2 and size(dec,/type) gt 5 then begin
    print,'dec is incorrect data type'
    return
  endif
  if size(radius,/type) lt 2 and size(radius,/type) gt 5 then begin
    print,'radius is incorrect data type'
    return
  endif
  if size(ra,/n_dim) gt 1 or size(dec,/n_dim) gt 1 or size(radius,/n_dim) gt 1 then begin
    print,'ra, dec, and radius must be vectors of the same length'
    return
  endif
  if size(ra,/n_ele) ne size(dec,/n_ele) or size(ra,/n_ele) ne size(radius,/n_ele) then begin
    print,'ra, dec, and radius must have the same number of elements'
    return
  endif
 
  if n_elements(COLOR) gt 0 then begin
    if size(PADDING,/type) ne 7 then begin
      print,'PADDING must be a string'
      return
    endif
  endif else begin
    COLOR = 'green'
  endelse


  ; Initiate file
  openw,tab1,outfile,/get_lun

  ; Print the color and Coordinate System
  printf,tab1,'global color='+color
  printf,tab1,'J2000'

  ; Print the circle regions
  for i=0,(n_elements(ra)-1) do printf,tab1,'circle '+string(ra,f='(f9.5)')+'d '+string(dec,f='(f9.6)')+'d '+$
                                                string(radius,f='(f7.3)')+'"'
  
  ; Close file
  free_lun,tab1
  close

end