function kirkpatrick_agn,f_36,f_80,f_24,f_250_100,pacs100=pacs100
;+
; NAME:
;	KIRKPATRICK_AGN
; PURPOSE:
;	To identify obscured MIR-AGN using the criteria from 
;     Kirkpatrick et al. 2013, ApJ, 763, 12
; EXPLANATION: 
;	A function that identifies AGN using the MIR color-color criteria 
;     from Kirkpatrick+(2013).
;
; CALLING SEQUENCE:
;	kirkpatrick_agn(f_36, f_80, f_24, f_250_100, [/pacs100])
;
; INPUTS:
;	f_36      - A vector containing the IRAC 3.6 micron fluxes for each source
;	f_80      - A vector containing the IRAC 8.0 micron fluxes for each source
;	f_24      - A vector containing the MIPS 24 micron fluxes for each source
;	f_250_100 - A vector containing the SPIRE 250 micron fluxes for each source,
;                 or PACS 100 micron fluxes is pacs100 keyword is set.
;
; OPTIONAL KEYWORD INPUT:
;   /pacs100  - if set, then the criteria will use the PACS 100 relation
;                 rather than the default SPIRE 250
;
; OUTPUT:
;   galtype - A vector of strings stating if the source is considered an AGN
;               or Galaxy
;
; REVISON HISTORY:
;	Written by K. Doore, 8/24/2021
;-
  Compile_opt idl2
  On_error,2

; Check arguments
  if size(f_36,/type) lt 2 or size(f_36,/type) gt 5 then begin
    print,'f_36 is incorrect data type'
    return,0
  endif
  if size(f_80,/type) lt 2 or size(f_80,/type) gt 5 then begin
    print,'f_80 is incorrect data type'
    return,0
  endif
  if size(f_24,/type) lt 2 or size(f_24,/type) gt 5 then begin
    print,'f_24 is incorrect data type'
    return,0
  endif
  if size(f_250_100,/type) lt 2 or size(f_250_100,/type) gt 5 then begin
    print,'f_250_100 is incorrect data type'
    return,0
  endif

  if min(f_36) le 0 then begin
    print,'Need IRAC 3.6um data for all galaxies'
    return,0
  endif
  if min(f_80) le 0 then begin
    print,'Need IRAC 8.0um data for all galaxies'
    return,0
  endif
  if min(f_24) le 0 then begin
    print,'Need MIPS 24um data for all galaxies'
    return,0
  endif
  if ~keyword_set(pacs100) then begin
    if min(f_250_100) le 0 then begin
      print,'Need SPIRE 250um data for all galaxies'
      return,0 
    endif  
  endif else begin
    if min(f_250_100) le 0 then begin
      print,'Need PACS 100um data for all galaxies'
      return,0
    endif
  endelse

  if size(f_36,/n_dim) gt 1 then begin
    print,'f_36 must be a vector'
    return,0
  endif
  if size(f_80,/n_dim) gt 1 then begin
    print,'f_80 must be a vector'
    return,0
  endif
  if size(f_24,/n_dim) gt 1 then begin
    print,'f_24 must be a vector'
    return,0
  endif
  if size(f_250_100,/n_dim) gt 1 then begin
    print,'f_250_100 must be a vector'
    return,0
  endif

  if n_elements(f_36) ne n_elements(f_80) or n_elements(f_36) ne n_elements(f_24) or $
     n_elements(f_36) ne n_elements(f_250_100)then begin
    print,'All data arrays need to be same size'
    return,0
  endif
  
  x=alog10(f_250_100/f_24)
  y=alog10(f_80/f_36)
 
  if ~keyword_set(pacs100) then begin
    y_x=0.74*x-0.78        
  endif else begin
    y_x=0.208*x+0.105
  endelse

  AGN_gal=where(y ge y_x,/null,no)
  
  galtype=replicate('Galaxy',n_elements(f_36))
  galtype[AGN_gal]='AGN'
  
  return, galtype
  
end