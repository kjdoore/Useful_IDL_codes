function donley_agn,f_36,f_45,f_58,f_80
;+
; NAME:
;	DONLEY_AGN
; PURPOSE:
;	To identify obscured IRAC-AGN using the criteria from 
;     Donley et al. 2012, ApJ, 748, 22
; EXPLANATION: 
;	A function that identifies AGN using the IRAC color criteria 
;     from Donley+(2012).
;
; CALLING SEQUENCE:
;	donley_agn(f_36, f_80, f_58, f_250_100, [/pacs100])
;
; INPUTS:
;	f_36 - A vector containing the IRAC 3.6 micron fluxes for each source
;	f_45 - A vector containing the IRAC 4.5 micron fluxes for each source
;	f_58 - A vector containing the IRAC 5.8 micron fluxes for each source
;	f_80 - A vector containing the IRAC 8.0 micron fluxes for each source
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
  if size(f_45,/type) lt 2 or size(f_45,/type) gt 5 then begin
    print,'f_45 is incorrect data type'
    return,0
  endif
  if size(f_58,/type) lt 2 or size(f_58,/type) gt 5 then begin
    print,'f_58 is incorrect data type'
    return,0
  endif
  if size(f_80,/type) lt 2 or size(f_80,/type) gt 5 then begin
    print,'f_80 is incorrect data type'
    return,0
  endif

  if min(f_36) le 0 then begin
    print,'Need IRAC 3.6um data for all galaxies'
    return,0
  endif
  if min(f_45) le 0 then begin
    print,'Need IRAC 4.5um data for all galaxies'
    return,0
  endif
  if min(f_58) le 0 then begin
    print,'Need IRAC 5.8um data for all galaxies'
    return,0
  endif
  if min(f_80) le 0 then begin
    print,'Need IRAC 8.0um data for all galaxies'
    return,0 
  endif  

  if size(f_36,/n_dim) gt 1 then begin
    print,'f_36 must be a vector'
    return,0
  endif
  if size(f_45,/n_dim) gt 1 then begin
    print,'f_45 must be a vector'
    return,0
  endif
  if size(f_58,/n_dim) gt 1 then begin
    print,'f_58 must be a vector'
    return,0
  endif
  if size(f_80,/n_dim) gt 1 then begin
    print,'f_80 must be a vector'
    return,0
  endif

  if n_elements(f_36) ne n_elements(f_45) or n_elements(f_36) ne n_elements(f_58) or $
     n_elements(f_36) ne n_elements(f_80)then begin
    print,'All data arrays need to be same size'
    return,0
  endif
  
  x=alog10(f_58/f_36)
  y=alog10(f_80/f_45)
  
  AGN_gal=where(x ge 0.08 and y ge 0.15 and y ge ((1.21*x)-0.27) and y le ((1.21*x)+0.27) and $
                f_45 gt f_36 and f_58 gt f_45 and f_80 gt f_58,/null,no)
  
  galtype=replicate('Galaxy',n_elements(f_36))
  galtype[AGN_gal]='AGN'
  
  return, galtype

end