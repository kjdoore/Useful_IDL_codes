function mode,X,DIMENSION=DIMENSION,DOUBLE=DOUBLE
;+
; NAME:
;   MODE
; PURPOSE:
;   To determine the mode or peak of a distribution
; EXPLANATION: 
;   Determines the mode of a distribution by binning the input and finding the largest 
;     bin. This bin value is then weighted by the neighboring two bins to give an average
;     to be used as the mode. The routine checks for occurrences of the IEEE 
;     floating-point values NaN or Infinity in the input data. Elements with the value 
;     NaN or Infinity are treated as missing data.
;
; CALLING SEQUENCE:
;   MODE( X   [, DIMENSION=value, /DOUBLE])
;
; INPUTS:
;   X - An n-element, integer, double-precision or floating-point array
;
; OPTIONAL INPUTS:
;   DIMENSION - Set this keyword to a positive scalar indicating the dimension across 
;                 which to calculate the mode. If this keyword is not present or is zero,
;                 then the mode is computed across all dimensions of the input array. If 
;                 this keyword is present, then the mode is only calculated across a 
;                 single dimension. In this case the result is an array with one less 
;                 dimension than the input.
;
; OPTIONAL KEYWORD
;   DOUBLE - If set then, computations are done in double precision.
;
; OUTPUTS:
;   D - The mode value of a set of numbers.
;
; EXAMPLE USAGE:
;   IDL> X = randomn(seed,1000)
;   IDL> D = mode(X)
;
; REVISON HISTORY:
;   Written by K. Doore, 5/13/2021
;-
  Compile_opt idl2
  On_error,2

; Check for allowable type and size of inputs
  if size(X,/type) lt 2 or size(X,/type) gt 5 then begin
    print,'Input array is incorrect data type'
    return,!null
  endif 
  if n_elements(DIMENSION) gt 0 then begin
    if size(DIMENSION,/type) lt 2 or size(DIMENSION,/type) gt 5 then begin
      print,'DIMENSION is incorrect data type'
      return,!null
    endif 
    if n_elements(DIMENSION) ne 1 then begin
      print,'DIMENSION must be a single value'
      return,!null
    endif
    if DIMENSION lt 0 or DIMENSION gt size(X,/N_DIMENSIONS) then begin
      print,'DIMENSION must be a value between 0 and the number of dimensions of the input'
      return,!null
    endif 
  endif else begin
    DIMENSION=0  
  endelse


; Determine number of bins for the data and reform it so it can be binned
  ndims = SIZE(X, /N_DIMENSION)
  if DIMENSION eq 0 or ndims le 1 then begin
    nX = TOTAL(FINITE(X), /INTEGER)
    fulldimens = [nX]
    Xdimens = 1
    binlength = PRODUCT(fulldimens, /INTEGER)
    nResult = 1
    nbins = ceil(sqrt(nX))
    reformedx=reform(X,binlength,nResult)
    reformednbins = reform(nbins,nResult)
  endif else begin
    fulldimens = SIZE(X, /DIMENSIONS)
    binlength = fulldimens[dimension-1]
    Xdimens = fulldimens
    Xdimens[dimension-1] = 1
    nResult = PRODUCT(Xdimens, /INTEGER)
    nX = TOTAL(FINITE(X), dimension, /INTEGER)
    nbins = ceil(sqrt(nX))
    newdim=where(indgen(ndims) ne dimension-1,/null)
    newX=transpose(X,[dimension-1,newdim])
    reformedx=reform(newX,binlength,nResult)
    reformednbins = reform(nbins,nResult)
  endelse

  if binlength le 2 then begin
    print,'Dimension for computing mode must have more than 2 elements'
    return,!null
  endif 


; Bin the data and extract the mode
  if keyword_set(DOUBLE) then modeval=reform(dblarr(xdimens)) else $
                              modeval=reform(fltarr(xdimens))
  for i=0,(nResult-1) do begin
    buffer=1
    binvals=!null
    pdf = float(histogram(reformedx[*,i],nbins=reformednbins[i],locations=binvals,/nan))
    binwidth=(max(reformedx[*,i])-min(reformedx[*,i]))/(reformednbins[i]-1+buffer-1)

    ;Add 0s to edges in case max value is an edge bin
    binloc=[min(binvals)-binwidth,binvals,max(binvals)+binwidth]
    pdf=[0,pdf,0]
    peak=where(pdf eq max(pdf))

    while n_elements(peak) ne 1 do begin
      pdf = float(histogram(reformedx[*,i],nbins=reformednbins[i]+buffer,locations=binvals,/nan))
      binwidth=(max(reformedx[*,i])-min(reformedx[*,i]))/(reformednbins[i]-1+buffer-1)

      ;Add 0s to edges in case max value is an edge bin
      binloc=[min(binvals)-binwidth,binvals,max(binvals)+binwidth]
      pdf=[0,pdf,0]
      peak=where(pdf eq max(pdf))
      buffer++
    endwhile

    modeval[i]=binloc[peak]+binwidth*(pdf[peak]-pdf[peak-1])/$
               (2*pdf[peak]-pdf[peak-1]-pdf[peak+1])
  endfor

  return, modeval
end