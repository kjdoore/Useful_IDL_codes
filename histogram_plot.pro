function histogram_plot,distribution,BINLOC=BINLOC,BINSIZE=BINSIZE,MINIMUM=MINIMUM,$
           MAXIMUM=MAXIMUM,NBINS=NBINS,PEAK=PEAK,SQUAREROOT=SQUAREROOT,PDF=PDF,_EXTRA=_extra_plot
;+
; NAME:
;   HISTOGRAM_PLOT
; PURPOSE:
;   To create basic histogram plot without any fuss
; EXPLANATION: 
;   Creates a histogram of the input distribution. Historgrams are plotted using Scott's 
;     normal reference rule (see https://en.wikipedia.org/wiki/Histogram).
;
; CALLING SEQUENCE:
;   corner_plot, distribution, [BINLOC=BINLOC, BINSIZE=BINSIZE, MINIMUM=MINIMUM,$
;                MAXIMUM=MAXIMUM, NBINS=NBINS, PEAK=PEAK, /SQUAREROOT]
;
; INPUTS:
;   distribution - a vector containing the values to be binned and plotted in a histogram.
;                    NaN values are ignored
;
; OPTIONAL INPUTS:
;   BINLOC  - A vector specifying the edge locations of each bin. Does not need to be 
;               regularly gridded. If not present, then the bin locations are determined 
;               from BINSIZE, MAXIMUM, MINIMUM, or NBINS. This input takes top priority if 
;               specified (see NOTE below).
;   BINSIZE - A positive scalar specifying the size of each bin of the histogram. If 
;                not present, the bin size is automatically calculated using Scott's 
;                normal reference rule. This input takes bottom priority if specified 
;                (see NOTE below).
;   MINIMUM - A scalar specifying the minimum value to consider. If not present, then the
;               smallest value of distribution is used.
;   MAXIMUM - A scalar specifying the maximum value to consider. If not present, then the
;               largest value of distribution is used.
;   NBINS   - A scalar specifying the number of bins to use. If not present, then the 
;               number of bins are determined from BINSIZE, MAXIMUM, and MINIMUM. This
;               input takes second priority if specified (see NOTE below).
;   PEAK    - A non-zero scalar specifying the peak value of the histogram. The
;                histogram is normalized to have a maximum value equal to the value 
;                in PEAK. If PEAK is negative, the histogram is inverted.
;
;   All optional inputs for the basic plot function are passed by reference into this 
;       function (i.e., xlabel, color, thickness, etc.)
;
;   NOTE on determining the bins: The key part of creating a histogram is the size and
;      location of the bins. This function is versatile in how it determines this. First,
;      if BINLOC is present, then the bin sizes and locations are directly specified by 
;      this vector and no other action is taken to determine size and location. Therefore,
;      any input values for BINSIZE, MINIMUM, MAXIMUM, and NBINS are ignored. If BINLOC is 
;      not specified, then the function checks if NBINS is specified. If given, the bin 
;      edge locations are determined by [MINIMUM:MAXIMUM:(MAXIMUM-MINIMUM)/NBINS], and 
;      BINSIZE is ignored. If neither BINLOC or NBINS is specified, then the bin edge 
;      locations are determined from BINSIZE by [MINIMUM:MAXIMUM:BINSIZE]. If BINLOC, 
;      BINSIZE, nor NBINS are given, then the bin size and locations are determined 
;      automatically using Scott's normal reference rule.
;      Summary of how bin size and locations are determined in priority order:
;          1: Specified directly in BINLOC 
;          2: Generated from NBINS via [MINIMUM:MAXIMUM:(MAXIMUM-MINIMUM)/NBINS]
;          3: Generated from BINSIZE via [MINIMUM:MAXIMUM:BINSIZE]
;          4: Automatically generated via Scott's normal reference rule
;
; OPTIONAL KEYWORD INPUT:
;   /SQUAREROOT   - if set, then the number of bins is the square root of
;                     the number of data points
;
;   All optional keyword inputs for the basic plot function are passed by reference into 
;       this function (i.e., overplot, current, nodata, buffer, etc.)
;
; OUTPUTS:
;   x - plot object
;
; OPTIONAL OUTPUTS:
;   PDF - A vector containing the density function of the distribution
;
; EXAMPLE USAGE:
;     IDL> distribution = randomn(seed,1000)
;     IDL> x = histogram_plot(distribution)
;     IDL> help, x
;
; NOTES:
;   To save the output plot, which is an IDL object, use:
;      x.save,'/YOUR_FOLDER/FILE_NAME.FILE_TYPE'
;   See https://www.harrisgeospatial.com/docs/Save_Method.html for file types 
;      and more details on saving graphics
;
; REVISON HISTORY:
;   Written by K. Doore, 3/31/2021
;-
  Compile_opt idl2
  On_error,2

; Check for allowable type and size of inputs
  if size(distribution,/type) lt 2 or size(distribution,/type) gt 5 then begin
    print,'distribution is incorrect data type'
    return,0
  endif 
  if size(distribution,/n_dim) ne 1 then begin
    print,'distribution must be one-dimensional vector'
    return,0
  endif
  
  if n_elements(BINLOC) gt 0 then begin
    if size(BINLOC,/type) lt 2 or size(BINLOC,/type) gt 5 then begin
      print,'BINLOC is incorrect data type'
      return,0
    endif 
    if size(BINLOC,/n_dim) ne 1 then begin
      print,'BINLOC must be one-dimensional vector'
      return,0
    endif
    NBINS=n_elements(binloc)-1
  endif


  if n_elements(MINIMUM) gt 0 then begin
    if size(MINIMUM,/type) lt 2 or size(MINIMUM,/type) gt 5 then begin
      print,'MINIMUM is incorrect data type'
      return,0
    endif 
    if size(MINIMUM,/n_dim) ne 0 then begin
      print,'MINIMUM must be single value scalar'
      return,0
    endif
  endif else begin
    MINIMUM=min(distribution,/nan)
  endelse

  if n_elements(MAXIMUM) gt 0 then begin
    if size(MAXIMUM,/type) lt 2 or size(MAXIMUM,/type) gt 5 then begin
      print,'MAXIMUM is incorrect data type'
      return,0
    endif 
    if size(MAXIMUM,/n_dim) ne 0 then begin
      print,'MAXIMUM must be single value scalar'
      return,0
    endif
  endif else begin
    MAXIMUM=max(distribution,/nan)
  endelse

  if n_elements(NBINS) gt 0 then begin
    if size(NBINS,/type) lt 2 or size(NBINS,/type) gt 5 then begin
      print,'NBINS is incorrect data type'
      return,0
    endif 
    if size(NBINS,/n_dim) ne 0 then begin
      print,'NBINS must be single value scalar'
      return,0
    endif
    if n_elements(BINLOC) eq 0 then begin
      BINLOC=[MINIMUM:MAXIMUM:(MAXIMUM-MINIMUM)/double(NBINS)]
    endif
  endif

  if n_elements(BINSIZE) gt 0 then begin
    if size(BINSIZE,/type) lt 2 or size(BINSIZE,/type) gt 5 then begin
      print,'BINSIZE is incorrect data type'
      return,0
    endif 
    if size(BINSIZE,/n_dim) ne 0 then begin
      print,'BINSIZE must be single value scalar'
      return,0
    endif
    if n_elements(BINLOC) eq 0 then begin
      if (MAXIMUM-MINIMUM) lt BINSIZE then begin
        print,'BINSIZE must be smaller than the data range'
        return,0
      endif
      BINLOC=[MINIMUM:MAXIMUM:BINSIZE]
      NBINS=n_elements(binloc)-1
    endif
  endif

  if n_elements(PEAK) gt 0 then begin
    if size(PEAK,/type) lt 2 or size(PEAK,/type) gt 5 then begin
      print,'PEAK is incorrect data type'
      return,0
    endif 
    if size(PEAK,/n_dim) ne 0 then begin
      print,'PEAK must be single value scalar'
      return,0
    endif
    if PEAK eq 0 then begin
      print,'PEAK must be non-zero'
      return,0
    endif

  endif

; Determine BINLOC if generating automatically
  if n_elements(BINLOC) eq 0 then begin
    N = (size(distribution,/dim))[0]
    binsize = 3.49*stddev(distribution)/(N)^(1/3.)
    nbins = ceil((MAXIMUM-MINIMUM)/binsize)
    if keyword_set(SQUAREROOT) then nbins = ceil(sqrt(N))
    BINLOC=[MINIMUM:MAXIMUM:(MAXIMUM-MINIMUM)/NBINS]
  endif

; Determine histogram for each distribution and normalize to PEAK if specified
  pdf = dblarr(nbins)
  for i=0,(nbins-1) do begin      
    if i ne (nbins-1) then begin
      pdf[i] = n_elements(where(distribution ge binloc[i] and distribution lt binloc[i+1],/null))
    endif else begin
      pdf[i] = n_elements(where(distribution ge binloc[i] and distribution le binloc[i+1],/null))
    endelse
  endfor
  if n_elements(PEAK) gt 0 then pdf=pdf/max(pdf)*PEAK

; Plot the histogram
  xpoints=[binloc,binloc]
  xpoints=xpoints[sort(xpoints)]
  ypoints=dblarr(n_elements(xpoints))
  ypoints[1:-2]=pdf[([indgen(nbins),indgen(nbins)])[sort([indgen(nbins),indgen(nbins)])]]

  x=plot(xpoints,ypoints,_EXTRA=_extra_plot)

  return,x

end