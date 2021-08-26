function sfh_plot,age_bounds,sfh_bins,sfh_bins_error,AGE_RANGE=AGE_RANGE, $
           SFH_ERROR_STYLE=SFH_ERROR_STYLE,OVERPLOT=OVERPLOT,CURRENT=CURRENT, $
	       SFH_ERROR_CAPSIZE=SFH_ERROR_CAPSIZE,_EXTRA=_extra_plot
;+
; NAME:
;	SFH_PLOT
; PURPOSE:
;	To create a non-parametric star formation history (SFH) plot
; EXPLANATION: 
;	Creates a non-parametric SFH plot for the input age bins and SFRs
;
; CALLING SEQUENCE:
;   triangle_dist_plot, distribution, distribution_labels, [/SQUAREROOT, $
;          /NORMALIZE, /NOSHOW_CONTOUR, /SHOW_MEDIAN, /SHOW_DOTS, /SHOW_CORRELATE, $
;          FONT_SIZE=FONT_SIZE, PADDING=PADDING, HEIGHT=HEIGHT, WIDTH=WIDTH, $
;          XWINDOW=XWINDOW, YWINDOW=YWINDOW, CONTOUR_COLOR=CONTOUR_COLOR, $
;          CONTOUR_LEVELS=CONTOUR_LEVELS, CONTOUR_SMOOTH=CONTOUR_SMOOTH, $
;          CONTOUR_THICK=CONTOUR_THICK, TICKINTERVAL=TICKINTERVAL, $
;          DISTRIBUTION_COLOR=DISTRIBUTION_COLOR, DISTRIBUTION_THICK=DISTRIBUTION_THICK, $
;          CORRELATE_LOCATION=CORRELATE_LOCATION,CORRELATE_COLOR=CORRELATE_COLOR, $
;          CORRELATE_PADDING=CORRELATE_PADDING]
;
; INPUTS:
;	age_bounds - a N element vector giving the age bounds of the bins of the SFH
;   sfh_bins   - a N-1 element vector giving the SFR of each age bin
;   sfh_bins_error - either a N-1 vector or a 2 x (N-1) array of error values. If the 
;                     input is a vector, each value will be used as both lower and upper
;                     error. If the argument is a 2 by (N-1) array, the [0, *] values
;                     define the lower error and the [1, *] values define the upper error.
;
; OPTIONAL INPUTS:
;  AGE_RANGE       - a 2 element vector containing the age range to be plotted (default is
;                     the range of age_bounds)
;  SFH_ERROR_STYLE - an integer value of 1 or 2, which gives the style of errors on the SFH.
;                     A value of 1 gives a retangular box that covers the age bin for the
;                     error. A value of 2 gives a vertical line for the error. (default is 1)
;  SFH_ERROR_CAPSIZE - the size to make the error bar capsize if SFH_ERROR_STYLE=2 in values 
;                       of 0 to 1.0, where a value of 1.0 results in an endcap that is 10% 
;                       of the data range. (default is 0)
; 
; OPTIONAL KEYWORD INPUT:
;  OVERPLOT - If set, will place the graphic on top of the currently-selected graphic
;               within the current window. The two graphics items will then share the 
;               same set of axes. If no window exists, a new window is created.
;  CURRENT  - If set, will create the graphic in the current window with a new set 
;               of axes. If no window exists, a new window is created.
;
; OUTPUTS:
;	x - plot object
;
; EXAMPLE USAGE:
;     IDL> age_bounds = 10^(findgen(6)*2+1)
;     IDL> sfh_bins = findgen(5)+1
;     IDL> sfh_bins_error = sfh_bins*0.5
;     IDL> x = sfh_plot(age_bounds,sfh_bins,sfh_bins_error)
;     IDL> help, x
;
; NOTES:
;   To save the output plot, which is an IDL object, use:
;      x.save,'/YOUR_FOLDER/FILE_NAME.FILE_TYPE'
;   See https://www.harrisgeospatial.com/docs/Save_Method.html for file types 
;      and more details on saving graphics
;
; REVISON HISTORY:
;	Written by K. Doore, 9/21/2020
;-
  Compile_opt idl2
  On_error,2

; Check for allowable type and size of inputs
  if size(age_bounds,/type) lt 2 or size(age_bounds,/type) gt 5 then begin
    print,'age_bounds is incorrect data type'
    return,0
  endif 
  if size(age_bounds,/N_DIMENSIONS) ne 1 then begin
    print,'age_bounds must be a 1D vector'
    return,0
  endif
  if n_elements(age_bounds) lt 2 then begin
    message,'age_bounds must have at least one age bin'
    return,0
  endif
  
  if size(sfh_bins,/type) lt 2 or size(sfh_bins,/type) gt 5 then begin
    print,'sfh_bins is incorrect data type'
    return,0
  endif 
  if size(sfh_bins,/N_DIMENSIONS) ne 1 then begin
    print,'sfh_bins must be a 1D vector'
    return,0
  endif
  if n_elements(sfh_bins) ne (n_elements(age_bounds)-1) then begin
    print,'sfh_bins must have one less element than age_bounds'
    return,0
  endif
  
  if size(sfh_bins_error,/type) lt 2 or size(sfh_bins_error,/type) gt 5 then begin
    print,'sfh_bins_error is incorrect data type'
    return,0
  endif 
  if size(sfh_bins_error,/N_DIMENSIONS) lt 1 or size(sfh_bins_error,/N_DIMENSIONS) gt 2 then begin
    print,'sfh_bins_error must be a N-1 vector or a 2 x (N-1) array'
    return,0
  endif
  if (size(sfh_bins_error,/dimensions))[-1] ne (n_elements(sfh_bins)) then begin
    print,'sfh_bins_error must have the same number of elements as sfh_bins'
    return,0
  endif
  if size(sfh_bins_error,/N_DIMENSIONS) eq 1 then if n_elements(where(sfh_bins-sfh_bins_error lt 0,/null)) gt 0 then begin
    print,'sfh-sfh_bins_error must not be less than 0'
    return,0
  endif
  if size(sfh_bins_error,/N_DIMENSIONS) eq 2 then if n_elements(where(sfh_bins-sfh_bins_error[0,*] lt 0,/null)) gt 0 then begin
    print,'sfh-sfh_bins_error must not be less than 0'
    return,0
  endif
  
  if n_elements(AGE_RANGE) gt 0 then begin
    if size(AGE_RANGE,/type) lt 2 or size(AGE_RANGE,/type) gt 5 then begin
      print,'AGE_RANGE is incorrect data type'
      return,0
    endif
    if min(AGE_RANGE) lt 0 then begin
      print,'AGE_RANGE must be greater than 0'
      return,0
    endif
    if n_elements(AGE_RANGE) ne 2 then begin
      print,'AGE_RANGE must be a 2 element vector'
      return,0
    endif
  endif else begin
    AGE_RANGE = minmax(age_bounds)
  endelse

  if n_elements(SFH_ERROR_STYLE) gt 0 then begin
    if size(SFH_ERROR_STYLE,/type) ne 2 then begin
      print,'SFH_ERROR_STYLE is not of integer type'
      return,0
    endif
    if n_elements(SFH_ERROR_STYLE) ne 1 then begin
      print,'SFH_ERROR_STYLE must be a single value'
      return,0
    endif
    if SFH_ERROR_STYLE lt 1 or SFH_ERROR_STYLE gt 2 then begin
      print,'SFH_ERROR_STYLE must be a value of 1 or 2'
      return,0
    endif
  endif else begin
    SFH_ERROR_STYLE = 1
  endelse

  if n_elements(SFH_ERROR_CAPSIZE) gt 0 then begin
    if size(SFH_ERROR_CAPSIZE,/type) lt 2 and size(SFH_ERROR_CAPSIZE,/type) gt 5 then begin
      print,'SFH_ERROR_CAPSIZE is incorrect data type'
      return,0
    endif
    if n_elements(SFH_ERROR_CAPSIZE) ne 1 then begin
      print,'SFH_ERROR_CAPSIZE must be a single value'
      return,0
    endif
    if SFH_ERROR_CAPSIZE lt 0 or SFH_ERROR_CAPSIZE gt 1 then begin
      print,'SFH_ERROR_CAPSIZE must be a value between 0 and 1'
      return,0
    endif
  endif else begin
    SFH_ERROR_CAPSIZE = 0
  endelse

; create age_bounds array for plotting
  age_bounds=age_bounds[where(finite(age_bounds) eq 1,/null)]
  
  bounds=([age_bounds,age_bounds])[sort([age_bounds,age_bounds])]
  bounds=bounds[1:-2]
  bounds[0]=min(AGE_RANGE)
  age_bounds[0]=min(AGE_RANGE)
  
  if n_elements(_extra_plot) gt 0 then begin
    if where(tag_names(_extra_plot) eq 'XLOG') ne -1 then sfh_xlog=1 
  endif
  if n_elements(sfh_xlog) eq 0 then sfh_xlog=0
  if sfh_xlog and bounds[0] eq 0 then begin
    bounds[0]=(bounds[1])/10.d0
    AGE_RANGE[0]=bounds[0]
  endif


; create sfh_bins array and SFH_BINS_ERROR array for plotting
  SFH=dblarr(n_elements(SFH_bins)*2)
  for i=0,(n_elements(SFH_bins)-1) do SFH[(2*i):(2*i)+1]=SFH_bins[i]
  
  if n_elements(_extra_plot) gt 0 then begin
    if where(tag_names(_extra_plot) eq 'YLOG') ne -1 then sfh_ylog=1 
  endif
  if n_elements(sfh_ylog) eq 0 then sfh_ylog=0
	
  if size(sfh_bins_error,/N_DIMENSIONS) eq 1 then begin
    SFH_err=dblarr(n_elements(sfh_bins_error)*2)
    for i=0,(n_elements(sfh_bins_error)-1) do SFH_err[(2*i):(2*i)+1]=sfh_bins_error[i]
    SFH_upper_err=SFH+SFH_err
    SFH_lower_err=SFH-SFH_err
    if sfh_ylog then SFH_lower_err[where(sfh_lower_err eq 0,/null)]=min(SFH_lower_err[where(sfh_lower_err ne 0,/null)])/10.
  endif else begin 
    SFH_err=dblarr(2,n_elements(sfh_bins_error[0,*])*2)
    for i=0,(n_elements(sfh_bins_error[0,*])-1) do SFH_err[0,(2*i):(2*i)+1]=sfh_bins_error[0,i]
    for i=0,(n_elements(sfh_bins_error[1,*])-1) do SFH_err[1,(2*i):(2*i)+1]=sfh_bins_error[1,i]
    SFH_upper_err=SFH+SFH_err[1,*]
    SFH_lower_err=SFH-SFH_err[0,*]
    if sfh_ylog then SFH_lower_err[where(sfh_lower_err eq 0,/null)]=min(SFH_lower_err[where(sfh_lower_err ne 0,/null)])/10.
  endelse


; create SFH plot
  if SFH_ERROR_STYLE eq 2 then begin
    bounds_mid=dblarr(n_elements(uniq(bounds))-1)
    if sfh_xlog then begin
      for i=0,(n_elements(age_bounds)-2) do bounds_mid[i]=10^((alog10((age_bounds)[i+1])+alog10((age_bounds)[i]))/2.d0)
      capwidth=(alog10(AGE_RANGE[1])-alog10(AGE_RANGE[0]))*SFH_ERROR_CAPSIZE*0.1
    endif else begin
      for i=0,(n_elements(age_bounds)-2) do bounds_mid[i]=((age_bounds)[i+1]+(age_bounds)[i])/2.d0
      capwidth=(AGE_RANGE[1]-AGE_RANGE[0])*SFH_ERROR_CAPSIZE*0.1
    endelse

    x=plot([bounds_mid[0],bounds_mid[0]],[SFH_upper_err[0],SFH_lower_err[0]],OVERPLOT=OVERPLOT,CURRENT=CURRENT,$
           xrange=AGE_RANGE,_EXTRA=_extra_plot)    
    for i=1,(n_elements(bounds_mid)-1) do $
      x=plot([bounds_mid[i],bounds_mid[i]],[SFH_upper_err[2*i],SFH_lower_err[2*i]],OVERPLOT=1,$
             _EXTRA=_extra_plot)
    for i=0,(n_elements(bounds_mid)-1) do begin $
      if sfh_xlog then begin
        x=plot([10^(alog10(bounds_mid[i])-capwidth),10^(alog10(bounds_mid[i])+capwidth)],[SFH_upper_err[2*i],SFH_upper_err[2*i]],OVERPLOT=1,$
               _EXTRA=_extra_plot)    
        x=plot([10^(alog10(bounds_mid[i])-capwidth),10^(alog10(bounds_mid[i])+capwidth)],[SFH_lower_err[2*i],SFH_lower_err[2*i]],OVERPLOT=1,$
               _EXTRA=_extra_plot)    
      endif else begin
        x=plot([bounds_mid[i]-capwidth,bounds_mid[i]+capwidth],[SFH_upper_err[2*i],SFH_upper_err[2*i]],OVERPLOT=1,$
               _EXTRA=_extra_plot)    
        x=plot([bounds_mid[i]-capwidth,bounds_mid[i]+capwidth],[SFH_lower_err[2*i],SFH_lower_err[2*i]],OVERPLOT=1,$
               _EXTRA=_extra_plot)    
      endelse
    endfor
  endif else if SFH_ERROR_STYLE eq 1 then begin
    x=FillPlot(bounds,[[SFH_upper_err],[SFH_lower_err]],xrange=AGE_RANGE,OVERPLOT=OVERPLOT,CURRENT=CURRENT,$
               linestyle='',_EXTRA=_extra_plot)
  endif

  x=plot(bounds,SFH,xrange=AGE_RANGE,OVERPLOT=1,_EXTRA=_extra_plot)
  
  return,x

end