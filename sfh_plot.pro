function sfh_plot, age_bounds, sfh, sfh_unc_range=sfh_unc_range, age_range=age_range, $
                   overplot=overplot, _extra=_extra_plot
;+
; Name
; ----
;	SFH_PLOT
;
; Purpose
; -------
;	Creates a non-parametric star formation history (SFH) plot for
;   the input age bins and SFRs.
;
; Calling Sequence
; ----------------
;   ::
;
;       plt = sfh_plot(age_bounds, sfh, sfh_unc_range [, age_range = , $
;                      /overplot, _extra=_extra_plot])
;
; Inputs
; ------
;	``age_bounds`` : int, float, or double array(Nsteps + 1)
;       The age bounds of the bins of the SFH.
;   ``sfh`` : int, float, or double array(Nsteps)
;       The SFR associated with each age bin.
;   ``sfh_unc_range`` : int, float or double array(2, Nsteps)
;       The uncertainty range on the SFR of each age bin. The first index of the 
;       first dimension gives the lower uncertainty values, and the second index
;       gives the upper uncertainty values. For example a SFR = 2 +/- 1 would
;       have an uncertainty range of [1, 3] (i.e., ``range = 2 + [-1, 1]``)
;
; Optional Input
; --------------
;   ``age_range`` : int, float, or double array(2)
;       The age range to be plotted (Default: ``minmax(age_bounds)``).
;   ``overplot`` : flag
;       If set, the graphic will be placed on top of the currently-selected graphic
;       within the current window. The two graphics items will then share the 
;       same set of axes. If no window exists, a new window is created.
;
; Output
; ------
;	``plt`` : object
;       The plot object containing the SED plot.
;
; Notes
; -----
;   - To save the output plot, which is an IDL object, use:
;     ``x.save,'/YOUR_FOLDER/FILE_NAME.FILE_TYPE'``
;   - See https://www.harrisgeospatial.com/docs/Save_Method.html for file types 
;     and more details on saving graphics
;
; Modification History
; --------------------
;   - 2020/09/21: Created (Keith Doore)
;   - 2023/02/15: Major overhaul to match current coding practices (Keith Doore)
;-
 Compile_opt idl2
 On_error, 2

; Error handling
 if size(age_bounds, /type) lt 2 or size(age_bounds, /type) gt 5 then $
   message, 'AGE_BOUNDS is not of type int, float, or double.'
 if size(age_bounds, /n_dim) ne 1 then $
   message, 'AGE_BOUNDS must be a 1D array.'
 if n_elements(age_bounds) lt 2 then $
   message, 'AGE_BOUNDS must have at least one age bin.'
 if min(age_bounds) lt 0 then $
   message, 'AGE_BOUNDS must only contain non-negative values.'
 Nsteps = n_elements(age_bounds) - 1

 if size(sfh, /type) lt 2 or size(sfh, /type) gt 5 then $
   message, 'SFH is not of type int, float, or double.'
 if size(sfh, /n_dim) ne 1 then $
   message, 'SFH must be a 1D array.'
 if n_elements(SFH) ne Nsteps then $
   message, 'SFH must have one less element than AGE_BOUNDS'

 if n_elements(sfh_unc_range) ne 0 then begin
   if size(sfh_unc_range, /type) lt 2 or size(sfh_unc_range, /type) gt 5 then $
     message, 'SFH_UNC_RANGE is not of type int, float, or double.'
   if size(sfh_unc_range, /n_dim) ne 2 then $
     message, 'SFH_UNC_RANGE must be a 2D array.'
   if (size(sfh_unc_range, /dim))[-1] ne Nsteps then $
     message, 'SFH_UNC_RANGE must have second dimension with a length of one less element than AGE_BOUNDS'
 endif

 if n_elements(age_range) ne 0 then begin
   if size(age_range, /type) lt 2 or size(age_range, /type) gt 5 then $
     message, 'AGE_RANGE is not of type int, float, or double.'
   if size(age_range, /n_dim) ne 1 then $
     message, 'AGE_RANGE must be a 1D array.'
   if n_elements(age_range) ne 2 then $
     message, 'AGE_RANGE must have only two elements.'
   if min(age_range) lt 0 then $
     message, 'AGE_RANGE must only contain non-negative values.'
 endif else age_range = minmax(age_bounds)



; Create age_bounds array for plotting
; Need to replicate each value to make stair step look
 age_bounds = age_bounds[where(finite(age_bounds) eq 1, /null)]

 bounds = ([age_bounds, age_bounds])[sort([age_bounds, age_bounds])]
 bounds = bounds[1:-2]
 bounds[0] = min(age_range)

 if n_elements(_extra_plot) gt 0 then $
   if where(tag_names(_extra_plot) eq 'XLOG') ne -1 then sfh_xlog = 1 
 if n_elements(sfh_xlog) eq 0 then sfh_xlog = 0
 if sfh_xlog and (bounds[0] eq 0) then begin
   bounds[0] = (bounds[1]) / 10.d0
   age_range[0] = bounds[0]
 endif


; create sfh_bins array and sfh_bin_unc_range array for plotting
 sfh_bins = dblarr(n_elements(sfh) * 2)
 for i=0, (n_elements(sfh)-1) do sfh_bins[(2*i):(2*i)+1] = sfh[i]
 
 if n_elements(_extra_plot) gt 0 then $
   if where(tag_names(_extra_plot) eq 'YLOG') ne -1 then sfh_ylog=1 
 if n_elements(sfh_ylog) eq 0 then sfh_ylog = 0
	
 if n_elements(sfh_unc_range) ne 0 then begin
   sfh_err = dblarr(2, n_elements(sfh_unc_range[0, *]) * 2)
   for i=0, (n_elements(sfh_unc_range[0, *])-1) do sfh_err[0, (2*i):(2*i)+1] = sfh_unc_range[0, i]
   for i=0, (n_elements(sfh_unc_range[1, *])-1) do sfh_err[1, (2*i):(2*i)+1] = sfh_unc_range[1, i]
   sfh_upper_err = reform(sfh_err[1, *])
   sfh_lower_err = reform(sfh_err[0, *])
   if sfh_ylog then sfh_lower_err[where(sfh_lower_err eq 0, /null)] = min(sfh_lower_err[where(sfh_lower_err ne 0, /null)]) / 10.d

   plt = FillPlot(bounds, [[sfh_lower_err], [sfh_upper_err]], xrange=age_range, overplot=overplot, linestyle='')
   yrange = alog10(minmax([SFH_upper_err, sfh_lower_err]))
   yrange[0] = 10.d^floor(yrange[0])
   yrange[1] = 10.d^ceil(yrange[1])
 endif


; create SFH plot
 plt = plot(bounds, sfh_bins, /overplot, yrange=yrange, _extra=_extra_plot)
  
 return, plt

end