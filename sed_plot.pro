function sed_plot, wave_filters, Lnu_obs, Lnu_unc=Lnu_unc, Lnu_mod=Lnu_mod, xray_bandpass=xray_bandpass, $
                   xray_obs=xray_obs, xray_unc=xray_unc, xray_mod=xray_mod, wave_hires=wave_hires, $
                   Lnu_hires=Lnu_hires, unc_range_Lnu_hires=unc_range_Lnu_hires, $
                   hires_model_names=hires_model_names, hires_color=hires_color, $
                   _extra=_extra_plot
;+
; Name
; ----
;	SED_PLOT
;
; Purpose
; -------
;	Creates a plot of an input SED. This can additionally include 
;   uncertainties on the data and high resolution models.
;
; Calling Sequence
; ----------------
;   ::
;
;       plt = sed_plot(wave_filters, Lnu_obs, Lnu_unc = , Lnu_mod = , xray_bandpass = , $
;                      xray_obs = , xray_unc = , xray_mod = , wave_hires = , $
;                      Lnu_hires = , unc_range_Lnu_hires = , hires_model_names = , 
;                      hires_color = , _extra=_extra_plot])
;
; Inputs
; ------
;	``wave_filters`` : int, float, or double array(Nfilters)
;       The mean wavelengths associated with each filter [um].
;   ``Lnu_obs`` : int, float, or double array(Nfilters)
;       The monochromatic luminosities associated with each filter [Lsun Hz-1].
;
; Optional Inputs
; ---------------
;   ``Lnu_unc`` : int, float, or double array(Nfilters)
;       The uncertainties associated with the monochromatic luminosities [Lsun Hz-1].
;   ``Lnu_mod`` : int, float, or double array(Nfilters)
;       The model monochromatic luminosities associated with each filter [Lsun Hz-1].
;       If specified then the residuals of the SED with the model will be plotted.
;   ``xray_bandpass`` : int, float, or double array(2, Nxray)
;       Bandpasses of X-ray observations: first column contains the lower energy 
;       bound, second column contains the upper. [keV]
;   ``xray_obs`` : int, float, or double array(Nxray)
;       The luminosities associated with the xray bandpasses [Lsun Hz-1].
;   ``xray_unc`` : int, float, or double array(Nxray)
;       The uncertainties associated with the xray luminosities  [Lsun Hz-1].
;   ``xray_mod`` : int, float, or double array(Nxray)
;       The model xray luminosities associated with each filter [Lsun Hz-1].
;       If specified then the residuals of the xray SED with the model will be plotted.
;   ``wave_hires`` : int, float, or double array(Nwave)
;       The wavelengths of the high resolution spectrum [um].
;   ``Lnu_hires`` : int, float, or double array(Nwave, Nmodels)
;       The monochromatic luminosities associated with high resolution spectrum [Lsun Hz-1].
;       This can include multiple model components (e.g., stellar, AGN, total, etc.).
;   ``unc_range_Lnu_hires`` : int, float, or double array(2, Nwave, Nmodels)
;       The minimum and maximum bounds of the uncertainty range for the monochromatic 
;       luminosities associated with high resolution spectrum of each model [Lsun Hz-1].
;   ``hires_model_names`` : string array(Nmodels)
;       The names associated with each high resolution model spectrum.
;       (Default = ``['Model 1', 'Model 2', ...]`` 
;   ``hires_color`` : string array(Nmodels) or int, float, or double array(Nmodels, 3)
;       A string or RGB vector that specifies the color of each high resolution
;       plot line. (Default = linear Nmodels sampling from color table 34)
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
;   - 2022/08/05: Created (Keith Doore)
;   - 2022/09/27: Added defaults to ``hires_model_names`` and ``hires_color`` (Keith Doore)
;   - 2022/09/27: Added method to plot upper limits on ``Lnu_obs`` (Keith Doore)
;-
  Compile_opt idl2
  On_error,2

; Check for allowable type and size of inputs
 if n_elements(wave_filters) eq 0 then message, 'Variable is undefined: WAVE_FILTERS.'
 if size(wave_filters, /type) lt 2 or size(wave_filters, /type) gt 5 then $
   message, 'WAVE_FILTERS must be of type int, float, or double.'
 if size(reform(wave_filters), /n_dim) ne 1 then message, 'WAVE_FILTERS must be a 1-D array.'
 if min(wave_filters) le 0 then message, 'WAVE_FILTERS must only contain positive values.'
 Nfilters = n_elements(wave_filters)

 if n_elements(Lnu_obs) eq 0 then message, 'Variable is undefined: LNU_OBS.'
 if size(Lnu_obs, /type) lt 2 or size(Lnu_obs, /type) gt 5 then $
   message, 'LNU_OBS must be of type int, float, or double.'
 if size(reform(Lnu_obs), /n_dim) ne 1 then message, 'LNU_OBS must be a 1-D array.'
 if n_elements(Lnu_obs) ne Nfilters then $
   message, 'LNU_OBS must have the same number of elements as WAVE_FILTERS.'

 if n_elements(Lnu_unc) ne 0 then begin
   if size(Lnu_unc, /type) lt 2 or size(Lnu_unc, /type) gt 5 then $
     message, 'LNU_UNC must be of type int, float, or double.'
   if size(reform(Lnu_unc), /n_dim) ne 1 then message, 'LNU_UNC must be a 1-D array.'
   if n_elements(Lnu_unc) ne Nfilters then $
     message, 'LNU_UNC must have the same number of elements as WAVE_FILTERS.'
   if min(Lnu_unc) lt 0 then message, 'LNU_UNC must only contain non-negative values.'
 endif else Lnu_unc = replicate(!values.D_nan, Nfilters)

 if n_elements(Lnu_mod) ne 0 then begin
   if size(Lnu_mod, /type) lt 2 or size(Lnu_mod, /type) gt 5 then $
     message, 'LNU_MOD must be of type int, float, or double.'
   if size(reform(Lnu_mod), /n_dim) ne 1 then message, 'LNU_MOD must be a 1-D array.'
   if n_elements(Lnu_mod) ne Nfilters then $
     message, 'LNU_MOD must have the same number of elements as WAVE_FILTERS.'
   if min(Lnu_mod) lt 0 then message, 'LNU_MOD must only contain non-negative values.'
   plot_residuals = 1
   if n_elements(Lnu_unc) eq 0 then begin
     message, 'Residuals will only be plotted if Lnu_unc and Lnu_mod are specified.', /info
     plot_residuals = 0
   endif
 endif else plot_residuals = 0

 if n_elements(xray_bandpass) ne 0 then begin
   if size(xray_bandpass, /type) lt 2 or size(xray_bandpass, /type) gt 5 then $
     message, 'XRAY_BANDPASS must be of type int, float, or double.'
   if size(reform(xray_bandpass), /n_dim) lt 1 or size(reform(xray_bandpass), /n_dim) gt 2 then $
     message, 'XRAY_BANDPASS must be a 1-D or 2-D array.'
   if min(xray_bandpass) le 0 then message, 'XRAY_BANDPASS must only contain positive values.'
   if size(xray_bandpass, /n_dim) eq 1 then Nxray = n_elements(xray_bandpass) else Nxray = (size(xray_bandpass, /dim))[1]
   xray_data = 1
 endif else xray_data = 0

 if n_elements(xray_obs) ne 0 and xray_data then begin
   if size(xray_obs, /type) lt 2 or size(xray_obs, /type) gt 5 then $
     message, 'XRAY_OBS must be of type int, float, or double.'
   if size(reform(xray_obs), /n_dim) ne 1 then $
     message, 'XRAY_OBS must be a 1-D array.'
   if n_elements(xray_obs) ne Nxray then $
     message, 'XRAY_OBS must have the same number of elements as the first dimension of XRAY_BANDPASS.'
 endif else xray_data = 0

 if n_elements(xray_unc) ne 0 and xray_data then begin
   if size(xray_unc, /type) lt 2 or size(xray_unc, /type) gt 5 then $
     message, 'XRAY_UNC must be of type int, float, or double.'
   if size(reform(xray_unc), /n_dim) ne 1 then $
     message, 'XRAY_UNC must be a 1-D array.'
   if n_elements(xray_unc) ne Nxray then $
     message, 'XRAY_UNC must have the same number of elements as the first dimension of XRAY_BANDPASS.'
   if min(xray_unc) lt 0 then message, 'XRAY_UNC must only contain non-negative values.'
 endif else if xray_data then xray_unc = replicate(!values.D_nan, Nxray)

 if n_elements(xray_mod) ne 0 and xray_data then begin
   if size(xray_mod, /type) lt 2 or size(xray_mod, /type) gt 5 then $
     message, 'XRAY_MOD must be of type int, float, or double.'
   if size(reform(xray_mod), /n_dim) ne 1 then $
     message, 'XRAY_MOD must be a 1-D array.'
   if n_elements(xray_mod) ne Nxray then $
     message, 'XRAY_MOD must have the same number of elements as the first dimension of XRAY_BANDPASS.'
   if min(xray_mod) lt 0 then message, 'XRAY_MOD must only contain non-negative values.'
   plot_xray_residuals = 1
   if n_elements(xray_unc) eq 0 then begin
     message, 'Xray residuals will only be plotted if xray_unc and xray_mod are specified.', /info
     plot_xray_residuals = 0
   endif
 endif else plot_xray_residuals = 0

 if n_elements(wave_hires) ne 0 then begin
   if size(wave_hires, /type) lt 2 or size(wave_hires, /type) gt 5 then $
     message, 'WAVE_HIRES must be of type int, float, or double.'
   if size(reform(wave_hires), /n_dim) ne 1 then message, 'WAVE_HIRES must be a 1-D array.'
   if min(wave_hires) le 0 then message, 'WAVE_HIRES must only contain positive values.'
   Nwave = n_elements(wave_hires)
   hires = 1
 endif else hires = 0

 if n_elements(Lnu_hires) ne 0 and hires then begin
   if size(Lnu_hires, /type) lt 2 or size(Lnu_hires, /type) gt 5 then $
     message, 'LNU_HIRES must be of type int, float, or double.'
   if size(reform(Lnu_hires), /n_dim) lt 1 or size(reform(Lnu_hires), /n_dim) gt 2 then $
     message, 'LNU_HIRES must be a 1-D or 2-D array.'
   if (size(Lnu_hires, /dim))[0] ne Nwave then $
     message, 'LNU_HIRES must have the same number of elements as WAVE_HIRES.'
   if size(reform(Lnu_hires), /n_dim) eq 1 then Nmodels = 1 else Nmodels = (size(Lnu_hires, /dim))[1]
 endif else hires = 0

 if n_elements(unc_range_Lnu_hires) ne 0 and hires then begin
   if size(unc_range_Lnu_hires, /type) lt 2 or size(unc_range_Lnu_hires, /type) gt 5 then $
     message, 'UNC_RANGE_LNU_HIRES must be of type int, float, or double.'
   if size(reform(unc_range_Lnu_hires), /n_dim) lt 2 or size(reform(unc_range_Lnu_hires), /n_dim) gt 3 then $
     message, 'UNC_RANGE_LNU_HIRES must be a 2-D or 3-D array.'
   if (size(unc_range_Lnu_hires, /dim))[1] ne Nwave then $
     message, 'UNC_RANGE_LNU_HIRES must have the same number of elements as WAVE_HIRES.'
   if min(unc_range_Lnu_hires) lt 0 then $
     message, 'UNC_RANGE_LNU_HIRES must only contain non-negative values.'
   if size(reform(unc_range_Lnu_hires), /n_dim) eq 2 then Nmodels_unc = 1 else Nmodels_unc = (size(unc_range_Lnu_hires, /dim))[2]
   if Nmodels ne Nmodels_unc then message, 'UNC_RANGE_LNU_HIRES must have the same number of model components as LNU_HIRES.'
 endif

 if n_elements(hires_model_names) ne 0 and hires then begin
   if size(hires_model_names, /type) ne 7 then $
     message, 'HIRES_MODEL_NAMES must be of type string.'
   if size(reform(hires_model_names), /n_dim) ne 1 then $
     message, 'HIRES_MODEL_NAMES must be a 1-D array.'
   if n_elements(hires_model_names) ne Nmodels then $
     message, 'HIRES_MODEL_NAMES must have the same number of model components as LNU_HIRES.'
 endif
 if n_elements(hires_model_names) eq 0 and hires then begin
   hires_model_names = 'Model '+string(findgen(Nmodels)+1,f='(I0)')
 endif

 if n_elements(hires_color) ne 0 and hires then begin
   if size(reform(hires_color), /n_dim) ne 1 then $
     message, 'HIRES_COLOR must be a 1-D array.'
   if n_elements(hires_color) ne Nmodels then $
     message, 'HIRES_COLOR must have the same number of model components as LNU_HIRES.'
 endif
 if n_elements(hires_color) eq 0 and hires then begin
   hires_color = transpose(interpolate(transpose(colortable(17)), [0.d:Nmodels-1.d]/(Nmodels-1.d)*256))
 endif


; Plot the data
 nu = 1.d4*!lightning_cgs.clight/wave_filters
 if hires then begin
   nu_highres = 1.d4*!lightning_cgs.clight/wave_hires
   if n_elements(unc_range_Lnu_hires) ne 0 then $
     nu_highres_fill = rebin(reform(nu_highres, 1, n_elements(nu_highres)), 2, n_elements(nu_highres))
 endif

 ; Plot high resolution spectra
 if hires then begin
   hires_plt = objarr(Nmodels)
   for i=0, Nmodels-1 do begin
     if i eq 0 then begin
       current = 1 
       overplot = 0
     endif else begin
       current = 0
       overplot = 1
     endelse
     if n_elements(unc_range_Lnu_hires) ne 0 then $
       plt_hires = FillPlot(wave_hires, nu_highres_fill*unc_range_Lnu_hires[*, *, i], linestyle='', $
                            fill_transp=65, current=current, overplot=overplot, fill_color=reform(hires_color[i, *]))
     hires_plt[i] = plot(wave_hires, nu_highres*lnu_hires[*, i], /over, name=hires_model_names[i], $
                         color=reform(hires_color[i, *]), thick=2)
   endfor
 endif else hires_plt = !null


 ; Remove Lnu_obs data that is negative or 0 since using log scale
 ;  Save those locations if we need to make upper limits
 missing_data = where(lnu_obs le 0, /null)
 nulnu_obs = nu*lnu_obs
 nulnu_obs[missing_data] = !values.D_NaN

 ; Check for uncertainties that may extend to or below 0
 low_unc = where(nu*(lnu_obs-lnu_unc) le 0, Nlow_unc, comp=pos_data, /null)
 
 ; Set range for positive values within 1 order of magnitude
 yrange=alog10(minmax([nu*(lnu_obs+lnu_unc), (nu*(lnu_obs-lnu_unc))[pos_data]]))+[-1, 1]
 yrange[0] = 10.d^floor(yrange[0])
 yrange[1] = 10.d^ceil(yrange[1])

 ; Reshape uncertainty array to (2 x Nfilters) if Nlow_unc > 0. Make lower uncertainty min yrange.
 if Nlow_unc gt 0 then begin
   nulnu_unc = rebin(reform(nu*lnu_unc, 1, Nfilters), 2, Nfilters)
   nulnu_unc[0, low_unc] = (nu*lnu_obs)[low_unc] - yrange[0]
 endif else nulnu_unc = nu*lnu_unc

 ; Plot observed data
 plt = errorplot(wave_filters, nulnu_obs, nulnu_unc, /over, /xlog, /ylog, linestyle='', name='Data', $
                 yrange=yrange, xtickunits='scientific', _extra=_extra_plot)
 position = plt.position

 if plot_residuals then begin
   xshowtext = 0 
   residual_position = [position[0], position[1], position[2], position[1]+(position[3]-position[1])*4/15]
   position = [position[0], position[1]+(position[3]-position[1])*4/15+0.02, position[2], position[3]]
   plt_temp = plot(wave_filters, nulnu_obs, /over, xshowtext=xshowtext, position=position, /nodata)
 endif

 ; Create upper limits if needed
 uplim_loc = where(lnu_unc gt 0 and lnu_obs le 0, Nuplim, /null)
 if Nuplim gt 0 then uplim=symbol(wave_filters[uplim_loc], (nu*lnu_unc)[uplim_loc], sym_text='$\downarrow$', /data)

 lgnd = LEGEND(target=[hires_plt, plt], transp=30, position=[position[2]-0.01, position[1]+0.01], $
               VERTICAL_ALIGNMENT=0, HORIZONTAL_ALIGNMENT=1)

 if xray_data then begin
   xray_obs[where(xray_obs le 0, /null)] = !values.D_NaN
   xray_nu = xray_bandpass / (!lightning_cgs.hplanck / !lightning_cgs.keV)
   xray_wave = 1.d4*!lightning_cgs.clight/xray_nu
   xray_mean_wave = mean(xray_wave, dim=1)
   xray_mean_nu = 1.d4*!lightning_cgs.clight/xray_mean_wave
   xray_wave_unc = dblarr(2, Nxray)
   xray_wave_unc[0, *] = xray_mean_wave - xray_wave[1, *]
   xray_wave_unc[1, *] = xray_wave[0, *] - xray_mean_wave 

   xray_yrange=alog10(minmax([xray_nu*xray_obs+xray_nu*xray_unc, xray_nu*xray_obs-xray_nu*xray_unc]))+[-1, 1]
   xray_yrange[0] = 10.d^floor(xray_yrange[0]) < yrange[0]
   xray_yrange[1] = 10.d^ceil(xray_yrange[1]) > yrange[1]

   plt = errorplot(xray_mean_wave, xray_mean_nu*xray_obs, xray_wave_unc, xray_mean_nu*xray_unc, /over, linestyle='', $
                   axis_style=1, yrange=xray_yrange, xshowtext=plt.xshowtext, _extra=_extra_plot)
   plt_temp = plot(xray_mean_wave, xray_mean_nu*xray_obs, /over, xshowtext=xshowtext, position=position, /nodata)

   ; Add upper axis in energy
   yaxis = axis('Y', location='right', showtext=0, target=plt)
   xray_xrange=(1.d4*!lightning_cgs.clight*(!lightning_cgs.hplanck / !lightning_cgs.keV)) / plt.xrange
   xpos = position
   plt_temp = plot(xray_mean_wave, xray_mean_nu*xray_obs, /xlog, /ylog, xrange=xray_xrange, $
                   position=[xpos[0],xpos[3],xpos[2],xpos[3]+(xpos[3]-xpos[1])], $
                   xtickdir=1, xtextpos=1, /current, axis_style=1, yshowtext=0, xtickunits='scientific', $
                   ytranspar=100, /nodata, xtitle='Observed-Frame Energy E [keV]', $
                   xTICKFONT_SIZE=plt.xTICKFONT_SIZE, yTICKFONT_SIZE=plt.yTICKFONT_SIZE)
 endif

 if plot_residuals then begin
   residuals = (Lnu_obs-Lnu_mod)/Lnu_unc
   non_nan = where(finite(residuals), /null)
   yrange_resid = replicate(max(abs([floor(residuals[non_nan]-1), ceil(residuals[non_nan]+1)])), 2)*[-0.9999,0.9999]

   plt = plot(plt.xrange, [0, 0], /current, ytitle='Residual [$\sigma$]', $
              /xlog, xtickunits='scientific', position=residual_position, $
              xrange=plt.xrange, yrange=yrange_resid, thick=2)
   plt = plot(plt.xrange, [1, 1], /over, color='light grey', thick=2)
   plt = plot(plt.xrange, [-1,-1], /over, color='light grey', thick=2)
   if n_elements(_extra_plot) gt 0 then if total(tag_names(_extra_plot) eq 'YTITLE') eq 1 then $
     _extra_plot.ytitle = 'Residual [$\sigma$]'
   plt = errorplot(wave_filters, residuals, replicate(1.d, n_elements(residuals)), $
                   /over, linestyle='', _extra=_extra_plot)

   if plot_xray_residuals then begin
     xray_residuals = (xray_obs-xray_mod)/xray_unc
     non_nan = where(finite(xray_residuals), /null)
     xray_yrange_resid = replicate(max(abs([floor(xray_residuals[non_nan]-1), ceil(xray_residuals[non_nan]+1)])), 2)*[-0.9999,0.9999]
     xray_yrange_resid[0] = 10.d^floor(xray_yrange_resid[0]) < yrange_resid[0]
     xray_yrange_resid[1] = 10.d^ceil(xray_yrange_resid[1]) > yrange_resid[1]
     plt = errorplot(xray_mean_wave, xray_residuals, xray_wave_unc, replicate(1.d, n_elements(xray_residuals)), $
                     /over, linestyle='', _extra=_extra_plot)
   endif

   plt = plot(plt.xrange, [0, 0], /over, position=residual_position, yrange=yrange_resid, /nodata)

 endif

 return, plt

end