function lightning_sed_plot, lght_struct, _extra=_extra_plot
;+
; Name
; ----
;	LIGHTNING_SED_PLOT
;
; Purpose
; -------
;	Creates a plot of an SED using the output structure from Lighting.
;   Values for the plot are automatically extracted from the output structure.
;
; Calling Sequence
; ----------------
;   ::
;
;       plt = lightning_sed_plot(lght_struct [, _extra=_extra_plot])
;
; Inputs
; ------
;	``lght_struct`` : structure
;       A Lightning output structure for a single SED. Can be for any fitting
;       algorithm or combination of models.
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
;   - 2022/11/10: Created (Keith Doore)
;   - 2022/11/14: Added model uncertainty to residual calculation for correct residual values (Keith Doore)
;-
 Compile_opt idl2
 On_error,2

; Check for allowable type and size of inputs
 if n_elements(lght_struct) eq 0 then message, 'Variable is undefined: LGHT_STRUCT.'
 if size(lght_struct, /type) ne 8 then message, 'LGHT_STRUCT is not of type structure.'
 if size(lght_struct, /dim) ne 1 then message, 'LGHT_STRUCT can only contain one SED at a time.'


; Extract the data from the structure
 lght_tags = strupcase(tag_names(lght_struct))

; Default outputs in structure
 bestfit = where(lght_struct.lnprob eq max(lght_struct.lnprob))

 wave_filters = lght_struct.WAVE_FILTERS
 Lnu_obs = lght_struct.LNU_OBS
 Lnu_unc = lght_struct.LNU_UNC
 Lnu_mod = (lght_struct.LNU_MOD)[*, bestfit]
 model_unc = lght_struct.model_unc
 
; Check for Xray data
 if total(lght_tags eq 'XRAY_BANDPASS') eq 1 then begin
   xray_data = 1
   xray_bandpass = lght_struct.XRAY_BANDPASS
   xray_mod = (lght_struct.LNU_XRAYMOD)[*, bestfit]
   if total(lght_tags eq 'NET_COUNTS') eq 1 then begin
     xray_obs = (lght_struct.LNU_XRAY_OBS)[*, bestfit]
     xray_unc = (lght_struct.LNU_XRAY_UNC)[*, bestfit]
   endif else begin
     xray_obs = lght_struct.LNU_XRAY_OBS
     xray_unc = lght_struct.LNU_XRAY_UNC
   endelse
 endif else xray_data = 0

; Check for types of high res models and compile data into single array
 if size(lght_struct.lnu_mod_hires, /n_dim) eq 2 then highres_unc = 1 else highres_unc = 0
 hires_model_names = 'Total Model'
 hires_color = 'gray'
 Lnu_hires_unc = !null
 if xray_data then begin
   wave_hires = [lght_struct.WAVE_XRAYMOD_HIRES, lght_struct.WAVE_HIRES]
   Lnu_hires = [(lght_struct.LNU_XRAYMOD_HIRES)[*, 0], (lght_struct.LNU_MOD_HIRES)[*, 0]]
   if highres_unc then Lnu_hires_unc = [[minmax(lght_struct.LNU_XRAYMOD_HIRES, dim=2)], [minmax(lght_struct.LNU_MOD_HIRES, dim=2)]]

   if total(lght_tags eq 'LNU_DUSTMOD_HIRES') eq 1 then begin
     hires_model_names = ['Dust Model', hires_model_names]
     hires_color = ['green', hires_color]
     Lnu_hires = [[replicate(0.d0, n_elements(lght_struct.WAVE_XRAYMOD_HIRES)), $
                   (lght_struct.LNU_DUSTMOD_HIRES)[*, 0]], [Lnu_hires]]
     if highres_unc then Lnu_hires_unc = [[[replicate(0.d0, 2, n_elements(lght_struct.WAVE_XRAYMOD_HIRES))], $
                                           [minmax(lght_struct.LNU_DUSTMOD_HIRES, dim=2)]], [[Lnu_hires_unc]]]
   endif
   if total(lght_tags eq 'LNU_AGNMOD_HIRES')  eq 1 then begin
     hires_model_names = ['AGN Model', hires_model_names]
     hires_color = ['orange', hires_color]
     if total(lght_tags eq 'LNU_XRAYMOD_AGN_HIRES')  eq 1 then begin
       Lnu_hires = [[(lght_struct.LNU_XRAYMOD_AGN_HIRES)[*, 0], (lght_struct.LNU_AGNMOD_HIRES)[*, 0]], [Lnu_hires]]
       if highres_unc then Lnu_hires_unc = [[[minmax(lght_struct.LNU_XRAYMOD_AGN_HIRES, dim=2)], $
                                             [minmax(lght_struct.LNU_AGNMOD_HIRES, dim=2)]], [[Lnu_hires_unc]]]
     endif else begin
       Lnu_hires = [[replicate(0.d0, n_elements(lght_struct.WAVE_XRAYMOD_HIRES)), $
                              (lght_struct.LNU_AGNMOD_HIRES)[*, 0]], [Lnu_hires]]
       if highres_unc then Lnu_hires_unc = [[[replicate(0.d0, 2, n_elements(lght_struct.WAVE_XRAYMOD_HIRES))], $
                                             [minmax(lght_struct.LNU_AGNMOD_HIRES, dim=2)]], [[Lnu_hires_unc]]]
     endelse
   endif
   if total(lght_tags eq 'LNU_STARMOD_HIRES') eq 1 then begin
     hires_model_names = ['Stellar Model', hires_model_names]
     hires_color = ['red', hires_color]
     Lnu_hires = [[(lght_struct.LNU_XRAYMOD_STAR_HIRES)[*, 0], (lght_struct.LNU_STARMOD_HIRES)[*, 0]], [Lnu_hires]]
     if highres_unc then Lnu_hires_unc = [[[minmax(lght_struct.LNU_XRAYMOD_STAR_HIRES, dim=2)], $
                                           [minmax(lght_struct.LNU_STARMOD_HIRES, dim=2)]], [[Lnu_hires_unc]]]
   endif
 endif else begin
   wave_hires = lght_struct.WAVE_HIRES
   Lnu_hires = (lght_struct.LNU_MOD_HIRES)[*, 0]
   if highres_unc then Lnu_hires_unc = minmax(lght_struct.LNU_MOD_HIRES, dim=2)

   if total(lght_tags eq 'LNU_DUSTMOD_HIRES') eq 1 then begin
     hires_model_names = ['Dust Model', hires_model_names]
     hires_color = ['green', hires_color]
     Lnu_hires = [[(lght_struct.LNU_DUSTMOD_HIRES)[*, 0]], [Lnu_hires]]
     if highres_unc then Lnu_hires_unc = [[[minmax(lght_struct.LNU_DUSTMOD_HIRES, dim=2)]], [[Lnu_hires_unc]]]
   endif
   if total(lght_tags eq 'LNU_AGNMOD_HIRES')  eq 1 then begin
     hires_model_names = ['AGN Model', hires_model_names]
     hires_color = ['orange', hires_color]
     Lnu_hires = [[(lght_struct.LNU_AGNMOD_HIRES)[*, 0]], [Lnu_hires]]
     if highres_unc then Lnu_hires_unc = [[[minmax(lght_struct.LNU_AGNMOD_HIRES, dim=2)]], [[Lnu_hires_unc]]]
   endif
   if total(lght_tags eq 'LNU_STARMOD_HIRES') eq 1 then begin
     hires_model_names = ['Stellar Model', hires_model_names]
     hires_color = ['red', hires_color]
     Lnu_hires = [[(lght_struct.LNU_STARMOD_HIRES)[*, 0]], [Lnu_hires]]
     if highres_unc then Lnu_hires_unc = [[[minmax(lght_struct.LNU_STARMOD_HIRES, dim=2)]], [[Lnu_hires_unc]]]
   endif
 endelse


; Plot the data
 nu = 1.d4*!lightning_cgs.clight/wave_filters
 nu_highres = 1.d4*!lightning_cgs.clight/wave_hires
 if highres_unc then $
   nu_highres_fill = rebin(reform(nu_highres, 1, n_elements(nu_highres)), 2, n_elements(nu_highres))

 ; Plot high resolution spectra
 hires_plt = objarr(n_elements(hires_model_names))
 for i=0, n_elements(hires_model_names)-1 do begin
   if i eq 0 then overplot = 0 else overplot = 1
   if highres_unc then $
     plt_hires = FillPlot(wave_hires, nu_highres_fill*Lnu_hires_unc[*, *, i], linestyle='', $
                          fill_transp=65, overplot=overplot, fill_color=hires_color[i])
   hires_plt[i] = plot(wave_hires, nu_highres*lnu_hires[*, i], /over, name=hires_model_names[i], $
                       color=hires_color[i], thick=2)
 endfor


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
                 yrange=yrange, xtickunits='scientific', ytitle='$\nuL_\nu [L_\odot]$', symbol='D', $
                 _extra=_extra_plot)
 position = plt.position

 residual_position = [position[0], position[1], position[2], position[1]+(position[3]-position[1])*4/15]
 position = [position[0], position[1]+(position[3]-position[1])*4/15+0.02, position[2], position[3]]
 plt_temp = plot(wave_filters, nulnu_obs, /over, xshowtext=0, position=position, /nodata)

 ; Create upper limits if needed
 uplim_loc = where(lnu_unc gt 0 and lnu_obs le 0, Nuplim, /null)
 if Nuplim gt 0 then uplim=symbol(wave_filters[uplim_loc], (nu*lnu_unc)[uplim_loc], sym_text='$\downarrow$', /data)

 lgnd = LEGEND(target=[hires_plt, plt], transp=30, position=[position[2]-0.01, position[1]+0.01], $
               VERTICAL_ALIGNMENT=0, HORIZONTAL_ALIGNMENT=1)

 if xray_data then begin
   Nxray = n_elements(xray_bandpass) / 2.d
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
                   axis_style=1, yrange=xray_yrange, xshowtext=0, symbol='D', _extra=_extra_plot)
   plt_temp = plot(xray_mean_wave, xray_mean_nu*xray_obs, /over, xshowtext=0, position=position, /nodata)

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


 residuals = (Lnu_obs - Lnu_mod)/sqrt(Lnu_unc^2.d + (model_unc * Lnu_mod)^2.d)
 non_nan = where(finite(residuals))
 yrange_resid = replicate(max(abs([floor(residuals[non_nan]-1), ceil(residuals[non_nan]+1)])), 2)*[-0.9999,0.9999]

 plt = plot(plt.xrange, [0, 0], /current, ytitle='Residual [$\sigma$]', $
            /xlog, xtickunits='scientific', position=residual_position, $
            xrange=plt.xrange, yrange=yrange_resid, thick=2)
 plt = plot(plt.xrange, [1, 1], /over, color='light grey', thick=2)
 plt = plot(plt.xrange, [-1,-1], /over, color='light grey', thick=2)
 plt = errorplot(wave_filters, residuals, replicate(1.d, n_elements(residuals)), symbol='D', $
                 xtitle='Observed-frame Wavelength $\lambda [\mu$m]', /over, linestyle='', _extra=_extra_plot)

 if xray_data then begin
   xray_residuals = (xray_obs - xray_mod)/sqrt(xray_unc^2.d + (model_unc * xray_mod)^2.d)
   non_nan = where(finite(xray_residuals))
   xray_yrange_resid = replicate(max(abs([floor(xray_residuals[non_nan]-1), ceil(xray_residuals[non_nan]+1)])), 2)*[-0.9999,0.9999]
   xray_yrange_resid[0] = 10.d^floor(xray_yrange_resid[0]) < yrange_resid[0]
   xray_yrange_resid[1] = 10.d^ceil(xray_yrange_resid[1]) > yrange_resid[1]
   plt = errorplot(xray_mean_wave, xray_residuals, xray_wave_unc, replicate(1.d, n_elements(xray_residuals)), $
                   /over, linestyle='', symbol='D', _extra=_extra_plot)
 endif

 plt = plot(plt.xrange, [0, 0], /over, position=residual_position, yrange=yrange_resid, /nodata)

 return, plt

end