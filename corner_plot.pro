function corner_plot, distribution, distribution_labels, distribution_color=distribution_color, $
                      distribution_thick=distribution_thick, distribution_range=distribution_range, $
                      show_median=show_median, bin=bin, normalize=normalize, contour_levels=contour_levels, $
                      contour_smooth=contour_smooth, contour_thick=contour_thick, show_correlate=show_correlate, $
                      correlate_padding=correlate_padding, correlate_location=correlate_location, $
                      truths_values=truths_values, truths_color=truths_color, inverted=inverted, $
                      padding=padding, position=position, tickinterval=tickinterval, font_size=font_size, $
                      _extra=_extra_plot
;+
; Name
; ----
;   CORNER_PLOT
;
; Purpose
; -------
;   Creates a corner multivariate distribution plot for a sampled
;   distribution. Diagonal plots contain the histogram of each parameter
;   in the distribution, and the lower off-diagonal plots contain
;   the contours of the 2D distributions of corresponding parameters.
;
; Calling Sequence
; ----------------
;   ::
;
;       plt = corner_plot(distribution, distribution_labels, [distribution_color = , $
;                         distribution_thick = , distribution_range = , /show_median, $
;                         bin = , /normalize, contour_levels = , contour_smooth = , $
;                         contour_thick = , /show_correlate, correlate_padding = , $
;                         correlate_location = , truths_values = , truths_color = , $
;                         /inverted, padding = , position = , tickinterval = , $
;                         font_size = , _extra=_extra_plot])
;
; Inputs
; ------
;   ``distribution`` : int, float, or double array(Nparam, Nsamples, Ndist)
;       The multivariate distribution. A value of ``Ndist > 1`` indicates that
;       multiple multivariate distributions for the same parameters are included 
;       and are to be overplotted (see examples).
;   ``distribution_labels`` : string array(Nparam)
;       The labels associated with each parameter of the distribution.
;
; Optional Inputs
; ---------------
;   ``distribution_color`` : string array(Ndist) or int, float, or double array(3, Ndist)
;       The color name or RGB vector that specifies the color associated with each
;       individual distribution. (Default = ``'black'``)
;   ``distribution_thick`` : int, float, or double scalar
;       The line thickness of the histogram distributions. (Default = ``1``)
;   ``distribution_range`` : int, float, or double array(2, Nparam)
;       The plotting range of the parameters. (Default = ``minmax(distribution[*, *, 0], dim=2)``)
;   ``show_median`` : flag
;       If set, then the histograms will have the median marked with a vertical
;       dashed line, and the contour plots will have a dot at the median.
;   ``bin`` : int, float, or double array(Nparam)
;       The size of the bins of each histogram. If not set, then the bin size is
;       automatically determined using Scott's normal reference rule.
;   ``normalize`` : flag
;       If set, then the histograms will be normalized to their maximum value.
;   ``contour_levels`` : float or double array(Nlevels)
;       The confidence levels at which to draw the contour lines. Values must
;       be between 0 and 1. (Default = ``0.6827``)
;   ``contour_smooth`` : int, float, or double scalar
;       The width of the smoothing window for smoothing the contour lines. (Default = ``1``)
;   ``contour_thick``  : int, float, or double scalar
;       The line thickness of the contour lines. (Default = ``1``)
;   ``show_correlate`` : flag
;       If set, then print the correlation coefficients of the parameters in a corner 
;       of each corresponding contour plot given by ``correlate_location``.
;   ``correlate_padding`` : float or double scalar
;       The amount of padding as a fraction of total plot size to offset the correlation value
;       away from the corner of each contour plot. Value must be between 0 and 1. (Default = ``0``)
;   ``correlate_location`` : int array(Ndist)
;       The location for placing the correlation coefficients. Values must be 0, 1, 2, or 3; where
;       ``0`` is the bottom left corner, ``1`` is the upper left corner,
;       ``2`` is the upper right corner (Default), and ``3`` is the bottom right corner.
;   ``truths_values`` : int, float, or double array(Nparam)
;       The truth or reference values of each parameter to indicate on the plots. The histograms 
;       will have the value marked with a solid vertical line, and the contour plots will have
;       the value marked with a diamond.
;   ``truths_color`` : string scalar or int, float, or double array(3)
;       The color name or RGB vector that specifies the color associated with each truth value.
;       (Default = ``'red'``)
;   ``inverted`` : flag
;       If set, then the contour plots will be located in the upper right conner, rather
;       than the default of the lower left.
;   ``padding`` : float or double scalar
;       The amount of padding as a fraction of total plot size to offset between each of the plots.
;       Value must  be between 0 and 1. (Default = ``0.075``)
;   ``position`` : int, float, or double array(4)
;       The position of the graphic within the window. The coordinates [X1, Y1, X2, Y2] define 
;       the lower left and upper right corners of the graphic. Coordinates are expressed in
;       normalized units. (Default = ``[0.1, 0.1, 0.9, 0.98]``)
;   ``tickinterval`` : int, float, double array(Nparam)
;       The intervals between major tick marks for each parameter.
;   ``font_size`` : int, float, double scalar
;       The font size of the ticks and labels in points. (Default = ``7``)
;
; Output
; ------
;   ``plt`` : object
;       The plot object containing the SED plot.
;
; Examples
; --------
;   .. highlight:: idl
;   ::
;
;       ;For P = 1
;       distribution = randomn(seed, 3, 1000)
;       distribution_labels = ['x', 'y', 'z']
;       x = corner_plot(distribution, distribution_labels)
;       ;
;       ;For P > 1
;       distribution = randomn(seed, 3, 1000, 2)
;       name = ['x', 'y', 'z']
;       x = corner_plot(distribution, distribution_labels, distribution_color=['red', 'blue'])
;
; Notes
; -----
;   - To save the output plot, which is an IDL object, use:
;     ``plt.save,'/YOUR_FOLDER/FILE_NAME.FILE_TYPE'``
;   - See https://www.harrisgeospatial.com/docs/Save_Method.html for file types 
;     and more details on saving graphics
;
; Modification History
; --------------------
;   - 2020/06/23: Created (Keith Doore)
;   - 2020/07/10: Fixed many remaining bugs and added saving image notes (Keith Doore)
;   - 2020/09/14: Added capabilities for distribution to be 3 dimensional, which allows for 
;     overplotting of distributions (Keith Doore)
;   - 2020/09/14: Added capabilities for distribution to be 3 dimensional, which allows for 
;     overplotting of distributions (Keith Doore)
;   - 2020/09/14: Added ``distribution_thick`` and ``distribution_color`` (Keith Doore)
;   - 2020/09/14: Added ``correlate_color``, ``correlate_location``, and ``correlate_padding`` (Keith Doore)
;   - 2021/01/12: Added ``inverted`` keyword (Keith Doore)
;   - 2022/08/04: Added optional ``truths_values`` and ``truths_color`` inputs (Keith Doore)
;   - 2022/12/07: Added optional ``distribution_range`` input (Keith Doore)
;   - 2022/12/15: Updated documentation (Keith Doore)
;   - 2022/12/15: Removed ``show_dots`` keyword (Keith Doore)
;   - 2022/12/15: Removed ``squareroot`` keyword and replaced with ``bins`` optional input (Keith Doore)
;   - 2022/12/15: Removed ``contour_color`` and ``correlate_color`` optional inputs and tied them
;     to ``distribution_color`` (Keith Doore)
;   - 2022/12/15: Removed ``noshow_contour`` keyword (Keith Doore)
;   - 2022/12/15: Replaced ``height`` and ``width`` optional inputs with ``position`` as standard
;     in IDL plotting (Keith Doore)
;   - 2022/12/15: Removed ``xwindow`` and ``ywindow`` optional inputs. Instead rely on IDL to
;     create window if needed. (Keith Doore)
;   - 2022/12/15: Updated error handling (Keith Doore)
;-
 Compile_opt idl2
 On_error,2

; Error Handling
 if n_elements(distribution) eq 0 then message, 'Variable is undefined: DISTRIBUTION.'
 if size(distribution, /type) lt 2 or size(distribution, /type) gt 5 then $
   message, 'DISTRIBUTION must be of type int, float, or double.'
 if size(distribution, /n_dim) lt 1 or size(distribution, /n_dim) lt 3 then $
   message, 'DISTRIBUTION must be a 2-D or 3-D array.'
 Nparam = (size(distribution, /dim))[0] 
 Nsamples = (size(distribution, /dim))[1]
 if size(distribution, /n_dim) eq 3 then Ndist = (size(distribution, /dim))[2] else Ndist = 1
 
 if n_elements(distribution_labels) eq 0 then message, 'Variable is undefined: DISTRIBUTION_LABELS.'
 if size(distribution_labels, /type) ne 7 then $
   message, 'DISTRIBUTION_LABELS must be of type string.'
 if size(distribution_labels, /n_dim) ne 1 then $
   message, 'DISTRIBUTION_LABELS must be a 1-D array.'
 if n_elements(distribution_labels) ne Nparam then $
   message, 'DISTRIBUTION_LABELS must have Nparam number of elements.'

 ; Check to make sure each parameter and distribution has more than 1 unique entry
 for j=0, Ndist-1 do begin
   for i=0, Nparam-1 do begin
     uniq_elements = n_elements(uniq(distribution[i, *, j], sort(distribution[i, *, j])))
     if uniq_elements lt 2 then $
       message, 'DISTRIBUTION needs more than one unique entry for parameter: '+distribution_labels[i]
   endfor
 endfor

 if n_elements(distribution_color) ne 0 then begin
   if size(distribution_color, /type) eq 7 then begin
     if size(distribution_color, /n_dim) gt 1 then $
       message, 'DISTRIBUTION_COLOR must be a scalar or 1-D array if string color.'
     if n_elements(distribution_color) ne Ndist then $
       message, 'DISTRIBUTION_COLOR must have Ndist number of elements if a string.'
   endif else if size(distribution_color, /type) lt 2 or size(distribution_color, /type) gt 5 then begin
     if size(distribution_color, /n_dim) lt 1 or size(distribution_color, /n_dim) gt 2 then $
       message, 'DISTRIBUTION_COLOR must be a 1-D or 2-D array if an RGB color.'
     if (size(distribution_color, /dim))[0] ne 3 then $
       message, 'DISTRIBUTION_COLOR must have a first dimension of length 3 if an RGB color.'
     if size(distribution_color, /n_dim) eq 2 then if (size(distribution_color, /dim))[1] ne Ndist then $
       message, 'DISTRIBUTION_COLOR must have a last dimension of length Ndist if an RGB color.'
   endif else $
     message, 'DISTRIBUTION_COLOR must be of type string or int, float, or double.'
 endif else distribution_color = replicate('black', Ndist)

 if n_elements(distribution_thick) ne 0 then begin
   if size(distribution_thick, /type) lt 2 or size(distribution_thick, /type) gt 5 then $
     message, 'DISTRIBUTION_THICK must be of type int, float, or double.'
   if size(distribution_thick, /n_dim) ne 0 then $
     message, 'DISTRIBUTION_THICK must be a scalar.'
   if distribution_thick le 0 then $
     message, 'DISTRIBUTION_THICK must be a positive value.'
 endif

 if n_elements(distribution_range) ne 0 then begin
   if size(distribution_range, /type) lt 2 or size(distribution_range, /type) gt 5 then $
     message, 'DISTRIBUTION_RANGE must be of type int, float, or double.'
   if size(distribution_range, /n_dim) ne 2 then $
     message, 'DISTRIBUTION_RANGE must be a 2-D array.'
   if (size(distribution_range, /dim))[0] ne 2 then $
     message, 'DISTRIBUTION_RANGE must have a first dimension of length 2.'
   if (size(distribution_range, /dim))[1] ne Nparam then $
     message, 'DISTRIBUTION_RANGE must have a last dimension of length Nparam.'
 endif else distribution_range = minmax(distribution[*, *, 0], dim=2)

 if n_elements(bin) ne 0 then begin
   if size(bin, /type) lt 2 or size(bin, /type) gt 5 then $
     message, 'BIN must be of type int, float, or double.'
   if size(bin, /n_dim) ne 1 then $
     message, 'BIN must be a 1-D array.'
   if n_elements(bin) ne Nparam then $
     message, 'BIN must have Nparam number of elements.'
   if total(bin le 0) ne 0 then $
     message, 'BIN must only contain positive values.'
 endif

 if n_elements(contour_levels) ne 0 then begin
   if size(contour_levels, /type) lt 4 or size(contour_levels, /type) gt 5 then $
     message, 'CONTOUR_LEVELS must be of type float or double.'
   if size(contour_levels, /n_dim) gt 1 then $
     message, 'CONTOUR_LEVELS must be a scalar or 1-D array.'
   if total(contour_levels le 0 or contour_levels ge 1) ne 0 then $
     message, 'CONTOUR_LEVELS must only contain values between 0 and 1.'
 endif else contour_levels = 0.6827d

 if n_elements(contour_smooth) ne 0 then begin
   if size(contour_smooth, /type) lt 2 or size(contour_smooth, /type) gt 5 then $
     message, 'CONTOUR_SMOOTH must be of type int, float, or double.'
   if size(contour_smooth, /n_dim) ne 0 then $
     message, 'CONTOUR_SMOOTH must be a scalar.'
   if contour_smooth lt 0 then $
     message, 'CONTOUR_SMOOTH must be a positive value.'
 endif else contour_smooth = 1

 if n_elements(contour_thick) ne 0 then begin
   if size(contour_thick, /type) lt 2 or size(contour_thick, /type) gt 5 then $
     message, 'CONTOUR_THICK must be of type int, float, or double.'
   if size(contour_thick, /n_dim) ne 0 then $
     message, 'CONTOUR_THICK must be a scalar.'
   if contour_thick le 0 then $
     message, 'CONTOUR_THICK must be a positive value.'
 endif

 if n_elements(correlate_padding) ne 0 then begin
   if size(correlate_padding, /type) lt 4 or size(correlate_padding, /type) gt 5 then $
     message, 'CORRELATE_PADDING must be of type float or double.'
   if size(correlate_padding, /n_dim) ne 0 then $
     message, 'CORRELATE_PADDING must be a scalar.'
   if correlate_padding lt 0 or correlate_padding gt 1 then $
     message, 'CORRELATE_PADDING must be a value between 0 and 1.'
 endif else correlate_padding = 0

 if n_elements(correlate_location) ne 0 then begin
   if size(correlate_location, /type) lt 2 or size(correlate_location, /type) gt 5 then $
     message, 'CORRELATE_LOCATION must be of type int, float, or double.'
   if size(correlate_location, /n_dim) gt 1 then $
     message, 'CORRELATE_LOCATION must be a scalar or 1-D array.'
   if n_elements(correlate_location) ne Ndist then $
     message, 'CORRELATE_LOCATION must have Ndist number of elements.'
   value_check = intarr(Ndist)
   for i=0, Ndist-1 do value_check[i] = total(correlate_location[i] eq [0, 1, 2, 3])
   if total(value_check) ne Ndist then $
     message, 'CORRELATE_LOCATION must be set to either 0, 1, 2, or 3.'
 endif else correlate_location = 2

 if n_elements(truths_values) ne 0 then begin
   if size(truths_values, /type) lt 2 or size(truths_values, /type) gt 5 then $
     message, 'TRUTHS_VALUES must be of type int, float, or double.'
   if size(truths_values, /n_dim) ne 1 then $
     message, 'TRUTHS_VALUES must be a 1-D array.'
   if n_elements(truths_values) ne Nparam then $
     message, 'TRUTHS_VALUES must have Nparam number of elements.'
 endif

 if n_elements(truths_color) ne 0 then begin
   if size(truths_color, /type) eq 7 then begin
     if size(truths_color, /n_dim) ne 0 then $
       message, 'TRUTHS_COLOR must be a scalar if a string.'
   endif else if size(truths_color, /type) lt 2 or size(truths_color, /type) gt 5 then begin
     if size(truths_color, /n_dim) ne 1 then $
       message, 'TRUTHS_COLOR must be a 1-D array if an RGB color.'
     if n_elements(truths_color) ne 3 then $
       message, 'TRUTHS_COLOR must have a length of 3 if an RGB color.'
   endif else $
     message, 'TRUTHS_COLOR must be of type string or int, float, or double.'
 endif else truths_color = 'red'

 if n_elements(padding) ne 0 then begin
   if size(padding, /type) lt 4 or size(padding, /type) gt 5 then $
     message, 'PADDING must be of type float or double.'
   if size(padding, /n_dim) ne 0 then $
     message, 'PADDING must be a scalar.'
   if padding lt 0 or padding gt 1 then $
     message, 'PADDING must be a value between 0 and 1.'
 endif else padding = 0.075d

 if n_elements(position) ne 0 then begin
   if size(position, /type) lt 2 or size(position, /type) gt 5 then $
     message, 'POSITION must be of type int, float, or double.'
   if size(position, /n_dim) ne 1 then $
     message, 'POSITION must be a 1-D array.'
   if n_elements(position) ne 4 then $
     message, 'POSITION must have a length of 4.'
   if total(position lt 0 or position gt 1) ne 0 then $
     message, 'POSITION must only contain values between 0 and 1.'
 endif else position = [0.1, 0.1, 0.9, 0.98]

 if n_elements(tickinterval) ne 0 then begin
   if size(tickinterval, /type) lt 2 or size(tickinterval, /type) gt 5 then $
     message, 'TICKINTERVAL must be of type int, float, or double.'
   if size(tickinterval, /n_dim) ne 1 then $
     message, 'TICKINTERVAL must be a 1-D array.'
   if n_elements(tickinterval) ne Nparam then $
     message, 'TICKINTERVAL must have Nparam number of elements.'
   if total(tickinterval le 0) ne 0 then $
     message, 'TICKINTERVAL must only contain positive values.'
 endif

 if n_elements(font_size) ne 0 then begin
   if size(font_size, /type) lt 2 or size(font_size, /type) gt 5 then $
     message, 'FONT_SIZE must be of type int, float, or double.'
   if size(font_size, /n_dim) ne 0 then $
     message, 'FONT_SIZE must be a scalar.'
   if font_size le 0 then $
     message, 'FONT_SIZE must be a positive value.'
 endif else font_size = 7


; Determine number of bins to use
 if n_elements(bin) eq 0 then bin = 3.49d*stddev(distribution, dim=2) / (double(Nsamples))^(1/3.d)
 nbins = ceil((max(distribution, dim=2) - min(distribution, dim=2)) / bin)
 
; Determine histogram for each distribution and normalize
 pdf = lonarr(max(nbins), Nparam, Ndist)
 binloc = dblarr(max(nbins), Nparam, Ndist)
 for j=0, Ndist-1 do begin
   for i=0, Nparam-1 do begin
     pdf[0:(nbins[i, j]-1), i, j] = histogram(distribution[i, *, j], nbins=nbins[i, j], locations=binvals)
     binloc[0:(nbins[i, j]-1), i, j] = binvals
   endfor
 endfor
 if keyword_set(normalize) then $
   pdf=double(pdf)/rebin(reform(max(double(pdf), dim=1), 1, Nparam, Ndist), max(nbins), Nparam, Ndist)

  
; Create corner plot
 ; Get total width and height of plotting area
 tot_width = position[2] - position[0]
 tot_height = position[3]-position[1]

 ; Create new window if we do not want to plot on any current window
 if n_elements(_extra_plot) gt 0 then if where(tag_names(_extra_plot) eq 'CURRENT') eq -1 then $
   plt = window(_extra=_extra_plot)

 ; Create ytitles depending on if histograms are normalized
 ytitle = 'P('+distribution_labels+')'
 if keyword_set(normalize) then begin
   yrange = [0, 1.1]
   ytitle = replicate('$P/P_{max}$', Nparam)
 endif

 ; Plot the histograms along the diagonal
 for i=0, Nparam-1 do begin

   if n_elements(tickinterval) ne 0 then xtickinterval=tickinterval[i]
   position_current = [position[0] + (tot_width/Nparam * i) + (tot_width/Nparam * padding), $
                       position[3] - (tot_height/Nparam*(i+1)) + (tot_height/Nparam * padding), $
                       position[0] + (tot_width/Nparam * (i+1)), position[3] - (tot_height/Nparam * i)]

   ; Plot the first distribution for the given parameter in the current window with the specified properties
   plt = plot(binloc[0:(nbins[i, 0]-1), i, 0], pdf[0:(nbins[i, 0]-1), i, 0], /stair, /current, $
              xtitle=distribution_labels[i], ytitle=ytitle[i], xtickinterval=xtickinterval, $
              position=position_current, xrange=distribution_range[*, i], yrange=yrange, $
              xtickfont_size=font_size, ytickfont_size=font_size, color=distribution_color[0], $
              thick=distribution_thick, _extra=_extra_plot)

   ; Update axis properties to have text if needed and appropriate side (0=bottom, 1=left, 2=top, 3=right)
   if ~keyword_set(inverted) then begin
     if i eq Nparam-1 then plt['axis0'].showtext = 1 else plt['axis0'].showtext = 0
     if i ne 0 then begin
       plt['axis1'].showtext = 0
       plt['axis3'].showtext = 1
     endif
   endif else begin
     plt['axis0'].showtext = 0
   endelse

   ; Overplot the remaining distributions for the given parameter
   for j=0, Ndist-1 do begin
      if j ne 0 then plt = plot(binloc[0:(nbins[i, j]-1), i, j], pdf[0:(nbins[i, j]-1), i, j], /stair, $
                                /over, color=distribution_color[j], thick=distribution_thick, _extra=_extra_plot)
   
     ; Add a median line if specified in input
     if keyword_set(show_median) then $
       plt = plot(replicate(median(distribution[i, *, j]), 2), plt.yrange, linestyle='--', $
                  color=distribution_color[j], /over, thick=distribution_thick)
   endfor
   ; Add truth value lines if specified in input
   if n_elements(truths_values) gt 0 then $
     plt = plot(replicate(truths_values[i], 2), plt.yrange, color=truths_color, /over)
 endfor

 ; Plot the contour plots off of the diagonal
 ;   i has a max of Nparam-2 due to it being off diagonal
 for i=0, (Nparam-2) do begin
   for j=0, (Nparam-1) do begin
     if i lt j then begin

       ; If plot is inverted swap i and j values
       if ~keyword_set(inverted) then begin
         ii = i
         jj = j
       endif else begin
         ii = j
         jj = i
       endelse

       ; Set contour plot properties and position
       position_current = [position[0] + (tot_width/Nparam * ii) + (tot_width/Nparam * padding), $
                           position[3] - (tot_height/Nparam*(jj+1)) + (tot_height/Nparam * padding), $
                           position[0] + (tot_width/Nparam * (ii+1)), position[3] - (tot_height/Nparam * jj)]
       if n_elements(tickinterval) gt 0 then begin
         xtickinterval = tickinterval[ii]
         ytickinterval = tickinterval[jj]
       endif
       xrange = distribution_range[*, ii]
       yrange = distribution_range[*, jj]


       for k=0, (Ndist-1) do begin
         dist1 = distribution[ii, *, k]
         dist2 = distribution[jj, *, k]

         ; Create a blank set of axes to plot the contours
         if k eq 0 then begin           
           plt = plot(dist1, dist2, /current, /nodata, xtitle=distribution_labels[ii], ytitle=distribution_labels[jj], $
                      position=position_current, xtickfont_size=font_size, ytickfont_size=font_size, $
                      xrange=xrange, yrange=yrange, xtickinterval=xtickinterval, ytickinterval=ytickinterval, $
                      _extra=_extra_plot)

           ; Update axis properties to have text if needed and appropriate side (0=bottom, 1=left, 2=top, 3=right)
           if ~keyword_set(inverted) then begin
             ; Note that i is row and j is column
             if j eq Nparam-1 then plt['axis0'].showtext = 1 else plt['axis0'].showtext = 0
             if i eq 0 then plt['axis1'].showtext = 1 else plt['axis1'].showtext = 0
           endif else begin
             plt['axis0'].showtext = 0
             plt['axis1'].showtext = 0
             ; Note that i is now column and j is row
             if i eq 0 then plt['axis2'].showtext = 1 else plt['axis2'].showtext = 0            
             if j eq Nparam-1 then plt['axis3'].showtext = 1 else plt['axis3'].showtext = 0
           endelse
         endif

         ; Add a median diamond if specified in input
         if keyword_set(show_median) then $
          x = plot(replicate(median(dist1), 2), replicate(median(dist2), 2), linestyle='', $
                   color=distribution_color[k], /over, symbol='o', sym_filled=1, sym_size=0.5)

         ; 2-D histogram the data for generating contours
         h2d = dblarr(nbins[i, k], nbins[j, k])
         xrange = dindgen(nbins[i, k] + 1) * (max(dist1) - min(dist1)) / (1 + nbins[i, k]) + min(dist1)
         yrange = dindgen(nbins[j, k] + 1) * (max(dist2) - min(dist2)) / (1 + nbins[j, k]) + min(dist2)
         h2d_xbinsize = (max(dist1) - min(dist1)) / (nbins[i, k])
         h2d_ybinsize = (max(dist2) - min(dist2)) / (nbins[j, k])
         for n=0, (nbins[i, k]-1) do begin
           for m=0,  (nbins[j, k]-1) do begin
             h2d[n ,m] = n_elements(where(dist1 gt xrange[n] and dist1 lt xrange[n+1] and $
                                          dist2 gt yrange[m] and dist2 lt yrange[m+1], /null))
           endfor
         endfor
         ; Area normalize the 2D histogram
         h2d  = (h2d * h2d_xbinsize * h2d_ybinsize) / total(h2d * h2d_xbinsize * h2d_ybinsize)
 
         pmarg = h2d[reverse(sort(h2d))]
         pcum  = total(pmarg, /cumulative)
         level = interpol(pmarg, pcum, contour_levels)
         xloc = dindgen(nbins[i,k])*(max(dist1)-min(dist1))/(nbins[i,k])+min(dist1)
         yloc = dindgen(nbins[j,k])*(max(dist2)-min(dist2))/(nbins[j,k])+min(dist2)
 
         x = contour(smooth(h2d, contour_smooth), xloc, yloc, c_value=level, $
                     /over, color=distribution_color[k], c_label_show=0, c_thick=contour_thick)

         ; Add correlation values is specified by the input
         if keyword_set(show_correlate) then begin
           if correlate_location[k] eq 0 then begin
             horiz = 0
             vert = 1
             align = 0
             vertical_align = 0
             corr_padh = correlate_padding
             corr_padv = correlate_padding
           endif
           if correlate_location[k] eq 1 then begin
             horiz = 0
             vert = 3
             align = 0
             vertical_align = 1
             corr_padh = correlate_padding
             corr_padv = -1.d0*correlate_padding
           endif
           if correlate_location[k] eq 2 then begin
             horiz = 2
             vert = 3
             align = 1
             vertical_align = 1
             corr_padh = -1.d0*correlate_padding
             corr_padv = -1.d0*correlate_padding
           endif
           if correlate_location[k] eq 3 then begin
             horiz = 2
             vert = 1
             align = 1
             vertical_align = 0
             corr_padh = -1.d0*correlate_padding
             corr_padv = correlate_padding
           endif
           text_plt = text(position_current[horiz] + corr_padh, position_current[vert] + corr_padv, $
                           string(correlate(dist1, dist2), f='(f5.2)'), align=align, $
                           vertical_align=vertical_align, font_color=distribution_color[k])
         endif
 
       endfor
     if n_elements(truths_values) gt 0 then $
       x = plot([truths_values[i]], [truths_values[j]], linestyle='', symbol='D', /sym_filled, $
                color=truths_color, /over, sym_size=0.5)
     endif
   endfor
 endfor

 return, plt

end