function corner_plot,distribution,distribution_labels,SQUAREROOT=SQUAREROOT,$
           NORMALIZE=NORMALIZE,FONT_SIZE=FONT_SIZE,PADDING=PADDING,WIDTH=WIDTH,HEIGHT=HEIGHT,$
           XWINDOW=XWINDOW,YWINDOW=YWINDOW,NOSHOW_CONTOUR=NOSHOW_CONTOUR,SHOW_MEDIAN=SHOW_MEDIAN,$
           CONTOUR_COLOR=CONTOUR_COLOR,CONTOUR_LEVELS=CONTOUR_LEVELS,CONTOUR_SMOOTH=CONTOUR_SMOOTH,$
           CONTOUR_THICK=CONTOUR_THICK,SHOW_DOTS=SHOW_DOTS,TICKINTERVAL=TICKINTERVAL, $
           SHOW_CORRELATE=SHOW_CORRELATE,DISTRIBUTION_COLOR=DISTRIBUTION_COLOR, $
           CORRELATE_LOCATION=CORRELATE_LOCATION,CORRELATE_COLOR=CORRELATE_COLOR, $
           DISTRIBUTION_THICK=DISTRIBUTION_THICK,CORRELATE_PADDING=CORRELATE_PADDING,INVERTED=INVERTED,$
           _EXTRA=_extra_plot
;+
; NAME:
;   CORNER_PLOT
; PURPOSE:
;   To create corner multivariate distribution plot
; EXPLANATION: 
;   Creates histograms and density plots of the input distribution
;     in a lower triangular form. Historgrams are plotted above
;     the density distributions using Scott's normal
;     reference rule (see https://en.wikipedia.org/wiki/Histogram )
;
; CALLING SEQUENCE:
;   corner_plot( distribution, distribution_labels, [/SQUAREROOT, /INVERTED, $
;          /NORMALIZE, /NOSHOW_CONTOUR, /SHOW_MEDIAN, /SHOW_DOTS, /SHOW_CORRELATE, $
;          FONT_SIZE=FONT_SIZE, PADDING=PADDING, HEIGHT=HEIGHT, WIDTH=WIDTH, $
;          XWINDOW=XWINDOW, YWINDOW=YWINDOW, CONTOUR_COLOR=CONTOUR_COLOR, $
;          CONTOUR_LEVELS=CONTOUR_LEVELS, CONTOUR_SMOOTH=CONTOUR_SMOOTH, $
;          CONTOUR_THICK=CONTOUR_THICK, TICKINTERVAL=TICKINTERVAL, $
;          DISTRIBUTION_COLOR=DISTRIBUTION_COLOR, DISTRIBUTION_THICK=DISTRIBUTION_THICK, $
;          CORRELATE_LOCATION=CORRELATE_LOCATION,CORRELATE_COLOR=CORRELATE_COLOR, $
;          CORRELATE_PADDING=CORRELATE_PADDING])
;
; INPUTS:
;   distribution - a M x N x P array of M parameters, N elements, and P distributions
;                     NOTE: if P is greater than 1 then this will allow for each P to be 
;                     overplotted on each other (See examples). If P is greater than 
;                     1 then inputs marked with a (**) can be P element vectors, but leaving
;                     them as a single value will make all distributions have the same property
;   distribution_labels - a string array of M elements giving the labels
;                            for each distribution
;
; OPTIONAL INPUTS:
;   FONT_SIZE - size of tick and label font in points 
;   PADDING   - amount of padding to add between plots in faction of total width
;   HEIGHT    - 2 element vector giving the bottom and top side of graphic in normalized units
;   WIDTH     - 2 element vector giving the left and right side of graphic in normalized units
;   XWINDOW   - value giving the x dimension of the window size
;   YWINDOW   - value giving the y dimension of the window size
;   CONTOUR_COLOR  - color to make the contour lines (**)
;   CONTOUR_LEVELS - confidence levels at which to draw the contour lines
;   CONTOUR_SMOOTH - value at which to smooth the contour lines (default = 1 -> no smoothing)
;   CONTOUR_THICK  - thickness value of the contours
;   TICKINTERVAL   - M component vector giving the intervals between major tick marks
;   DISTRIBUTION_COLOR - color to make the individual distributions (**)
;   DISTRIBUTION_THICK - thickness value of the distributions
;   CORRELATE_COLOR - color to make the correlation coefficients (**)
;   CORRELATE_PADDING - padding as fraction of total plot to add away from the corner of each 
;                          plot, where the value is placed
;   CORRELATE_LOCATION - location for placing the correlation coefficients in values of 0-3,
;                          0 is the bottom left corner, 1 is the upper left corner,
;                          2 is the upper right corner (default), and 3 is the bottom right corner (**)
;
; OPTIONAL KEYWORD INPUT:
;   /SQUAREROOT   - if set, then the number of bins is the square root of
;                     the number of data points
;   /NORMALIZE    - if set, then the histograms will be normalized to
;                     their maximum value
;   /NOSHOW_CONTOUR - if set, then the contour lines will not be drawn
;   /SHOW_MEDIAN  - if set, then the histograms will have the median marked with a vertical
;                     line, and the 2D distributions will have a dot at the median
;                     their maximum value
;   /SHOW_DOTS    - if set, then the scattered data of each distribution will
;                     be plotted
;   /SHOW_CORRELATE - prints correlation coefficients in a corner of each distribution plot
;                       given bu CORRELATE_LOCATION
;   /INVERTED     - if set, then the plots will be located in the upper right conner, rather
;                     than the default of the lower left
;
; OUTPUTS:
;   x - plot object
;
; EXAMPLE USAGE:
;   For P = 1
;     IDL> distribution = randomn(seed,3,1000)
;     IDL> name = ['x','y','z']
;     IDL> x = corner_plot(distribution,name)
;     IDL> help, x
;
;   For P > 1
;     IDL> distribution = randomn(seed,3,1000,2)
;     IDL> name = ['x','y','z']
;     IDL> x = corner_plot(distribution,name,CONTOUR_COLOR=['red','blue'],$
;               DISTRIBUTION_COLOR=['red','blue'])
;     IDL> help, x
;
; NOTES:
;   To save the output plot, which is an IDL object, use:
;      x.save,'/YOUR_FOLDER/FILE_NAME.FILE_TYPE'
;   See https://www.harrisgeospatial.com/docs/Save_Method.html for file types 
;      and more details on saving graphics
;
; REVISON HISTORY:
;   Written by K. Doore, 6/23/2020
;   Updated by K. Doore, 7/10/2020
;     - Fixed many remaining bugs
;     - Added saving image NOTES
;   Updated by K. Doore, 9/14/2020
;     - Added capabilities for distribution to be 3 dimensional, which allows for 
;         overplotting of distributions
;     - Added DISTRIBUTION_THICK and DISTRIBUTION_COLOR
;     - Added CORRELATE_COLOR, CORRELATE_LOCATION, and CORRELATE_PADDING
;   Updated by K. Doore, 1/12/2021
;     - Added the optional keyword INVERTED
;-
  Compile_opt idl2
  On_error,2

; Check for allowable type and size of inputs
  if size(distribution,/type) lt 2 or size(distribution,/type) gt 5 then begin
    print,'Distribution is incorrect data type'
    return,0
  endif 
  if size(distribution_labels,/type) ne 7 then begin
    print,'Distribution labels are not of type string'
    return,0
  endif 
  if n_elements(FONT_SIZE) gt 0 then begin
    if size(FONT_SIZE,/type) lt 2 or size(FONT_SIZE,/type) gt 5 then begin
      print,'FONT_SIZE is incorrect data type'
      return,0
    endif
    if n_elements(FONT_SIZE) ne 1 then begin
      print,'FONT_SIZE must be a single value'
      return,0
    endif
  endif else begin
    FONT_SIZE = 7 
  endelse
  if n_elements(PADDING) gt 0 then begin
    if size(PADDING,/type) lt 4 or size(PADDING,/type) gt 5 then begin
      print,'PADDING must be float or double'
      return,0
    endif
    if PADDING lt 0 or PADDING gt 1 then begin
      print,'PADDING must be between 0 and 1'
      return,0
    endif
    if n_elements(PADDING) ne 1 then begin
      print,'PADDING must be a single value'
      return,0
    endif
  endif else begin
    PADDING = 0.075 
  endelse
  if n_elements(WIDTH) gt 0 then begin
    if size(WIDTH,/type) lt 4 or size(WIDTH,/type) gt 5 then begin
      print,'Width must be float or double'
      return,0
    endif
    if min(WIDTH) lt 0 or max(WIDTH) gt 1 then begin
      print,'WIDTH must be between 0 and 1'
      return,0
    endif
    if n_elements(WIDTH) ne 2 then begin
      print,'WIDTH must be a two element vector'
      return,0
    endif
  endif else begin
    WIDTH = [0.1,0.9] 
  endelse
  if n_elements(HEIGHT) gt 0 then begin
    if size(HEIGHT,/type) lt 4 or size(HEIGHT,/type) gt 5 then begin
      print,'HEIGHT must be float or double'
      return,0
    endif
    if min(HEIGHT) lt 0 or max(HEIGHT) gt 1 then begin
      print,'HEIGHT must be between 0 and 1'
      return,0
    endif
    if n_elements(HEIGHT) ne 2 then begin
      print,'HEIGHT must be a two element vector'
      return,0
    endif
  endif else begin
    HEIGHT = [0.1,0.98]
  endelse
  if n_elements(XWINDOW) gt 0 then begin
    if size(XWINDOW,/type) lt 2 or size(XWINDOW,/type) gt 5 then begin
      print,'XWINDOW is incorrect data type'
      return,0
    endif
    if XWINDOW le 0 then begin
      print,'XWINDOW must be greater than 0'
      return,0
    endif
    if n_elements(XWINDOW) ne 1 then begin
      print,'XWINDOW must be a single value'
      return,0
    endif
  endif else begin
    XWINDOW = [800]
  endelse
  if n_elements(YWINDOW) gt 0 then begin
    if size(YWINDOW,/type) lt 2 or size(YWINDOW,/type) gt 5 then begin
      print,'YWINDOW is incorrect data type'
      return,0
    endif
    if YWINDOW le 0 then begin
      print,'YWINDOW must be greater than 0'
      return,0
    endif
    if n_elements(YWINDOW) ne 1 then begin
      print,'YWINDOW must be a single value'
      return,0
    endif
  endif else begin
    YWINDOW = [727]
  endelse
  if n_elements(CONTOUR_COLOR) gt 0 then begin
    if size(CONTOUR_COLOR,/type) ne 7 then begin
      print,'CONTOUR_COLOR is not of type string'
      return,0
    endif
  endif else begin
    CONTOUR_COLOR = 'red'
  endelse
  if n_elements(CONTOUR_LEVELS) gt 0 then begin
    if size(CONTOUR_LEVELS,/type) lt 4 or size(CONTOUR_LEVELS,/type) gt 5 then begin
      print,'CONTOUR_LEVELS must be float or double'
      return,0
    endif
    if min(CONTOUR_LEVELS) lt 0 or max(CONTOUR_LEVELS) gt 1 then begin
      print,'CONTOUR_LEVELS must be between 0 and 1'
      return,0
    endif
  endif else begin
    CONTOUR_LEVELS = 0.6827
  endelse
  if n_elements(CONTOUR_SMOOTH) gt 0 then begin
    if size(CONTOUR_SMOOTH,/type) lt 2 or size(CONTOUR_SMOOTH,/type) gt 5 then begin
      print,'CONTOUR_SMOOTH is incorrect data type'
      return,0
    endif
    if CONTOUR_SMOOTH lt 0 then begin
      print,'CONTOUR_SMOOTH must be greater than 0'
      return,0
    endif
  endif else begin
    CONTOUR_SMOOTH = 1
  endelse
  if n_elements(CONTOUR_THICK) gt 0 then begin
    if size(CONTOUR_THICK,/type) lt 2 or size(CONTOUR_THICK,/type) gt 5 then begin
      print,'CONTOUR_THICK is incorrect data type'
      return,0
    endif
    if CONTOUR_THICK lt 0 then begin
      print,'CONTOUR_THICK must be greater than 0'
      return,0
    endif
  endif else begin
    CONTOUR_THICK = 1
  endelse
  if n_elements(TICKINTERVAL) gt 0 then begin
    if size(TICKINTERVAL,/type) lt 2 or size(TICKINTERVAL,/type) gt 5 then begin
      print,'TICKINTERVAL is incorrect data type'
      return,0
    endif
    if n_elements(TICKINTERVAL) ne (size(distribution))[1] then begin
      print,'TICKINTERVAL must be same length as number of distributions'
      return,0
    endif
  endif
  if n_elements(DISTRIBUTION_COLOR) gt 0 then begin
    if size(DISTRIBUTION_COLOR,/type) ne 7 then begin
      print,'DISTRIBUTION_COLOR is not of type string'
      return,0
    endif
  endif else begin
    DISTRIBUTION_COLOR = 'black'
  endelse
  if n_elements(DISTRIBUTION_THICK) gt 0 then begin
    if size(DISTRIBUTION_THICK,/type) lt 2 or size(DISTRIBUTION_THICK,/type) gt 5 then begin
      print,'CONTOUR_THICK is incorrect data type'
      return,0
    endif
    if DISTRIBUTION_THICK lt 0 then begin
      print,'DISTRIBUTION_THICK must be greater than 0'
      return,0
    endif
  endif else begin
    DISTRIBUTION_THICK = 1
  endelse
  if n_elements(CORRELATE_LOCATION) gt 0 then begin
    if size(CORRELATE_LOCATION,/type) ne 2 then begin
      print,'CORRELATE_LOCATION must be of integer data type'
      return,0
    endif
    if min(CORRELATE_LOCATION) lt 0 or max(CORRELATE_LOCATION) gt 3 then begin
      print,'CORRELATE_LOCATION must be between 0 and 3'
      return,0
    endif
  endif else begin
    CORRELATE_LOCATION = 2
  endelse
  if n_elements(CORRELATE_PADDING) gt 0 then begin
    if size(CORRELATE_PADDING,/type) lt 4 or size(CORRELATE_PADDING,/type) gt 5 then begin
      print,'PADDING must be float or double'
      return,0
    endif
    if CORRELATE_PADDING lt 0 or CORRELATE_PADDING gt 1 then begin
      print,'PADDING must be between 0 and 1'
      return,0
    endif
    if n_elements(CORRELATE_PADDING) ne 1 then begin
      print,'PADDING must be a single value'
      return,0
    endif
  endif else begin
    CORRELATE_PADDING = 0.0
  endelse
  if n_elements(CORRELATE_COLOR) gt 0 then begin
    if size(CORRELATE_COLOR,/type) ne 7 then begin
      print,'CORRELATE_COLOR is not of type string'
      return,0
    endif
  endif else begin
    CORRELATE_COLOR = 'black'
  endelse

  size_dist = size(distribution)
  dim = size_dist[0]
  if dim eq 3 then num_dist = size_dist[3] else num_dist = 1
  if dim lt 2 or dim gt 3 then begin
    print,'Distribution array must be two-dimensional or three-dimensional'
    return,0
  endif
  
; Check if label size matches distribution size
  if size_dist[1] ne n_elements(distribution_labels) then begin
    print,'Distribution labels dimensions do not match number of parameters'
    return,0  
  endif


; Check to make sure each distribution has more than 1 unique entry
  for j=0,(num_dist-1) do begin
    for i=0,(size_dist[1]-1) do begin
      uniq_elements = n_elements(UNIQ(distribution[i,*,j], SORT(distribution[i,*,j])))
      if uniq_elements lt 2 then begin
        print,'Distribution needs more than one unique entry'
        return,0
      endif
    endfor
  endfor

; Check and fix properties that can have P elements to have P elements
  if n_elements(CONTOUR_COLOR) ne 1 and n_elements(CONTOUR_COLOR) ne num_dist then begin
    print,'CONTOUR_COLOR must have 1 or P elements' 
    return,0
  endif
  if n_elements(CONTOUR_COLOR) eq 1 then CONTOUR_COLOR=replicate(CONTOUR_COLOR,num_dist)
    
  if n_elements(DISTRIBUTION_COLOR) ne 1 and n_elements(DISTRIBUTION_COLOR) ne num_dist then begin
    print,'DISTRIBUTION_COLOR must have 1 or P elements' 
    return,0
  endif
  if n_elements(DISTRIBUTION_COLOR) eq 1 then DISTRIBUTION_COLOR=replicate(DISTRIBUTION_COLOR,num_dist)
  
  if n_elements(CORRELATE_COLOR) ne 1 and n_elements(CORRELATE_COLOR) ne num_dist then begin
    print,'CORRELATE_COLOR must have 1 or P elements' 
    return,0
  endif
  if n_elements(CORRELATE_COLOR) eq 1 then CORRELATE_COLOR=replicate(CORRELATE_COLOR,num_dist)

  if n_elements(CORRELATE_LOCATION) ne 1 and n_elements(CORRELATE_LOCATION) ne num_dist then begin
    print,'CORRELATE_LOCATION must have 1 or P elements' 
    return,0
  endif
  if n_elements(CORRELATE_LOCATION) eq 1 then CORRELATE_LOCATION=replicate(CORRELATE_LOCATION,num_dist)


; Determine number of bins to use
  N = size_dist[2]
  binsize = 3.49*stddev(distribution,dim=2)/(N)^(1/3.)
  nbins = ceil((max(distribution,dim=2)-min(distribution,dim=2))/binsize)
  if keyword_set(SQUAREROOT) then nbins = ceil(sqrt(N))
  
; Determine histogram for each distribution and normalize
  pdf = lonarr(max(nbins),size_dist[1],num_dist)
  binloc = dblarr(max(nbins),size_dist[1],num_dist)
  for j=0,(num_dist-1) do begin
    for i=0,(size_dist[1]-1) do begin
      pdf[0:(nbins[i,j]-1),i,j] = histogram(distribution[i,*,j],nbins=nbins[i,j],locations=binvals)
      binloc[0:(nbins[i,j]-1),i,j] = binvals
    endfor
  endfor
  if keyword_set(NORMALIZE) then begin
    pdf=double(pdf)/rebin(reform(max(double(pdf),dim=1),1,size_dist[1],num_dist),max(nbins),size_dist[1],num_dist)
  endif
  
; Create corner plot
  if n_elements(_extra_plot) gt 0 then if where(tag_names(_extra_plot) eq 'CURRENT') eq -1 then $
    x = window(dim=[xwindow,ywindow],_EXTRA=_extra_plot)

  range=dblarr(2,size_dist[1])
  ytitle='P('+distribution_labels+')'
  if keyword_set(NORMALIZE) then begin
    yrange=[0,1.1]
    ytitle=replicate('$P/P_{max}$',n_elements(distribution_labels))
  endif
  tot_width=width[1]-width[0]
  tot_height=height[1]-height[0]
  
  for i=0,(size_dist[1]-1) do begin
    j=0
    position=[width[0]+(tot_width/size_dist[1]*i)+(tot_width/size_dist[1]*PADDING),$
          height[1]-(tot_height/size_dist[1]*(i+1))+(tot_height/size_dist[1]*PADDING),$
          width[0]+(tot_width/size_dist[1]*(i+1)),height[1]-(tot_height/size_dist[1]*i)]
    if ~keyword_set(INVERTED) then begin
      if i eq 0 then yTICKFORMAT='' else yTICKFORMAT='(A1)' 
      if i eq (size_dist[1]-1) then xTICKFORMAT='' else xTICKFORMAT='(A1)'
      if i ne 0 then ytitle_temp=!null else ytitle_temp=ytitle[i]
      if i eq (size_dist[1]-1) then xtitle_temp=distribution_labels[i] else xtitle_temp=!null
      if n_elements(TICKINTERVAL) gt 0 then xtickinterval = TICKINTERVAL[i]
      x = plot(binloc[0:(nbins[i,j]-1),i,j],pdf[0:(nbins[i,j]-1),i,j],$
               /stair,/current,xtitle=xtitle_temp,xtickinterval=xtickinterval,$
               position=position,yTICKFORMAT=yTICKFORMAT,xTICKFORMAT=xTICKFORMAT,$
               yrange=yrange,ytitle=ytitle_temp,xTICKFONT_SIZE=FONT_SIZE,yTICKFONT_SIZE=FONT_SIZE,$
               color=DISTRIBUTION_COLOR[j],thick=DISTRIBUTION_THICK,_EXTRA=_extra_plot)
      ytitle_temp=ytitle[i]
      range[*,i]=x.xrange
      if n_elements(_extra_plot) gt 0 then if where(tag_names(_extra_plot) eq 'YMAJOR') ne -1 then major=_extra_plot.ymajor
      if i ne 0 then yaxis = AXIS(1, LOCATION='right',title=ytitle_temp,target=x,TICKFONT_SIZE=FONT_SIZE,major=major)
      if keyword_set(SHOW_MEDIAN) then begin
        x = plot(replicate(median(distribution[i,*,j]),2),x.yrange,linestyle='--',color=DISTRIBUTION_COLOR[j],/over)
      endif
    
      for j=1,num_dist-1 do begin
         x = plot(binloc[0:(nbins[i,j]-1),i,j],pdf[0:(nbins[i,j]-1),i,j],$
           /stair,/over,color=DISTRIBUTION_COLOR[j],thick=DISTRIBUTION_THICK,_EXTRA=_extra_plot)
        range[*,i]=x.xrange
     
        if keyword_set(SHOW_MEDIAN) then begin
          x = plot(replicate(median(distribution[i,*,j]),2),x.yrange,linestyle='--',color=DISTRIBUTION_COLOR[j],/over)
        endif
      endfor
    endif else begin
      if i eq 0 then xtitle_temp=distribution_labels[i] else xtitle_temp=!null
      if n_elements(TICKINTERVAL) gt 0 then xtickinterval = TICKINTERVAL[i]
      x = plot(binloc[0:(nbins[i,j]-1),i,j],pdf[0:(nbins[i,j]-1),i,j],$
               /stair,/current,xtickinterval=xtickinterval,$
               position=position,yTICKFORMAT=yTICKFORMAT,xTICKFORMAT='(A1)',$
               yrange=yrange,ytitle=ytitle[i],xTICKFONT_SIZE=FONT_SIZE,yTICKFONT_SIZE=FONT_SIZE,$
               color=DISTRIBUTION_COLOR[j],thick=DISTRIBUTION_THICK,_EXTRA=_extra_plot)
      range[*,i]=x.xrange
      if keyword_set(SHOW_MEDIAN) then begin
        x = plot(replicate(median(distribution[i,*,j]),2),x.yrange,linestyle='--',color=DISTRIBUTION_COLOR[j],/over)
      endif
      if i eq 0 then xaxis = AXIS(0, LOCATION='top',title=xtitle_temp,target=x,TICKFONT_SIZE=FONT_SIZE,AXIS_RANGE=range[*,i],$
             TICKINTERVAL=xtickinterval)

      for j=1,num_dist-1 do begin
         x = plot(binloc[0:(nbins[i,j]-1),i,j],pdf[0:(nbins[i,j]-1),i,j],$
           /stair,/over,color=DISTRIBUTION_COLOR[j],thick=DISTRIBUTION_THICK,_EXTRA=_extra_plot)
        range[*,i]=x.xrange
     
        if keyword_set(SHOW_MEDIAN) then begin
          x = plot(replicate(median(distribution[i,*,j]),2),x.yrange,linestyle='--',color=DISTRIBUTION_COLOR[j],/over)
        endif
      endfor
    endelse
  endfor


  for i=0,(size_dist[1]-2) do begin
    for j=0,(size_dist[1]-1) do begin
      if i lt j then begin
        for k=0,(num_dist-1) do begin
          if k eq 0 then begin
            if keyword_set(SHOW_DOTS) then nodata=0 else nodata=1
            if ~keyword_set(INVERTED) then begin
              position=[width[0]+(tot_width/size_dist[1]*i)+(tot_width/size_dist[1]*PADDING),$
                        height[1]-(tot_height/size_dist[1]*(j+1))+(tot_height/size_dist[1]*PADDING),$
                        width[0]+(tot_width/size_dist[1]*(i+1)),height[1]-(tot_height/size_dist[1]*j)]
              if i eq 0 then yTICKFORMAT='' else yTICKFORMAT='(A1)' 
              if i eq 0 then ytitle_temp=distribution_labels[j] else ytitle_temp=!null
              if j eq (size_dist[1]-1) then xTICKFORMAT='' else xTICKFORMAT='(A1)'
              if j eq (size_dist[1]-1) then xtitle_temp=distribution_labels[i] else xtitle_temp=!null
              if n_elements(TICKINTERVAL) gt 0 then begin
                xtickinterval = TICKINTERVAL[i]
                ytickinterval = TICKINTERVAL[j]
              endif
              xrange=range[*,i]
              yrange=range[*,j]
              dist1=distribution[i,*,k]
              dist2=distribution[j,*,k]
            endif else begin
              position=[width[0]+(tot_width/size_dist[1]*j)+(tot_width/size_dist[1]*PADDING),$
                        height[1]-(tot_height/size_dist[1]*(i+1))+(tot_height/size_dist[1]*PADDING),$
                        width[0]+(tot_width/size_dist[1]*(j+1)),height[1]-(tot_height/size_dist[1]*i)]
              yTICKFORMAT='(A1)' 
              ytitle_temp=!null
              xTICKFORMAT='(A1)'
              xtitle_temp=!null
              if n_elements(TICKINTERVAL) gt 0 then begin
                xtickinterval = TICKINTERVAL[j]
                ytickinterval = TICKINTERVAL[i]
              endif
              xrange=range[*,j]
              yrange=range[*,i]
              dist1=distribution[j,*,k]
              dist2=distribution[i,*,k]
            endelse
            
            x = plot(dist1,dist2,/current,xtitle=xtitle_temp,linestyle='',symbol='.',$
                     position=position,yTICKFORMAT=yTICKFORMAT,xTICKFORMAT=xTICKFORMAT,$
                     ytitle=ytitle_temp,xTICKFONT_SIZE=FONT_SIZE,yTICKFONT_SIZE=FONT_SIZE,$
                     xrange=xrange,yrange=yrange,nodata=nodata,xtickinterval=xtickinterval,$
                     ytickinterval=ytickinterval,color=DISTRIBUTION_COLOR[k],_EXTRA=_extra_plot)
                     
           if i eq 0 and keyword_set(INVERTED) then xaxis = AXIS(0, LOCATION='top',title=distribution_labels[j],$
             target=x,TICKFONT_SIZE=FONT_SIZE,AXIS_RANGE=range[*,j],TICKINTERVAL=xtickinterval)
           if j eq (size_dist[1]-1) and keyword_set(INVERTED) then yaxis = AXIS(1, LOCATION='right',title=distribution_labels[i],$
             target=x,TICKFONT_SIZE=FONT_SIZE,AXIS_RANGE=range[*,i],TICKINTERVAL=ytickinterval)

            if keyword_set(SHOW_MEDIAN) then begin
              x = plot(replicate(median(dist1),2),replicate(median(dist2),2),linestyle='',$
                       color=DISTRIBUTION_COLOR[k],/over,symbol='o',sym_filled=1)
            endif
            
          endif else begin
            if ~keyword_set(INVERTED) then begin
              dist1=distribution[i,*,k]
              dist2=distribution[j,*,k]
            endif else begin
              dist1=distribution[j,*,k]
              dist2=distribution[i,*,k]
            endelse

            x = plot(dist1,dist2,/over,linestyle='',symbol='.',$
                     nodata=nodata,color=DISTRIBUTION_COLOR[k],_EXTRA=_extra_plot)           
            if keyword_set(SHOW_MEDIAN) then begin
              x = plot(replicate(median(dist1),2),replicate(median(dist2),2),linestyle='',$
                       color=DISTRIBUTION_COLOR[k],/over,symbol='o',sym_filled=1)
            endif
          endelse
          
          if ~keyword_set(NOSHOW_CONTOUR) then begin
            h2d = dblarr(nbins[i,k],nbins[j,k])
            xrange = dindgen(nbins[i,k]+1)*(max(dist1)-min(dist1))/(1+nbins[i,k])+min(dist1)
            yrange = dindgen(nbins[j,k]+1)*(max(dist2)-min(dist2))/(1+nbins[j,k])+min(dist2)
            h2d_xbinsize=(max(dist1)-min(dist1))/(nbins[i,k])
            h2d_ybinsize=(max(dist2)-min(dist2))/(nbins[j,k])
            for n=0,(nbins[i,k]-1) do begin $
              for m=0,(nbins[j,k]-1) do begin $
                h2d[n,m] = n_elements(where(dist1 gt xrange[n] and dist1 lt xrange[n+1] and $
                                            dist2 gt yrange[m] and dist2 lt yrange[m+1],/null))
              endfor
            endfor
            h2d  = h2d*h2d_xbinsize*h2d_ybinsize/total(h2d*h2d_xbinsize*h2d_ybinsize)
  
            pmarg = h2d[reverse(sort(h2d))]
            pcum  = total(pmarg,/cumulative)
            level = interpol(pmarg,pcum,CONTOUR_LEVELS)
            xloc = dindgen(nbins[i,k])*(max(dist1)-min(dist1))/(nbins[i,k])+min(dist1)
            yloc = dindgen(nbins[j,k])*(max(dist2)-min(dist2))/(nbins[j,k])+min(dist2)
  
            x = contour(smooth(h2d,CONTOUR_SMOOTH),xloc,yloc,c_value=level,$
                        /over,color=CONTOUR_COLOR[k],C_LABEL_SHOW=0,c_thick=CONTOUR_THICK)
          endif
          
          if keyword_set(SHOW_CORRELATE) then begin
            if CORRELATE_LOCATION[k] eq 0 then begin
              horiz=0
              vert=1
              align=0
              vertical_align=0
              corr_padh=CORRELATE_PADDING
              corr_padv=CORRELATE_PADDING
            endif
            if CORRELATE_LOCATION[k] eq 1 then begin
              horiz=0
              vert=3
              align=0
              vertical_align=1
              corr_padh=CORRELATE_PADDING
              corr_padv=-1.d0*CORRELATE_PADDING
            endif
            if CORRELATE_LOCATION[k] eq 2 then begin
              horiz=2
              vert=3
              align=1
              vertical_align=1
              corr_padh=-1.d0*CORRELATE_PADDING
              corr_padv=-1.d0*CORRELATE_PADDING
            endif
            if CORRELATE_LOCATION[k] eq 3 then begin
              horiz=2
              vert=1
              align=1
              vertical_align=0
              corr_padh=-1.d0*CORRELATE_PADDING
              corr_padv=CORRELATE_PADDING
            endif
             text_plot=text(position[horiz]+corr_padh,position[vert]+corr_padv,$
                       string(CORRELATE(dist1,dist2),f='(f5.2)'),align=align,$
                       vertical_align=vertical_align,font_color=CORRELATE_COLOR[k])
          endif
  
        endfor
      endif
    endfor
  endfor

  return,x

end