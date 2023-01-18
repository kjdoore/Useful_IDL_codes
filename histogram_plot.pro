function histogram_plot, distribution, min=min, max=max, nbins=nbins, binsize=binsize, peak=peak, $
                         swap_axes=swap_axes, _extra=_extra_plot
;+
; Name
; ----
;   HISTOGRAM_PLOT
;
; Purpose
; -------
;   Creates a histogram of the input distribution. Historgrams are plotted using Scott's normal
;   reference rule (see https://en.wikipedia.org/wiki/Histogram) or user specified bins.
;
; Calling Sequence
; ----------------
;   ::
;
;       plt = histogram_plot(distribution [, min = , max = , nbins = , binsize = , peak = , $
;                            /swap_axes, _extra=_extra_plot])
;
; Inputs
; ------
;   ``distribution`` : int, float, or double array(Nsamples)
;       The distribution to be binned and plotted in a histogram. NaN values are ignored.
;
; Optional Inputs
; ---------------
;   ``min`` : int, float, or double scalar
;       The minimum value to consider for the histogram. (Default = ``min(distribution)``)
;   ``max`` : int, float, or double scalar
;       The maximum value to consider for the histogram. (Default = ``max(distribution)``)
;   ``nbins`` : int, float, or double scalar
;       The number of equally spaced bins to use for the histogram. If not set, then the
;       number of bins to use is determined from ``binsize``, ``min``, and ``max``; or
;       Scott's normal reference rule (see note below).
;   ``binsize`` : int, float, or double scalar
;       The width of each bin of the histogram. If not set, then the bin width is
;       determined from Scott's normal reference rule (see note below).
;   ``peak`` : int, float, or double scalar
;       The peak value to normalize the histogram. If ``peak`` is negative, the histogram
;       is inverted.
;   ``swap_axes`` : flag
;       If set, then the x and y axes are swapped, allowing for the histogram to be
;       plotted on the y axis
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
;       distribution = randomn(seed, 1000)
;       plt = histogram_plot(distribution)
;
; Notes
; -----
;   - To save the output plot, which is an IDL object, use:
;     ``plt.save,'/YOUR_FOLDER/FILE_NAME.FILE_TYPE'``
;   - See https://www.harrisgeospatial.com/docs/Save_Method.html for file types 
;     and more details on saving graphics
;   - When determining the bins: The key part of creating a histogram is the number of equally 
;     spaced bins. This function is versatile in how it determines the bins. First,
;     the function checks if ``nbins`` is specified. If not specified, then the
;     number of bins is determined from ``binsize`` by ``n_elements([min:max:binsize]) - 1``.
;     If `nbins`` nor ``binsize`` are given, then the number of bins is determined 
;     automatically using Scott's normal reference rule.
;
; Modification History
; --------------------
;   - 2021/03/31: Created (Keith Doore)
;   - 2022/01/20: Added the keyword ``swap_axes`` (Keith Doore)
;   - 2023/01/18: Updated input names to better match built in IDL histogram function (Keith Doore)
;   - 2023/01/18: Updated to require equally spaced bins (Keith Doore)
;   - 2023/01/18: Updated documentation (Keith Doore)
;   - 2023/01/18: Updated error handling (Keith Doore)
;-
 ;Compile_opt idl2
 ;On_error,2

; Error Handling
 if n_elements(distribution) eq 0 then message, 'Variable is undefined: DISTRIBUTION.'
 if size(distribution, /type) lt 2 or size(distribution, /type) gt 5 then $
   message, 'DISTRIBUTION must be of type int, float, or double.'
 if size(distribution, /n_dim) ne 1 then $
   message, 'DISTRIBUTION must be a 1-D array.'
 Nsamples = n_elements(distribution)

 if n_elements(min) ne 0 then begin
   if size(min, /type) lt 2 or size(min, /type) gt 5 then $
     message, 'MIN must be of type int, float, or double.'
   if size(min, /n_dim) ne 0 then $
     message, 'MIN must be a scalar.'
 endif else min = min(distribution, /nan)

 if n_elements(max) ne 0 then begin
   if size(max, /type) lt 2 or size(max, /type) gt 5 then $
     message, 'MAX must be of type int, float, or double.'
   if size(max, /n_dim) ne 0 then $
     message, 'MAX must be a scalar.'
   if max le min then $
     message, 'MAX must be a value greater than MIN.'
 endif else max = max(distribution, /nan)

 if n_elements(nbins) ne 0 then begin
   if size(nbins, /type) lt 2 or size(nbins, /type) gt 5 then $
     message, 'NBINS must be of type int, float, or double.'
   if size(nbins, /n_dim) ne 0 then $
     message, 'NBINS must be a scalar.'
   if nbins le 0 then $
     message, 'NBINS must be a positive value.'
 endif

 if n_elements(binsize) ne 0 then begin
   if size(binsize, /type) lt 2 or size(binsize, /type) gt 5 then $
     message, 'BINSIZE must be of type int, float, or double.'
   if size(binsize, /n_dim) ne 0 then $
     message, 'BINSIZE must be a scalar.'
   if binsize le 0 then $
     message, 'BINSIZE must be a positive value.'

   if n_elements(nbins) eq 0 then nbins = n_elements([min:max:binsize]) - 1
 endif

 if n_elements(peak) ne 0 then begin
   if size(peak, /type) lt 2 or size(peak, /type) gt 5 then $
     message, 'PEAK must be of type int, float, or double.'
   if size(peak, /n_dim) ne 0 then $
     message, 'PEAK must be a scalar.'
   if peak eq 0 then $
     message, 'PEAK must be a non-zero value.'
 endif


; Determine locations if generating automatically
  if n_elements(nbins) eq 0 then begin
    binsize = 3.49d * stddev(distribution) / (double(Nsamples))^(1/3.d)
    nbins = ceil((max - min)/binsize)
  endif

; Determine histogram for each distribution and normalize
 pdf = histogram(distribution, nbins=nbins, min=min, max=max, locations=binloc)
 if n_elements(peak) ne 0 then pdf = double(pdf)/max(double(pdf)) * peak

 if ~keyword_set(swap_axes) then begin
   plt = plot(binloc, pdf, /stair, _extra=_extra_plot)
 endif else begin
   plt = plot(pdf, binloc, /stair, _extra=_extra_plot)
 endelse

 return, plt

end