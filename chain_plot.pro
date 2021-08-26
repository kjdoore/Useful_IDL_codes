function chain_plot,chain,chain_labels,CHAIN_ELEMENTS=CHAIN_ELEMENTS,CHI2_CHAIN=CHI2_CHAIN,$
                    _EXTRA=_extra_plot
;+
; NAME:
;	CHAIN_PLOT
; PURPOSE:
;	To plot the elements in sequential order for each parameter of an MCMC chain
; EXPLANATION: 
;	Plots an MCMC chain for an arbitrary number of elements. Can limit the
;     number of elements that can be plotted, and include a chain of chisqr.
;
; CALLING SEQUENCE:
;	chain_plot(chain, chain_labels, [CHAIN_ELEMENTS=CHAIN_ELEMENTS, $
;              CHI2_CHAIN=CHI2_CHAIN])
;
; INPUTS:
;   chain        - a M x N array of M chain parameters and N elements in 
;                    the chain
;   chain_labels - a string array of M elements giving the labels for 
;                    each parameter
;
; OPTIONAL INPUTS:
;   CHAIN_ELEMENTS - a 2 element array giving the minimum and maximum indices
;                      of the MCMC chain to consider
;   CHI2_CHAIN     - an N element vector giving the corresponding chisqr
;                      elements of the MCMC chain
;
; OUTPUT:
;   x - plot object
;
; NOTES:
;   To save the output plot, which is an IDL object, use:
;      x.save,'/YOUR_FOLDER/FILE_NAME.FILE_TYPE'
;   See https://www.harrisgeospatial.com/docs/Save_Method.html for file types 
;      and more details on saving graphics
;
; REVISON HISTORY:
;	Written by K. Doore, 8/25/2021
;-
  Compile_opt idl2
  On_error,2

; Check for allowable type and size of inputs
  if size(chain,/type) lt 2 or size(chain,/type) gt 5 then begin
    print,'chain is incorrect data type'
    return,0
  endif 
  if size(chain_labels,/type) ne 7 then begin
    print,'chain_labels are not of type string'
    return,0
  endif 

  if size(chain,/n_dim) ne 2 then begin
    print,'chain must be two-dimensional'
    return,0
  endif
  if size(chain_labels,/n_dim) ne 1 then begin
    print,'chain_labels must be one-dimensional'
    return,0
  endif
  num_params=(size(chain,/dim))[0]
  if num_params ne n_elements(chain_labels) then begin
    print,'chain_labels length does not match number of parameters in chain'
    return,0  
  endif

  if n_elements(chi2_chain) gt 0 then begin
    if size(chi2_chain,/type) lt 2 or size(chi2_chain,/type) gt 5 then begin
      print,'chi2_chain is incorrect data type'
      return,0
    endif
    if size(chi2_chain,/n_dim) ne 1 then begin
      print,'chi2_chain must be one-dimensional'
      return,0
    endif
    if size(chi2_chain,/dim) ne num_elemts then begin
      print,'chi2_chain length does not match number of elements in chain'
      return,0
    endif
    num_params += 1
  endif

  if n_elements(chain_elements) gt 0 then begin
    if size(chain_elements,/type) lt 2 or size(chain_elements,/type) gt 5 then begin
      print,'chain_elements is incorrect data type'
      return,0
    endif
    if size(chain_elements,/n_ele) ne 2 then begin
      print,'chain_elements can only have 2 elements'
      return,0
    endif
    if max(chain_elements) gt num_elemts-1 or min(chain_elements) lt -1.*num_elemts then begin
      print,'chain_elements must be within the number of elements in chain'
      return,0
    endif
  endif else begin
    chain_elements=[0,-1]
  endelse

  ; Restrict chain size and chi2_chain (if given) based on chain_elements
  chain=chain[*,chain_elements[0]:chain_elements[1]]
  if n_elements(chi2_chain) gt 0 then begin
    chi2_chain=chi2_chain[chain_elements[0]:chain_elements[1]]
  endif

  height=num_params*100.+50.
  x=window(dim=[500,height],_EXTRA=_extra_plot)
  for j=0,(num_params-1) do begin
      if j eq (num_params-1) then xtickformat='' else xtickformat='(A1)'
      if j eq (num_params-1) then xaxistitle='Iteration' else xaxistitle=''
      if n_elements(chi2_chain) gt 0 and j eq num_params-1 then begin
        x=plot(findgen(n_elements(chi2_chain))+1,chi2_chain,/current,$
                   position=[0.12, 0.95-(0.8/float(num_params)*(j+1))-0.1/float(num_params)*j,0.95,$
                             0.95-(0.8/float(num_params)*(j))-0.1/float(num_params)*j],$
                   xtitle=xaxistitle,ytitle='$\chi^2$')
      endif else begin
        x=plot(findgen(n_elements(chain[0,*]))+1,chain[j,*],/current,$
                   position=[0.12, 0.95-(0.8/float(num_params)*(j+1))-0.1/float(num_params)*j,0.95,$
                             0.95-(0.8/float(num_params)*(j))-0.1/float(num_params)*j],$
                   xtickformat=xtickformat,ystyle=2,ytitle=chain_labels[j],xtitle=xaxistitle)
      endelse
  endfor

  return,x

end