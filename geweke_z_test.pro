pro geweke_z_test,chain,na,nb,na_start,zg,PERCENTAGE=percentage,$
     THINNING=thinning
;+
; NAME:
;	GEWEKE_Z_TEST
; PURPOSE:
;	To calculate the Geweke Z Score from the Geweke Z Score Test
; EXPLANATION: 
;	Use a Markov chain to compute its Geweke Z Score from an initial
;    segment and final segment of the input chain. The final segment
;    is always the last nb elements of the input chain, while the initial
;    segment is na elements of a starting point na_start in the chain.
;
; CALLING SEQUENCE:
;	geweke_z_test, chain, na, nb, na_start, zg, [/PERCENTAGE, THINNING=thinning]
;
; INPUTS:
;	chain    - a P x N array that is a Markov chain of N elements and P
;               parameters
;	na       - number of elements of the input chain to use as the
;               initial segment
;	nb       - number of elements of the input chain to use as the
;               final segment
;	na_start - length of the initial chain segment
;
; OPTIONAL INPUTS:
;	THINNING   - value at which to thin the chain to compute its standard
;                 deviation given as a decimal. (Default is 10%, i.e. 0.1)
;
; OPTIONAL KEYWORD INPUT:
;	PERCENTAGE - if set, then na and nb are in percentages of
;                 the input chain rather than number of elements
;
; OUTPUTS:
;	zg - the geweke z score of the chain
;
; EXAMPLE USAGE:
;   IDL> chain = randomn(seed,1000)
;   IDL> geweke_z_test,chain,100,500,0,zg
;   IDL> print,zg
;
; REVISON HISTORY:
;	Written by K. Doore, 6/10/2020
;-
  Compile_opt idl2
  On_error,2

; Check arguments
  if (N_params() LT 5 ) then begin
    print,'Syntax - geweke_z_test, chain, na, nb, na_start, zg, [/PERCENTAGE, THINNING=thinning]'
    return
  endif
  
  if n_elements(thinning) eq 0 then thinning = 0.1

; Check for allowable chain
  if size(chain,/type) lt 2 or size(chain,/type) gt 5 then begin
    print,'Chain is incorrect data type'
    return
  endif 
  size_chain = size(chain)
  dim = size_chain[0]
  if dim gt 2 or dim lt 1 then begin
    print,'Chain must be either one or two-dimensional'
    return
  endif

  if dim eq 1 then N = size_chain[1]
  if dim eq 2 then N = size_chain[2]
  
; Check for allowable na, nb, and na_start inputs
  if size(na,/type) lt 2 or size(na,/type) gt 5 then begin
    print,'na is incorrect data type'
    return
  endif 
  if size(nb,/type) lt 2 or size(nb,/type) gt 5 then begin
    print,'nb is incorrect data type'
    return
  endif 
  if size(na_start,/type) lt 2 or size(na_start,/type) gt 5 then begin
    print,'na_start is incorrect data type'
    return
  endif 
  
  if keyword_set(PERCENTAGE) then begin
    if na gt 1 or na le 0 then begin
      print,'na must be a value between 0 and 1 when using keyword PERCENTAGE'
      return
    endif
    if nb gt 1 or nb le 0 then begin
      print,'nb must be a value between 0 and 1 when using keyword PERCENTAGE'
      return
    endif
    na_nelement = N*na
    nb_nelement = N*nb
  endif else begin
    na_nelement = na
    nb_nelement = nb
  endelse
  
  if na_nelement lt 10 then begin
    print,'Initial segment is too small to compute Geweke Z Score'
    return
  endif
  if nb_nelement lt 10 then begin
    print,'Final segment is too small to compute Geweke Z Score'
    return
  endif
  if na_start lt 0 then begin
    print,'Initial segment starting point must be greater than 0'
    return
  endif


; Check that chain is long enough so that the initial segment
;  and final segment do not overlap
  nb_start = N - nb_nelement
  na_end = na_start+na_nelement
  if (na_end ge nb_start) then begin
    print,'Initial and final segments overlap'
    return
  endif
  
; Check that thinning percentage is allowable to compute standard deviation
  if thinning gt 1 or thinning le 0 then begin
    print,'THINNING must be a value between 0 and 1'
    return
  endif
  if na_nelement*thinning lt 10 then begin
    print,'Initial segment overly thinned'
    return
  endif
  if nb_nelement*thinning lt 10 then begin
    print,'Final segment overly thinned'
    return
  endif
  
; Compute Geweke Z Score
  if dim eq 1 then begin
    mean_a = mean(chain[na_start:na_end:1/thinning])
    mean_b = mean(chain[nb_start:*:1/thinning])
    var_a = variance(chain[na_start:na_end:1/thinning])
    var_b = variance(chain[nb_start:*:1/thinning])
  endif else begin
    mean_a = mean(chain[*,na_start:na_end:1/thinning],dim=2)
    mean_b = mean(chain[*,nb_start:*:1/thinning],dim=2)
    var_a = variance(chain[*,na_start:na_end:1/thinning],dim=2)/na_nelement
    var_b = variance(chain[*,nb_start:*:1/thinning],dim=2)/nb_nelement
  endelse
  zg = (mean_b-mean_a)/sqrt(var_a+var_b)

  return
end