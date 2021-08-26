function gelman_rubin_stat,chain
;+
; NAME:
;	GELMAN_RUBIN_STAT
; PURPOSE:
;	To compute the Gelman-Rubin statistic (r_hat) [1] of multiple Markov chains
; EXPLANATION:
;	Use multiple Markov chains to compute their Gelman-Rubin statistic, with
;    the option of including the Brooks and Gelman additional term to the
;    variance. Uses the within-chain variance W, and between-chain variance
;    B to compare convergence of the chains.
;
; CALLING SEQUENCE:
;	gelman_rubin_stat(chain)
;
; INPUTS:
;	chain - a P x N x M array of M Markov chains of N elements and P
;               parameters. Optionally, chain can be a filename pointing
;               to an IDL SAVE file containing an MCMC chain called 'chain'
;               as described above.
;
; OUTPUTS:
;	r_hat - the Gelman-Rubin statistic of the chains, the sqrt(r_hat) should be
;            less than or equal to 1.2 if convergence of the chains was reached.
;            An exact value of 1 mean the chains are exactly the same.
;
; EXAMPLE USAGE:
;   IDL> chain = randomn(seed,1000,3)
;   IDL> r_hat = gelman_rubin_stat(chain)
;   IDL> print,r_hat
;
;   IDL> chain = randomn(seed,1000,3)
;   IDL> save, chain, filename='path-to-chain.sav'
;   IDL> ; some time later...
;   IDL> restore, 'path-to-chain.sav'
;   IDL> r_hat = gelman_rubin_stat(chain)
;   IDL> print,r_hat
;
; REFERENCE:
;   [1]: http://www.stat.columbia.edu/~gelman/research/published/itsim.pdf
;
; REVISON HISTORY:
;	Written by K. Doore, 6/10/2020
;   Added reference and option to read from file, E.B. Monson, 8/26/2021
;-
  Compile_opt idl2
  On_error,2

; Check arguments
  if (n_params() ne 1 ) then begin
    print,'Syntax - gelman_rubin_stat(chain)'
    return,!null
  endif

; If `chain` is a filename then try to RESTORE it.
; If this succeeds it should define a new variable called
; `chain` with the appropriate array structure.
  if(isa(chain, /string)) then begin
      print, 'Restoring ', strtrim(chain, 2)
      restore, chain
  endif

; Check for allowable input chains
  if size(chain,/type) lt 2 or size(chain,/type) gt 5 then begin
    print,'Chain is incorrect data type'
    return,!null
  endif
  size_chain = size(chain)
  dim = size_chain[0]
  if dim gt 3 or dim lt 2 then begin
    print,'Chain must be either two or three-dimensional'
    return,!null
  endif

  if dim eq 2 then N = double(size_chain[1])
  if dim eq 3 then N = double(size_chain[2])

  if dim eq 2 then M = double(size_chain[2])
  if dim eq 3 then M = double(size_chain[3])

  if dim eq 3 then P = double(size_chain[1]) else P = 0

; Check Chains are long enough
  if (N lt 10) then begin
       print,'Chains not long enough to compute r_hat'
       return,!null
  endif

; compute Gelman-Rubin statistic
  mean_i = mean(chain,dim=dim-1)
  var_i = variance(chain,dim=dim-1)
  xbar = mean(mean_i,dim=dim-1)

  B = N*variance(mean_i,dim=dim-1)
  W = mean(var_i,dim=dim-1)

  var_hat = W*(N-1)/N + B/N
  v_hat = var_hat + B/(M*N)

  if p gt 0 then begin
    covar1 = dblarr(P)
    covar2 = dblarr(P)
  endif else begin
    covar1 = 0.d0
    covar2 = 0.d0
  endelse
  for i=0,P-1 do begin
    covar1[i]=CORRELATE(var_i[i,*],mean_i[i,*]^2, /COVARIANCE)
    covar2[i]=CORRELATE(var_i[i,*],mean_i[i,*],   /COVARIANCE)
  endfor

  var_hat_v_hat=((n-1)/n)^2/m*variance(var_i,dim=dim-1)+$
                ((m+1)/(m*n))^2*2/(m-1)*B^2+$
                2*((m+1)*(n-1))/(m*n^2)*n/m*(covar1-2*xbar*covar2)

  df=2*v_hat/var_hat_v_hat

  r_hat = (df+3)/(df+1)*v_hat/W

  return, r_hat

end
