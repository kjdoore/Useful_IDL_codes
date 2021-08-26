function gelman_rubin_stat_multivariate,chains
;+
; NAME:
;	GELMAN_RUBIN_STAT_MULTIVARIATE
; PURPOSE:
;	To compute the Gelman-Rubin statistic (r_hat) of multiple multivariate Markov chains 
; EXPLANATION: 
;	Use multiple Markov chains to compute their Gelman-Rubin statistic using
;    the multivariate approach as given by Brooks and Gelman (1998).
;    Uses the within-chain variance W, and between-chain variance
;    B to compare convergence of the chains.
;
; CALLING SEQUENCE:
;	gelman_rubin_stat_multivariate(chains)
;
; INPUTS:
;	chains - a P x N x M array of M Markov chains of N elements and P
;               parameters
;;
; OUTPUTS:
;	r_hat - the Gelman-Rubin statistic of the chains, the sqrt(r_hat) should be
;            less than or equal to 1.2 if convergence of the chains was reached.
;            An exact value of 1 mean the chains are exactly the same.
;
; EXAMPLE USAGE:
;   IDL> chains = randomn(seed,10,1000,3)
;   IDL> r_hat = gelman_rubin_stat_multivariate(chains)
;   IDL> print,r_hat
;
; REVISON HISTORY:
;	Written by K. Doore, 8/26/2021
;-
  Compile_opt idl2
  On_error,2

; Check arguments
  if (n_params() ne 1 ) then begin
    print,'Syntax - gelman_rubin_stat(chains)'
    return,!null
  endif
  
; Check for allowable input chains
  if size(chains,/type) lt 2 or size(chains,/type) gt 5 then begin
    print,'Chain is incorrect data type'
    return,!null
  endif 
  size_chain = size(chains)
  dim = size_chain[0]
  if dim ne 3 then begin
    print,'Chain must be three-dimensional'
    return,!null
  endif

  N = double(size_chain[2])
  M = double(size_chain[3])
  P = double(size_chain[1])

; Check Chains are long enough
  if (N lt 10) then begin
       print,'Chains not long enough to compute r_hat'
       return,!null
  endif
  
; compute Gelman-Rubin statistic
  psi_j_t = chains
  psi_j_dot = mean(chains,dim=2)
  psi_dot_dot = mean(mean(chains,dim=2),dim=2)


  w_sum = 0.d0
  bn_sum = 0.d0
  for j = 0,M-1 do begin
    for t = 0,N-1 do begin
      w_sum += (psi_j_t[*,t,j] - psi_j_dot[*,j]) ## (psi_j_t[*,t,j] - psi_j_dot[*,j])
    endfor
    bn_sum += (psi_j_dot[*,j] - psi_dot_dot) ## (psi_j_dot[*,j] - psi_dot_dot)
  endfor

  W = 1/(M*(N-1))*w_sum
  Bn = 1/(m-1)*bn_sum

  temp = invert(W) ## Bn
  temp = (temp+transpose(temp))/2.
  lam1 = max(eigenql(temp))

  r_hat = (N-1)/N + (M+1)/M * lam1

  return, r_hat

end