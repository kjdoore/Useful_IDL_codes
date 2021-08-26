function galactic_extinction,input_file,wavelengths
;+
; NAME:
;	GALACTIC_EXTINCTION
; PURPOSE:
;	To compute the Galactic extinction at the desired wavelengths from 
;     an input file from http://ned.ipac.caltech.edu/extinction_calculator
; EXPLANATION: 
;	Takes an input file with Galactic extinction values at a given location
;     from NED and interpolates the extinction values at the desired wavelengths
;
; CALLING SEQUENCE:
;	galactic_extinction, input_file, wavelengths
;
; INPUTS:
;	input_file - A string containing the file name that has the Galactic 
;                  extinction data from NED
;   wavelengths - A vector of wavelengths to be interpolated
;
; OUTPUTS:
;	A_lambda_output - The extinction values at the input wavelengths in magnitudes
;
; REVISON HISTORY:
;	Written by K. Doore, 8/24/2021
;-
	
	readcol,input_file,central_wavelength,A_lambda,format='x,x,f,f,x'
	
	srt=sort(central_wavelength)
	central_wavelength=[central_wavelength[srt],4.0,4.1,4.2]
	A_lambda=[A_lambda[srt],0,0,0]
	
	A_lambda_output=interpol(A_lambda,central_wavelength,wavelengths)
	
	return,A_lambda_output

end