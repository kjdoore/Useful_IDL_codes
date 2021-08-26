function J2000_identifier, ra, dec
;+
; NAME:
;   J2000_IDENTIFIER
; PURPOSE:
;   To generate a Jhhmmss.ss+/-ddmmss.s positional identifier for an
;   object with a given (in degrees) right ascension 'ra'
;   and declination 'dec'
;
; CALLING SEQUENCE:
;   J2000_IDENTIFIER(ra, dec)
;
; INPUT:
;   ra - Right Ascension in degrees
;   dec - Declination in degrees
;
; OUTPUT:
;   name - Identifier in Jhhmmss.ss+/-ddmmss.s format.
;
; EXAMPLE USAGE:
;   IDL> ra = 202.47
;   IDL> dec = 47.1956
;   IDL> ident = J2000_identifier(ra, dec)
;   IDL> print, ident
;
; HISTORY:
;   2021-03-31: Created (E.B. Monson)
;-
    compile_opt idl2
    on_error,2

    ; Check for allowable type and size of inputs
    if size(ra,/type) lt 2 or size(ra,/type) gt 5 then begin
        print,'Input Right Ascension is incorrect data type'
        return,!null
    endif

    if size(dec,/type) lt 2 or size(dec,/type) gt 5 then begin
        print,'Input Declination is incorrect data type'
        return,!null
    endif

    ; Get right ascension strings
    ra_hh = floor(ra / 15)
    ra_mm = floor(4 * ra - 60 * ra_hh)
    ra_ss =  3600 * ra / 15 - 3600 * ra_hh - 60 * ra_mm

    rastr = string(ra_hh, format='(I02)') + string(ra_mm, format='(I02)') + string(ra_ss, format='(F05.2)')

    if (dec ge 0) then sign = '+' else sign = '-'

    ; Get declination strings
    dec = abs(dec)
    dec_dd = floor(dec)
    dec_mm = floor(60 * (dec - dec_dd))
    dec_ss = 3600 * dec - 3600 * dec_dd - 60 * dec_mm

    decstr = sign + string(dec_dd, format='(I02)') + string(dec_mm, format='(I02)') + string(dec_ss, format='(F04.1)')

    name = 'J' + rastr + decstr
    return, name
end
