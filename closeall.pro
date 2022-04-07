pro closeall
;+
;Name
;----
;   CLOSEALL
;
;Purpose
;-------
;   A simple procedure that closes all open IDL windows.
;-
 compile_opt idl2
 
 w = getwindows()
 if n_elements(w) gt 0 then foreach i, w do i.close

end
