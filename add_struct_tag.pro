function add_struct_tag, struct, tag, values, name=name, no_match=no_match
;+
;Name
;----
;   ADD_STRUCT_TAG
;
;Purpose
;-------
;   Allows for a new tag to be added to an already existing structure
;   or array of structures. The tag will be added to the end of the
;   structure. Only one tag can be added at a time.
;
;Calling Sequence
;----------------
;
;
;Inputs
;------
;   struct : structure; scalar or array
;       Input structure that is to have an additional tag added to it. The
;       structure can be either a scalar or array of structures.
;   tag : string; scalar
;       The name of new tag that is to be added to the input structure. Must
;       be a tag that is not already contained in the structure.
;   values : any; scalar or array
;       The values associated with the new tag. By default, when adding an 
;       array of values to an array of structures, the last dimension of the
;       array of values must be the same length as the array of structures.
;       Each element of the last dimension of the subarray of values will then
;       be matched to each corresponding element in the array of structures.
;       Example: 
;          IDL> values = findgen(3, 3, 4)
;          IDL> struct = replicate({a: 0.0}, 4)
;          IDL> new_struct = add_struct_tag(struct, 'b', values)
;          IDL> help, new_struct, /str
;        Results in new_struct having a new tag 'b' that is a [3, 3] array
;        for each element array element of new_struct.
;   name : string; scalar, optional
;       The optional name to assign to the new structure.
;
;Input Keywords
;--------------
;   no_match : flag
;       If set, then the default behaviour when adding an array of values to 
;       an array of structures is disabled, and the whole values array is
;       added to each element in the array of structures.
;       Example: 
;          IDL> values = findgen(3, 3, 4)
;          IDL> struct = replicate({a: 0.0}, 4)
;          IDL> new_struct = add_struct_tag(struct, 'b', values, /no_match)
;          IDL> help, new_struct, /str
;        Results in new_struct having a new tag 'b' that is a [3, 3 ,4] array
;        for each element array element of new_struct.
;
;Outputs
;-------
;   new_struct : structure
;       A structure that is a copy of the input structure with the addition
;       of the new tag and associated data.
;
;Modification History
;--------------------
;   - 2022/03/30: Created (Keith Doore)
;-
 Compile_opt idl2
 On_error,2

; Error handling
 nstruct = n_elements(struct)
 if nstruct eq 0 then message, 'Variable is undefined: STRUCT.'
 if size(struct, /type) ne 8 then message, 'STRUCT must be a structure.'
 ntags=n_elements(tag_names(struct))

 if n_elements(tag) eq 0 then message, 'Variable is undefined: TAG.'
 if size(tag, /type) ne 7 then message, 'TAG must be of type: string.'
 if size(tag, /n_dim) ne 0 then message, 'TAG must be a scalar.'
 if total(tag eq tag_names(struct)) ne 0 then $
   message, 'TAG name is already contained in STRUCT.'

 if n_elements(values) eq 0 then message, 'Variable is undefined: VALUES.'
 ndim_values = size(values, /n_dim)
 if ~keyword_set(no_match) then $
   if nstruct gt 1 and nstruct ne (size(values, /dim))[-1] then $
     message, 'VALUES must have a last dimension with the same number of elements as STRUCT.'

 if n_elements(name) ne 0 then begin
   if size(name, /type) ne 7 then $
     message, 'NAME must be of type: string.'
   if size(name, /n_dim) ne 0 then message, 'NAME must be a scalar.'
 endif


; Generate new structure with new tag added
 ; Determine the size of the values array to be added
 if keyword_set(no_match) then begin
   temp_values = values
 endif else begin
   if (nstruct eq 1 and ndim_values eq 0) or (nstruct gt 1 and ndim_values eq 1) then begin
     temp_values = (make_array(dimension=1 , type=size(values, /type)))[0]
   endif else if nstruct eq 1 and ndim_values eq 1 then begin
     temp_values = make_array(dimension=size(values, /dim) , type=size(values, /type))
   endif else begin
     temp_values = make_array(dimension=(size(values, /dim))[0:-2] , type=size(values, /type))
   endelse
 endelse

 new_struct=create_struct(struct[0], tag, temp_values, name=name)

 if nstruct gt 1 then new_struct=replicate(temporary(new_struct), nstruct)

 struct_assign,struct,new_struct,/nozero

 ; if values are matched, then values need to be assigned
 if ~keyword_set(no_match) then begin
   new_struct.(ntags) = values
 endif

 return, new_struct

end