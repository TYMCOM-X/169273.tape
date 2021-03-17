$TITLE DEBPRT - Pascal Debugger print and assignment  routines.
  
$HEADER debprt

module debpr$  options nocheck, special (coercions, word, ptr);

$INCLUDE debug.typ
$INCLUDE debasm.inc (* TEMPORARY *)
  
$INCLUDE debio.inc
$INCLUDE debug.inc
$INCLUDE debref.inc
$INCLUDE debsym.inc
$PAGE print$
(* PRINT$ prints an arbitrary value described via a descriptor record.
   Parameter FORMAT_CODE has the value OCTAL_RADIX or HEX_RADIX if the
   value is either an integer or a (possibly multi_dimensional) array of
   integers which are to be printed in octal or hexadecimal, respectively.  *)

public procedure print$ (    desc:		descriptor;
			     format_code:	radix_type;
			 var status:		status_code);

const
  print_pos_max := 72;				(* length of print line *)
  blanks: packed array [1..print_pos_max] of char :=
			'                                                                        ';

var
  print_pos: pos_int;			(* column next char will print in *)
  indentation: 1..print_pos_max;		(* all printing begins at or beyond this
						   column, increased when records are printed *)


procedure print_desc (    desc:		descriptor;
			  width:	pos_int);   forward;
$PAGE new_line - in print$
(* NEW_LINE writes a new line and updates PRINT_POS *)

procedure new_line;

begin
  writ$eol;
  print_pos := 1
end;
$PAGE write_string - in PRINT$
(* WRITE_STRING writes a string to the terminal.  If necessary the terminal's
   cursor is first moved to the current indentation column.  If the text will not
   fit on the current line, then a new line is output first.  In all cases non-local
   variable PRINT_POS is updated to reflect the next print position.  All printing done by
   the routines within PRINT$ should be done via either this routine or routine
   NEW_LINE. Most of the callers of write_string force left or right justification
   or are otherwise concocting the desired text from two or more substrings -
   therefore to avoid many concatenations this routine accepts two strings at a time.  *)

procedure write_string (text_left, text_right: packed array [1..*] of char);

begin
  (* Print blanks until at current indentation column.  *)

  if print_pos < indentation then begin
    writ$str (blanks [1 : indentation - print_pos]);
    print_pos := indentation
  end;

  (* If there is not enough room left on the current line for TEXT
     then print a new line and indent up to the current indentation
     column.  *)

  if (print_pos > indentation) andif
     ((print_pos + upperbound (text_left) + upperbound (text_right) - 1) > print_pos_max) then begin
    writ$eol;
    if indentation > 1 then
      writ$str (blanks [1 : indentation - 1]);
    print_pos := indentation
  end;

  (* Write the text and update PRINT_POS. *)

  writ$str (text_left);
  writ$str (text_right);
  print_pos := print_pos + upperbound (text_left) + upperbound (text_right);
  if print_pos > (print_pos_max + 1) then
    new_line

end (* write_string *);
$PAGE put_char - in print$
(* PUT_CHAR prepares a single character for printing.  Non-printable
   characters are converted to a backslash followed be the character's
   ordinal value in octal.  The single quote character is converted
   to two single quotes.  All other characters are returned as is.  *)

function put_char (ch: char): id_string;

begin
  if (ch < ' ') or (ch > '~') then	(* non-printable char *)
    putstring (put_char, '\', ord (ch):3:o)
  else if ch = '''' then  (* printing character *)
    put_char := ''''''
  else
    put_char := ch
end (* put_char *);
$PAGE calc_field_width - in print$
(* CALC_FIELD_WIDTH is given a descriptor and returns a field width for 
   printing objects of the type described by the descriptor.  It is
   called by PRINT_ARRAY in an attempt to format the printing of 
   array elements nicely.  *)

function calc_field_width (desc: descriptor): char_range;


var 
  int_text:	id_string;
  base_typ:	typ;
  last_intsym,
  next_intsym,
  cur_intsym:	intsym;
  cur_sym:	sym;
  real_text:	id_string;


begin
  with desc, dtype do

    case dkind of 

      int_dt:
	if format_code = octal_radix then begin
	  calc_field_width := max_octal_width + 1;
$IF P10   calc_field_width := calc_field_width + 2 (* form: nnnnnn,,nnnnnn *)
	end
	else if format_code = hex_radix then
          calc_field_width := max_hex_width + 1 (* form: nnnnnnnnn *)
	else begin
	  putstring (int_text, minval);
	  calc_field_width := length (int_text);
	  putstring (int_text, maxval);
	  calc_field_width := max (calc_field_width, length (int_text)) + 1
	end;

      char_dt:
	if (minval < ord (' ')) or (maxval > ord ('~'))  then
	  calc_field_width := 7 (* form: '/nnn' *)
	else
	  calc_field_width := 5; (* form: ' ' or '''' *)

      scalar_dt:
        begin
          calc_field_width := 0;
          base_typ := deref$ptr (base_type);
          last_intsym := base_typ^.cst_list.last;
          next_intsym := base_typ^.cst_list.first;

          repeat
            cur_intsym := next_intsym;
            cur_sym := deref$ptr (cur_intsym);
            next_intsym := cur_sym^.next;
            calc_field_width := max (calc_field_width, nam (deref$ptr (cur_sym^.name))^.len)
          until cur_intsym = last_intsym;
          calc_field_width := calc_field_width + 1
        end;

      real_dt:
	calc_field_width := precision + 10;  (* blank, precision + 3 digits, sign, "." and exp. *)

      file_dt,
      pointer_dt:
	calc_field_width := 8;

      string_dt:
	calc_field_width := str_length + 4;

      subr_dt:
        calc_field_width := 0;
  
      others:
	calc_field_width := 0

    end
end (* calc_field_width *);
$PAGE print_scalar - in print$
(* PRINT_SCALAR prints an indentifier of an enumerated type.  The text
   of the id will be left justified in a field whose width is specified
   by parameter WIDTH, if the length of the text is less than WIDTH.  *)

procedure print_scalar (    desc:	descriptor;
			    width:	pos_int);
 
var
  scalar_value:	int_type;
  base_typ:	typ;
  cur_intsym,
  last_intsym:	intsym;
  cur_value:	half_word;

begin
  scalar_value := ext$scalar (desc, status);		(* get scalar's ordinal value *)
  if status in severe then return;

  base_typ := deref$ptr (desc.dtype.base_type);
  cur_intsym := base_typ^.cst_list.first;	(* first symbol of base type *)
  last_intsym := base_typ^.cst_list.last;	(* last symbol of base type *)
  cur_value := 0;

  (* Traverse list of symbol nodes for scalar type.  When the counter
     CUR_VALUE equals SCALAR_VALUE, then we've found the symbol node
     for the scalar. *)

  while (cur_value <> scalar_value) and	(* haven't found it yet *)
        (cur_intsym <> last_intsym) do begin	(* not at end of sublist *)
    cur_intsym := sym (deref$ptr (cur_intsym))^.next;
    cur_value := cur_value + 1
  end;

  (* If we've found it then get the name node and print the scalar's name.  *)

  if cur_value = scalar_value then
    with nam (deref$ptr (sym (deref$ptr (cur_intsym))^.name))^ do
      write_string (text [1 : len], blanks [1 : max (0, width - len)])
  else					(* never found a symbol node corresponding *)
    status := illegal_value			(* to scalar's value *)

end (* print_scalar *);
$PAGE print_int - in print$
(* PRINT_INT prints an integer.  If non_local FORMAT_CODE is octal_radix
   then the value will be printed as 2 octal halfwords seperated by
   commas.  If its hex_radix, the value will be printed as 9 hex digits.
   The text will be right justified in a field of size specified by
   parameter WIDTH if the length of the text is less than WIDTH.  *)

procedure print_int (desc:	descriptor;
		     width:	pos_int);

var
  int_value:	int_type;
  int_text:	id_string;

begin
  int_value := ext$scalar (desc, status);
  if status in severe then return;
  
  case format_code of
    octal_radix: begin
      putstring (int_text, int_value:max_octal_width:o);
$IF P10
      if substr (int_text, 1, 6) = '000000' then
	int_text := substr (int_text, 7, 6)
      else
	int_text := substr (int_text, 1, 6) || ',,' || substr (int_text, 7, 6)
$ENDIF
    end;

    hex_radix:	(* hexadecimal formatting *)
      putstring (int_text, int_value:max_hex_width:h);

    decimal_radix:				(* decimal formatting *)
      putstring (int_text, int_value: width);

    others:
      a$$ert (false)
  end;
  write_string (blanks [1 : max (0, width - length (int_text))], int_text)
end (* print_int *);
$PAGE print_char - in print$
(* PRINT_CHAR prints a character, bracketing it with single quote characters.
   The character is printed according to the formatting rules of procedure
   PUT_CHAR.  The text printed is left justified in a field whose width is
   specified by parameter WIDTH, if the length of the text is less than WIDTH.  *)

procedure print_char (    desc:		descriptor;
			  width:	pos_int);

var
  ord_char:  int_type;
  char_text: id_string;

begin
  ord_char := ext$scalar (desc, status);
  if status in severe then return;
  
  if (ord_char < ord (minimum (char))) or (ord_char > ord (maximum (char))) then
    status := illegal_value
  
  else begin
    char_text := '''' || put_char (chr (ord_char)) || '''';	(* format the char *)
    write_string (char_text, blanks [1 : max (0, width - length (char_text))])
  end
end (* print_char *);
$PAGE print_real - in print$
(* PRINT_REAL - prints a real value.  The text will be right justified in a field of
   size specified by parameter WIDTH if the length of the text is less than WIDTH. *)

procedure print_real (desc:	descriptor;
		      width:	pos_int);

var
  temp_real,
  real_value:	real_type;
  real_text:	id_string;
  temp_width:	pos_int;
  frac_digits:	int_type;
  go_expo:	boolean;
  real_template: packed record
		   case boolean of
		     true:  (real_num: real_type);
		     false: (negative: boolean;
			     exponent: 0..377b;
			     first_frac_bit: boolean;
			     rest_of_frac: 0..377777777b;
			     ignored: boolean;
			     frac_extension: 0..377777777777b);
		 end;

begin
  if width > 0 then
    temp_width := width
  else
    temp_width := desc.dtype.precision + 9; (* imitate calc_field_width but w/o leading blank *)
  real_value := ext$real (desc, status);
  if status in severe then return;

  temp_real := abs (real_value);
  with real_template do begin
    real_num := temp_real;
    if ((not first_frac_bit and (rest_of_frac = 0) and (frac_extension = 0)) and (exponent <> 0))
        or
       ((first_frac_bit or (rest_of_frac <> 0) or (frac_extension <> 0)) and (negative = first_frac_bit)) then begin
      status := bad_real; (* unnormalized, or zero frac. with nonzero exp. *)
      return
    end
  end;
  frac_digits := desc.dtype.precision + 3;
  go_expo := false; (* assume it will fit with "F" format *)
  
  if (temp_real < 0.1) and (temp_real > 0.0) then (* leading zeroes in fraction *) begin
    repeat
      frac_digits := frac_digits + 1;
      temp_real := temp_real * 10.0
    until temp_real >= 0.1;
    if frac_digits > (temp_width - 3) then
      go_expo := true  (* won't fit - force exponential format *)
  end
  
  else if temp_real >= 1.0 then (* significant digits left of decimal *) begin
    repeat
      frac_digits := frac_digits - 1;
      temp_real := temp_real / 10.0
    until temp_real < 1.0;
    if frac_digits < 0 then
      if (desc.dtype.precision + 3 - frac_digits) > (temp_width - 3) then
        go_expo := true (* won't fit - force exponential format *)
      else
        frac_digits := 0 (* don't annoy runtime with negative fraction width *)
  end;
  
  if go_expo then
    putstring (real_text, real_value: temp_width: desc.dtype.precision + 3: E)
  else
    putstring (real_text, real_value: temp_width: frac_digits);
  write_string (real_text, '')
end (* print_real *);
$PAGE print_pointer - in print$
(* PRINT_POINTER prints a pointer.  If the length of the text to be printed
   is less than parameter WIDTH, then the pointer is right justified in
   a field of width WIDTH.  *)

procedure print_pointer (desc:  descriptor;
			 width: pos_int);

var
  ptr_value:	int_type;
  ptr_text:	id_string;

begin
  ptr_value := ext$address (desc, status);
  if status in severe then return;
  ptr_text := '';
  
  if ptr_value = ord (nil) then
    if desc.dkind = pointer_dt then
      ptr_text := 'NIL'
    else
      ptr_text := 'NILF'
  else if address_radix = octal_radix then
    putstring (ptr_text, ptr_value:max_octal_width:o)
  else if address_radix = hex_radix then
    putstring (ptr_text, ptr_value:max_hex_width:h)
  else
    a$$ert (false);
  
  ptr_text := substr (ptr_text, verify (ptr_text, ['0'], length (ptr_text)));

  write_string (blanks [1 : max (0, width - length (ptr_text))], ptr_text)
end (* print_pointer *);
$PAGE print_set - in print$
(* PRINT_SET prints a set represented by a descriptor.  *)

procedure print_set (desc: descriptor);

type
  automaton_state = (previous_bit_not_set, first_bit_of_range, within_range, after_range);

const
  transition_table: packed array [automaton_state, boolean] of automaton_state := (
	(*				present bit off        present bit on   *)
	(*				---------------        --------------   *)
	(* previous_bit_not_set: *)  (previous_bit_not_set,  first_bit_of_range),
	(* first_bit_of_range:   *)  (previous_bit_not_set,  within_range      ),
	(* within_range:         *)  (after_range,           within_range      ),
	(* after_range:          *)  (previous_bit_not_set,  first_bit_of_range)  );

var
  state:	automaton_state;
  prefix:	char;
  elem_desc:	descriptor;
  idx:		set_range;

begin
  write_string ('[', '');

  if desc.dtype.set_element_type <> ord (nil) then begin
    mak$scalar$desc (elem_desc, desc.dtype.set_element_type, 0 (* filled later *));

    (* The following loop examines the bits of the set, printing those set
       elements corresponding to the bits that are set.  Where more than one bit
       is set in a row, range notation "x..y" is used.  *)

    prefix := char (0);
    state := previous_bit_not_set; (* initialize our little automaton *)

    for idx := desc.set_min to desc.set_max do begin
      state := transition_table [state, ext$setelem (desc, idx)];
      if state = first_bit_of_range then begin
	write_string (prefix, '');
	prefix := ',';
	elem_desc.value.scalar_val [1] := idx;
	print_desc (elem_desc, 0)
      end
      else if state = after_range then begin
	write_string ('..', '');
	elem_desc.value.scalar_val [1] := idx - 1;
	print_desc (elem_desc, 0)
      end;
      if status in severe then return
    end;
      if state=within_range then
	begin write_string('..','');
	  elem_desc.value.scalar_val[1]:=desc.set_max;
	  print_desc(elem_desc,0)
	end
  end (* if set not empty *);
   
  write_string (']', '');
  new_line					(* sets always printed one per line *)
end (* print_set *);
$PAGE print_string - in print$
(* PRINT_STRING prints a string described by a STRING_DT or SUBSTR_DT desriptor.  *)

procedure print_string (desc:  descriptor;
			width: pos_int);

var
  len, i, text_width:	char_range;
  char_text:		id_string;

begin
  len := ext$lenstr (desc);

  (* catch string length word problems *)

  if (desc.dtype.flexible and (len > 1024 (* arbitrarily chosen *)))
       or (len > desc.dtype.str_length) or (len < 0) then begin
    writ$str ('String''s length word appears to be invalid - its value is ');
    writ$int (len, decimal_radix);
    writ$nl (' (decimal).');
    if len > 0 then
      writ$nl ('Use substring notation to examine text of string.');
    status := bad_length_word;
    return  (* <-- return *)
  end;

  if (print_pos > indentation) andif	(* room left on current line? *)
     (print_pos + width > print_pos_max + 1) then new_line;

  write_string ('''', '');
  text_width := 2;
  for i := 1 to len do begin
    char_text := put_char (ext$strchar (desc, i));
    text_width := text_width + length (char_text); (* more than 1 if nonprinting char *)
    write_string (char_text, '')
  end;

  (* write closing quote, and enough blanks to left justify if we haven't filled
     up the required width (and being careful not to overrun the available space)  *)

  write_string ('''',
		blanks [1 : max (0, min (width, print_pos_max - indentation + 1) - text_width)])

end (* print_string *);
$PAGE print_array - in print$
(* PRINT_ARRAY prints an array or slice of an array, given a descriptor
   of kind ARRAY_DT or SLICE_DT.  *)

procedure print_array (desc: descriptor);

var
  elem_desc:	descriptor;
  i:		int_type;
  elem_width:	bit_range;

begin

  (* create descriptor for element type *)

  init$desc (elem_desc);
  set$kind (elem_desc, desc.dtype.element_type);
  elem_desc.dtype.size := desc.dtype.element_size;
  if elem_desc.dkind in [array_dt, string_dt] then
    set$bounds (elem_desc, ord (nil), nil);
  elem_width := calc_field_width (elem_desc);

  (* print each element *)

  for i := desc.lower_bnd to desc.upper_bnd do begin
    elem_desc.addr := get$element$addr (desc, i);
    print_desc (elem_desc, elem_width);
    if status in severe then return;
    if elem_desc.dkind in [record_dt, subr_dt] then
      new_line			(* newline for readability *)
  end;

  new_line					(* newline always printed at end of array *)
end (* print_array *);
$PAGE print_record - in print$
(* PRINT_RECORD prints a record.  Each field is printed on a new
   line, with the value following the field name.  Non-local var
   INDENTATION is incremented by 3 before each field value is
   printed and decremented by 3 after the value is printed.  An
   error code will be returned if an attempt is made to print a
   record containing an undiscriminated union.  *)

procedure print_record (desc: descriptor);

var
  field_intsym:		intsym;
  rec_var_inttyp:	inttyp;
  first:		boolean;
  field_sym:		sym;
  field_intnam:		intnam;
  field_inttyp:		inttyp;
  field_desc:		descriptor;
  field_text:		id_string;
$PAGE find_variant - in print_record in print$
(D_VARIANT finds a variant type node corresponding to a given
   tag field value.  RECVAR is the type node for the record or
   variant containing the desired variant.  Ord (NIL) is returned
   if no variant corresponding to the tag field value can be found.  *)

function find_variant (recvar:    inttyp;
		       tag_value: int_type): inttyp;

var
  recvar_typ:	  typ;
  cur_var_inttyp: inttyp;

begin
  find_variant := ord (nil);			(* updated if variant found or OTHERS case found *)
  recvar_typ := deref$ptr (recvar);
  cur_var_inttyp := typ (deref$ptr (recvar_typ^.variant_tag))^.first_variant;

  while cur_var_inttyp <> ord (nil) do begin
    with typ (deref$ptr (cur_var_inttyp))^ do begin
      if others_var
        then find_variant := cur_var_inttyp	(* if others, set return value but keep looking *)
      else if (minlab <= tag_value) and (tag_value <= maxlab) then begin
        find_variant := cur_var_inttyp;
        return					(* <-- return if specific variant found *)
      end;
      cur_var_inttyp := next_variant
    end
  end
end (* find_variant *);
$PAGE get_next_field - in print_record in print$
(* GET_NEXT_FIELD returns the symbol table offset of the next
   symbol node of the record currently being printed.  Parameter
   FIELD_INTSYM initially is set to the symbol node of the last 
   symbol printed.  It is updated to point to the next field
   symbol of the record.  If FIELD_INTSYM is initially NIL then the
   symbol table offset of the first symbol of the record is
   returned.  RECVAR_INTTYP initially should contain the symbol table 
   offset of the record or variant type node for the record or variant 
   containing the current field.  It is updated, if necessary, to
   point to the type node for the record or variant containing the
   next field symbol.  An error code is returned if an undiscriminated
   union is encountered.  *)

procedure get_next_field (var field_intsym:	intsym;
			  var recvar_inttyp:	inttyp);

var
  next_intsym:	intsym;
  next_recvar:	inttyp;
  variant_tag:	inttyp;
  tag_field:	intsym;
  tag_value:	int_type;
  temp_recvar:	inttyp;
  temp_typ:	typ;

begin
  
  (* if FIELD_INTSYM is NIL then RECVAR_INTTYP should be the entire
     record's type node.  In that case just get the first symbol of
     the record.  *)

  if field_intsym = ord (nil) then begin	(* just get first field of record *)
    field_intsym := typ (deref$ptr (recvar_inttyp))^.field_list;	(* field_list is nil only if record begins *)
    if field_intsym <> ord (nil) then
      if sym (deref$ptr (field_intsym))^.fld_variant <> recvar_inttyp then
        status := undiscriminated_union;	(* ERROR: record begins with undiscr. union *)
    return  (* <-- return *)
  end;

  (* Get the next (i.e. not the first) field's symbol node. *)

  next_intsym := sym (deref$ptr (field_intsym))^.next;		(* symbol node *)

  (* if NEXT field is NIL, then no fields remain.  Otherwise check to
     see if the next field is in the same variant as the current field.  *)

  if next_intsym <> ord (nil) then begin
    next_recvar := sym (deref$ptr (next_intsym))^.fld_variant;

    (* if next field is in the same variant as the current one, then
       NEXT_INTSYM must be correct; otherwise we must find the correct
       new variant, if any, and use its first field. *)

    if next_recvar <> recvar_inttyp then begin

      (* If the variant has changed, but the current variant has no
	 subvariants then we must be done! *)

      variant_tag := typ (deref$ptr (recvar_inttyp))^.variant_tag;
      if variant_tag = ord (nil) then
	next_intsym := ord (nil)

      (* Variant has changed and old variant had subvariants, thus unless
	 we have an undiscriminated union, the current field must be the
	 tag field.  *)

      else begin
	tag_field := typ (deref$ptr (variant_tag))^.tag_field;
	if tag_field <> field_intsym then
	  status := undiscriminated_union
	else begin

	  (* O.K., it's not an undiscriminated union, so the current field
	     must be the tag field.  Get the tag value from non_local var
	     FIELD_DESC and use it to find the correct variant type node.
	     From the variant type node we can get the variant's first field. *)

	  tag_value := ext$scalar (field_desc, status);
	  temp_recvar := find_variant (recvar_inttyp, tag_value);
	  if temp_recvar = ord (nil) then	(* no variant selected *)
	    next_intsym := ord (nil)
	  else begin				(* valid variant found *)
	    temp_typ := deref$ptr (temp_recvar);
	    next_intsym := temp_typ^.field_list;

	    (* If NEXT_INTSYM is NIL either we have a null variant, which is
	       fine, or we have an undiscriminated union (an error). *)

	    if next_intsym = ord (nil) then begin
	      if temp_typ^.variant_tag <> ord (nil) then
		status := undiscriminated_union;
	    end

	    (* NEXT_INTSYM is not NIL so find its variant type node.
	       We cannot use TEMP_RECVAR - if there are multiple case
	       labels on a variant, then there are multiple variant type
	       records.  For our change of variant checking to work, we
	       must always have the first variant record for the
	       particular variant.  Field FLD_VARIANT always points to
	       the first variant record.  *)

	    else
	      recvar_inttyp := sym (deref$ptr (next_intsym))^.fld_variant
	  end  (* else *);
	end  (* else *);
      end  (* else *);
    end  (* if *);
  end  (* if *);

  (* Finally NEXT_INTSYM becomes the 'current' field symbol. *)

  field_intsym := next_intsym
end (* get_next_field *);
$PAGE print_record - body in print$
begin						(* body of print_record *)
  field_intsym := ord (nil);			(* this 'primes' GET_NEXT_FIELD *)
  rec_var_inttyp := desc.type_ptr;		(* current record/variant is entire record *)
  first := true;

  (* Print one field per iteration of while loop. *)

  while first or (field_intsym <> ord (nil)) do begin
    first := false;
    get_next_field (field_intsym, rec_var_inttyp);	(* get symbol node for next field *)
    if status in severe then return;
    
    if field_intsym <> ord (nil) then begin	(* if not yet done... *)
      
      (* Initialize a descriptor for the field. *)

      init$desc (field_desc);
      field_sym := deref$ptr (field_intsym);
      field_intnam := field_sym^.name;
      field_inttyp := field_sym^.type_desc;
      field_desc.addr := set$field$addr (field_intsym, desc.addr);	(* get field address *)
      set$kind (field_desc, field_inttyp);	(* set field type info *)
      if field_desc.dkind in [array_dt, string_dt] then
        set$bounds (field_desc, ord (nil), nil);
      set$size (field_intsym, field_desc);	(* set allocated size *)

      (* Print the field name, the field value and a new line. *)

      with nam (deref$ptr (field_intnam))^ do
        field_text := substr (text, 1, len);
      write_string (field_text, ': ');

      indentation := indentation + 3;
      if typ (deref$ptr (field_inttyp))^.kind in [records, arrays] then
	new_line;
      print_desc (field_desc, 0);
      if status in severe then return;
      indentation := indentation - 3;
      if print_pos > indentation then
	new_line
    end
  end
end  (* print_record *);
$PAGE print_subr - in print$
(* PRINT_SUBR prints a subroutine variable.  The start address and
   parent basis halfwords are printed in octal, each following an
   appropriate message.  *)

procedure print_subr (desc: descriptor);

var
  octal_text:		id_string;

begin
  putstring (octal_text, ext$address (desc, status):max_octal_width:o);
  write_string ('Parent basis: ', octal_text[1:6]);  (* left half *)
  new_line;
  write_string ('Start address: ', octal_text[7:6]); (* right half *)
  new_line
end (* print_subr *);
$PAGE print_desc - in print$
(* PRINT_DESC is the dispatcher for printing an arbitrary descriptor's value.  *)

procedure print_desc
		     (*    desc:   descriptor;
			   width:  pos_int *);

begin
  if (format_code <> decimal_radix) and
     not (desc.dkind in [array_dt, int_dt, pointer_dt, file_dt]) then
    status := oct_hex_invalid
  else
    case desc.dkind of

      scalar_dt:
	print_scalar (desc, width);
      int_dt:
	print_int (desc, width);
      char_dt:
	print_char (desc, width);
      real_dt:
	print_real (desc, width);
      set_dt:
	print_set (desc);
      file_dt,
      pointer_dt:
	print_pointer (desc, width);
      slice_dt,
      array_dt:
	print_array (desc);
      record_dt:
	print_record (desc);
      substr_dt,
      string_dt:
	print_string (desc, width);
      subr_dt:
	print_subr (desc);
      others:
	status := cannot_print

    end
end (* print_desc *);
$PAGE print$ - body
begin						(* body of print$ *)
  print_pos := 1;
  indentation := 1;

  print_desc (desc, 0 (* width: i.e. whatever it takes *));
 
  if print_pos > 1 then
    writ$eol
end  (* print$ *);
$PAGE a$$ign
(* A$$IGN implements the debugger's assignment capability.  It is
   passed descriptors for both the lhs and rhs objects and then
   performs the assignment.  The ASSIGNABLE field of the lhs descriptor
   is assumed to have been checked before A$$IGN was called.  *)

public procedure a$$ign (var lhs_desc: descriptor;
			 var rhs_desc: descriptor;
			 var status:   status_code);
  
type
  real_ptr =	^real_type;
  single_real_ptr =
		^minimum(real_type)..maximum(real_type) prec single_real_prec;

var
  scalar_value:	machine_word;
  lhs_length,
  rhs_length:	char_range;
  i:		pos_int;
  real_value:	real_type;
  pad_length,
  copy_length,
  index:	char_range;

begin
  with lhs_desc do begin

    (* Check descriptors for type compatability.  The assignment of user-entered
       address-radix constants to pointers is treated as a special case.  *)

    if (dkind <> pointer_dt) or not ((rhs_desc.dkind = int_dt) and rhs_desc.user_const) then
      check$type (rhs_desc, type_ptr, status);
    if status in severe then return;


    case dkind of

      scalar_dt,				(* scalars, integers, files and pointers *)
      int_dt,
      file_dt,
      pointer_dt:
	begin
	  if dkind = pointer_dt then
	    scalar_value := ext$address (rhs_desc, status)
	  else
	    scalar_value := ext$scalar (rhs_desc, status);
	  if not (status in severe) then
	    sto$scalar (lhs_desc, scalar_value)
	end;

      char_dt:					(* character lhs *)
	sto$scalar (lhs_desc, ord (ext$strchar (rhs_desc, 1)));

      substr_dt,				(* string and substrings *)
      string_dt:
	begin
	  rhs_length := ext$lenstr (rhs_desc);
	  lhs_length := ext$lenstr (lhs_desc);

	  if (dkind = string_dt) andif	(* if lhs is varying string, then get max *)
	     (dtype.str_kind = varying) then	(* string length rather than current length *)
	    lhs_length := dtype.str_length;

	  (* perform the assignment - do it a character at a time. *)
  
	  pad_length := lhs_length - rhs_length;  (* if rhs shorter, must pad *)
	  copy_length := min (lhs_length, rhs_length);  (* if lhs shorter, must truncate *)
  
	  if not a$$backwards (lhs_desc, rhs_desc) then
	    for index := 1 to copy_length do
	      sto$strchar (lhs_desc, index, ext$strchar (rhs_desc, index))
	  else
	    for index := copy_length downto 1 do
	      sto$strchar (lhs_desc, index, ext$strchar (rhs_desc, index));
  
	  for index := (copy_length + 1) to (copy_length + pad_length) do
	    sto$strchar (lhs_desc, index, ' ');

	  if (dkind = string_dt) andif	(* if lhs was varying string, then update *)
	     (dtype.str_kind = varying) then	(* string length word *)
	    sto$lenstr (lhs_desc, min (lhs_length, rhs_length));
	end;

      set_dt:					(* sets *)
	for i := set_min to set_max do
	  sto$setelem (lhs_desc, i, ext$setelem (rhs_desc, i));

      real_dt:  (* reals *)
        begin
	  real_value := ext$real (rhs_desc, status);
	  if dtype.precision > single_real_prec then
	    real_ptr (addr.wordoffset)^ := real_value  (* move double-prec. real *)
	  else
	    single_real_ptr (addr.wordoffset)^ := real_value (* convert to single-prec. and move *)
	end;

      others:
	  status := not_assignable

    end

  end  (* with *);
end  (* a$$ign *).
    {@C