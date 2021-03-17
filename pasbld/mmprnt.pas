module mmprnt;

(* The PRINT command processor for ODMS *)

$INCLUDE MMSSYM.TYP
$INCLUDE TIMUTL.INC
$PAGE prtsym print symbols in a module
public procedure prtsym (m: modptr);

(* PRTSYM simply lists the symbols on the LTV chain of the module
   given as parameter. *)

const
  sword: array [symtype] of string[9] := (
    'procedure', 'function', 'variable',
    'constant', 'loseg_sym', '<unknown>' );

var
  s: symptr;

begin
  writeln (tty);
  s := m^.syms;
  if s <> nil then repeat
    write (tty, ' ', s^.name^.text: mdlsig+2 :L, sword[s^.stype], ' in ',
      m^.name^.text, ',');
    if s^.ltvloc <> -1 then write (tty, ' LTV=', s^.ltvloc:6:o);
    writeln (tty, ' MTV=', s^.mtvloc:6:O);
    s := s^.lnext
    until s = m^.syms;
  writeln (tty)
end (* procedure prtsym *);
$PAGE prtarea area suboption of info option of print command
procedure prtarea (mdl: ^pnode);

(* PRTAREA lists the areas of an overlaid program (if any), with all
   kinds of useful information. *)

var
  a: areaptr;

begin
  if mdl^.alist = nil then
    writeln (tty, '%ODMPRT -- No overlay areas in system ', mdl^.pname, '.')
  else begin
    a := mdl^.alist;
    repeat
      writeln (tty, ' ', a^.name^.text: mdlsig+2: L, 'originating at ',
	a^.aorig:6:O, ' size ', a^.asize:6:O, ' (', a^.asize:0,
	' decimal) words');
      a := a^.next
      until a = nil
    end
end (* procedure prtarea *);
$PAGE prtprg program suboption of info option of print command
procedure prtprg (mdl: ^pnode);

(* PRTPRG prints general information about the overlaid program. *)

begin
  with mdl^ do begin
    writeln (tty, ' System ', pname, ' from MDL file ', mdlname);
    write (tty, ' Compiled ');
    if debug then write (tty, 'for DEBUG mode operation ');
    writeln (tty, 'on ', dc_ext (cre_dt));
    writeln (tty, ' Total overlay static size ', stsize:6:O, '(',
      stsize:0, ' decimal) words');
    writeln (tty, ' Master transfer vector at ', mtvaddr:6:O,
      ', declared size ', mtvsize:6:O, '(', mtvsize:0, ' decimal) words');
    writeln (tty, ' Low segment break is ', losegbreak:6:O)
    end
end (* procedure prtprg *);
$PAGE prtmod module suboptio of info option of print command
procedure prtmod (mdl: ^pnode);

(* PRTMOD prints out general information for each module in the sytem. *)

var
  m: modptr;

begin
  m := mdl^.mlist;
  repeat
    with m^ do begin
      write (tty, ' Module ', name^.text);
      if marea = nil then write (tty, ' sharable ')
      else write (tty, ' in area ', marea^.name^.text, ' ');
      writeln (tty, ' static at ', storig:6:O, ', ', stsize:6:O,
	'(', stsize:0, ' decimal) words');
      end;
    m := m^.next
    until m = nil
end (* procedure prtmod *);
$PAGE prtinf process info suboption of print command
public procedure prtinf (
  item: keyword;			(* command type scalar *)
  mdl: ^pnode );			(* MDL structure pointer *)

(* PRTINF gives the various information needed by the print command,
   extracting it from the MDL symbol table given as parameter. *)

begin
  writeln (tty);
  case item of
    area_cmd: prtarea (mdl);
    module_cmd: prtmod (mdl);
    program_cmd: prtprg (mdl);
    all_cmd: begin
      prtprg (mdl);
      writeln (tty);
      prtarea (mdl);
      writeln (tty);
      prtmod (mdl)
      end
    end (* case *);
  writeln (tty)
end (* procedure prtinf *).
