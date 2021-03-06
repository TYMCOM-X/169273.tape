$PAGE PASXRF.TYP, last modified 1/16/84, zw
$IFNOT pasxrftyp

(*SYSTEM PASADR.TYP*)
(*SYSTEM PASIST.TYP*)

(*the XST structure is still under construction*)

TYPE (*cross-refrence data structures*)
    xrf_class = ( value_ctxt, mod_ctxt, var_parm_ctxt, ref_ctxt, file_xrf,
      page_xrf, line_xrf, block_xrf, end_xrf, index_xrf, deref_xrf, call_xrf,
	field_xrf, wfield_xrf, decl_xrf, fileblk_xrf, baserec_xrf );
    xparm_val = id_range;
    xrf_record = PACKED RECORD
      code: xrf_class;
      var_lab_parm: BOOLEAN;
      parameter: xparm_val
    END;
    xst_record = PACKED RECORD
      id_number, name, parent: xparm_val;
      type_name: xparm_val;
      type_class: type_kind;
      CASE class: sym_kind OF
	blocks: (
	  blk_class: block_kind);
	vars, consts, values: (
	  var_class: storage_class);
	labels, fields, types, for_inds, std_procs, std_funcs, conditions: (
	  )
    END;

$ENABLE pasxrftyp
$ENDIF
 