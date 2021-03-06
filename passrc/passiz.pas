program passize;

$include pascal.inc
$include pasist.inc
$include paspt.typ
$include pasif.typ
$options special


procedure out ( name: string; s: integer );
 var field: packed array[1..16] of char;
 begin
  field := name;
  writeln (field, s:5);
 end;


begin
 rewrite (output, 'passiz.lst');
 writeln ('Summary of PASCAL node sizes:');
 writeln;

  out ('token_type', size (token_type));
  out ('source_id', size (source_id));
  out ('name_node', size (name_node));
  out ('val', size (val));
  out ('value_node', size (value_node));
  out ('  real_cst', size (value_node, real_cst));
  out ('  string_cst', size (value_node, string_cst));
  out ('  set_cst', size (value_node, set_cst));
  out ('  ptr_cst', size (value_node, ptr_cst));
  out ('  array_cst', size (value_node, array_cst));
  out ('symbol_node', size (symbol_node));
  out ('  labels', size (symbol_node, labels));
  out ('  fields', size (symbol_node,  fields));
  out ('  types', size (symbol_node,  types));
  out ('  consts', size (symbol_node,  consts));
  out ('  vars', size (symbol_node,  vars));
  out ('  values', size (symbol_node,  values));
  out ('  std_procs    ', size (symbol_node,  std_procs));
  out ('  std_funcs    ', size (symbol_node,  std_funcs));
  out ('sym_list', size (sym_list));
  out ('type_node', size (type_node));
  out ('  scalars', size (type_node, scalars));
  out ('  bools', size (type_node, bools));
  out ('  chars', size (type_node, chars));
  out ('  ints', size (type_node, ints));
  out ('  reals', size (type_node, reals));
  out ('  sets', size (type_node, sets));
  out ('  pointers', size (type_node, pointers));
  out ('  arrays', size (type_node, arrays));
  out ('  files', size (type_node, files));
  out ('  strings', size (type_node, strings));
  out ('  records', size (type_node, records));
  out ('  variants', size (type_node, variants, variants));
  out ('  tags', size (type_node, tags));
  out ('  procs', size (type_node, procs));
  out ('  funcs', size (type_node, funcs));
  out ('  unknown_type', size (type_node, unknown_type));
  out ('block_node', size (block_node));
  out ('  root_blk', size (block_node, root_blk));
  out ('  program_blk', size (block_node, program_blk));
  out ('  module_blk', size (block_node, module_blk));
  out ('  subr_blk', size (block_node, subr_blk));

  out ('tuple', size (tuple_node));
  out ('  start_block', size (tuple_node, start_block));
  out ('  end_block', size (tuple_node, end_block));
  out ('  start_stmt', size (tuple_node, start_stmt));
  out ('  start_with', size (tuple_node, start_with));
  out ('  end_with', size (tuple_node, end_with));
  out ('  start_cond', size (tuple_node, start_cond));
  out ('  label_node', size (tuple_node, label_node));
  out ('  jump_op', size (tuple_node, jump_op));
  out ('  stop_op', size (tuple_node, stop_op));
  out ('  return_op', size (tuple_node, return_op));
  out ('  goto_op', size (tuple_node, goto_op));
  out ('  dispose_op', size (tuple_node, dispose_op));
  out ('  nop', size (tuple_node, nop));
  out ('  start_io_op', size (tuple_node, start_io_op));
  out ('  write_op', size (tuple_node, write_op));
  out ('  expressions', size (tuple_node, first_expr));
  out ('    cst_ref', size (tuple_node, first_expr, cst_ref));
  out ('    ident_ref', size (tuple_node, first_expr, ident_ref));
  out ('    field_ref', size (tuple_node, first_expr, field_ref));
  out ('    ptr_ref', size (tuple_node, first_expr, ptr_ref));
  out ('    array_ref', size (tuple_node, first_expr, array_ref));
  out ('    display_op', size (tuple_node, first_expr, display_op));
  out ('    memory access', size (tuple_node, first_expr, mem_ref));
  out ('      call_op', size (tuple_node, first_expr, call_op));
  out ('      nary_op', size (tuple_node, first_expr, nary_op));
end.
    