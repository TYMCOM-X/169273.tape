$PAGE QED.TYP, last modified 2/16/84, zw

TYPE
qedcmds =
  (append, change, delete, insert, edit, modify, load, print, substitute,
  after, before, writecmd, save, find, gotocmd, resetcmd, join, copy,
  move, transfer, bound, list, eqcmd, number, opencmd, outputcmd, closecmd,
  setcmd, split, quit, exitcmd, uparrow, why, indent, underbar, readcmd);
qed_cmd_set = SET OF qedcmds;
set_params =
  (del_param, lcnt_param, mark_param, tab_param, wild_param, case_param);
set_param_set = SET OF set_params;
