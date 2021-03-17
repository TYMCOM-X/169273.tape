
(*********   S C N R E A  - -  SCANNR Regular Expression Allocation   *********)

external procedure rea_init;
external procedure rea_finish;
external function  new_re ( kind: reg_ops; lambda_in: boolean ): reg_exp;
external function  use_re ( re: reg_exp ): reg_exp;
external procedure free_re ( re: reg_exp );
external procedure test_re ( re: reg_exp );
external procedure del_re ( re: reg_exp );
