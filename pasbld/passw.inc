$PAGE PASSW.INC, last modified 5/11/84, zw
EXTERNAL
FUNCTION switch (head_sw: switch_ptr; switch_name: switch_string): BOOLEAN;
EXTERNAL
FUNCTION enable_switch (head_sw: switch_ptr; switch_name: switch_string;
  enable: BOOLEAN): switch_ptr;
EXTERNAL
PROCEDURE pop_switches (head_sw: switch_ptr; final_sw: switch_ptr);
EXTERNAL
PROCEDURE sw_save (head_sw: switch_ptr; VAR f: FILE OF * );
EXTERNAL
FUNCTION sw_load (VAR f: FILE OF * ): switch_ptr;
 