$PAGE COROUT.INC, last modified 5/14/84, zw
TYPE co_routine = ^ co_routine_record;
EXTERNAL FUNCTION create(p: procedure (); stacksize: integer): co_routine;
EXTERNAL PROCEDURE call(e: co_routine);
EXTERNAL PROCEDURE detach;
EXTERNAL PROCEDURE resume(e: co_routine);
  