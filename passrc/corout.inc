$PAGE COROUT.INC, last modified 1/31/84, zw
$IFNOT coroutinc

$SYSTEM COROUT.TYP

EXTERNAL FUNCTION create(PROCEDURE(); INTEGER): environment;
EXTERNAL PROCEDURE call(environment);
EXTERNAL PROCEDURE detach;
EXTERNAL PROCEDURE resume(environment);

$ENABLE coroutinc
$ENDIF 