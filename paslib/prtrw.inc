const
    maxrw = 42;
    max_rw_len = 10;

var
    nrw: array [1..11] of 1..43 :=
      ( 1, 2, 7, 11, 19, 26, 33, 36, 39, 41, 43 );
    rw: array [1..42] of string [10] :=
      ( '', 'OF', 'DO', 'IF', 'OR', '', 'END', 'VAR', 'AND', '', 'LOOP', 
        'EXIT', 'THEN', 'ELSE', 'TYPE', 'ORIF', 'CASE', '', 'BEGIN', 'WHILE', 
        'UNTIL', 'CONST', 'LABEL', 'ANDIF', '', 'REPEAT', 'RECORD', 'PUBLIC', 
        'STATIC', 'EXTERN', 'PASCAL', '', 'FORWARD', 'FORTRAN', '', 'FUNCTION', 
        'EXTERNAL', '', 'PROCEDURE', '', 'INITPROCED', '' );
    rwsym: array [1..42] of token_kind :=
      ( etc, ofsy, dosy, ifsy, orsy, etc, endsy, varsy, andsy, etc, loopsy, 
        exitsy, thensy, elsesy, typesy, orifsy, casesy, etc, beginsy, whilesy, 
        untilsy, constsy, labelsy, andifsy, etc, repeatsy, recordsy, publicsy, 
        staticsy, externsy, pascalsy, etc, forwardsy, fortransy, etc, 
        functionsy, externalsy, etc, procedursy, etc, initprocsy, etc );  