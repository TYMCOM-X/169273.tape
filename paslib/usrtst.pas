PROGRAM test;
EXTERNAL PROCEDURE usrtst; 
EXTERNAL VAR cmdfil: FILE_NAME;
BEGIN
  cmdfil := 'USRTST.CMD';
  usrtst
END.
   