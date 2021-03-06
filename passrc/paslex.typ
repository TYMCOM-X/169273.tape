$PAGE PASLEX.TYP, last modified 3/27/84, zw
$IFNOT paslextyp

TYPE
rwordtype = PACKED ARRAY [1 .. 13] OF CHAR;
rwtype  = ARRAY [1 .. 61] OF rwordtype;
rsytype = ARRAY [1 .. 61] OF symbols;
roptype = ARRAY [1 .. 61] OF operators;
frwtype = ARRAY [1 .. 14] OF 1 .. 62;
ssytype = ARRAY [' ' .. '~'] OF symbols;
soptype = ARRAY [' ' .. '~'] OF operators;

$PAGE rw, rsy in PASRW.TAB

CONST rw: rwtype :=
     (  'IF',            'DO',            'OF',            'TO',            
	'IN',            'OR',            'END',           'FOR',           
	'VAR',           'DIV',           'MOD',           'SET',           
	'AND',           'NOT',           'THEN',          'ELSE',          
	'WITH',          'GOTO',          'LOOP',          'CASE',          
	'TYPE',          'FILE',          'EXIT',          'PREC',          
	'ORIF',          'STOP',          'READ',          'ANDIF',         
	'BEGIN',         'UNTIL',         'WHILE',         'ARRAY',         
	'CONST',         'LABEL',         'WRITE',         'RECORD',        
	'DOWNTO',        'PACKED',        'OTHERS',        'REPEAT',        
	'PUBLIC',        'STATIC',        'STRING',        'RETURN',        
	'MODULE',        'READLN',        'READRN',        'FORWARD',       
	'PROGRAM',       'OPTIONS',       'WRITELN',       'WRITERN',       
	'FUNCTION',      'EXTERNAL',      'PROCEDURE',     'EXCEPTION',     
	'GETSTRING',     'PUTSTRING',     'ENVMODULE',     'DATAMODULE',    
	'ALLCONDITIONS'  );

CONST rsy: rsytype :=
     (  ifsy,            dosy,            OFsy,            tosy,            
	relop,           addop,           endsy,           forsy,           
	varsy,           mulop,           mulop,           setsy,           
	mulop,           notsy,           thensy,          elsesy,          
	withsy,          gotosy,          loopsy,          casesy,          
	typesy,          filesy,          exitsy,          precsy,          
	addop,           stopsy,          iosy,            mulop,           
	beginsy,         untilsy,         whilesy,         ARRAYsy,         
	CONSTsy,         labelsy,         iosy,            recordsy,        
	downtosy,        PACKEDsy,        otherssy,        repeatsy,        
	publicsy,        staticsy,        stringsy,        returnsy,        
	modulesy,        iosy,            iosy,            forwardsy,       
	programsy,       optionssy,       iosy,            iosy,            
	functionsy,      externalsy,      proceduresy,     exceptionsy,     
	iosy,            iosy,            envmodsy,        datamodsy,       
	allcondsy        );

$PAGE rop, frw in PASRW.TAB

CONST rop: roptype :=
     (  noop,            noop,            noop,            noop,            
	inop,            orop,            noop,            noop,            
	noop,            idiv,            imod,            noop,            
	andop,           noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	orifop,          noop,            readsy,          andifop,         
	noop,            noop,            noop,            noop,            
	noop,            noop,            writesy,         noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            readlnsy,        readrnsy,        noop,            
	noop,            noop,            writelnsy,       writernsy,       
	noop,            noop,            noop,            noop,            
	getstrsy,        putstrsy,        noop,            noop,            
	noop             );

CONST frw: frwtype :=
     (  1,               1,               7,               15,              
	28,              36,              48,              53,              
	55,              60,              61,              61,              
	61,              62               );

$PAGE ssy in PASRW.TAB

CONST ssy: ssytype := 
     (  badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	lparent,         rparent,         mulop,           addop,           
	comma,           addop,           period,          mulop,           
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       colon,           semicolon,       
	relop,           relop,           relop,           badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       lbracket,        
	badsymbol,       rbracket,        arrow,           badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol        );

$PAGE sop in PASRW.TAB

CONST sop: soptype :=
     (  noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            mul,             plus,            
	noop,            minus,           noop,            rdiv,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	ltop,            eqop,            gtop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop             );

$ENABLE paslextyp
$ENDIF
    