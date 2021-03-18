.  << Settings for tables and figures.  >>
.
.IF NOT DECLARATION ( gottab ) THEN  gottab _ FALSE;
.IF NOT DECLARATION ( tabname ) THEN  tabname _ "Table";
.IF NOT DECLARATION ( tablisthead ) THEN  tablisthead _ "List of Tables";
.
.
.MACRO standard back $( LIBRARY |YAMBAK.DFS| ; )$
.
.IF NOT DECLARATION(titleing) THEN titleing _ TRUE;
.IF titleing THEN START INSERT titl END;
.INSERT cont;
.INSERT tble;
.
.MACRO table (!title,!size,!tabs1,!tabs2,!lbl) $(
.BEGIN "table"
.   IF LINES < !size THEN SKIP TO COLUMN 1;
.   IF LINE THEN GROUP;
.   Tmp _ (IF LENGTH("!lbl") THEN "!lbl." ELSE UseFile);
.   T1 _ "tab!" & SCAN(Tmp,".",NULL,"IS") & ":";
.   gottab _ TRUE;
.   IF AdjustTab THEN ADJUST ELSE NOJUST;
.   BEGIN "TabHdr"
.	TURN ON "{#\";
.	CENTER;
.	SKIP 2;
.   	eval(T1) NEXT TabCnt!;
.	}{tabname}#{TabCnt!}
.	}!title
.	SKIP 2;
.	SEND tble $(
.	    }${SecName}${TabCnt!}(10)!title####{Page!}{BREAK)$;
.	SC!Send $(
.	    }<T>{TabCnt!}(10)!title####{Page!}{BREAK)$;
.	ind |!title|;
.	TURN OFF;
.   END "TabHdr";
.   Tmp _ "!tabs1";
.   REPEAT $( IF LENGTH(Tmp)=0 THEN DONE;
.   	T1 _ SCAN(Tmp,",",NULL,"IS") )$;
.   Tmp _ "!tabs1";
.   IF LENGTH("!tabs2") THEN Tmp _ Tmp & ",!tabs2";
.   Crown (0,T1-1,0);
.	eval("TABS "&Tmp);
.	PREFACE 0;
.	RETAIN;
.	AT "--" $( CONTINUE )$;
.	AT "==" $( BREAK )$;
.)$;
.
.MACRO endTable $(
.   endCrown;
.   !sh _ NULL;			<< cancel sub headings (if any) >>
.   SKIP 1;
.END "table"
.)$;
.
.MACRO TabHead (T) $(
.   Sub Heading |T|;
.   }T
.)$;
.
.<< TABREF is called by the user to refer to a table >>
.
.TabQueue _ NULL;
.
.MACRO TabRef ( label ) $(
.START
.   BugNote |TabRef label|;
.   Tmp _ "tab!"&"label";
.   }{TabName}#{eval("TabCnt! "&Tmp)}{
.END
.)$
.
.  << UseTab is called when a table is required...if the table is
.	already declared, then the call is ignored >>
.
.MACRO QueueTab (TabName) $(
.   Queue (TabQueue, "TabName")
.)$;
