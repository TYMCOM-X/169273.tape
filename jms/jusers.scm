File 1)	DSK:JUSERS.SAI	created: 1830 11-JUN-87
File 2)	DSK:USERS.SAI	created: 1848 01-JUN-87

1)1	own integer User1, User2, L.Only, D.Only, U.Only, U.Name, Total.PPNs;
1)2	procedure BadHash( Integer Bad; r!p (link) r );
****
2)1	own integer User1, User2, L.Only, D.Only, U.Only, Total.PPNs;
2)2	procedure BadHash( Integer Bad; r!p (link) r );
**************
1)6	     do If ( link:LUD[ Lptr ] and link:DUL[ Lptr ] and link:UFD[ Lptr ]
1)		   and ( link:DULname[ Lptr ] = link:UFDname[ Lptr ] ) )
1)		 then link:Next[ Last ]_ link:Next[ Lptr ]
****
2)6	     do If ( link:LUD[ Lptr ] and link:DUL[ Lptr ] and link:UFD[ Lptr ] )
 2)		 then link:Next[ Last ]_ link:Next[ Lptr ]
**************
1)6		"  " & Rpad( link:UFDname[ r ], 14) & Rpad( link:DULname[ r ], 14)
1)	      );
****
2)6		"  " & Rpad( link:DULname[ r ], 12 )
2)	      );
**************
1)6	    L.Only_ 0;  Print( Crlf, "LUD entries missing DUL entries:", Crlf );
1)	    While ( Lptr_ link:Next[ Lptr ] )
****
2)6	    D.Only_ 0;  Print( Crlf, "LUD entries missing DUL entries:", Crlf );
2)	    While ( Lptr_ link:Next[ Lptr ] )
**************
1)6	    Lptr_ UserList;
1)	    U.Name_ 0;  Print( Crlf, "Name mismatch:     UFD name      DUL name", Crlf);
1)	    While ( Lptr_ link:Next[ Lptr ] )
1)	     do If ( link:UFD[ Lptr ] ) and ( link:UFDname[ Lptr ] neq link:DULname[ Lptr ])
1)	         and ( link:DULname[ Lptr ] neq "")
1)		 then Print( PrintLink( U.Name, Lptr ), crlf );
1)	    If ( U.Name = 0 )
1)	     then Print( "--None--", Crlf );
1)	    If Hptr neq HashList
****
2)6	    If Hptr neq HashList
**************
1)6			"    LUD Hash     UFD Hash     UFD Name", Crlf, Crlf );
1)	    Hptr_ HashList;
****
2)6			"    Bad Hash     UFD Hash     UFD Name", Crlf, Crlf );
2)	    Hptr_ HashList;
**************
    