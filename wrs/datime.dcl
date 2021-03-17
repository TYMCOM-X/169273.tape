external string procedure cvDate( integer DECD );
external string procedure cvTime( integer SECS );
external string procedure cvZone( integer Z,D(0),T(0) );
external string procedure daytime;
comment
    Date:
	Return current local date and time in form
		WWW, DD MMM YY HH:MM:SS ZZZ
	as recommended in RFC #822.
;
