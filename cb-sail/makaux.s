Simple Integer Procedure MakeCircuit( String Where; Integer T(0) );
begin
    Integer Aux;

    If length(Where)
     then begin
	IntAux( Aux_ CreAux( Where ) );
	If Aux < 0
	 then Return( Aux );
	While -1 < Auxin(T) do;
	AuxOut( "PDP" & #CR );
	AuxSync( Crlf & "." );
	Return( Aux );
     end
     else Return( -1 );
end;

Simple procedure KillCircuit;
begin
    AuxOut( '3&'3 );
    AuxSync( "." );
    AuxOut( "EXIT" & #CR );
    AuxSync( "EXIT" & Crlf );
    AuxZap;
end;


    