(**************************************************************)
(*                                                            *)
(*         AAAAAAAAA     test routine      AAAAAAAAA          *)
(*                                                            *)
(**************************************************************)

module testma options overlay ;

public procedure testa ;

var

   line : integer ;


begin

   line := 1 ;

   rewrite(tty);
    writeln( tty, ' In AAAAAAA. Line : ',line) ; line := line + 1 ;

end .
