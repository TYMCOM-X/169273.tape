; This file is EDGE.TST
! Horizontal edges at -0.15 and 10.85 from start of image area (10.70 usable) ;
!   Vertical edges at -0.25 and +8.25 from start of image area  (8.00 usable) ;
! (0.50,00.00) = 0360,0000 = top left corner (5400-24=5376, 7704-24=7680) ;
! (7.50,10.70) = 5400,7704 = bottom right corner (5400-360=5040) ;
		!   dir, X, Y, length, width ;
	AuxOut( CSI &"0;0360;0000;5040;24!|" );	! Top left to right ;
	AuxOut( CSI &"0;0360;7680;5040;24!|" );	! Bottom left to right ;
	AuxOut( CSI &"1;0360;0000;7799;24!|" );	! Left top to bottom (holes) ;
	AuxOut( CSI &"1;5376;0000;7799;24!|" );	! Right top to bottom ;

[1;360;0;7799;24!|
[0;700;7700;999;2!|
[0;710;7710;100;2!|
[0;720;7720;100;2!|
[0;730;7730;100;2!|
[0;740;7740;100;2!|
[0;750;7750;100;2!|
[0;760;7760;100;2!|
[0;770;7770;100;2!|

   