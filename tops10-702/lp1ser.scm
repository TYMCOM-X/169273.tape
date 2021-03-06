TITLE LP1SER - Line printer service routine for LP11 controller - V006
SUBTTL	Wayne Wall @ CSM/Joe Smith @ CSM 1-Apr-80

	SEARCH	F,S
	$RELOC
	$HIGH

;COPYRIGHT (C) 1979 BY WAYNE WALL, C.S.M.


XP VLPSSR,6			;DEFINE GLOBAL VERSION NUMBER FOR LOADER MAP

	ENTRY	LP1SER,LP2SER
LP1SER:	LP2SER:		;Satisfy dummy request from COMDEV


	CSMEDT	02	;Initialize CSM edit 02 - LPT changes
	CSMEDT	02,1	;This entire module is part 1 of CSM edit 02

;The following must be defined in MONGEN's "OCTAL SYMBOL,VALUE" section
; so that COMMON and COMDEV are correct (ignoring definitions in S.UNV)
	;Southern Systems LP11 controller              (different from LP20)
	LP11CA=:3,,777514    ;Address of LPT control CSRA          (3775400)
	LP11IV==:200	     ;Interrupt vector for LP11                (754)
	LP12CA=:3,,777520    ;Address of 2nd LP11 controller       (3775420)
	LP12IV==:174	     ;Interrupt vector for 2nd LP11            (750)




;[LP1SER.SCM consists of only the first page of LP1SER.CSM]
 