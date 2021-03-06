	*** Storage Technology's changes to WILD %7A(330) ***

*1	Fix the output of the protection code on error messages, more
	of edit 314.

*2	Allow traditional wild card replacement.  For instance, PIP
	allows ".RENAME TEST.LPT=Q??S0.LPT".

*3	Allow the default extension to be wild and supply the default
	PATH on calls to '.SCWLD'.

*4	Add calls to '$VRSN' and '$TITLE' macros in STCMAC.
	(The $SETUP macro in SYSUNV. UNV is better. 3-Oct-80)

*5	Add support for the /SCAN switch (from U. of A.).

*6	Add support to read SFD's before higher level directories
	(from U. of A.).

*7	Fix a bug in DTA code (from U. of A.).

*10	Move the .LKWLD and .SCWLD flags into SCNMAC.

*11	Improve the /SCAN switch.

	End of Revision History

See CSM spr # 166 for edit 444.
  