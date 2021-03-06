;%7C(575)-2  04-May-77  STC's modifications to SCAN

;*1	Set up to use the 'VRSN.' macro.
;*2	Allow protections in the form of <nnn>.
;*3	Add support for /SCAN switch (from U. of A.).
;*4	Clear file specification on an .OSCAN call (from U. of A.).
;*5	Allow version numbers in the form of '%1A(1)-1'.
;*6	Improve the /SCAN switch.
;*7	Add a flag which forces a switch name to be exact.
;*10	Output [no] on switches which have FS.NOS turned on in the
;	'/HELP:SWITCHES' output.
;*11	Add the '/HELP:ARGUMENTS' which outputs the current switch
;	information than can be obtained from the switch tables.
;*12	Don't output the astrisk (*) before the switch name.
;*13	Allow the program to specify the messages to output for the
;	program switches, '/HELP:ARGUMENTS'.
;*14	Tweak .VERBO a little bit.
    