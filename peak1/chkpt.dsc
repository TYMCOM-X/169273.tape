***************************************************************************

			     Version 1.2 (132)

	This is a detailed description of the checkpoint file header for
	PEAK. The contents are specified by word offset from the
	beginning of the header, with the first offset being zero.  As of
	the current version, the header is '200 (= 128) words long.


***************************************************************************


    Word Offset		Contents
    ------------	--------

    ['000 = 000]	cvsix("*PEAK*")
    ['001 = 001]	B!Point
    ['002 = 002]	B!Mark0
    ['003 = 003]	B!Mark1
    ['004 = 004]	B!Mark2
    ['005 = 005]	B!Mark3
    ['006 = 006]	B!Mark4
    ['007 = 007]	B!Mark5
    ['010 = 008]	B!Mark6
    ['011 = 009]	B!Mark7
    ['012 = 010]	B!Mark8
    ['013 = 011]	B!Mark9
    ['014 = 012]	B!Size
    ['015 = 013]	B!ModP
    ['016 = 014]	B!Lock
    ['017 = 015]	B!GapStart
    ['020 = 016]	B!GapSize

    ['021 = 017]	<Not Used>
    ['022 = 018]	<Not Used>
    ['023 = 019]	<Not Used>

    ['024 = 020]	B!File [1  for 5]
    ['025 = 021]	B!File [6  for 5]
    ['026 = 022]	B!File [11 for 5]
    ['027 = 023]	B!File [16 for 5]
    ['030 = 024]	B!File [21 for 5]

    ['031 = 025]	<Not Used>
    ['032 = 026]	<Not Used>
    ['033 = 027]	<Not Used>
    ['034 = 028]	<Not Used>
    ['035 = 029]	<Not Used>

    ['036 = 030]	B!Alias [1  for 5]
    ['037 = 031]	B!Alias [6  for 5]
    ['040 = 032]	B!Alias [11 for 5]
    ['041 = 033]	B!Alias [16 for 5]
    ['042 = 034]	B!Alias [21 for 5]
    ['043 = 035]	B!Alias [26 for 5]
    ['044 = 036]	B!Alias [31 for 5]

    ['045 = 037]	B!CkPtSer
    ['046 = 038]	B!DedVer
    ['047 = 039]	B!Prot

    ['050 = 040]	B!Mode [1 for 5]
    ['051 = 041]	B!Mode [6 for 5]

    ['052 = 042]	B!WS0
    ['053 = 043]	B!WS0
    ['054 = 044]	B!WE0
    ['055 = 045]	B!WE1

    ['056 = 046]	<Not Used>
      ...		  ...
    ['176 = 126]	<Not Used>

    ['177 = 127]	">>>>>"


***************************************************************************
