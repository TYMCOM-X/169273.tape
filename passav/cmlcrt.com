;cmlcrt.com, last modified 5/31/83, zw
;create PASCAL
:
;assemble/compile additional items
:com cmladd.com
:
;compile all programs
run build
c *; qn
:
rename PASCAL.LOW, OLDCAL.LOW 
rename PASCAL.SHR, OLDCAL.SHR 
rename PASCMD.LOW, OLDCMD.LOW 
rename PASCMD.SHR, OLDCMD.SHR 
rename PASANL.LOW, OLDANL.LOW 
rename PASANL.SHR, OLDANL.SHR 
rename P10SHP.LOW, OLDSHP.LOW 
rename P10SHP.SHR, OLDSHP.SHR 
rename PASLST.LOW, OLDLST.LOW 
rename PASLST.SHR, OLDLST.SHR 
rename P10CCG.LOW, OLDCCG.LOW 
rename P10CCG.SHR, OLDCCG.SHR 
rename P10OCG.LOW, OLDOCG.LOW 
rename P10OCG.SHR, OLDOCG.SHR 
rename P10INI.ENV, OLDINI.ENV 
:
;link all programs
run build
l *; qn
:
;create standard environment P10INI.ENV
run p10ini
