PMF VERSION 2.0 LIB FILE
E
E
E
E
E
E
E
E
E
E
E
M 5 44 #COPY
-1)))~A","#num(~A!&#copy (~A!,0),,~A"#if ("#len(
E
E
E
L 8 0 #SYSNAME

E
L 8 7 #EXTREFS
notseen
E
E
E
E
L 8 7 #OUTARGS
notseen
E
E
E
L 10 7 #INCLFILES
notseen
E
E
M 4 27 #END
$justify$ind left 0$skip
E
E
M 7 89 #BLANKS
)~A!#substr (                                                                         ,1,
E
E
E
E
E
E
E
E
E
L 7 7 #INARGS
notseen
E
M 10 51 #ALGORITHM
$justify$skip$ind left 2$par -2Algorithm:$ski 
E
M 7 184 #MODULE
$ski ~A!')$page$justify$skip<:&Entry&:>:\~A"',$title right '~A"$tabs 9,17,25,33,41,49,57,65,73,81,89$margin 10$width 75$number off#if ("#sysname,"$title right '#sysname' right '
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
M 13 54 #REQUIREMENTS
$justify$skip$ind left 2$par -2Requirements:$ski
E
E
E
E
E
M 7 21 #SYSTEM
)~A!#assign (#sysname,
E
M 6 48 #USAGE
$skip$ind left 5$par -5Usage:$skip$verbatim
E
E
E
E
E
E
E
M 10 69 #SETSUBSTR
)))~A#+#length (~A","#num ("~A!&#substr (~A#-1))&~A",1,"#num (~A!#substr (
L 9 2 #HDRWIDTH
64
E
E
E
E
M 7 195 #OUTPUT
))),$par)~A!),8),#copy (\,"#num("8-#length(~A!&#if ("#ltn ("#length (~A!#if ("#eqc ("#outargs,notseen),$skip$ind left 10$par -10Output arguments:#assign (#outargs,seen))$skip$justify$par -8
M 6 22 #WIDTH
)~A!#assign (#hdrwidth,
E
E
E
E
E
E
E
E
E
M 6 47 #NOTES
$justify$skip$ind left 2$par -2Notes:$ski 
E
E
E
E
E
E
M 8 49 #EFFECTS
$justify$skip$ind left 2$par -2Effects:$ski 
E
M 6 192 #INPUT
))),$par)~A!),8),#copy (\,"#num("8-#length(~A!&#if ("#ltn ("#length (~A!#if ("#eqc ("#inargs,notseen),$skip$ind left 10$par -10Input arguments:#assign (#inargs,seen))$skip$justify$par -8
E
L 5 7 #CHNG
notseen
E
E
M 8 137 #INCLUDE
~A!#if ("#eqc ("#inclfiles,notseen),$skip$ind left 10$par -10INCLUDE files required:$skip#assign (#inclfiles,seen))$justify$par -8
E
E
E
E
E
E
E
E
E
E
E
E
E
E
   