
hrli p1,312000

hrri p1,430572

came p1,%upt+uptpdl

jrst nocrsh#

hrrzi p1,acblk#

blt p1,acblk#+16

setom crsflg#

nocrsh: movei p1,kclear

aos (p)

popj p,

crsflg: 0

rstflg: pushj p,(p1)

jrst .+2

aos (p)

setzm crsflg

popj p,

acblk: 0

0

0

0

0

0

0

0

0

0

0

0

0

0

0
  