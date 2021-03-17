:LOGFILE ged.log
:PARAMETERS filename
COM \filename\.PED/COM/PUB
:SEND CHARACTER 12
R(PUB)UNDER
\filename\.GED=\filename\.DOC/N/I/H
DELETE \filename\.DOC
DECLARE ALL RD RD \filename\.GED
