:LOGFILE gid.log
:PARAMETERS filename
COM \filename\.PID/COM/PUB
:SEND CHARACTER 12
R(PUB)UNDER
\filename\.GID=\filename\.DOC/N/I/H
DELETE \filename\.DOC
DECLARE ALL RD RD \filename\.GID
