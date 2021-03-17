:PARAMETERS filename
COM \filename\.PDO/COM/PUB
:SEND CHARACTER 12
R(PUB)UNDER
\filename\.DO=\filename\.DOC/N/I/H
DELETE \filename\.DOC
DECLARE ALL RD RD \filename\.DO
