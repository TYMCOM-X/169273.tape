!COMMAND FILE FOR OBTAINING ALL FILES RELEVANT TO BUILDING MACSYM
INFORMATION LOGICAL-NAMES

;Files required to build product
COPY FROM-SOURCE:MACSYM.CMD TO-SOURCE:*.*.-1
COPY FROM-SOURCE:MACSYM.CTL TO-SOURCE:*.*.-1
COPY FROM-SOURCE:REL1.MAC TO-SOURCE:*.*.-1
COPY FROM-SOURCE:MACSYM.MAC TO-SOURCE:*.*.-1

;Documentation for product
COPY FROM-SOURCE:MACSYM.MEM TO-DOC:*.*.-1
COPY FROM-SOURCE:MACSYM.TCO TO-DOC:*.*.-1
COPY FROM-SOURCE:MACSYM.DOC TO-DOC:*.*.-1

;Library files used for symbols
;COPY FROM-SUBSYS:MONSYM.UNV TO-SUBSYS:*.*.-1

;Final product
COPY FROM-SUBSYS:MACREL.REL TO-SUBSYS:*.*.-1
COPY FROM-SOURCE:MACSYM.UNV TO-SUBSYS:*.*.-1

