!COMMAND FILE FOR OBTAINING ALL FILES RELEVANT TO BUILDING MONSYM
INFORMATION LOGICAL-NAMES

;Files required to build product
COPY FROM-SOURCE:MONSYM.CMD TO-SOURCE:*.*.-1
COPY FROM-SOURCE:MONSYM.CTL TO-SOURCE:*.*.-1
COPY FROM-SOURCE:REL1.MAC TO-SOURCE:*.*.-1
COPY FROM-SOURCE:MONSYM.MAC TO-SOURCE:*.*.-1

;Documentation for product
COPY FROM-SOURCE:MONSYM.TCO TO-DOC:*.*.-1
COPY FROM-SOURCE:MONSYM.DOC TO-DOC:*.*.-1

;Library files used for symbols
;COPY FROM-SUBSYS:MACREL.REL TO-SUBSYS:*.*.-1
;COPY FROM-SUBSYS:MACSYM.UNV TO-SUBSYS:*.*.-1

;Final product
COPY FROM-SOURCE:MONSYM.UNV TO-SUBSYS:*.*.-1
COPY FROM-SOURCE:ERRMES.BIN TO-SYSTEM:*.*.-1
COPY FROM-SUBSYS:MONSYM.REL TO-SUBSYS:*.*.-1
  