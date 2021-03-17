!COMMAND FILE FOR OBTAINING ALL FILES RELEVANT TO BUILDING TELNET
INFORMATION LOGICAL-NAMES

;Files required to build product
COPY FROM-SOURCE:TELNET.CMD TO-SOURCE:*.*.-1
COPY FROM-SOURCE:TELNET.CTL TO-SOURCE:*.*.-1
COPY FROM-SOURCE:TELNET.MAC TO-SOURCE:*.*.-1

;Documentation for product
COPY FROM-SOURCE:TELNET.DOC TO-DOC:*.*.-1
COPY FROM-SOURCE:TELNET.HLP TO-SUBSYS:*.*.-1
COPY FROM-SOURCE:TELNET.TCO TO-DOC:*.*.-1
COPY FROM-SOURCE:TELNET.MANUAL TO-DOC:*.*.-1
COPY FROM-SOURCE:HSTNAM.TXT TO-SYSTEM:*.*.-1

;Final product
COPY FROM-SOURCE:TELNET.EXE TO-SUBSYS:*.*.-1
    