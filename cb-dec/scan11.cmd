INFORMATION LOGICAL-NAMES JOB

;Files required to build product
COPY FROM-SOURCE:SCAN11.CMD TO-SOURCE:*.*.-1
COPY FROM-SOURCE:SCAN11.CTL TO-SOURCE:*.*.-1
COPY FROM-SOURCE:SCAN11.MAC TO-SOURCE:*.*.-1
COPY FROM-SOURCE:RNFSCC.MAC TO-SOURCE:*.*.-1

;Library files used for symbols
COPY FROM-SUBSYS:SCAN11.REL TO-SUBSYS:*.*.-1
COPY FROM-SUBSYS:SCNM10.UNV TO-SUBSYS:*.*.-1
COPY FROM-SUBSYS:SCNM20.UNV TO-SUBSYS:*.*.-1

;Final product
   