;GEN  --  Master MONGEN dialog control file
.DECLARE@BUILDS.DCL
.PA/LIST:ALL
;Standard KL "Features" MONGEN dialog
MKL::
.PA =KL:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*F	;Feature test dialog
*F	;File name
*KLFULL	;Option
*YES	;Standard switch settings
=^Z
;KL1026 "RL" MONGEN dialog
;
;	Standard KL dual-processor
;		ANF-10
;		DECNET-10
;		ETHERNET
;		LAT
;		CI-disk
;		DECtapes
;		DX10 magtapes
M1026L::
.PA =L:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*MUNGEN	;MUNGEN dialog
*I	;Include our random options
*DTA,DX10	;Our unsupported hardware
*	;Exit from MUNGEN dialog
*N	;No save
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*2	;# of CPUs
*RL###  DEC10 Development
*1026	;CPU0 serial number
*1042	;CPU1 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*Y	;Account verification ?
*Y	;MOS memory?
*Y	;SCA support?
*Y	;RNXKON
*Y	;RAXKON
*Y	;TM2KON
*Y	;TX1KON
*Y	;TD2KON
*Y	;T78KON
*4	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*16	;# terminals on CPU0
*0	;# line printers on CPU0
*IBM	;Type for FE 01
*ANF10	;Type for FE 02
*DECNET	;Type for FE 03
*4	;# DTEs on CPU1
*RSX20F	;Type for FE 10
*16	;# terminals on CPU1
*0	;# line printers on CPU1
*ANF10	;Type for FE 11
*ANF10	;Type for FE 12
*DECNET	;Type for FE 13
*CTY	;OPR
*10-23	;Data set lines
*34-47
*
*100	;Jobs
*	;Pages core/user
*2048	;Total core
*60	;Ticks/sec
*4	;Real time devices
*0	;Core for non-locking jobs
*15	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*1	;# TD10s
*4	;# DTAs
*0	;# LPTs
*0	;# TD10s
*1	;# LPTs
*Y	;CPU1 LPT0 lower case ?
*100	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*M.CBMX,400	;Big disk cache for big SIRS ufds
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*CP1NRP,5
*CP1NRN,2
*CP1NTP,1
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*26	;Central site node number
;Central site name
*KL1026
*200	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*Y	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*250	;# of devices that can be connected
*Y	;Load DECnet
*7	;DECnet area number
*110	;DECnet node number
*ROUTING;DECnet router type
*Y	;Ethernet support
*Y	;LAT support
=^Z
;KL1026 "RN" MONGEN dialog
;
;	Standard KL triple-processor
;		ANF-10
;		DECNET-10
;		ETHERNET
;		LAT
;		CI-disk
;		DECtapes
;		DX10 magtapes
M1026N::
.PA =N:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*MUNGEN	;MUNGEN dialog
*I	;Include our random options
*DTA,DX10	;Our unsupported hardware
*	;Exit from MUNGEN dialog
*N	;No save
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*3	;# of CPUs
*RN###  DEC10 Development
*1026	;CPU0 serial number
*1042	;CPU1 serial number
*2476	;CPU2 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*Y	;Account verification ?
*Y	;MOS memory?
*Y	;SCA support?
*Y	;RNXKON
*Y	;RAXKON
*Y	;TM2KON
*Y	;TX1KON
*Y	;TD2KON
*Y	;T78KON
*4	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*16	;# terminals on CPU0
*0	;# line printers on CPU0
*IBM	;Type for FE 01
*ANF10	;Type for FE 02
*DECNET	;Type for FE 03
*4	;# DTEs on CPU1
*RSX20F	;Type for FE 10
*16	;# terminals on CPU1
*0	;# line printers on CPU1
*ANF10	;Type for FE 11
*ANF10	;Type for FE 12
*DECNET	;Type for FE 13
*4	;#DTEs on CPU2
*RSX20F	;Type for FE 20
*32	;#terminals on CPU2
*1	;# line printers on CPU2
*Y	;line printer have lower case?
*IBM	;type for FE 21
*ANF	;type for FE 22
*ANF	;type for FE 23
*CTY	;OPR
*10-23	;Data set lines
*34-47
*64-103
*
*100	;Jobs
*	;Pages core/user
*2048	;Total core
*60	;Ticks/sec
*4	;Real time devices
*0	;Core for non-locking jobs
*15	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*1	;# TD10s
*4	;# DTAs
*0	;# LPTs
*0	;# TD10s
*1	;# LPTs
*Y	;CPU1 LPT0 lower case ?
*0	;# TD10s
*0	;# LPTs
*100	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*M.CBMX,400	;Big disk cache for big SIRS ufds
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*CP1NRP,5
*CP1NRN,2
*CP1NTP,1
*CP2NTP,4
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*ANFNIP,0	;ANF Ethernet protocol 60-06
*ANFNIM,<60006B15>;ANF Ethernet broadcast address AB-00-04-00-60-06
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*26	;Central site node number
;Central site name
*KL1026
*200	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*Y	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*250	;# of devices that can be connected
*Y	;Load DECnet
*7	;DECnet area number
*110	;DECnet node number
*ROUTING;DECnet router type
*Y	;Ethernet support
*Y	;LAT support
=^Z
;KL1026 "RQ" MONGEN dialog
;
;	Standard KL quadruple-processor
;		ANF-10
;		DECNET-10
;		ETHERNET
;		LAT
;		CI-disk
;		DECtapes
;		DX10 magtapes
M1026Q::
.GOTO M2476A
.PA =Q:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*MUNGEN	;MUNGEN dialog
*I	;Include our random options
*DTA,DX10,CPUS	;Our unsupported hardware
*	;Exit from MUNGEN dialog
*N	;No save
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*4	;# of CPUs
*RQ###  DEC10 Quad-SMP
*1026	;CPU0 serial number
*1042	;CPU1 serial number
*2476	;CPU2 serial number
*3333	;CPU3 serial number (wrong!)
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*Y	;Account verification ?
*Y	;MOS memory?
*Y	;SCA support?
*Y	;RNXKON
*Y	;RAXKON
*Y	;TM2KON
*Y	;TX1KON
*Y	;TD2KON
*Y	;T78KON
*4	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*16	;# terminals on CPU0
*0	;# line printers on CPU0
*IBM	;Type for FE 01
*ANF10	;Type for FE 02
*DECNET	;Type for FE 03
*4	;# DTEs on CPU1
*RSX20F	;Type for FE 10
*16	;# terminals on CPU1
*0	;# line printers on CPU1
*ANF10	;Type for FE 11
*ANF10	;Type for FE 12
*DECNET	;Type for FE 13
*4	;#DTEs on CPU2
*RSX20F	;Type for FE 20
*32	;#terminals on CPU2
*1	;# line printers on CPU2
*Y	;line printer have lower case?
*ANF10	;type for FE 21
*ANF10	;type for FE 22
*ANF10	;type for FE 23
*3	;#DTEs on CPU3
*RSX20F	;Type for FE 30
*32	;#terminals on CPU3
*1	;# line printers on CPU3
*Y	;line printer have lower case?
*DECNET	;type for FE 31
*ANF10	;type for FE 32
*CTY	;OPR
*10-23	;Data set lines
*34-47
*
*100	;Jobs
*	;Pages core/user
*2048	;Total core
*60	;Ticks/sec
*4	;Real time devices
*0	;Core for non-locking jobs
*15	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*1	;# TD10s
*4	;# DTAs
*0	;# LPTs on CPU0
*0	;# TD10s
*1	;# LPTs on CPU1
*Y	;CPU1 LPT0 lower case ?
*0	;# TD10s on CPU2
*0	;# LPTs on CPU2
*0	;# TD10s on CPU3
*0	;# LPTs on CPU3
*100	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*M.CBMX,400	;Big disk cache for big SIRS ufds
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*CP1NRP,5
*CP1NRN,2
*CP1NTP,1
*CP2NTP,4
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*26	;Central site node number
;Central site name
*KL1026
*200	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*Y	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*250	;# of devices that can be connected
*Y	;Load DECnet
*7	;DECnet area number
*110	;DECnet node number
*ROUTING;DECnet router type
*Y	;Ethernet support
*Y	;LAT support
=^Z
;KL2476 "RA" MONGEN dialog
;
;	Standard KL single-processor
;		ANF-10
M2476A::
.PA =A:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*1	;# of CPUs
*RA###  KL2476 Standalone
*2476	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*N	;Account verification ?
*Y	;MOS memory?
*N	;SCA support?
*N	;RNXKON
*Y	;TM2KON
*Y	;TD2KON
*Y	;T78KON
*3	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*32	;# terminals on CPU0
*1	;# line printers on CPU0
*Y	;Line printer have lower case?
*ANF10	;Type for FE 01
*ANF10	;Type for FE 02
*CTY	;OPR
*	;Data set lines
*40	;Jobs
*	;Pages core/user
*768	;Total core
*60	;Ticks/sec
*4	;Real time devices
*0	;Core for non-locking jobs
*1	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*0	;# LPTs
*20	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*67	;Central site node number
;Central site name
*KL2476
*40	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*Y	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*50	;# of devices that can be connected
*N	;Load DECnet
*N	;Ethernet support
=^Z
;KL2476 "RB" MONGEN dialog
;
;	Standard KL single-processor
;		ANF-10
;		DECNET-10
M2476B::
.PA =B:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*1	;# of CPUs
*RB###  KL2476 Standalone
*2476	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*N	;Account verification ?
*Y	;MOS memory?
*N	;SCA support?
*N	;RNXKON
*Y	;TM2KON
*Y	;TD2KON
*Y	;T78KON
*3	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*32	;# terminals on CPU0
*1	;# line printers on CPU0
*Y	;Line printer have lower case?
*DECNET	;Type for FE 01
*ANF	;Type for FE 02
*CTY	;OPR
*	;Data set lines
*40	;Jobs
*	;Pages core/user
*768	;Total core
*60	;Ticks/sec
*4	;Real time devices
*0	;Core for non-locking jobs
*1	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*0	;# LPTs
*20	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*67	;Central site node number
;Central site name
*KL2476
*40	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*Y	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*50	;# of devices that can be connected
*Y	;Load DECnet
*7	;DECnet area number
*80	;DECnet node number
*ROUTING;DECnet router type
*N	;Ethernet support
=^Z
;KL2476 "RC" MONGEN dialog
;
;	Standard KL single-processor
;		No networks
M2476C::
.PA =C:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*1	;# of CPUs
*RC###  KL2476 Standalone
*2476	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*N	;Account verification ?
*Y	;MOS memory?
*N	;SCA support?
*N	;RNXKON
*Y	;TM2KON
*Y	;TD2KON
*Y	;T78KON
*1	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*32	;# terminals on CPU0
*1	;# line printers on CPU0
*Y	;Line printer have lower case?
*CTY	;OPR
*	;Data set lines
*40	;Jobs
*	;Pages core/user
*768	;Total core
*60	;Ticks/sec
*0	;Real time devices
*Y	;Allow jobs to
*0	;Core for non-locking jobs
*1	;# HPQs
*N	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*0	;# LPTs
*20	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*N	;Networks supported ?
=^Z
;KL2476 "RD" MONGEN dialog
;
;	Standard KL single-processor
;		ANF-10
;		DECNET-10
;		ETHERNET
;		LAT
M2476D::
.PA =D:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*1	;# of CPUs
*RD###  KL2476 Standalone
*2476	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*N	;Account verification ?
*Y	;MOS memory?
*N	;SCA support?
*N	;RNXKON
*Y	;TM2KON
*Y	;TD2KON
*Y	;T78KON
*3	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*32	;# terminals on CPU0
*1	;# line printers on CPU0
*Y	;Line printer have lower case?
*DECNET	;Type for FE 01
*ANF10	;Type for FE 02
*CTY	;OPR
*	;Data set lines
*40	;Jobs
*	;Pages core/user
*768	;Total core
*60	;Ticks/sec
*4	;Real time devices
*0	;Core for non-locking jobs
*1	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*0	;# LPTs
*20	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*67	;Central site node number
;Central site name
*KL2476
*40	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*Y	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*50	;# of devices that can be connected
*Y	;Load DECnet
*7	;DECnet area number
*80	;DECnet node number
*ROUTING;DECnet router type
*Y	;Ethernet support
*Y	;LAT support
=^Z
;PFOUR "RG" MONGEN dialog
;
;	Standard KL single-processor
;		DECNET-10
;		ETHERNET
;		LAT
;		CI-disk
;		RP20-disk
M1030G::
.PA =G:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*1	;# of CPUs
*RG###  PFOUR Diag-S/A
*1030	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*N	;Account verification ?
*Y	;MOS memory?
*Y	;SCA support?
*Y	;RNXKON
*Y	;RAXKON
*Y	;TM2KON
*Y	;TD2KON
*Y	;T78KON
*2	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*16	;# terminals on CPU0
*1	;# line printers on CPU0
*Y	;Line printer have lower case?
*DECNET	;Type for FE 01
*CTY	;OPR
*	;Data set lines
*40	;Jobs
*	;Pages core/user
*1024	;Total core
*60	;Ticks/sec
*4	;Real time devices
*0	;Core for non-locking jobs
*5	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*0	;# LPTs
*20	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*4	;Central site node number
;Central site name
*PFOUR
*1	;# remote TTYs
*N	;Network virtual terminals ?
*N	;CDRs ?
*N	;LPTs ?
*N	;DDPs ?
*N	;RDXs ?
*N	;TSKs ?
*1	;# of devices that can be connected
*Y	;Load DECnet
*7	;DECnet area number
*152	;DECnet node number
*NONROUTING;DECnet router type
*Y	;Ethernet support
*Y	;LAT support
=^Z
;KL2798 "RX" MONGEN dialog
;
;	Standard KL single-processor
;		ANF-10
;		DECNET-10
;		ETHERNET
;		LAT
;		CI-20 DISK
M2798X::
.PA =X:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*1	;# of CPUs
*RX###  KL2798 CHIP S/A
*2798	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*N	;Account verification ?
*Y	;MOS memory?
*Y	;SCA support?
*N	;RNXKON
*Y	;RAXKON
*Y	;TM2KON
*Y	;TD2KON
*Y	;T78KON
*1	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*32	;# terminals on CPU0
*1	;# line printers on CPU0
*Y	;Line printer have lower case?
*CTY	;OPR
*	;Data set lines
*40	;Jobs
*	;Pages core/user
*768	;Total core
*60	;Ticks/sec
*4	;Real time devices
*0	;Core for non-locking jobs
*1	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*0	;# LPTs
*20	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*ANFNIP,60006	;ANF Ethernet protocol 60-06
*ANFNIM,<60006B15>;ANF Ethernet broadcast address AB-00-04-00-60-06
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*20	;Central site node number
;Central site name
*KL2798
*40	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*Y	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*50	;# of devices that can be connected
*Y	;Load DECnet
*7	;DECnet area number
*448	;DECnet node number
*ROUTING;DECnet router type
*Y	;Ethernet support
*Y	;LAT support
=^Z
;KL2996 "RY" MONGEN dialog
;
;	Standard KL single-processor
;		ANF-10
;		DECNET-10
;		ETHERNET
;		LAT
;		RP20 DISK
M2996Y::
.PA =Y:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*1	;# of CPUs
*RY###  KL2996 DALE S/A
*2996	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*N	;Account verification ?
*Y	;MOS memory?
*N	;SCA support?
*Y	;RNXKON
*Y	;TM2KON
*Y	;TD2KON
*Y	;T78KON
*1	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*32	;# terminals on CPU0
*1	;# line printers on CPU0
*Y	;Line printer have lower case?
*CTY	;OPR
*	;Data set lines
*40	;Jobs
*	;Pages core/user
*768	;Total core
*60	;Ticks/sec
*4	;Real time devices
*0	;Core for non-locking jobs
*1	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*0	;# LPTs
*20	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*ANFNIP,60006	;ANF Ethernet protocol 60-06
*ANFNIM,<60006B15>;ANF Ethernet broadcast address AB-00-04-00-60-06
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*21	;Central site node number
;Central site name
*KL2996
*40	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*Y	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*50	;# of devices that can be connected
*Y	;Load DECnet
*7	;DECnet area number
*449	;DECnet node number
*ROUTING;DECnet router type
*Y	;Ethernet support
*Y	;LAT support
=^Z
;Standard/FTMP=0 KL "Features" MONGEN dialog
MKLF::
.PA =KLF:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*F	;Feature test dialog
*F	;File name
*KLFULL	;Option
*NO	;Standard switch settings
*FTMP,0			;No multiprocessor support
*
*N	;Each switch
=^Z
;KL2798 "RE" MONGEN dialog
;
;	Standard/FTMP=0 KL single-processor
;		ANF-10
;		DECNET-10
;		ETHERNET
;		LAT
;		CI-disk
M2798E::
.PA =E:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*1	;# of CPUs
*RE###  KL2798 CHIP S/A
*2798	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*N	;Account verification ?
*Y	;MOS memory?
*Y	;SCA support?
*Y	;RNXKON
*Y	;RAXKON
*Y	;TM2KON
*Y	;TD2KON
*Y	;T78KON
*3	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*32	;# terminals on CPU0
*1	;# line printers on CPU0
*Y	;Line printer have lower case?
*DECNET	;Type for FE 01
*ANF10	;Type for FE 02
*CTY	;OPR
*	;Data set lines
*40	;Jobs
*	;Pages core/user
*768	;Total core
*60	;Ticks/sec
*4	;Real time devices
*0	;Core for non-locking jobs
*1	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*0	;# LPTs
*20	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*67	;Central site node number
;Central site name
*KL2798
*40	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*Y	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*50	;# of devices that can be connected
*Y	;Load DECnet
*7	;DECnet area number
*448	;DECnet node number
*ROUTING;DECnet router type
*Y	;Ethernet support
*Y	;LAT support
=^Z
;KL2476 "RF" MONGEN dialog
;
;	Standard/FTMP=0 KL single-processor
;		ANF-10
;		ETHERNET
M2476F::
.PA =F:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*1	;# of CPUs
*RF###  KL2476 Standalone
*2476	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*N	;Account verification ?
*Y	;MOS memory?
*N	;SCA support?
*N	;RNXKON
*Y	;TM2KON
*Y	;TD2KON
*Y	;T78KON
*3	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*32	;# terminals on CPU0
*1	;# line printers on CPU0
*Y	;Line printer have lower case?
*ANF10	;Type for FE 01
*ANF10	;Type for FE 02
*CTY	;OPR
*	;Data set lines
*40	;Jobs
*	;Pages core/user
*768	;Total core
*60	;Ticks/sec
*2	;Real time devices
*0	;Core for non-locking jobs
*1	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*0	;# LPTs on CPU0
*20	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*67	;Central site node number
;Central site name
*KL2476
*40	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*Y	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*50	;# of devices that can be connected
*N	;Load DECnet
*N	;Ethernet support
=^Z
;Itsy bitsy KL "Features" MONGEN dialog
MKLI::
.PA =KLI:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*F	;Feature test dialog
*F	;File name
*KLFULL	;Option
*NO	;Standard switch settings
*FTRTTRP,0		;No real time
*FTTRPSET,0		;No real time (in another guise)
*FTRSP,0		;No terminal response time tracking
*FTMP,0			;No multiprocessor support
*FTNET,0		;No networks
*FTTSK,0		;No ANF-10 TSK devices
*FTRDX,0		;No ANF-10 RDX devices
*FTDDP,0		;No ANF-10 DDP devices
*FTDECNET,0		;No DECnet-10
*FTCMSR,0		;No communications measurement code
*FTMSGSER,0		;No MPX devices
*FTXTC,0		;No XTC (DA28) support
*FTDN60,0		;No IBM (DN6x) support
*FTDX10,0		;No DX10 tape support
*FTPSCD,0		;No scheduler performance statistics
*FTSCA,0		;No SCA support
*FTCIDSK,0		;No CI disk support
*FTCIDNET,0		;No CI DECnet support
*FTKLPS,0		;No KLINIK parameter setting
*
*N	;Each switch
=^Z
;KL2476 "RI" MONGEN dialog
;
;	Itsy-Bitsy KL single-processor
;		No nuthin
M2476I::
.PA =I:
.RUN DSK:MONGEN
*SHORT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KL	;Processor type
*1	;# of CPUs
*RI###  KL2476 Itsy Bitsy
*2476	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;EBOX/MBOX runtime accounting ?
*Y	;Exclude PI time ?
*N	;Account verification ?
*Y	;MOS memory?
*N	;SCA support?
*N	;RNXKON
*Y	;TM2KON
*Y	;TD2KON
*Y	;T78KON
*1	;# DTEs on CPU0
*RSX20F	;Type for FE 00
*32	;# terminals on CPU0
*1	;# line printers on CPU0
*Y	;Line printer have lower case?
*CTY	;OPR
*	;Data set lines
*40	;Jobs
*	;Pages core/user
*768	;Total core
*60	;Ticks/sec
*0	;Real time devices
*Y	;Allow jobs to lock
*0	;Core for non-locking jobs
*1	;# HPQs
*N	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*0	;# LPTs on CPU0
*20	;# PTYs
*TTDMOS,96	;Make -20F strings longer
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*FLCDEF,0	;Default filler class 
*TTYWID,80	;default tty width
*TTCHKS,8	;Larger tty chunk size
*TTYLCT,0	;TTY LC
*%RTBEA,512	;Maximum DECnet broadcast endnode adjacencies
*%DLBSZ,1476	;Maximum DECnet message size on ethernet
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;End SIXBIT value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN!APPFLG,UNIQ.1!UNIQ.2,
*STRUCT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*UNIT,WHO,RUNFLG!NOLOGIN!APPFLG,,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*N	;Networks supported ?
=^Z
;Standard KS "Features" MONGEN dialog
MKS::
.PA =KS:
.RUN DSK:MONGEN
*PROMPT	;Type of help desired
*F	;Feature test dialog
*F	;File name
*KSFULL	;Option
*Y	;Standard switch settings
=^Z
;KS4097 "RS" MONGEN dialog
;
;	Standard KS single-processor
;		ANF-10
;		DECNET-10
M4097S::
.PA =S:
.RUN DSK:MONGEN
*PROMPT	;Type of help desired
*HDW	;Hardware dialog
*HDWCNF	;File name
*KS	;Processor type
*RS###  KS10 Engineering
*4097	;CPU0 serial number
*Y	;Exclude monitor overhead ?
*Y	;Account verification ?
*4	;# RPAs
*2	;# MTAs
*32	;# DZ lines
*CTY	;OPR
*36-37	;Data set lines
*
*40	;Jobs
*	;Pages core/user
*512	;Total core
*60	;Ticks/sec
*Y	;Allow locking ?
*0	;Core for non-locking jobs
*1	;# HPQs
*Y	;MSGSER ?
*Y	;PSISER ?
*Y	;IPCSER ?
*Y	;ENQ/DEQ ?
*25	;# PTYs
*1	;# LPTs
*N	;LPT0 lower case ?
*2	;# KMC/DUP-11s
*ANF10	;Default for line 0 (Goes to NOVA/KL1026)
*DECNET	;Default for line 1 (Goes to JINX/KL1026)
*MSGMAX,596	;Large network message size to accomodate DECnetwork
*TTYWID,80	;default tty width
*TTCHKS,8	;increased chunk size
*TTYLCT,0	;default tty lc
*	;End decimal value definitions
*PRVFIL,055
*RTCH1,1
*MTDLTP,0	;Standard magtape labels are bypass
*	;End octal value definitions
*	;Sixbit value definitions
*	;DEV,PI
*	;DEV,PI,#
*	;DEV,PI,AC
*MDC,5,100,SYS	;Local ersatz device names (Monitor DoCumentation)
*ASG,66,3024,JOB	;SPR transfer area (DSKB: only)
*	;End user defined ersatz device names
*PIVOT,,RUNFLG,,
*MS,,RUNFLG,,
*MAIL,MS,RUNFLG,,
*PATH,,RUNFLG,UNIQ.2,
*WHO,,RUNFLG!NOLOGIN,UNIQ.1!UNIQ.2,WHO
*LINE,WHO,RUNFLG!NOLOGIN,UNIQ.1!UNIQ.2,
*NCOPY,#RUNNFT,RUNFLG,,
*NDELET,#RUNNFT,RUNFLG,,
*NDIREC,#RUNNFT,RUNFLG,,
*NMOVE,#RUNNFT,RUNFLG,,
*NRENAM,#RUNNFT,RUNFLG,,
*NREVIE,#RUNNFT,RUNFLG,,
*NTYPE,#RUNNFT,RUNFLG,,
*TLINK,#RUNNFT,RUNFLG,,
*TRANSL,,RUNFLG,,
*	;End user defined commands
*H19/VT52,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,VTXXEP,VTXXBP
*H19A/VT100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*DAS21A/VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*	;End of user defined terminal types
*NET	;Network dialog
*NETCNF	;File name
*Y	;Networks supported ?
*76	;Central site node number
;Central site name
*KS4097
*100	;# remote TTYs
*Y	;Network virtual terminals ?
*Y	;CDRs ?
*Y	;LPTs ?
*N	;DDPs ?
*Y	;RDXs ?
*Y	;TSKs ?
*32	;# of devices that can be connected
*Y	;Load DECnet code
*7	;DECnet area number
*117	;Our central site node number
*ROUTING;DECnet router type
*N	;Ethernet support
=^Z
;KS4149 "RU" TLitt's MREAST KS-10 MONGEN dialog
;
;	Standard KS single-processor
;		ANF-10
;		DECNET-10
;		RX20
;		DMR
M4149U::
.PA =U:
.RUN MONGEN		! Hardware configuration
*SHORT	;Help
MUNGEN	;Dialog
*INCLUDE
*RX20,DMR,RTRXPW
*EXIT	;End unsupported options
*N	;Don't save
*SHORT
*HDW
*
*KS		; DECsystem type
*RU### Marlboro East
*4149		; CPU0 serial number
*Y		; Exclude monitor overhead from user runtime
*N		; Account verification
*3		; Number of Disk drives on system (RP06's,RM03's)
*2		; Number of Tape drives on system (TU45's)
*1		; RX211 Floppy ctl
*2		;  Units on RXA
*32		; Total number of TTY lines connected to DZ11's
*CTY		; OPR line, in octal
!  Data set lines:
*
*40		; Number of jobs
*		; Maximum pages of core for each job
*512		; Number of K of memory on system
*60		; Clock ticks per second
*Y		; Alow jobs to be locked in core?
*160		; Number of pages min for unlocked jobs
*1		; Number of HPQs
*Y		; MSGSER for MPX:?
*Y		; PSISER for interrupt trapping?
*Y		; IPCF for inter-job communication?
*Y		; ENQ/DEQ for I/O synchronization?
*14		; Number of PTYs
*0		; Number of LPTs
!*Y		; Does LPT0 have lower case?
*2		; Number of KMC/DUP's
*ANF10		; Type of line for KDP0
*DECNET		; Type of line for KDP1
*1		; DMRs
*DECNET		;  Type of line
*N		;  Dialup (switched)?
!  Decimal symbol, value
*MTDLTP,1	; ASCII tape levels are default
*STDENS,4	; 1600 BPI default tape density
*		; Blank line to terminate
!  Octal symbol, value
*SCHEDN,400	; System starts with SCHED 400
*PRVSYS,155	; Protection code for SYS:*.*
*PRYSYS,177	; Protection code for SYS:*.SYS
*		; Blank line to terminate
!  SIXBIT symbol, value
*M.QSTR,DSKB	; Structure for queue master
*		; Blank line to terminate
! Type "device-mnemonic, PI-channel" for  special devices
*
! Type "device-mnemonic, PI-channel, no-of-devices"
*
! Type "device-mnemonic, PI-channel, highest-AC-to-save"
*
! Type "ersatz-device,P,Pn,search-list-type"
*ADM,16,743,SYS
*ASF,5,61,SYS
*BBL,366,3400,SYS
*CAL,11,4,SYS
*CBL,11,12,JOB
*CDB,11,4215,ALL
*CPL,5,60,SYS
*DCS,5,77,SYS	; Ersatz area for Billing.
*GAL,5,66,SYS
*MPB,11,2,SYS
*NET,5,55,SYS
*NWP,1,16,SYS
*OOP,10,744,ALL
*PAS,5,63,SYS
*PC2,5,64,SYS
*PLG,11,40,ALL
*SIM,5,62,SYS
*SVE,11,1,SYS
*PEX,33,34,SYS	;Pony Express area for NETMAI
! site-dependent ersatz devices follow
*		; Blank line to terminate
! command-name,dispatch,flags,unique-bits,PRVTAB-name
*PIVOT,,RUNFLG,UNIQ.1,
*		; Blank line to terminate
! Site dependant terminal types follow
! terminal-type,WIDTH,LENTH,FF,TAB,LC,ALT,DISPLAY,CRLF,XON,8BIT,FILL,
!   erase-to-EOL,backspace-space-backspace
VT220,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
VT240,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
VT241,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
PC100,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
PC350,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
PC380,80,24,NOFF,TAB,LC,NOALT,DIS,CRLF,XON,NO8BIT,0,V100EP,VTXXBP
*
*NET		; NETwork generation
*
*Y		; Network software?
*71		; ANF node number of central site
*MREAST;	; Name of central site
*16		; Number of remote TTYs
*Y		; Network virtual terminals?
*N		; Remote CDRs?
*Y		; Remote LPTs?
*N		; Remote DN8x DDCMP devices?
*N		; Remote data entry terminals?
*Y		; Remote processes? (TSK:)
*32		; Maximum number of connects - don't need many for a KS
*Y		; DECnet software?
*11		; DECnet area
*10		; DECnet node number of central site
*ROUTING	; DECnet router type
*11		; Transmit password
*N		; No ethernet - yet
!*Y		; Ethernet
!*Y		; LAT support
=^Z
   O<GE