:*********************************************************************
:  X.25 DCE TEST                     HTS                 XOM:  2500  *
:  TSI SLOT:  XX                     TCK                HOST:  2500  *
:                       INTERSLOT-INTER/INTRA MACHINE                *   
:*********************************************************************

PROTOCOL(X.25,HDLC,LAPB)                 
DPORTS(24)                               
HOSTLINKS(2500,0)                          
LINKS(0)                                 
K(7)                                     
N2(10)                                   
ILRR(5)
T1(200)
PSADR(03,01)                             :  DCE
PWIND(7,7)                               
PSIZE(128,128)                           
MAXPSIZE(512,512)                        
CUDSTRING()                              
CLDADR(3106)                              
PKTOPT(+TKSUP,+IPRNR)                    
INPARITY(SAVE,+DATA,-CUD,+ALLCALLS)      
PADOPT(-NECHO,-NCRLF,+NLFCR,-HLFCR,+HCRLF)
ECHOABLE(08,09,0A,0D,20-7E)
PADIDLE(40,40)
PADFORWARD(2,2)
PADBREAK(21)
TCLASS(12,12)
:   XOM STATEMENTS
XOMUSERNAME(XOM)

:   END
  