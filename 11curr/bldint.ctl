SET WIDTH 200
MAC INT11,INT11=INT11
MAC INT86,INT86=MAC86#,INT86
LINK INT11=INT11,INT86
RN 2716#
INT11
COPY INT11A.ROM=A.ROM/RE
COPY INT11B.ROM=B.ROM/RE
MAC INTX11,INTX11=INTX11
MAC INTX86,INTX86=MAC86#,INTX86
LINK INTX11=INTX11,INTX86
RN 2716#
INTX11
COPY INTXA.ROM=A.ROM/RE
COPY INTXB.ROM=B.ROM/RE
MAC TALK11,TALK11=TALK11
MAC TALK86,TALK86=MAC86#,TALK86
LINK TALK11=TALK11,TALK86
RN 2716#
TALK11
COPY TALKA.ROM=A.ROM/RE
COPY TALKB.ROM=B.ROM/RE
MACRO DCPDBA,DCPDBA=ROMMAC#,82S130#,DCPDBA/E:LC
LINK DCPDBA.RMC=DCPDBA/X
RUN RMCSRT#
DCPDBA.RMC
DCPDBA.SAV
RUN SAVROM#
DCPDBA.SAV
DCPDBA.RSX
COPY DCPDBA.ROM=DCPDBA.RSX/RMS:FA
                                                                                                                                                                                                                                                                                                                                                                                   