SET WIDTH 200
MACRO DCPDBA,DCPDBA=ROMMAC#,82S130#,DCPDBA/E:LC
LINK DCPDBA.RMC=DCPDBA/X
RUN RMCSRT#
DCPDBA.RMC
DCPDBA.SAV
RUN SAVROM#
DCPDBA.SAV
DCPDBA.RSX
COPY DCPDBA.ROM=DCPDBA.RSX/RMS:FA
PIP DCPDBA.OBJ,DCPDBA.RMC,DCPDBA.SAV,DCPDBA.RSX/DE/W
                                                                                                                                                                                                                                                                   