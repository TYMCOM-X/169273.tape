SET WIDTH 200
MACRO DCPQBA,DCPQBA=ROMMAC#,82S130#,DCPQBA/E:LC
LINK DCPQBA.RMC=DCPQBA/X
RUN RMCSRT#
DCPQBA.RMC
DCPQBA.SAV
RUN SAVROM#
DCPQBA.SAV
DCPQBA.RSX
COPY DCPQBA.ROM=DCPQBA.RSX/RMS:FA
PIP DCPQBA.OBJ,DCPQBA.RMC,DCPQBA.SAV,DCPQBA.RSX/DE/W
                                                                                                                                                                                                                                                                   