SET WIDTH 200
MACRO DCPQBB,DCPQBB=ROMMAC#,82S130#,DCPQBB/E:LC
LINK DCPQBB.RMC=DCPQBB/X
RUN RMCSRT#
DCPQBB.RMC
DCPQBB.SAV
RUN SAVROM#
DCPQBB.SAV
DCPQBB.RSX
COPY DCPQBB.ROM=DCPQBB.RSX/RMS:FA
PIP DCPQBB.OBJ,DCPQBB.RMC,DCPQBB.SAV,DCPQBB.RSX/DE/W
                                                                                                                                                                                                                                                                   