SET WIDTH 200
MACRO DCPMSM,DCPMSM=ROMMAC#,82S146#,DCPMSM/E:LC
LINK DCPMSM.RMC=DCPMSM/X
RUN RMCSRT#
DCPMSM.RMC
DCPMSM.SAV
RUN SAVROM#
DCPMSM.SAV
DCPMSM.RSX
COPY DCPMSM.ROM=DCPMSM.RSX/RMS:FA
PIP DCPMSM.OBJ,DCPMSM.RMC,DCPMSM.SAV,DCPMSM.RSX/DE/W
                                                                                                                                                                                                                                                                   