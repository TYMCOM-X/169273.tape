                                                             September 28, 1984








                      *****       ****  *   *  ****  ****
                      *           *   * *   * *     *    
                      ****  ***** ****  *   *  ***   *** 
                      *           *   * *   *     *     *
                      *****       ****   ***  ****  **** 

                         PDP-10 / 940 E-BUSS INTERFACE
                               DESIGN OBJECTIVE

                      Abstract:  This DO is a part of the
                         TYMNET-I elimination project.

                                   John Kopf
                          NETWORK TECHNOLOGY DIVISION
                              September 28, 1984










     ====================================================================
     |       THIS DOCUMENT IS THE SOLE PROPERTY AND CONFIDENTIAL        |
     |       INFORMATION  OF  TYMSHARE,  INC.,  AND  MAY  NOT BE        |
     |       COPIED  IN  WHOLE OR IN PART  OR  DISCLOSED  TO ANY        |
     |       THIRD PARTY  WITHOUT THE PRIOR  WRITTEN  CONSENT OF        |
     |       TYMSHARE.                                                  |
     ====================================================================
















                                   Design Objective                     EBUS.DO
        PDP-10 / 940 E-buss Interface                        September 28, 1984






                          1 -  PDP-10 / 940 E-buss Interface


             The existing interface between TYMNET and both the PDP-10  and 940
        host machines can be represented as:

                |       ---------       ---------       ---------
        TYMNET-1| ----- |VARIAN | ----- |Blk Box| ----- | Host  |
                |       ---------       ---------       ---------

             The Varians  are obsolete;  capable  of supporting  only TYMNET-I,
        limited  in communications  capacity (no  lines over  9.6  K-baud), and
        becoming more difficult to support.

             Furthermore,  TYMNET-I  is   also  obsolete,  and   its  continued
        existence imposes a disproportionate load upon both the Supervisor, and
        other network resources.

             It is proposed to replace the existing interface as follows:

        ENGINE/ |       ---------       ---------       ---------
        TYMNET-2| ----- |  XPI  | ----- |Blk Box| ----- | Host  |
        /ISIS   |       ---------       ---------       ---------

             TYMNET-2  / ISIS  technology is  well defined,  and will  retain a
        strong maintainance base.

             XPI  is  a  M68000-based  processor  peripheral  to   the  Engine,
        currently in use for MXP,  and intended as the vehicle for  many future
        products;  thus it should  also be easily maintainable.  Because  it is
        closely coupled to  the Engine (via direct  DMA transfers), it  is also
        susceptible to bandwidths higher  than can be provided by  the Varians.
        Being an  independent peripheral  processor, it is  capable of  a great
        deal of functionality without loading down its resident Engine.

             In  as much  as the  PDP-10 and  940 technology  are  perceived as
        "stagnant", it is not considered  reasonable to expend a great  deal of
        engineering effort to develop  a new black box;  the  proposed approach
        instead merely replaces the device (Varian) attached to the box  with a
        new device (XPI).

             The intention would be to use XPI/ISIS/TYMNET-2/ENGINE hardware to
        replace the Varians.   Code would be developed  for the XPI  to EMULATE
        the  Host-Black  box-Varian  protocol  and  interface,  while providing
        TYMNET-2  and ISIS  capabilities.  While  these capabilities  would not
        explicitly affect the  hosts (implicit effects  would be seen,  such as
        increased bandwidth available between the host and network), they would
        be  available  if  future development  was  required.   The  removal of


        1                          Design Objective                     EBUS.DO
        PDP-10 / 940 E-buss Interface                        September 28, 1984


        certain  TYMNET-1 constraints,  such as  no more  than  128 ports/host,
        might also be of value in the existing implementations.

             New development would be required for:

               o       XPI-Black box interface;

               o       PDP10-interface code for the XPI;

               o       PDP10-interface diagnostics as appropriate;

               o       940-interface code for the XPI;

               o       940-interface diagnostics as appropriate;

               o       ISIS-XPI interface  (required for  other products
                       as well, such as the CMF under UNIX).

             Finally, this project requires  the availability of (at  least one
        of  each  type  of)   host  for  hardware,  diagnostic,   and  software
        development.

































        2                          Design Objective                     EBUS.DO
