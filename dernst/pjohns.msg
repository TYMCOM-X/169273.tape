Paul,
  The problem with long frames is due to the way the SIO Motherboard
is micro-programmed.  It ignores frames that are too long. To get around this problem
configure the slot with MAXPSIZE(512,128) and PSIZE(128,128) and it will
reject frames between 256 and 512 correctly.  Setting the maximum transmit
frame size has the side affect of causing the receive record size to be set
to 512, but the code will use PSIZE and the size of the largest call
request to determine the maximum allowable receive frame.  If the slot
is generated without fast select then the largest call request will be
about 144 and this will be point above which frames are rejected.
Skywalker and TSI have a fix for this problem, but it will take a while 
before the X.25 code can be changed to take advantage of it. (In the
above example us 1024 instead of 512 if the test packet is in the 512 to
1024 range. If it is larger than that then you will have to use a TSI machine
which allows 2048 and 4096 max packet sizes).

The correct setting of the Final bit on a timed out FRMR is off, according to
the ISO X.25 conformance tests.  These tests are written by the ISO HDLC 
experts.  The tests are in section 6.8.6.  Any patch you have for the
Germans will have to be considered a special patch because their test
is wrong.  This is also true for the DM response of DM, which is legal
according to test 114 in section 6.4.1.

Dennis
   