Carl,
    The TENEX code times out if it doesn't receive a message
in 60 seconds. The micronode code sends a type 1 message every 10
seconds if there is nothing to send.  If you did the same you
could detect the crash of the node.  The 300 zeros are to make
sure we haven't hung on a partial message.  The zeros are ignored
if they are not part of a message.  After the 300 zeros, then you
should go to automatic mode (resetting the ring pointers if the
micro-code doesn't) and just wait.  The reason we may have
gone down in the first place may have been a bad message from
the F3 which makes us do a one minute timeout, so make your code be
patient.  You do have to send the "specify host ports" 41 message
as the first message.  This is actually what gets to stop throwing
stuff away.  After that everything should be back up.

Dennis
 