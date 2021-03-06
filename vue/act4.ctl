Carl,

I have used the ACT-IV terminal fairly extensively and can tell you
which cursor controls are safe to use.  It should be noted that many of
the controls take considerable time, and thus tend to cause problems in
a networking environment where delays are not easy to control.  Other
advertised features only exist in the 'local mode'.  That is they can
only be executed from the keyboard (not remotly).  Anyway, here is a
list of the 'safe' controls:


    character        hex        function       comments

        ^K            0b        cursor down
        ^L            0c        home & clear
        ^N            0e        toggle intensity
        ^T            14        cursor positioning lead-in
                                             followed by ascii
                                                Y then X
        ^X            18        cursor right
        ^Z            1a        cursor up
        GS            1d        home cursor
        RS            1e        erase to end of line
        VS            1f        erase to end of screen

note :  use linefeed (0a) and backspace (08) for cursor left and cursor
down.  Also, the use of half intensity for highlighting is not very
reliable since at low screen intensity, it is hard to tell the
difference (there is an intensity control on the terminal).

One thing that i noticed was that VUE does not turn off expansion of
form feeds for the ACT-IV.  I can fix that using the TTY FORM command
before I enter VUE.  Also, cursor addressing does not always work
correctly, or possibly ^T is being sent for some other reason.  At any
rate, the cursor often goes places where its not supposed to go.  Also,
control characters (ei clear to end of line and toggle half intensity
mode) are sometimes sent spuriously; that is they are sent when their
action is not desired as if a cursor positioning command should have
been sent, but no control-T was sent.  Alternatively, some control
sequence that takes too long has been received, and so the control-t
has been ignored.

It might be helpful to list the cursor controls that should be avoided.
These are as follows:


    character      hex      claimed function      comment


      ^A            01      insert line        Local mode only
      ^B            02      send screen        Local mode only
                                              Sends to serial port
      ^D            04      delete next char   Local mode only
      ^E            05      request curs pos   Strange results!
      ^I            09      tab                Local mode only
      ^P            10      print screen       Local mode only
                                              Sends to printer port
      ^Q            11      displ ctrl chars   Local mode only
      ^S            13      insert chars       Local mode only
      ^W            17      delete line        Local mode only
      

Note that some ACT-IV simulators will also perform these functions in
'remote' mode, but they tend to be slow.  The delete line and insert
line controls are particularly slow.  Also some people have obtained
special PROM sets from Micro-term for the ACT-IV to allow some of these
functions in remote mode.  This is not recommended by Micro-term because
these functions are slow, and no buffering is done.  It is probably best
not to use these at all!  (perhaps for the Mime it is ok to use some of
them).

  