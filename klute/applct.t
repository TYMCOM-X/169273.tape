------------------------------------------------------------------------

LAYER:            APPLICATION

------------------------------------------------------------------------

COMPONENTS:
                  File Transfer Driver
                  Get Commandline Parameters
                  Parse Commandline
                  Check Host File
                  Startup Abort
                  Initialize Everything
                  Get Positive Reply
                  Send Startup Commands
                  Get Micro Response
                  Handle Micro Command
                  Transfer Out
                  Read Host File
                  Transfer In
                  Write Host File
                  Close Host File
                  Close Up

------------------------------------------------------------------------

DESCRIPTION:

   This is the controller for the COPYPC.X program.  This layer parses the
commandline, sends startup commands to the micro, and waits for a
positive reply before beginning the actual transfer.  It is this layer's
responsibility to perform conversions to and from PDP-10 formatted bytes
and the standard 8-bit bytes which the Tymshare File Transfer Protocol
requires.  This layer also handles abnormal situations requiring program
abort.


------------------------------------------------------------------------

------------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   File Transfer Driver
PROCEDURE ID:     main program
PROCEDURE TYPE:   not applicable

------------------------------------------------------------------------

INPUT:            cmdline.params:  pc.filename, host.filename
                                   file.type, replace
                  startup.abort.type
                  abort.type

GLOBALS: direction.of.transfer

OUTPUT:           cmdline.params
                  startup.abort.type
                  abort.type

------------------------------------------------------------------------

CALLED BY:        user

CALLS:            Get Commandline Parameters
                  Startup Abort
                  Initialize Everything
                  Get Positive Reply
                  Transfer In
                  Transfer Out
                  Close Up
                  Abort Transfer

------------------------------------------------------------------------

DESCRIPTION:

   This is the main program driver.  If Get Commandline Parameters returns
false then Startup Abort is called, otherwise the transfer proceeds.

------------------------------------------------------------------------

------------------------------------------------------------------------

PROCEDURE NAME:   File Transfer Driver
PROCEDURE ID:     main program

------------------------------------------------------------------------

PSEUDOCODE:

execute AP.get.cmdline.params returning boolean value
                                        cmdline.params
                                        startup.abort.type
if AP.get.cmdline.params then
   execute AP.init.everything using direction.of.transfer, replace,
                                    file.type, pc.filename
   execute AP.get.positive.reply returning boolean value
   if AP.get.positive.reply then
      case direction.of.transfer of
         tohost   :  execute AP.transfer.in using file.type, host.filename
         topc     :  execute AP.transfer.out using file.type, host.filename
      execute AP.close.up
   else
      set abort.type to positive.reply.error
      execute SU.abort.transfer using abort.type
else
   execute AP.startup.abort using startup.abort.type

------------------------------------------------------------------------

------------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Get Commandline Parameters
PROCEDURE ID:     AP.get.cmdline.params
PROCEDURE TYPE:   boolean

------------------------------------------------------------------------

INPUT:            commandline
                  cmdline.params
                  file.size

OUTPUT:           PARAMETERS:   cmdline.params (reference)
                                startup.abort.type (reference)

                  host.filename
                  file.type
                  replace
                  commandline

------------------------------------------------------------------------

CALLED BY:        File Transfer Driver

CALLS:            Parse Commandline
                  Check Host File

------------------------------------------------------------------------

DESCRIPTION:

   Determines whether there is a syntax or file error and passes
commandline parameters back to the driver.

------------------------------------------------------------------------

PSEUDOCODE:

rescan the command line forming commandline  ! string
set AP.get.cmdline.params to false
execute AP.parse.cmdline using commandline
                         returning boolean value
                                   cmdline.params
if AP.parse.cmdline then
   execute AP.check.host.file using host.filename, file.type, replace
                              returning integer value, file.size
   case AP.check.host.file of
      [0] : set AP.get.cmdline.params to true
      [1] : set startup.abort.type to file.missing.error
      [2] : set startup.abort.type to replace.file.error
else
   set startup.abort.type to syntax.error

------------------------------------------------------------------------

------------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Parse Commandline
PROCEDURE ID:     AP.parse.cmdline
PROCEDURE TYPE:   boolean

------------------------------------------------------------------------

INPUT:            PARAMETERS:   commandline

GLOBALS: direction.of.transfer

OUTPUT:           PARAMETERS:   cmdline.params (reference)
                                ! host.filename, pc.filename,
                                  file.type, replace

------------------------------------------------------------------------

CALLED BY:        Get Commandline Parameters

CALLS:            none

------------------------------------------------------------------------

DESCRIPTION:

   This procedure parses out the commandline, determining the cmdline
parameters:  direction.of.transfer, host.filename, pc.filename,
file.type, replace.  If there is a syntax error, the procedure returns
false.


The program is invoked as follows:

-run copypc; KEYWORD <host filename> <pc filename> / b(inary r(eplace

where KEYWORD = fromhost | tohost | topc | frompc
      the / is optional but is required if either binary or replace
         is desired
      the options b(inary and r(eplace must be specified with their

------------------------------------------------------------------------

------------------------------------------------------------------------

PROCEDURE NAME:   Parse Commandline
PROCEDURE ID:     AP.parse.cmdline

------------------------------------------------------------------------

PSEUDOCODE:

set AP.parse.cmdline to true
search commandline for keyword "FROMHOST" or "TOPC" following the ';'
if "FROMHOST" or "TOPC" found then
    set direction.of.transfer equal to TOPC
    if there is a token following "FROMHOST" or "TOPC" then
         set host.filename equal to token following "FROMHOST" or "TOPC"
         if there is a token following host.filename then
              set pc.filename equal to token following host.filename
         else { no pc-filename }
              set AP.parse.cmdline to false
    else { no host-filename }
         set AP.parse.cmdline to false
else { no fromhost or topc }
    search cmdline.buffer for keyword "TOHOST" or "FROMPC" following the ';'
    if "TOHOST" or "FROMPC" found then
         set direction.of.transfer to TOHOST
         if there is a token following "TOHOST" or "FROMPC" then
              set host.filename equal to token following "TOHOST" or "FROMPC"
              if there is a token following host.filename then
                   set pc.filename equal to token following host.filename
              else { no pc-filename }
                   set AP.parse.cmdline to false
         else { no host.filename }
              set AP.parse.cmdline to false
    else { no keyword found }
      set AP.parse.cmdline to false

if AP.parse.cmdline then
    set file.type to AST { Abstract Symbolic Text }
    set replace to false
    search for the character '/' { options follow }
    if '/' found then
         if there is at least one token following the '/' then
            do until no.more.tokens
                   case token of
                        'B'  : set file.type to binary
                        'R'  : set replace to true
                        other: set AP.parse.cmdline to false
         else { no token }
            set AP.parse.cmdline to false

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Check Host File
PROCEDURE ID:     AP.check.host.file
PROCEDURE TYPE:   integer

-----------------------------------------------------------------------

INPUT:            PARAMETERS:   host.filename (value)
                                file.type (value)
                                replace (value)

GLOBALS: direction.of.transfer

OUTPUT:           PARAMETERS:   file.size (reference)

-----------------------------------------------------------------------

CALLED BY:        Get Commandline Parameters

CALLS:            none

-----------------------------------------------------------------------

DESCRIPTION:

   For TOPC direction of transfer, determines whether host file is
present, and if it is, sets the file.size parameter.  For TOHOST
direction of transfer, if file already exists, checks the replace
option to determine whether transfer may proceed.

-----------------------------------------------------------------------


PSEUDOCODE:

set AP.check.host.file to 0   ! everything OK
if direction.of.transfer = TOPC then
   if host.filename exists then
      if file.type = AST then
         set file.size to (5 * number.of.words) in file
      else
         set file.size to (4 * number.of.words) in file
   else
      set AP.check.host.file to 1 ! file.missing.error
else  ! TOHOST
   if host.filename exists then
      if not replace then
         set AP.check.host.file to 2 ! replace.file.error

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Startup Abort
PROCEDURE ID:     AP.startup.abort
PROCEDURE TYPE:   untyped

-----------------------------------------------------------------------

INPUT:            PARAMETERS: startup.abort.type   (value)

OUTPUT:           error message on user's terminal

-----------------------------------------------------------------------

CALLED BY:        File Transfer Driver

CALLS:            none

-----------------------------------------------------------------------

DESCRIPTION:

   Prints appropriate error message and exits.

-----------------------------------------------------------------------

PSEUDOCODE:

case startup.abort.type of
   syntax.error   :  print
"Syntax error.  Enter commandline in the following format:
COPYPC FROMHOST <host filename> <PC filename> / Binary Replace
       TOHOST"
   file.missing.error   :  print
"No such host file."
   replace.file.error   :  print
"Host file already exists.  Specify replace."

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Initialize Everything
PROCEDURE ID:     AP.init.everything

-----------------------------------------------------------------------

INPUT:            PARAMETERS: direction.of.transfer (value)
                              replace  (value)
                              file.type (value)
                              pc.filename (value)

GLOBALS: command.table
         number.of.cmds.in.table

OUTPUT:

-----------------------------------------------------------------------

CALLED BY:        File Transfer Driver

CALLS:            none

-----------------------------------------------------------------------

DESCRIPTION:

   Initialize the commands the micro needs to start the transfer,
variable values, and the set of characters which need to be encoded.

-----------------------------------------------------------------------

-----------------------------------------------------------------------

PROCEDURE NAME:   Initialize Everything
PROCEDURE ID:     AP.init.everything

-----------------------------------------------------------------------

PSEUDOCODE:

initialize Positive.Reply.Received, End.Of.File (EOF), EOR,
           transfer.error to false
initialize EOD, looking.for.first.ACK to true

initialize communications variables:
   in.closeup.mode to false            last.PID.ACKed to 0
   timeout to false                    current.incoming.PID to hex 20
   incoming.timeout.counter to 0       greatest.outgoing.PID to 0
   outgoing.timeout.counter to 0       current.outgoing.PID to hex 19
   max.incoming.timeouts to 25
   max.outgoing.timeouts to 25
   number.of.seconds to 5

   set characters which must be encoded in xlate table:
      carriage return   xon      us (unit separator)
      line feed         bel      null
      escape            vt       del
      shift in          ht
      shift out         bs
      xoff              ff

initialize command.table:

cmd.num appl.cmd        link.type   who.sent  importance cmd.type  cmd.data
   1    12 {use table B}   link     requestor important  reply.opt   B

if direction.of.transfer is TOPC then
   2     2 {receive mode} prim.appl requestor  advisory  reply.opt   null
   3     6 {filename}     prim.appl requestor  advisory  reply.opt   pc.filename

   if replace then
   4     7                prim.appl requestor  advisory  reply.opt   1 {replace}
   else
   4     7                prim.appl requestor  advisory  reply.opt   3 {error}

   5     8 {file size is} prim.appl requestor  advisory  reply.opt   file.size

   if file.type is binary then
   6     9                prim.appl requestor  advisory  reply.opt   1 {binary}
   else
   6     9                prim.appl requestor  advisory  reply.opt   3 {AST}
   set number.of.cmds.in.table to 7

else { direction.of.transfer is TOHOST }
   2    1 {send mode}     prim.appl requestor  advisory  reply.opt   null
   3    6 {filename}      prim.appl requestor  advisory  reply.opt   pc.filename
   set number.of.cmds.in.table to 4

  last  3 {start transfr} prim.appl requestor  important reply.req   null


-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Get Positive Reply
PROCEDURE ID:     AP.get.positive.reply
PROCEDURE TYPE:   boolean

-----------------------------------------------------------------------

INPUT:            positive.reply.received

GLOBALS: command.table
         number.of.cmds.in.table
         transfer.error

OUTPUT:           boolean value

-----------------------------------------------------------------------

CALLED BY:        File Transfer Driver

CALLS:            Send Startup Cmds
                  Get Micro Response

-----------------------------------------------------------------------

DESCRIPTION:

   Send the appropriate set of commands to the micro and await his
response.  Transfer cannot proceed unless a positive reply to the
last command is received.

   This procedure sends null application.record and parameters to Get Micro
Response so that it may account for commands being received from the micro
during the transfer.  Note that Init Everything set transfer.error to false.

-----------------------------------------------------------------------

PSEUDOCODE:

execute AP.send.startup.cmds ! using number.of.cmds.in.table,
                                   command.table
execute AP.get.micro.response returning positive.reply.received
set AP.get.positive.reply to positive.reply.received

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Send Startup Cmds
PROCEDURE ID:     AP.send.startup.cmds
PROCEDURE TYPE:   untyped

-----------------------------------------------------------------------

INPUT:

GLOBALS: command.table
         number.of.cmds.in.table

OUTPUT:           application.record,
                  application.parameters,
                  appl.rec.size,
                  EOF, EOR.seen

-----------------------------------------------------------------------

CALLED BY:        Get Positive Reply

CALLS:            Create Packet     ! Valid Packet Layer

-----------------------------------------------------------------------

DESCRIPTION:

   The application commands and parameters are sent one at a time to
the valid packet layer.  All the commands will fit in one packet.

-----------------------------------------------------------------------

PSEUDOCODE:

set cmd.table.pointer to 0
set record.type to command
do until EOF
   increment cmd.table.pointer
   if cmd.table.pointer = number.of.cmds.in.table then
      set EOF to true
   set application.parameters from cmd.table entry pointed at
      by cmd.table.pointer:  link.type, who.sent, importance, cmd.type
   build application.rec
      set appl.rec.size to 0
      move rightmost 8 bits of application.record into leftmost
           application.record
      increment appl.rec.size
      if cmd.data <> null then
         loop length (cmd.data) times
            move cmd.data byte to integer
            move rightmost 8 bits of integer to application.record
            increment appl.rec.size

   execute VA.create.packet using application.record,
                               application.parameters,
                               appl.rec.size
                               EOF

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Get Micro Response
PROCEDURE ID:     AP.get.micro.response
PROCEDURE TYPE:   untyped

-----------------------------------------------------------------------

INPUT:
                  application.record
                  application.parameters
                  appl.rec.size
                  EOF
                  EOR.seen

OUTPUT:           PARAMETERS: positive.reply.received (reference)

-----------------------------------------------------------------------

CALLED BY:        Get Positive Reply
                  Transfer In

CALLS:            Get Incoming Application Record
                  Handle Micro Command

-----------------------------------------------------------------------

DESCRIPTION:

   Loop through the micro's responses until either a reply that requires
the program to abort is received or a positive response to the last
command sent is received.  If aborting, set appropriate abort.type.

-----------------------------------------------------------------------

PSEUDOCODE:

set positive.reply.received to false
while not EOF do
   execute VA.get.incoming.application.record
              returning application.record,
                        application.params,
                        appl.rec.size
                        EOF,
                        EOR.seen
   execute AP.handle.micro.command using application.record,
                        application.params, appl.rec.size

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Handle Micro Command
PROCEDURE ID:     AP.handle.micro.command
PROCEDURE TYPE:   untyped

-----------------------------------------------------------------------

INPUT:            PARAMETERS: application.record
                              application.parameters
                              appl.rec.size

GLOBALS: positive.reply.received

OUTPUT:           abort.type

-----------------------------------------------------------------------

CALLED BY:        Get Micro Response
                  Transfer In
                  Get Unexpected Response

CALLS:            Abort Transfer

-----------------------------------------------------------------------

DESCRIPTION:

   This procedure figures out what the micro's command means.  Anything
other than a positive reply causes an abort.

-----------------------------------------------------------------------

PSEUDOCODE:

   set application.command to first byte of application.record
   set cmd.data to next byte of application.record
   ! other bytes of cmd.data are ignored in this release

   if importance = true then  { ignore advisory commands }
   case cmd.type of
      0,1   [reply.opt], [reply.req]
            : set abort.type to unimplemented.micro.command.received
              execute SU.abort.transfer using abort.type
      2     [pos.reply]
            : if command = 3 then
                  set positive.reply.received to true
              { else ignore }
      3,5   [neg.reply], [req.for.info]
            : set abort.type to Micro.cannot.continue
              execute SU.abort.transfer using abort.type
      4     [exp.reply]
            : set reason.code to first byte of cmd.data
              set abort.type to reason.code
              execute SU.abort.transfer using abort.type
      [other]  :
            : set abort.type to unknown.cmd.type
              execute SU.abort.transfer using abort.type

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Transfer Out
PROCEDURE ID:     AP.transfer.out
PROCEDURE TYPE:   untyped

-----------------------------------------------------------------------

INPUT:            PARAMETERS: file.type   (value),
                              host.filename  (value)

                  file.record
                  appl.rec.size
                  EOF
                  EOR.seen

GLOBALS:  file.open

OUTPUT:           application.parameters
                  application.record
                  appl.rec.size
                  EOF
                  file.type
                  EOR.seen

-----------------------------------------------------------------------

CALLED BY:        File Transfer Driver

CALLS:            Read Host File
                  Create Packet     ! Valid Packet Layer

                  SAIL intrinsics:  OPEN, LOOKUP
-----------------------------------------------------------------------

DESCRIPTION:

   This procedure does the actual transfer when the direction.of.transfer
is TOPC.  It opens the host file, reads the file.records until EOF
is encountered, sends the file.records to the valid packet
layer, and closes the host file.

   Since data is dealt with in 8-bit bytes using a byte pointer and an
integer array, it is necessary to keep track of the actual number of
8-bit bytes in the file.record.

   It is also necessary to determine whether an end-of-record was
encountered on reads.  For text files, end-of-record (EOR.seen) occurs
when the break character is not null (the usual end-of-line).  For
binary files, EOR.seen is indicated in the low-order 4 bits of the
integer in which the end-of-record occurs.  The 4th bit is a 1 and
the remaining 3 bits contain the actual number of bytes in the integer.

-----------------------------------------------------------------------

-----------------------------------------------------------------------

PROCEDURE NAME:   Transfer Out
PROCEDURE ID:     AP.transfer.out

-----------------------------------------------------------------------

PSEUDOCODE:

open host file using host.filename for reading  ! OPEN, LOOKUP
set file.open to true
set application.parameters:
   record.type = data
   link.type = prim.appl
   who.sent = requestor
   importance = advisory
   cmd.type = not.a.command
   cmd.data = null
set how.many to 64

do until EOF
   execute AP.read.host.file returning file.record, appl.rec.size,
                                EOF, EOR.seen
   execute VA.create.pkt using application.record,
                                  application.parameters,
                                  appl.rec.size, EOF, EOR.seen

close host file

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Read Host File
PROCEDURE ID:     AP.read.host.file
PROCEDURE TYPE:   untyped

-----------------------------------------------------------------------

INPUT:            PARAMETERS: file.type   (value)

OUTPUT:           PARAMETERS: file.record (reference)
                              appl.rec.size (reference)
                              EOF   (reference)
                              EOR.seen (reference)

-----------------------------------------------------------------------

CALLED BY:        Transfer Out

CALLS:            SAIL intrinsics:  INPUT, ARRYIN, LOP

-----------------------------------------------------------------------

DESCRIPTION:

   This procedure looks at the file.type and does the appropriate
method of reading the host file.  AST files are read using INPUT,
binary files are read with ARRYIN.  The file.record is an integer
array with an 8-bit byte pointer set to the first element of the
array.  AST files are read first into a string variable.  They are
then converted into 8-bit format.

-----------------------------------------------------------------------


-----------------------------------------------------------------------

PROCEDURE NAME:   Read Host File
PROCEDURE ID:     AP.read.host.file

-----------------------------------------------------------------------

PSEUDOCODE:

case file.type of
   AST    : call INPUT using channel,
                             break.table
                       returning string.record
            set EOR.seen to (break.char = 0)
            set appl.rec.size to length (string.record)
            loop length (string.record) times
               call LOP using string.record
                        returning character  ! an integer
               move low-order 7 bits of character into file.record
                  using byte.pointer

   binary : set how.many to 64
            if number.left is 0 then
            execute ARRYIN using channel,
                                 how.many    ! contains actual number read
                           returning file.record
            if EOF then
               set number.left to right half of EOF flag
            else
               set number.left to how.many
            loop number.left times
               scan low-order 4 bits of each integer read
               if non-zero then
                  set EOR.seen to true
                  set appl.rec.size to 4 * index of the scanned integer
                  extract low-order 4 bits of the scanned integer and
                     subtract (4 - 3 bit quantity) from appl.rec.size
                  subtract index of scanned integer from number.left
                  quit loop
            left shift file.record

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Transfer In
PROCEDURE ID:     AP.transfer.in
PROCEDURE TYPE:   untyped

-----------------------------------------------------------------------

INPUT:            PARAMETERS: file.type   (value),
                              host.filename  (value)

                  application.parameters
                  application.record
                  EOF
                  EOR.seen
                  appl.rec.size

GLOBALS: left.over

OUTPUT:           file.record,
                  appl.rec.size
                  file.type,
                  EOF
                  EOR.seen
                  application.record
                  application.parameters
                  host.filename

-----------------------------------------------------------------------

CALLED BY:        File Transfer Driver

CALLS:            Write Host File
                  Close Host File
                  Handle Micro Command

-----------------------------------------------------------------------

DESCRIPTION:

   This procedure controls the actual transfer when the direction.of.transfer
is TOHOST.  It opens the host file, retrieves the data coming from the
micro from the valid packet layer, and writes the data to the host file.
When the EOF indicator is received from the valid packet layer, the
file is closed.  If the data coming from the valid packet layer is a
command, it is assumed the micro has a problem and control passes to
Handle Micro Command.  If the micro has sent an unable to continue command,
the process then aborts.  If some positive response comes through,
it is ignored and the transfer continues.

-----------------------------------------------------------------------

-----------------------------------------------------------------------

PROCEDURE NAME:   Transfer In
PROCEDURE ID:     AP.transfer.in

-----------------------------------------------------------------------

PSEUDOCODE:

open temporary host file in write mode using temp.filename   ! ENTER
set file.open to true
set left.over to 0   ! number of bytes left over after a write to a
                       binary file - full integers only are written

do until EOF
   execute VA.Get.Incoming.Application.Record returning
                                              application.record,
                                              application.parameters,
                                              appl.rec.size,
                                              EOF, EOR.seen
   { check to be sure you've got a data-record }
   if record.type is command then
      execute AP.handle.Micro.command using application.record,
                                          application.parameters,
                                          appl.rec.size,
   else
      execute AP.write.host.file using appl.record, file.type
                                       appl.rec.size, EOF, EOR.seen

execute AP.close.host.file using host.filename

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Write Host File
PROCEDURE ID:     AP.write.host.file
PROCEDURE TYPE:   untyped

-----------------------------------------------------------------------

INPUT:            PARAMETERS: file.record (value),
                              file.type   (value),
                              file.rec.size (value),
                              EOF         (value),
                              EOR.seen    (value)

                  GLOBALS:    left.over
                              saved.bytes

OUTPUT:           file.record,
                  string.record

-----------------------------------------------------------------------

CALLED BY:        Transfer In

CALLS:            SAIL intrinsics:  CPRINT, ARRYOUT

-----------------------------------------------------------------------

DESCRIPTION:

   This procedure looks at the file.type and performs the appropriate
method of writing the host file.  AST files are first converted from the
8-bit ASCII of the file.record's integer array to 7-bit ASCII in a string
variable, and then written to the file using CPRINT.  Binary files are
written using ARRYOUT.  On binary files, the last word written gets the
actual number of 8-bit bytes contained in the last word in the low-order
4 bits (which are usually unused).  All writing is done to a temporary
file.  If the transfer completes normally, the close routine renames
the temporary file to the name the user gave as the host.filename.

   Binary files may have end-of-record indicators in them.  If the
EOR.seen boolean is true, set the 4th bit of the last integer to be
written to a 1, set the remaining 3 bits of this integer to the actual
number of 8-bit bytes in the integer.

-----------------------------------------------------------------------

-----------------------------------------------------------------------

PROCEDURE NAME:   Write Host File
PROCEDURE ID:     AP.write.host.file

-----------------------------------------------------------------------

PSEUDOCODE:

case file.type of
   AST    :
         if EOR.seen then
            set string.record to null
            loop through file.record appl.rec.size times
               deposit byte and increment byte pointer into integer
               concatenate the integer onto the string.record
            call CPRINT using string.record
         else
            loop through file.record appl.rec.size times
               deposit byte and increment byte pointer into integer
               concatenate the integer onto the string.record (append)

   binary:
            if left.over > 0 then   ! number left from last write
               right shift file.record (8 * left.over) places
               move saved.bytes into leftmost part of file.record
               add left.over to appl.rec.size
            set how.many to appl.rec.size DIV 4
            set left.over to appl.rec.size MOD 4  ! extra bytes
            if left.over > 0 then
               left shift (8 * left.over) places from
                     file.record[how.many + 1] into saved.bytes
            if EOF or EOR.seen then
               if left.over > 0 then   ! write 'em all
                  increment how.many by 1
               if EOR.seen then
                  add 8 to left.over
               move left.over into rightmost 4 bits of
                                                file.record[how.many]

            call ARRYOUT using file.record, how.many

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Close Host File
PROCEDURE ID:     AP.close.host.file
PROCEDURE TYPE:   untyped

-----------------------------------------------------------------------

INPUT:            PARAMETERS: host.filename  (value)

OUTPUT:

-----------------------------------------------------------------------

CALLED BY:        Transfer In

CALLS:            SAIL intrisics:   RENAME, CLOSE

-----------------------------------------------------------------------

DESCRIPTION:

   Writes to the host file are really done to a temporary file.  When
the transfer has completed normally (EOF is sensed from the micro),
the temporary file is renamed as the host.filename.

-----------------------------------------------------------------------

PSEUDOCODE:

if replace then   ! delete original
   call OPEN using another.channel, host.filename
   call RENAME using another.channel, null
   call CLOSE using another.channel

execute RENAME using channel, host.filename
execute CLOSE using channel

-----------------------------------------------------------------------

-----------------------------------------------------------------------

LAYER:            APPLICATION
PROCEDURE NAME:   Close Up
PROCEDURE ID:     AP.close.up
PROCEDURE TYPE:   untyped

-----------------------------------------------------------------------

INPUT:

OUTPUT:           application.record
                  application.parameters
                  EOF
                  EOR.seen
                  in.closeup.mode

-----------------------------------------------------------------------

CALLED BY:        File Transfer Driver

CALLS:            Create Packet        ! Valid Packet Layer

-----------------------------------------------------------------------

DESCRIPTION:

   Resets application.record and application.parameters to end the
protocol transfer.  Sets EOF to force the packet to be sent.

-----------------------------------------------------------------------

PSEUDOCODE:

set application.parameters:
   record.type = command
   link.type = prim.appl
   who.sent = requestor
   importance = advisory
   cmd.type = reply.opt
   cmd.data = null
set application.record to 5   ! terminate session, end protocol mode
set appl.rec.size to 1

set in.closeup.mode to true
set EOF to true   ! should already be true
set EOR.seen to true
execute VA.create.packet using application.record,
                               application.parameters,
                               appl.rec.size,
                               EOF, EOR.seen
print "File transfer completed normally."

-----------------------------------------------------------------------

    8 rd