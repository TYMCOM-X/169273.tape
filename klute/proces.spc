                               COPYPC.X


                        PROCESS SPECIFICATIONS


                         1.0  TRANSPORT LAYER

                       Incoming Transport Layer

1.1  Get Decoded Packet
  init searching-for-lead-in-character to true
  init in-decode-mode to false
  init first-char to true
  init End-of-Packet to false

  while not End-of-Packet do
     Read byte
     If searching-for-lead-in-character then
          if lead-in-character then
               initialize byte counter to 1
               clear decoded-packet
               set searching-for-lead-in-character to false
               discard lead-in-character
          else
               discard byte
     Else  { not searching-for-lead-in-char }
          increment byte counter
          if byte counter > maximum-packet-size then
               set searching-for-lead-in-character to true
               discard byte
          else  { within max-packet-size }
          { assume end-of-packet char can appear while in-decode-mode }
            if byte is end-of-packet character then
               discard end-of-packet character
               set end-of-packet to true
            else
               if in-decode-mode then
                  if first-char then
                     if not shift-out-of-hex char then
                        save byte
                        set first-char to false
                     else
                        set in-decode-mode to false
                        discard shift-out-of-hex char
                  else  { second char }
                     decode saved byte with current byte
                     stuff decoded byte into decoded-packet
                     set first-char to true
               else   { not decoding }
                  if shift-to-hex char then
                     set in-decode-mode to true
                     discard shift-to-hex
                  else  { normal, non-encoded byte }
                     stuff byte into decoded-packet




                       Outgoing Transport Layer


1.2  Encode and Send Bytes
     init in-encode-mode to false
     Send lead-in-character
     read one byte from packet
     Do until no more bytes in packet
        if byte is not in transmittable set then
          if not in-encode-mode then
             send shift-to-hex-char
             set in-encode-mode to true
          encode byte using algorithm in TYMFTP
          send encoded bytes
        else  { byte is transmittable without encoding }
          if in-encode-mode then
             read next packet byte
             if not end-of-packet then
                if next packet byte is not in transmittable set then
                   encode and send current byte
                   encode and send next packet byte
                else
                   send shift-out-of-hex char
                   set in-encode-mode to false
                   send current byte
                   send next byte
             else { hit end-of-packet on read of next byte }
                send shift-out-of-hex byte
                send current byte
          else  { not in-encode-mode }
            send byte
        if not end-of-packet then
           read next packet byte

     Send end-of-packet character






1.4  Send Reset
   send escape
   send lower case u
   create initial-ACK  { escape x hex 20 hex 20 }
   { this is outside the protocol but TYMCOMM needs it to reset
     his block id counter  }
   execute 2.8 Calculate Chksum using initial-ACK



                           2.0  BINARY LAYER


                         Incoming Binary Layer


2.1  Get Incoming Packet
{ called from 3.1 Get Incoming Application-Record }
   while there's no in-sequence-packet (a Valid Packet) in the window do
      set timeout-type to waiting-for-data/cmd
      execute 2.5 Reset Timer using timeout-type
      execute 1.1 Get Decoded Packet returning decoded-packet
      execute 2.2 Chksum Preprocessor
      if good-chksum then
         execute 2.3 Determine Packet Type
      else
         execute 2.6 ACK/NAK using current-incoming-PID
         set timeout-type to waiting-for-data/cmd
         execute 2.5 Reset Timer using timeout-type


2.2  Chksum Preprocessor
     Remove checksum bytes from decoded-packet
     Recalculate checksum on remaining bytes using TYMFTP algorithm
     If new checksum equals old checksum then
         set good-chksum to true
     Else
         set good-chksum to false
         discard packet

     set timeout-type to shut-off
     execute 2.5 Reset Timer using timeout-type


2.3  Determine Packet Type
     Case Packet-Type-Character of
          ACK:           clear ACKed packets from Outgoing Window buffer
          NAK:           execute 1.2 Encode and Send using NAKed packet
          RFR:           execute 2.6 ACK/NAK Processor using
                                 current-incoming-PID
          Data/command:  remove PTC byte
                         execute 2.4 Incoming Window Manager using packet
          else:          execute 4.7 Abort using PTC-error




2.4  Incoming Window Manager
{ in sequence PID means either one more than current-incoming-PID or
  beginning of PID range }

    If this-PID is in sequence then
      set current-incoming-PID to this-PID
      remove PID from packet
      place packet into incoming-window

    execute 2.6 ACK/NAK Processor using current-incoming-PID
    { duplicates will effect a NAK being sent on the next one expected }



2.5  Reset Timer
   if timeout-type = shut-off then
     close timer
   else
     if timer is already-running then
        exit process

     if timeout then
        case timeout-type of
         waiting-for-data/cmd
            execute 2.6 ACK/NAK Processor using current-incoming-PID
            increment incoming-timeout-counter
            if incoming-timeout-counter > max-incoming-timeouts then
               execute 4.7 Abort using max-incoming-timeouts-error
         waiting-for-ACK
            execute 2.10 Send RFR using current-outgoing-PID
            increment outgoing-timeout-counter
            if outgoing-timeout-counter > max-outgoing-timeouts then
               execute 4.7 Abort using max-outgoing-timeouts-error

     start timer

* Note:  Timer is restarted using same parameter as it was
         when timeout occurred following the execution of the
         appropriate module.



                         Outgoing Binary Layer


2.6  ACK/NAK Processor
     If last-PID-ACKed = current-incoming-PID then  (already ACKed)
          assign NAK PTC and current-incoming-PID+1 to outgoing-ACK/NAK
     Else
          assign ACK PTC, current-incoming-PID and greatest-incoming-PID
             to outgoing-ACK/NAK
          set last-PID-ACKed to current-incoming-PID
     execute 2.8 Calculate Chksum using outgoing-ACK/NAK


2.7  Outgoing Window Manager
   if looking-for-first-ACK then
      execute 1.4 Send Reset
      execute 2.5 Reset Timer using waiting-for-ACK
   insert data/cmd-packet into window buffer
   set packet-sent to false
   do until packet-sent
      if current-outgoing-PID <= greatest-outgoing-PID then
         execute 1.2 Encode and Send Bytes using data/cmd-packet
         execute 2.5 Reset Timer using waiting-for-ACK
         set packet-sent to true
         if in-closeup-mode then
            execute 2.5 Reset Timer using shut-off
      else
         execute 1.1 Get Decoded Packet
         execute 2.2 Chksum Preprocessor
         if good-chksum then
          Case packet-type of
            incoming-ACK-packet -
             if looking-for-first-ACK then
                set looking-for-first-ACK to false
             else
                clear ACKed packets from window buffer
             update current-outgoing-PID, greatest-outgoing-PID from ACK
            incoming-NAK-packet -
             clear packets from window up to but not including NAKed PID
             { receiving a NAK on next packet implies ACK of previous
               packet }
             execute 1.2 Encode and Send Bytes using NAKed-PID-packet
            data/cmd-cksummed-packet -  { this should only occur if the
                                        micro needs to abort }
             execute 2.4 Incoming Window Manager { put into window }
             execute 3.2 Get Unexpected Response { tell Application
                                        layer to deal with response }
         else { not good-chksum }
           execute 2.10 Send RFR
           set timeout-type to waiting-for-ACK
           execute 2.5 Reset Timer using timeout-type




2.8  Calculate Chksum
     Use TYMFTP algorithm to calculate checksum
     Attach checksum onto end of packet
     If PTC is flow control then
          execute 1.2 Encode and Send Bytes using
                             flow-control-cksummed-packet
     Else
          execute 2.7 Outgoing Window Manager using
                             data/cmd-cksummed-packet

2.9  Assign PTC, PID
     Assign data/command PTC to valid-packet
     Increment current-outgoing-PID
     If current-outgoing-PID > end-of-PID-range then
          set current-outgoing-PID = beginning-of-PID-range
     Assign current-outgoing-PID to packet
     execute 2.8 Calculate Cksum using packet-with-header


2.10 Send RFR
     Assign RFR PTC to RFR-packet
     execute 2.8 Calculate Checksum using RFR-packet


                        3.0  VALID PACKET LAYER

                      Incoming Valid Packet Layer


3.1  Get Incoming Application-Record
do until EOR
   if the packet-buffer is empty then
      execute 2.1 Get Incoming Packet returning incoming-packet
      init incoming-packet-pointer
      clear incoming-window
      set EOD to true

   if EOD then
      read a byte  {DCB}
      increment incoming-packet-pointer
      init data-buffer
      set EOD to false

   case class-type of:
      data: execute 3.3 Fill Data Buffer using data-packet
      cmd : execute 3.4 Build Application Command and Parameters
                           using command-packet and DCB
   end case

   read next packet byte
   if byte is an EOF-byte then
      set EOF to true
      increment incoming-packet-pointer
      read next packet byte
      if byte is an EOD-byte then
         set EOD to true
         increment incoming-packet-pointer

end do until EOR


3.2  Get Unexpected Response
{ only invoked during an Outgoing Transfer when a data/cmd packet is
  received from the micro instead of a flow control packet }
   execute 4.5 Get Micro Response




3.3  Fill Data Buffer
     Do until EOR or end-of-packet
     {end-of-valid-packet implies end-of-data-unit}
          Read next byte of valid-packet (assume it is a DH)
          Increment incoming-packet-pointer
          divide DH into its 2 fields  (ABC, DEFGH)
          case field ABC of
               000:  (invalid)
                     execute 4.7 Abort using data-error(1)
               001:  if DEFGH = 0 then    (EOR)
                       set EOR to true
                     else
                          insert next DEFGH # bytes into buffer
                          increment incoming-packet-pointer by DEFGH # bytes
               010:  if DEFGH = 0 then    (EOF)
                          execute 4.7 Abort using data-error
                          { should not be reading EOF in this process }
                     else
                          insert DEFGH # bytes of binary zero into buffer
               011:  if DEFGH = 0 then    (EOD)
                          execute 4.7 Abort using data-error
                          { should not be reading EOD in this process }
                     else
                          insert DEFGH # ASCII blanks into buffer
               100:  if DEFGH = 0 then
                          (invalid)
                          execute 4.7 Abort using data-error(2)
                     else
                          read next byte of data unit into CHAR
                          insert DEFGH # CHARs into buffer
                          increment incoming-packet-pointer
               101:  if DEFGH = 0 then
                          (invalid)
                          execute 4.7 Abort using data-error(3)
                     else
                          insert DEFGH # EBCDIC blanks into buffer
               110:  execute 4.7 Abort using data-error(4)
               111:  execute 4.7 Abort using data-error(5)
          end case
          if there are no more bytes in packet then
               set end-of-packet to true
     End do until EOR or end-of-packet


3.4 Build Application Command and Parameters
     Set application-parameters from DCB:
          Link-Type
          Who-Sent
          Importance
          Command-Type
     initialize application-command to 0
     initialize cmd-data to ' '
     set end-of-command off
     do until end-of-command
          read data header byte
          increment incoming-packet-pointer
          divide data header into fields ABC, DEFGH
          case ABC of
                    000:  (invalid)
                          execute 4.7 Abort using command-error(1)
                    001:  if DEFGH > 0 then
                               read DEFGH # bytes
                               increment incoming-packet-pointer
                               if application-command = 0 then
                                    set application-command to command-byte
                                    set cmd-data to remainder
                               else
                                    append DEFGH # bytes to cmd-data
                          else
                               if application-command > 0 then   (EOR)
                                    set end-of-command on
                               else
                                    execute 4.7 Abort using
                                                command-error(2)
                     010:  if DEFGH = 0 then   (EOF)
                                execute 4.7 Abort using command-error(3)
                                { should not be reading EOF here }
                           else
                                if application-command > 0 then
                                     append DEFGH # bytes of binary zero
                                        to cmd-data
                                else
                                     execute 4.7 Abort using
                                                 command-error(4)
                     011:  if DEFGH = 0 then   (EOD)
                                execute 4.7 Abort using command-error(5)
                                { should not be reading EOD here }
                           else
                                if application-command > 0 then
                                     append DEFGH # ASCII blanks to
                                        cmd-data
                                else
                                     execute 4.7 Abort using
                                                 command-error(6)
 
                     100:  if DEFGH = 0 then
                                (invalid)
                                execute 4.7 Abort using command-error(7)
                           else
                                if application-command > 0 then
                                     read next byte into CHAR
                                     increment incoming-packet-pointer
                                     append DEFGH # CHARs to cmd-data
                                else
                                   execute 4.7 Abort using
                                               command-error(8)

                     101:  if DEFGH = 0 then
                                (invalid)
                                execute 4.7 Abort using command-error(9)
                           else
                                if application-command > 0 then
                                     append DEFGH # EBCDIC blanks to
                                        cmd-data
                                else
                                   execute 4.7 Abort using
                                               command-error(10)
                     110:  execute 4.7 Abort using command-error(11)
                     111:  execute 4.7 Abort using command-error(12)
                endcase
           end do until end-of-command




                      Outgoing Valid Packet Layer

3.5  Read Outgoing Application-Record + Application-Parameters
     Read application-record into buffer
     Read application-parameters   (ex. record-type, EOF-set, etc)
     If record-type is command and room-in-packet - record-size < 0 then
          execute 3.9 Send Ready-Packet using packet
          execute 3.6 Create Data Class Byte using
                  initialized-packet, buffer and application-parameters
     Else
          if max-out-pkt-size > room-in-packet and  (pkt not empty)
            application-parameters = previous-application-parameters then
               execute 3.7 Create Data Header using packet and buffer
          else
               execute 3.6 Create Data Class Byte using packet, buffer
                           and application-parameters


3.6  Create Data Class Byte
     If max-out-pkt-size > room-in-packet then  (pkt not empty)
          insert EOD DH into packet
          decrement room-in-packet
     Create DCB using application-parameters
     Insert DCB into packet
     Decrement room-in-packet
     execute 3.7 Create Data Header using packet and buffer


3.7 Create Data Header
     If (room-in-packet >= record-size) and (record-size <= 31) then
          set data header equal to record-size
     Else if room-in-packet >= record-size then
          set data header equal to 31
     Else
          set data header equal to room-in-packet
     t data header into packet
     Decrement room-in-packet
     execute 3.8 Add Buffer to Packet using packet and buffer



3.8  Add Buffer to Packet
     Stuff data header # bytes from buffer into packet
     Decrement room-in-packet by data header # bytes
     Left shift buffer data header # bytes
     If record-size = 0 then
          insert EOR data header into packet
          decrement room-in-packet
          if EOF then
               insert EOF data header into packet
               execute 3.9 Send Ready-Packet using packet
     Else
          if room-in-packet > 1
               execute 3.7 Create Data Header using packet
          else
               execute 3.9 Send Ready-Packet using packet
               execute 3.6 Create Data Class Byte using buffer



3.9  Send Ready-Packet
     execute 2.9 Assign PTC PID using valid-packet
     Initialize packet
     Set room-in-packet equal to max-out-pkt-size

       NOTE: max-out-pkt-size = .5 * max-pkt-size-on-micro - 9

                   {9 = LIC + PTC + PID + 6(CKSUM) }


4.0  APPLICATION LAYER               PROCESS SPECIFICATIONS


4.1                         *** Startup ***

4.1.1 Parse Cmdline
search cmdline for keyword FROMHOST
if FROMHOST or TOPC found then
    set direction-of-transfer equal to FROMHOST
    if there is a token following FROMHOST or TOPC then
         set host-filename equal to token following FROMHOST or TOPC
         if there is a token following host-filename then
              set pc-filename equal to token following host-filename
         else { no pc-filename }
              set cmdline-error
    else { no host-filename }
         set cmdline-error
else { no fromhost or topc }
    search cmdline for keyword TOHOST or FROMPC
    if TOHOST or FROMPC found then
         set direction-of-transfer to TOHOST
         if there is a token following TOHOST or FROMPC then
              set host-filename equal to token following TOHOST or FROMPC
              if there is a token following host-filename then
                   set pc-filename equal to token following host-filename
              else { no pc-filename }
                   set cmdline-error
         else { no host-filename }
              set cmdline-error
    else { no keyword found }
      set cmdline-error

if not cmdline-error then
    set file-type to AST { Abstract Symbolic Text }
    set carry-date off
    set replace off
    search for the character '/' { options follow }
    if '/' found then
         if there is at least one token following the '/' then
            do until no-more-tokens
                   case token of
                        'B'  : set file-type to binary
                        'D'  : set carry-date on
                        'R'  : set replace on
                        other: set cmdline-error
         else { no token }
              set cmdline-error
if cmdline-error then
    execute 4.1.3 Syntax Abort
else
   execute 4.1.2 Check Host Files


4.1.2 Check Host Files
if FROMHOST then
    if host-filename exists then
         if file-type is AST then
              set file-size to 5 x number of words
         else
              set file-size to 4 x number of words
   else { file does not exist }
         set file-missing-error
else { TOHOST }
    if host-filename exists then
         if replace is false then
              set replace-file-error
if file-missing-error or replace-file-error then
    execute 4.1.4 File Error Abort
else
    execute 4.2 Set Up Transfer


4.1.3 Syntax Abort
display message:
"Commandline syntax error."
"Enter command in the following format:"
"COPYPC FROMHOST host-filename pc-filename / Binary Date Replace"
"       TOHOST                                                  "
exit program


4.1.4 File Error Abort
case file-error of
    file-missing-error  : display message:
         "No such host file."
    replace-file-error  : display message:
         "Host file already exists.  Specify replace."
exit program

                          *** End Startup ***



4.2  Set Up Transfer

initialize Positive-Reply-Received, End-Of-File (EOF), EOR,
           transfer-error to false
initialize EOD to true
initialize communications variables
initialize command-table:

cmd-num  appl-cmd          rec-type  who-sent  import  cmd-type  cmd-data
   1     12 { use table B }   L         R       false     0          B

if direction-of-transfer is FROMHOST then
   2      2 { receive mode}   C         R       false     0       null
   3      6 { filename }      C         R       false     0       pc-filename

   if replace then
   4      7                   C         R       false     0       1 {replace}
   else
   4      7                   C         R       false     0       3 {error}

   5      8 { file size is }  C         R       false     0       file-size

   if file-type is binary then
   6      9                   C         R       false     0       1 {binary}
   else
   6      9                   C         R       false     0       3 {AST}

else { direction-of-transfer is TOHOST }
   2     1 { send mode }      C         R       false     0       null
   3     6 { filename }       C         R       false     0       pc-filename

  last   3 { start transfer } C         R       true      1       null
  { last command is cmd-type 1 = command, reply required }

set cmd-table-ptr to 0
set looking-for-first-ACK to true
do until EOF
   increment cmd-table-pointer
   if cmd-table-pointer = number of commands in cmd-table then
      set EOF to true
   set application-cmd and application-parameters from cmd-table entry
         pointed at by cmd-table-pointer
   execute 3.5 Read Outgoing Application-Record + Application-Parameters

set EOF to false  { was turned on at the end of startup cmd sequence }
execute 4.5 Get Micro Responses { looking for positive-reply }

if positive-reply-received then
   set EOF to false
   if direction-of-transfer is FROMHOST then
      execute 4.3 Outgoing Transfer
   else
      execute 4.4 Incoming Transfer
else
   execute 4.7 Abort using no-positive-reply-received





4.3 Outgoing Transfer { direction of transfer = FROMHOST }
{ This process is started by Set Up Transfer following receipt
  of Positive reply from the micro coming from the Valid Packet
  Layer. }

open file
set appl-params:
   record-type = D
   link-type = P
   who-sent = R
   importance = true
   cmd-type = null
   cmd-data = null

read file record
do until EOF
   if file-type is AST then
      convert 7-bit ASCII bytes to 8-bit ASCII
   else { binary }
      convert 36-bit words to 4 8-bit bytes
   move converted bytes to data-record
   read file record
   if EOF then
      set EOF to true
   execute 3.5 Read Outgoing Application-Record + Application-Parameters

close file
execute 4.6 Close-Up


4.4 Incoming Transfer { direction of transfer = TOHOST }
open file

while not EOF-flag do
   execute 3.1 Get Incoming Application Record   { Valid Packet Layer }
   { check cmd-type to be sure you've got a data-record }
   if cmd-type is command then
      set transfer-error to true
      execute 4.5 Get Micro Response
   else
   if data-record is not null then
      if file-type is AST then
         convert 8-bit ASCII bytes to 7-bit ASCII
      else { binary }
         move 4 8-bit bytes into one 36-bit word
      move converted bytes to file-record
      write file-record
      clear data-record

close file
execute 4.6 Close-Up


4.5 Get Micro Responses
{ This process receives all control or commands from the Valid Packet
  Layer (data from the Valid Packet Layer is received by Incoming
  Transfer).  It determines whether or not the transfer can proceed.
  This release of the program will not support link control negotiation.
  Also, a response from the micro during the transfer process will
  cause an abort (there should not be any - unless a disk full
  condition is encountered).
  If the transfer is to be terminated, it sends the appropriate error
  message to Abort (which closes any open files, and aborts). }

while not EOF do
   if not transfer-error then
      execute 3.1 Get Incoming Application-Record returning command,
                        parameters, EOF-flag

   if importance = true then  { ignore advisory commands }
   case cmd-type of
      0,1   : execute 4.7 Abort using
                          "Unimplemented micro command received"
      2     : if command = 3 then
                  set Positive-Reply-Received to true
              { else ignore }
      3,5   : execute 4.7 Abort using
                          "Micro cannot continue.  Aborting."
      4     : set reason-code to first byte of cmd-data
              case reason-code of { see TYMFTP document appendix A-5
                                    for reason-code table message }
              { this should handle disk full conditions }
              set abort-type to reason-code table entry
              execute 4.7 Abort using abort-type



4.6 Close-Up

set appl-params to:
   record-type = C
   link-type = P
   who-sent = R
   importance = false
   cmd-type = 0 { reply optional }
   cmd-data = null
set appl-cmd to 5 { terminate session, end protocol mode }

set in-closeup-mode to true
execute 3.5 Read Outgoing Application-Record + Application-Parameters
print normal close message
exit program


4.7 Abort
if host-filename is open then
   if direction-of-transfer is TOHOST then
      if replace is true then
         restore original file
      else
         purge file
   else
      close file
if connection to PC is still present then
   set application-record, parameters: (cmd-type = 4), echo command,
        reason-code = 5 (DO NOT RETRY)
   execute 3.5 Read Outgoing Application-Record + Application-Parameters
   set application-record, parameters: (cmd-type = 1, no reply),
         appl-cmd = 5 (terminate)
   execute 3.6 Read Outgoing Application-Record + Application-Parameters

shut off timer(s)
print appropriate termination message using abort-type
exit program


  i@G’