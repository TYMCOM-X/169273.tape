4.0  APPLICATION LAYER               PROCESS SPECIFICATIONS


4.1                         *** Startup ***

4.1.1 Parse Cmdline
search cmdline for keyword FROMHOST
if FROMHOST or TOPC found then
    set direction of transfer equal to FROMHOST
    if there is a token following FROMHOST or TOPC then
         set host_filename equal to token following FROMHOST or TOPC
         if there is a token following host_filename then
              set pc_filename equal to token following host_filename
         else { no pc_filename }
              set cmdline_error
    else { no host_filename }
         set cmdline_error
else { no fromhost or topc }
    search cmdline for keyword TOHOST or FROMPC
    if TOHOST or FROMPC found then
         set direction of transfer to TOHOST
         if there is a token following TOHOST or FROMPC then
              set host_filename equal to token following TOHOST or FROMPC
              if there is a token following host_filename then
                   set pc_filename equal to token following host_filename
              else { no pc_filename }
                   set cmdline_error
         else { no host_filename }
              set cmdline_error
    else { no keyword found }
      set cmdline_error

if not cmdline_error then
    set file_type to AST { Abstract Symbolic Text }
    set carry_date off
    set replace off
    search for the character '/' { options follow }
    if '/' found then
         if there is at least one token following the '/' then
            do until no_more_tokens
                   case token of
                        'B'  : set file_type to binary
                        'D'  : set carry_date on
                        'R'  : set replace on
                        other: set cmdline_error
         else { no token }
              set cmdline_error
if cmdline_error then
    execute Syntax Abort
else
   execute Check Host Files


4.1.2 Check Host Files
if FROMHOST then
    if host-filename exists then
         if file_type is AST then
              set file_size to 5 x number of words
         else
              set file_size to 4 x number of words
   else { file does not exist }
         set file-missing-error
else { TOHOST }
    if host-filename exists then
         if replace is false then
              set replace-file-error
if file-missing-error or replace-file-error then
    execute File Error Abort
else
    execute Wake Micro


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



4.2                     *** Set Up Transfer ***

4.2.1 Wake Micro
initialize Positive_Reply_Received, End_Of_File flag to false

set application_cmd equal to 12
set application_parameters to:
  record_type = C (command)
  link_type = L (link control)
  who_sent = R (host)
  importance = true
  cmd_type = 0 (command, reply optional)
  cmd_data = B
send application_cmd and application_parameters to Valid Packet Layer
execute Set Up Transfer


4.2.2 Set Up Transfer
set application_parameters to:
   record_type = C
   link_type = P
   who_sent = R
   importance = true
   cmd_type = 0
if direction_of_transfer is FROMHOST then
  send the following appl_cmds to the Valid Packet Layer:
         application_cmd                  cmd_data
      2 { put yourself in receive mode }    null
      6 { here's the filename }            pc_filename
  if replace is true then
      7 { here's what to do with existing file } 1 { replace }
  else { replace is false }
      7                                          3 { error }
      8 { size of file }                    file_size
  if file_type is binary then
      9 { file type }                            1 { binary }
  else { AST }
      9                                          3

else { direction is TOHOST }
  send the following appl_cmds and appl_params to the Valid Packet Layer:
      1 { put yourself in send mode }         null
      6 { here's the filename }              pc_filename

reset cmd_type, appl_cmd, and cmd_data and send to Valid Packet Layer:
cmd_type = 1 { command, reply is required }
      3 { start the transfer }                null




                      *** End Set Up Transfer ***



4.3 Outgoing Transfer { direction of transfer = FROMHOST }
{ This process is started by Deal With Answers following receipt
  of Positive reply from the micro coming from the Valid Packet
  Layer. }

open file
set appl_params:
   record_type = D
   link_type = P
   who_sent = R
   importance = true
   cmd_type = null
   cmd_data = null
read file_record

do until end_of_file
   if file_type is AST then
      convert 7-bit ASCII bytes to 8-bit ASCII
   else { binary }
      convert 36-bit words to 4 8-bit bytes
   move converted bytes to appl_record
   send appl_record, appl_params to Valid Packet Layer
   read next record

close file
execute Close-Up


4.4 Incoming Transfer { direction of transfer = TOHOST }
  { Incoming Transfer is executed following Set Up Transfer but
    not directly - it is initiated from the Valid Packet Layer
    as data from the micro is received }

if no Positive Reply has been received from the micro then
   execute Abort { this means data has started coming in without the
                   start command being acknowledged }
open file
while not EOF_flag do
   read next data_record
   if data_record is not null then
      if file_type is AST then
         convert 8-bit ASCII bytes to 7-bit ASCII
      else { binary }
         move 4 8-bit bytes into one 36-bit word
      move converted bytes to file_record
      write file_record

close file
execute Close-Up


4.5 Deal With Answers
{ This process receives all control or commands from the Valid Packet
  Layer (data from the Valid Packet Layer is received by Incoming
  Transfer).  It distinguishes between link control and commands and
  determines whether or not the transfer can proceed.
  This release of the program will not support link control negotiation.
  Also, a response from the micro during the transfer process will
  cause an abort (there should not be any - unless a disk full
  condition is encountered).
  If the transfer is to be terminated, it sends the appropriate error
  message to Abort (which closes any open files, and aborts). }

if Positive_Reply_Received then
   case cmd_type of
      0,1   : set abort message to
              "Unimplemented micro command received.  Aborting."
              execute Abort
      2     : { ignore }
      3,5   : set abort message to
              "Micro cannot continue.  Aborting."
              execute Abort
      4     : set reason_code to first byte of cmd_data
              case reason_code of { see TYMFTP document appendix A-5
                                    for reason_code table message }
              { this should handle disk full conditions }
              set abort message to reason_code table message
              execute Abort
if link_type is L { link control }
  if who_sent is S then
  case cmd_type of  { 2 = positive reply }
    0,1,5 : set abort message to
            "Unimplemented micro request received.  Aborting."
            execute Abort
    2     :  { null - if the micro sends a positive reply to
               a reply-optional command, disregard it }
    3     : set abort message to
            "Unable to establish communications link with micro.
            Aborting."
            execute Abort
    4     : set reason_code to first byte of cmd_data
            case reason_code of { see TYMFTP document appendix A-5
                                  for reason_code table message }
            set abort message to reason_code table message
            execute Abort
  else { who_sent isn't S  - this should never happen }
    set abort message to
    "Unable to establish communications link with micro.  Aborting."
    execute Abort
else { link_type is P - this is a response to a host command }
  case cmd_type of
    0,1,5 : same as above
    2   : if appl_cmd {command echoed} is 3 then
             set Positive_Reply_Received flag to true
             if direction_of_transfer is FROMHOST then
                execute Outgoing Transfer
    3   : set command_echoed to first byte of cmd_data
          set abort message to
          "Micro unable to perform host command:", appl_cmd, "Aborting."
          execute Abort
   4   : set command_echoed to first byte of cmd_data
         set reason_code to next byte of cmd_data
         set abort message to
         "Micro unable to perform host command:', command_echoed,
         "reason follows:"
         determine reason from TYMFTP Appendix A-5 and print message
         execute Abort


4.6 Close-Up
set appl_params to:
   record_type = C
   link_type = P
   who_sent = R
   importance = true
   cmd_type = 0 { reply optional }
   cmd_data = null
set appl_cmd to 5 { terminate session, end protocol mode }

send appl_cmd, appl_params to Valid Packet Layer
print normal close message
exit program


4.7 Abort
print appropriate termination message
if host_filename is open then
   if replace is true then
      restore original file
   else
      purge file
if connection to PC is still present then
   send PC explanatory reply (cmd_type = 4), echo command,
   reason_code = 5 (DO NOT RETRY)
   send PC command (cmd_type = 1, no reply), appl_cmd = 5 (terminate)
exit program

 