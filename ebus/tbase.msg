.	<<	TBASE.370	>>

.SEC(|Base-Host Interface|)
.BEGIN TURN ON ""


	This chapter describes the protocol used between certain TYMSHARE
hosts, such as IBM-370, and the specialized ~<BASE> or ~<TYMCOM>
(~<ISIS Host Interface>)
connecting them to TYMNET.  It does not describe the hardware used to
connect a host to TYMNET.  Here the hardware is viewed as a mechanism for
transmitting messages consisting of some number of eight-bit bytes to and from
TYMNET.
.BEGIN BLOX
	NOTE:###Do not confuse base-host messages with the format of data
and control signals for a circuit within TYMNET.
.END

.SS(|TYMNET Overview|)
	A ~<TYMNET Circuit> is a full-duplex communication path for data
and control signals between two ports bounding TYMNET.

	A ~<circuit> is like two pipes:  The information that flows through
a pipe is in the form of 8-bit characters;  these may be either data
bytes or control signals.  In each pipe the information always flows
in the same direction;  the individual bytes never get out of sequence.

	The pipes are somewhat elastic, but the information within
the pipe can get crowded, either because the information is not being
removed from the end of the pipe fast enough, or because the flow is
restricted.  Normally, when characters get crowded (increased pressure),
they simply back-up in the pipe ("back-pressure").  Occasionally, the
information get over-crowded, and it necessary to relieve the pressure.
When this happens, a contiguous block of information is discarded and
replaced by a control signal called a "Black-Ball".  Since the Black-ball
is smaller than the information it replaces, the pressure is relieved.

.SS(|The BASE|)
	A host communicates with a part of TYMNET called a ~<base>.
TYMNET identifies a base port with a node number, identifying
the base, and a base port number.
The base port number is assigned by TYMNET, and does not necessarily
correspond to the host port number for a particular circuit.
The host port number is likely to be assigned to different base ports
for successive circuits.  Host port numbers
range from zero up to but not including a limit called the
~<base port limit>.  For the 370 bases the base port limit is currently
254 for TYMNET-II and 62 for TYMNET-I.
	Messages transfered between the host and base are packed into
~<Packets>, and the packets are the entity transmitted.
	Messages to and from the base are summarized in the
appendices.  Numbers in these tables are hexadecimal.  A
comma separates bytes in a message.  All bytes are considered
to be unsigned.  The first byte of a message is a type code.
The length of a message is a function of the type code.  Any
base-to-host message which is shown as being an odd number of
bytes in length has an extra byte at the end, not shown, whose
contents is of no significance.  Thus all base-to-host messages
are an even number of bytes in length, and each message starts
on an even byte-boundary (half-word) within a packet.
	In these tables, the symbol "P" indicates a host port number, and
it must be less than the base port limit (otherwise the message is
invalid).  Other symbols are explained below.
.BEGIN BLOX
	NOTE:###Any message not listed in the table is invalid.
.END

.SS(|Establishing Communication with the BASE|)
	In order to establish communications betwen the host and the base,
it is first necessary to reset the interface, using a ~<Reset Dialog>.
The reset may be initiated by either the host or by the base:

.SSS(|Host-initiated Reset Dialog|)

	When the host wishes to reset the interface, the following occurs:
.BEGIN OFFSET
	[]##The host assumes all circuits through the interface have been
zapped;  however, it does not send any corresponding zappers to the base.
It assumes any outstanding circuit-building dialogs have been terminated.
	[]##The host sends a Reset_Interface message to the base;  the
signature field (D1-D8) specifies a unique number for this Reset_Interface
message.
.END
.BEGIN BLOX
	Note:###The host will typically include a Set_Timeout message and
a Set_Host_Number message in the same packet as the Reset_Interface
message.  (It may include other messages as it sees fit.)
.END
	On receipt of the Reset_Interface message, the base resets the
interface.  It then processes the rest of the packet containing the
Reset_Interface message.  The base then sends a Reset_Acknowledgement
message to the host.  The signature (D1-D8) in the Reset_Acknowledgement
message matches that in the Reset_Interface message sent by the host.
	When the base resets the interface, the following occurs:
.BEGIN OFFSET
	[]##All circuits through the interface are zapped (but zappers are
not sent to the host).
	[]##Any circuit-building dialogs are terminated.
	[]##The timeout period is set to 60 seconds.
	[]##The interface is considered down.
.END
	The next level-1 message the host receives should have a matching
Reset_Acknowledgement message.  If not, the host starts the reset
dialog from the beginning again.

.SSS(|Base-initiated Reset Dialog|)
	When the base wishes to start a reset dialog, the following occurs:
.BEGIN OFFSET
	[]##The base resets the interface.
	[]##The base sends a Reset_Interface message to the host with a
signature field (D1-D8) of "1234567812345678".  This packet should have
no ordinary messages.
	[]##On receipt of the Reset_Interface message, the host initiates a
reset dialog.
	[]##If the next packet the base receives does not have a Reset_Interface
message, the base will start the reset dialog over.
.END
.SSS(|Protocol errors and Timeouts|)
.BEGIN BLOX
	NOTE:###When the host detects a protocol error in a record from the
base, or times-out the base, it should initiate a reset dialog.  The
base does likewise.
.END

.SS(|HOST State|)
.TOPIC(|HOST State|)

.TOPIC(|up, host|)
.TOPIC(|down, host|)
.TOPIC(|host up|)
.TOPIC(|host down|)
	TYMNET considers each host to be either up or down.  A
host goes down when its interface is reset.  A host comes up
when it sends its base a Set_Host_Number, Host_Open, or Host_Shut
message.  The supervisor will not build
circuits to a host which is down.
	TYMNET remembers a ~<host number> for each host.  When a base
receives a Set_Host_Number message, if the HN1/HN2 bytes of the
message is nonzero the host number for this host is set to HN1_HN2.
If two hosts have the same host number confusion will result.
The ~<host port limit> for this host is set to the NP field of the
message if that is nonzero, otherwise to 48.  The NP field must
be less than or equal to the base port limit (otherwise the
message is invalid).  In addition, if this is not the first
Set_Host_Number message since the host began exercising the
hardware interface since the interface was reset, circuit
zappers are sent on all circuits from this host.
.TOPIC(|host open|)
.TOPIC(|host shut|)
.TOPIC(|shut, host|)
.TOPIC(|open, host|)
	A host is considered to be either open or shut.  The Host_is_Open
message causes the host to be open.  The Host_is_Shut and
Set_Host_Number messages cause the host
to be shut.  The supervisor will not build to a shut host
circuits whose username does not have shut override.
	When the host sends a Test_Pattern_Probe message, the base
will send a Test_Pattern_Response message with the same data in
the D1, D2, D3, and D4 bytes.
.BEGIN TURN ON "#"
	When a host comes up, or in response to a Supervisor_Clock_Request,
the base will send a Supervisor_Clock_Data message.
Bytes T1, T2, T3, and T4 form a 32 bit binary number (T1 is
most significant) which is the number of seconds since
GMT#00:00:00 1#January#1974.
.END

.SS(|Building Circuits|)
.TOPIC(|circuit initiation|)
.TOPIC(|circuit origination|)
.TOPIC(|circuit destination|)
.TOPIC(|circuit termination|)
.TOPIC(|termination, circuit|)
.TOPIC(|destination, circuit|)
.TOPIC(|origination, circuit|)
.TOPIC(|initiation, circuit|)
.TOPIC(|Caller|)
.TOPIC(|Callee|)
	When a circuit is built, one end is the caller (initiating,
origination) end, the other is the callee (passive, destination) end.
The callee is also known as a HOST;  circuits can only be built to ~<HOSTS>.

.SSS(|Externally-initiated Circuits|)
	When a circuit is built to a host,
that host receives an Externally-Initiated_New_Circuit message.
An available host port is selected on the host and the message
identifies the newly assigned host port number.  The host port
number will be less than the host port limit.  The following
data bytes are placed in the circuit going to the callee,
in order:
.BEGIN OFFSET
	[0]#The terminal type code for the circuit (see appendix).
	[1]#The high 6 bits of the node number of the initiating
port (right justified).
	[2]#The low 6 bits of the node number of the initiating port
(right justified).
	[3]#The base or remote port number of the initiating port.
	[4]#If the username of the circuit has shut override,
(ASCII "!" (hex 21), otherwise no data byte
is sent in this position.
	[5]#The upper case ASCII characters of the username of the
circuit.
	[6]#The login line terminator of the circuit (semicolon or
carriage return).
	[7]#If the number of data bytes in [0] through [6] is odd,
zero, otherwise no data byte is sent in this position.
.END
	The data bytes and control codes sent by the initiating end
will arrive subsequent to the above information.
.BEGIN BLOX
	NOTE:###the above information is subject to character gobblers.
.END

.SSS(|Internally-initiated Circuits|)
	A host can initiate building a circuit; such a circuit is
called an ~<auxiliary circuit>.  The host begins a
circuit-building dialog by sending a Supervisor_Request_message (to the
base).  The Q field ("requesting port") of this message
identifies this particular circuit-building dialog.  There must
not be another circuit-building dialog in progress by this host
with the same Q field.  All Q fields must be less than the base
port limit (otherwise the message is invalid).
	When the supervisor is willing to proceed with the dialog
the host will receive a Supervisor_Response_message with a Q
field identifying this dialog.  The E field will be zero.  Then
the host sends a sequence of Supervisor_Login_Character
messages.  Again, the Q fields identify the dialog.  The D
fields of these messages contain the login string, consisting
of the following ASCII characters, in order:
.BEGIN OFFSET
	11]#The string to be used as the username of the circuit.
	12]#A colon.
	[3]#The decimal representation of the host number of the
host to which the circuit is to be built (the
destination host); or an asterisk, signifying the host
number of the supervisor.
The username must be valid on the destination host.
	[4]#The character to be used as the login line terminator of
the circuit, which must be semicolon or carriage return.
.END
	An available host port will be selected
to be used as the initiating port.  Its host port number will
be less than the host port limit.  If the circuit is
successfully built the host will then receive an
Internally-Initiated_New_Circuit message giving (in the P
field) the (newly assigned) initiating host port number.
This is followed by a
Supervisor_Response message with a Q field identifying this
dialog and a zero E field.  This second Supervisor_Response
message ends the dialog.
	If the circuit could not be built (for any of the reasons
listed below), the host will receive a Supervisor_Response
message with a Q field identifying this dialog and with a
nonzero error code in the E field telling the reason for the
failure:
.BEGIN OFFSET
	Error Code		Meaning
.SKIP
	1###Format error in login string;
	2###Username not in MUD;
	3###MUD unreadable due to disk error;
	4###Other (including destination host down, destination
host cannot be reached through the network,
circuits busy, initiating or destination host out
of ports, destination host is shut and username
does not have shut override, username not valid on
destination host, etc.);
	5###Error related to downline load or dialout request;
	6###Timeout.  Host was slow in sending login string
and/or network was slow in building circuit;
	7###Access not permitted, or ISIS node code has rejected request;
	8###out of origination-ports;
	9###Supervisor will not honor requests at this time--try
again later.
.END
This second Supervisor_Response message ends the dialog.
	The supervisor does not require the host to supply the
password for the username.
	At any time, the host may receive a Supervisor_Reset
message.  This message indicates that the supervisor is
terminating all circuit-building dialogs with this host.  This
will be an infrequent occurrence.  Note that a circuit may have
been built even if the dialog is terminated in this manner
(instead of by a Supervisor_Response).  The host may initiate
the dialogs again after receiving this message.  Also, if the
most recent of the events (1) interface was reset and (2) host
sent a Set_Host_Number message was (1), the host port limit is
set to 48.
	Auxiliary circuits have a terminal type code of 0 and a
high throughput capability.
	After a circuit has been built the distinction between
caller and callee is of no significance.

.SS(|Tearing Down Circuits|)
	Either end of a circuit can initiate tearing the circuit
down.  When the host sends a Circuit_Zapper_message to the
base, a circuit_zapper control-signal is sent on the specified
circuit.  When a circuit_zapper is sent on a circuit (including the
cases of resetting the interface and using the Set_Host_Number
message), no subsequent messages
may be sent by the host on that port until a new circuit is
established on it; if they are, they will be discarded.
	After the zapper is sent, the next base-to-host
transmission may contain messages from that circuit, but no
subsequent transmissions will.  If there was a base-to-host
transmission in progress at the time the zapper is sent,
that transmission may contain
messages from that circuit but no subsequent transmissions
will.  In any case, if the port number is used in a new
Externally- or Internally-Initiated_New_Circuit message,
subsequent messages will be from the new circuit, not the old
one.  Also, if the host receives a Circuit_Zapper message from
a circuit, it will receive no subsequent messages from that
circuit.
.TOPIC(|free port|)
.TOPIC(|busy port|)
	A port is either free or busy.  A free port becomes busy
when it is assigned to a circuit by TYMNET.  Except in
unusual circumstances, the host will be notified that a port
has been assigned by an Externally- or Internally-Initiated_New_Circuit
message.  A port becomes free when a circuit zapper is
sent on it.
	If a message arrives at a port from a circuit and messages
may not be delivered (because a zapper was sent on that port),
the message is discarded.

.SS(|Using Circuits|)
	The base sends the host a Backpressure_On message when the
pipe leading out of the port becomes crowded.  The base sends
the host a Backpressure_Off message when the pipe leading out
of the port becomes not crowded.  Initially the pipes of a
circuit are not crowded.
	If a host sends messages (other than character gobblers or
circuit zappers) on the port while the pipe leading out of it
is crowded, there is an increased chance of black balls being
created, i.e.  data loss.  If there is a host-to-base
transmission in progress when the Backpressure_On message is
received, the host may be unable to
prevent some messages (i.e.  those that are part of the
transmission in progress) from being sent on the port.  It is
believed that up to 150 data bytes may be sent in this
situation without significantly increasing the chances of black
balls being created.  It is possible, though unlikely, for
black balls to be created even if the host strictly obeys this
backpressure protocol.
	The base considers a host to be willing to receive
messages on a port or not.  When a circuit is built the host is
initially considered to be willing to receive messages on the
port.  The host sends a Backpressure_On message to indicate it
is unwilling, and a Backpressure_Off message to indicate it is
willing.  When a host indicates it is unwilling, the base will
quickly stop delivering Data_Bytes messages on that port.  However,
character gobblers and circuit zappers will be delivered
regardless of the host's willingness.  The rules determining
when messages will stop are the same as in the case where the
host sends a Circuit_Zapper message.  When a host is
unwilling to receive messages, the messages coming to it will
back up in the pipe, and eventually the sending port will be
notified that the pipe is crowded ("propagation of
backpressure").
	The following host-to-base messages will cause the
corresponding control-signal to be sent on the port identified in
the P field:
.BEGIN BOX
	Circuit Zapper
	Character Gobbler
	Enter Deferred Echo Mode
	Leave Deferred Echo Mode
	Green Ball
	Red Ball
	Yellow Ball
	Orange Ball
	Hang Character
	Enter Transparency Mode
	Leave Transparency Mode
	Enter Alternate Output Device Mode
	Leave Alternate Output Device Mode
.END
	The host-to-base messages Echo_On and Echo_Off are
equivalent to Set_Terminal_Characteristics(0:1 and 0:0 respectively).
	If TC is not equal to 5 (Input_Baud_Rate),
Set_Terminal_Characteristics and
Probe_Terminal_Characteristics cause the corresponding control
signal to be sent.
If TC is 5, then only a local variable kept by the base for each circuit
is set or  probed.  This is done because of incompatibilities in various
TYMSATS in the network, and terminal characteristic 5 will eventually
be treated like the other terminal characteristics.
If a variable is set to a value bigger
than will fit, the low-order bits will be used (truncated).
	Probe_Terminal_Characteristics(63)
is exactly equivalent to 19 Probe_Terminal_Characteristics(TC) messages
with values of TC from 0 to 19 inclusive.  For other values of
TC, Probe_Terminal_Characteristics(TC) is ignored.
	The Data_Bytes message has a variable lengthat (see
summary).  N must be in the range 0 <= n <= 177 octal.  The
message contains the type, which is 200 octal + n, the host
port number, and n data bytes which are to be sent on the
circuit.
	Note that the following control codes cannot be sent by a
host:
.BEGIN BOX

	black ball
	gray ball
	break character
	terminal characteristics response(tc, val)
.END
	The following control codes arriving on a circuit cause a
corresponding message to be sent to the host (subject to the
rules above about zapping and willingness):
.BEGIN BOX

	circuit zapper
	character gobbler
	green ball
	red ball
	yellow ball
	orange ball
	black ball
	gray ball
	enter deferred echo mode
	leave deferred echo mode
	break character
	terminal characteristics response(tc, val)
	port status(d)
.END
	When data bytes arrive from a circuit, the host will
receive a Data_Bytes message.  The data bytes are not
necessarily grouped into base-to-host messages in the same
groups in which they pass through TYMNET.  The format is the same as for
the host-to-base Data_Bytes message.
	The following control codes are discarded when they arrive
at a base port:
.BEGIN BOX
	enter transparency mode
	leave transparency mode
	enter alternate output device mode
	leave alternate output device mode
	probe terminal characteristics(tc)
	hang character
.END
.END
     0 7e