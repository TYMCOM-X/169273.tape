From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 30 May 85 16:19:17 PDT 
To: (Skywalker Task Force) Ralph Brown <R/BROWN@Ontyme>, Pat Driscoll
	<P/DRISCOLL@Ontyme>, Tau Hsieh <T/HSIEH@Ontyme>, Stan King
	<S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>, Michael
	Stimac <stimac@tymix>, Jim Willott <J/WILLOTT@Ontyme> 
Keywords: SKYWALKER 
Subject: O.S. subcommittee - random thoughts 

It appears that the following basic capabilities need to be provided by
the SBE in order to support future growth needs and provide a flexible
environment for the operating system.  This is my first impression and
is not necessarily complete or well thought out.  I'm sending this to
the whole task force, and all comments are welcome, although I only
expect them from the O.S. subcommittee.


MEMORY MANAGEMENT

If the SBE and memory are implemented on seperate VME boards, utilizing
the VMX bus to avoid contention, we could have optional memory
configurations which simply involved replacing the memory board.  One of
the more useful variations would support a true multiprocessing
environment:  In the future, a multi-cached distributed memory system
could be implemented which would free the programmer from a need to be
concerned with the location of data structures which might be sharred
between processors.  This is particularly important when using a high
level language which abstracts data structures from the programmer.
Also consider the case of a sharred data structure which is the target
of serialized transactions.  Durring a transaction, the transaction
master would make several repetitous accesses to a limited area of
memory.  This would have a high hit rate.  The same efficiency could not
be derrived from any amount of planning on the part of the programmer by
using a conventional sharred memory arrangement because the schedule is
not known.  An example of this situation is the node descriptor data
base in the supervisor.


PHYSICAL ADDRESS SPACE

2^24 bytes is inadaquate, 2^32 bytes is probably reasonable.  This requires
a new format for MMU segment registers.  Come to think of it, why are we
using segment registers, anyway?


VIRTUAL ADDRESSING AND MMU

At the very minimum, the virtual address space should be 2^24.  John has
pointed out that it would be very desirable to be able to address
(through index registers) all of physical memory when the MMU is
disabled.  This is particularly important for processors which are
acting as I/O controllers.  So, since we're providing a path for a 32
bit address in the case of the MMU disabled, we might as well allow the
this whole space to be mapped with the MMU enabled.  Due to the enormous
number of segments, this would require a segment register cache in the
MMU, a complication which must be carefully weighed.  Such a cache would
benefit the speed of context switching even with the existing 16 segment
MMU.  Of course, the cache eliminates the requirement for multiple sets
of segmentation registers.


VME BUS OPERATIONS

In order to support atomic transactions (multiprocessing), we must have
full hardware support of at least an atomic test-and-set memory.  It
would be nice to support more general atomic read-modify-write operations
such as add-to-both ( [MEM],AC <- [MEM] + AC ), and exchange-with-memory.


-Bill
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Thu, 30 May 85 16:52:39 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <NTD.stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Keywords: SKYWALKER 
Subject: Ramblings on SME 

	These are a number of thoughts on SBE, in no particular order...

	A useful model for the overall SBE system architecture is to
look at the system components.  A basic module for this purpose is the
Processor Module (PM).  Any PM can be considered to consist of:

	o   Processor

	o   Local ROM (i.e., firmware -- may be microcode, "boot", etc.)

	o   Local RAM (may range from a register to real memory)

	o   "Controlled" interface -- that which controls this PM.

	o   "Controlling" interface -- that which this PM controls.

	A PM may be:  SBE, XPI card (controlled by SBE), LAN (controlled
by XPI), SIO (controlled by SBE), and even individual USARTs.

	In general, the PM hardware applies "absolute" constraints (i.e.,
determines the limiting capability of that PM;  Firmware and Code are
what determine the actual capability implemented.  These may be modified
(with varying degrees of difficulty) to extend or change the capabilities
provided, within the constraints provided by the hardware.

	Conclusion -- If the entire family of PMs making up Skywalker are
subject to the same hardware constraints with respect to addressing, etc.,
then it makes no difference what the actual collection of PMs making up
a system actually are...they may be SBEs, SIO, M68000, etc.  In particular,
a "multi-processor" system (multiple SBEs) is not a special case of hardware;
it is only a special case of the general capability.  It will be up to the
software to actually implement a useful multiple SBE system.


	Many of the existing or perceived "defects" of the existing Engine
are in the "software/firmware" domain, rather than the hardware capabilities
provided.  (Although many of these implementations were chosen because
they were the "simplest" within the existing hardware constraints.)

	True hardware defects include:

	o   limits to address-space (CPU and DMA can access only first Meg.
	    directly; only via MAC can other 3-MEG be addressed).
	o   16-bit path via MUX-bus (among other things, this forced the
	    16-byte boundary constraint upon SIO operations).
	o   lack of sufficient micro-code space to allow desirable extensions
	    to instruction-set.
	o   imposition of specific instruction-formats (derived from the
	    Interdata), and inability of microcode to address certain
	    instruction-fields (e.g., R1 can be used to select a register,
	    incremented, and decremented, but cannot be used to contain an
	    immediate operand), thereby precluding certain instructions which
	    need not use a register, but could use that field for another
	    purpose.
	o   lack of "atomic" capabilities (the ability to change memory
	    concurrently with other processors, except via TS instruction;
	    two processors require a "read/modify/write" capability to insure
	    that each can lock out the other during such a transaction).
	    Such operations include: Add to memory, Store Byte, modify Bit,
	    and various Queue and linked-list operations.  Implementing these
	    in Microcode would make them atomic for a single processor;  the
	    hardware need provide the capability for multiple processors.
	o   raw processor compute-speed (not as critical as claimed;  in
	    many cases, the machine limitations are due to the algorithms
	    used, and throughput could be significantly increased by the
	    use of alternative code algorithms and data structures).

	More important to performance is the software, firmware, and the
interaction between processors.  Some examples are:

	o   Node code -- A character passing through a node must be copied
	    5 times (including input and output);  better designs of both
	    node code and SIO algorithms could reduce this to 2!
	o   Node code -- Has been observed to spend 40% of cpu time in
	    RMAKE polling bit-arrays to determine if there is anything
	    to do;  an alternate polling mechanism could possibly reduce
	    this to 5-10%, making more cpu available to performing actual
	    throughput!
	o   SIO's inability to "data-chain" different buffers into a single
	    transmission forces code to copy data into a single transmit-
	    buffer, rather than letting SIO "gather" the successive buffers
	    (via a channel-program).
	o   Lack of micro-code support for stacks -- without stacks, several
	    registers are typically dedicated to subroutine-linkage;  the
	    code is then excessively convoluted to enable computations using
	    the remaining registers.
	o   Lack of micro-code support for Queues and linked-lists -- the
	    only support is for GCI/WCI, but only for placing characters;
	    manipulating the free-list is non-atomic, and therefore
	    interruptable.  This precludes Foreground and background from
	    communication via GCI, and again makes the code unwieldy at the
	    expense of performance.

	In many cases, we don't KNOW the "best" implementation to use.

	However, with adequately flexible hardware design, we can try out
different implementations of software/firmware/inter-machine communications,
and measure the performance benefits (if any) before "fixing" things.

	Conclusion -- We have more problems with software than with hardware;
if the new hardware is flexible enough, we can proceed with throughput
enhancements AFTER the hardware is committed, rather than having to
commit to these design decisions before the hardware can be designed.

	John Kopf
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 30 May 85 17:15:20 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <NTD.stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Keywords: SKYWALKER 
Subject: O.S. group: ATOMIC WORD LOAD/STORE 

John just mentioned that 32 bit load/store operations on the existing
engine are non-atomic.  Many frequently accessed shared data structures
(rings, bufferlet lists, etc.)  contain a pointer to memory which is
written by one process and read by another as an indication of the
amount of data in the structure.  Due to the frequency of access, it not
desirable to obtain a software level interlock for each access.  It is
suggested that at least simple load and store operations on 32 bit
memory operhands be guarenteed atomic at the hardware level.  As an
alternative, a sequence similar to the following could be implemented
in microcode at the expense of the sign bit:

	ATOMIC.LONG.READ( MEMORY.ADDRESS );
	{
		do {
			test.and.set.bit( 31, MEMORY.ADDRESS );
		} until equal.zero;
		X = LONG.READ( MEMORY.ADDRESS );
		X = X and 7FFFFFFF;
		LONG.WRITE( MEMORY.ADDRESS, X );
				/* only needs to write high order word */
		return X;
	}

	ATOMIC.LONG.WRITE( MEMORY.ADDRESS, VALUE );
	{
		X = VALUE and 7FFFFFFF;
		do {
			test.and.set.bit( 31, MEMORY.ADDRESS );
		} until equal.zero;
		LONG.WRITE( MEMORY.ADDRESS, X );
				/* writes high order word last */
	}

The most serious problem with this is that a program error causing the
sign bit to be left set would cause the uCode to hang for any process
trying to get the lock.  Implementing a timeout would make the process
even slower.

-Bill
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Fri, 31 May 85 16:58:41 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: more thoughts on SBE 

	SBE.2

		More Thoughts on SBE
			John Kopf

	One concern with the VME is the fact that a single-point failure
(shorted bus-driver) can take the entire system down.

	Using the PM model with VME, it would probably be reasonable that
the VMX be used for local (intra-PM) operations, and the VME bus be used
for global (inter-PM) operations.  A failure on the VMX bus would just be
another way that the PM can fail, and is an acceptable risk.

	However, a VME bus failure would take the entire system down.  There
really is a need for VME bus redundancy.

	A possible solution would be to define a "Super-VME" form-factor and
back-plane, which does have redundant VME busses.  This could still be made
compatible with "standard-VME" form-factors by extending the card sideways,
so that existing VME cards could still be used for local purposes, and fit
into the cage with the addition of an "edge-wise" extender;  these would
be limited to only the single (standard) VME bus, but in most cases would
probably be accessed only via the VMX bus anyway.

	An additional bonus would be the extra area available on the super-
cards, permitting higher card density.

	The following pictures may make this idea clearer:

    Super-VME
|--------------|		:- - - - - - - .
|              x		.              .
|              x redundant VME  .  (extender)  .
|              x		.              .
|              |		|--------------|
|              x		|              x
|              x VME		|              x VME
|              x		|              x
|              |		| standard VME |
|              x		|              x
|              x VMX		|              x VMX
|              x		|              x
|--------------|		|--------------|

where "x" are the pin-clusters.

	An alternate solution would be to define a private, "hugh" form-
factor and a bus architecture which is a superset of VME;  a simple (?)
extender-card-carrier could be used to map standard VME cards into the
private architecture.

	This would have the additional advantage that the hugh form-factor
could be tailored to permit a true "SBE" (i.e., really a single board),
but be compatable with existing VME- memory, CPU, and I/O boards.

	The disadvantage would be the inability to use standard VME card-
cages, etc.  (Question...can VME be mapped onto another existing bus
architecture, such as VERSAbus?  If so, standard components would still
be usable, and active components be selectable from two families rather
than just one.)


	As long as the specifications refer only to the number of "ports"
supported, without reference to the type of port (e.g., ASYNC, SYNC, SIO),
then it is necessary to support all of these, in any combination.  If,
conversely, the specification can be constrained to either ASYNC/SYNC
or SIO, then at least there is a straight-forward hardware solution.

	Perhaps a better solution would be to devise separate ASYNC, SYNC,
and SIO modules, each of which looks (more or less) identical (functionally)
to the SBE, but which has on-board the necessary capability to support
the differing varieties of processing we now do for these distinct devices.

	Then the problem is reduced to module selection, rather than trying
to package all options on a single module.


	It is worth observing that there are two different aspects to multi-
processor usage, and we need to support both:

	o   multi-processor, where the processors are running in parallel,
	    performing concurrent activities in concert -- these typically
	    need some degree of "tight" coupling.
	o   communications, where the processors are actually pipelined, and
	    each is acting as a successive "filter" upon the information
	    stream -- these are typically much more loosely coupled (e.g.,
	    USART --> SIO --> memory[CPU];  each performs another filtering
	    function).

	Pipelined usage is how we expect to get x10 throughput per instruction
on the superEngine;  multi-processor is an alternative to the x10 speed
increase of the superEngine.
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Sun, 2 Jun 85 3:47:44 UT
From: NTD.T/HSIEH@Ontyme.Tymnet 
Date: 01 JUN 85 19:44:08 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I94122@Ontyme.Tymnet 
Subject: Some thoughts on VME Bus 

 
 
   From   :  Tao-Yang Hsieh <Taohsieh@C39.Tymnet>
   Date   : 01 jun 85 19:43:10
   To     : (Skywalker Task Force)
            Ralph  Brown,  Pat Driscoll,  Tao Hsieh,  Stan King,  John Kopf,
            Bill Soley, Michael Stimac, Jim Willott
 
   Subject: Some thoughts on VME Bus
        _______________________________________________________________
 
 
   I  just  read  John  Kopf's  "more thoughts on  SBE"  and  was  pleasantly 
   surprised that he had proposed an extension to the VME form-factor similar 
   to what I am just going to propose. May be we can all agree on this!!!
 
   Using  VME bus as our system bus suffers not only from the fact that it is 
   a  single point failure in the system,  it has an additional problem  that 
   there is no parity information on the bus. This means that the correctness 
   of  the  information  transfer on the bus cannot  be  verified.  Having  a 
   redundant  bus increases the availability in case of a  failure  affecting 
   one of the buses but it does not satisfactorily solve the problem of error 
   detection.  I  propose  that  we extend the form-factor of  the  SKYWALKER 
   boards to provide connector space not only for a redundant VME/VMX bus but 
   also for a 'PARITY BUS' not defined in the VME/VMX structure.  This parity 
   bus will be wide enough to carry parity information for both pairs of  the 
   VME/VMX buses. The configuration will then look like this:
 
    --------------                                  -------------
    |            |                                  |           |
    |            <------- VME BUS 1 ---------------->           |
    | Single     |                                  | Vendor    |
    | Board      <------- VME/VMX BUS 1 ------------> VME       |
    | Engine     |                                  | product   |
    |            |                                  |           |
    |            |                                  |___________|
    |            |
    |            <-- PARITY BUS for VME/VMX BUS 1 -->           
    |            |
    |            <-- PARITY BUS for VME/VMX BUS 2 -->
    |            |
    |            |
    |            |
    |            |
    |            <-------- VME BUS 2 --------------->
    |            |
    |            <-------- VME/VMX BUS 2 ----------->
    |              |____________|
 
 
   All  TYMNET  developed  boards will use both the VME and  the  PARITY  BUS 
   whereas vendor supplied VME compatible boards will use only the VME bus.
 
 
 
 
 
 
                                   1

 
 
   An  unused bit on the VME bus can be used to indicate the validity of  the 
   information on the parity bus for every data transfer.  The inactive state 
   of  this VALID PARITY indicator bit is defined to indicate invalid  parity 
   information on the bus.  If data is transferred from the SBE to the vendor 
   supplied VME board, the parity information will simply be ignored. However 
   if data is transfered from the vendor supplied VME board to the  SBE,  the 
   VALID  PARITY bit will not be asserted and will therefore indicate invalid 
   parity.  The SBE, upon detecting that the parity information is not valid, 
   disables  the  parity error indication and uses the result of  the  parity 
   checker to generate the parity bit which can then be appended to the data. 
   Thus  all data on the SBE will have valid parity bit and the  addition  of 
   the  PARITY BUS does not create any problem for data transfer between  SBE 
   and  vendor  supplied VME  compatible  product.  Naturally,  communication 
   between SKYWALKER processor modules will use the PARITY BUS.
 
   There are several advantages to this larger board size:
 
     1. With  the  redundant PARITY BUS and the VME/VMX BUS,  we are  in  the 
        position to design a fault tolerant system like TANDEM.
 
     2. We  can  supply  a  single  board engine  for  our  low  end  product 
        consisting  of  the  CPU,  memory  and an I/O controller  for  a  few 
        channels.  (The connectors need not be installed for the SBE.)  There 
        may even be enough room to put the power supply on the board.  Single 
        board engine using double wide VME card is highly unlikely.
          
     3. Eventhough  this SBE contains on board RAM,  this configuration  does 
        not  preclude the SBE from addressing additional memory on a separate 
        board.  Thus,  low end products will have limited memory and high end 
        products will have more memory.
 
     4. If we adpot this board size as the standard SKYWALKER board,  it will 
        be possible to develop a single board intelligent I/O processor using 
        the Z80-SCC chips as the serial interface to do the ASYNC,  SYNC, and 
        SIO to support many channels.  Thus we will have only one type of I/O 
        processor.  In  addition,  the  cost  of the I/O controller  will  be 
        distributed over a greater number of channels resulting in lower  per 
        port  cost.  This  I/O  processor  can be used to  provide  ports  in 
        addition to those on the SBE.
     
     5. With  the  larger board,  it may be possible to implement a  new  CPU 
        design  which is three to five times faster than the current  CPU.  I 
        believe that the fact that we have to resort to microcoding  assembly 
        routines  to  achieve  the desired performance and the need  for  new 
        instructions is an indication of the limitation of the compute  power 
        of  the  current  CPU.  If we continue to move in  the  direction  of 
        needing  to put more and more functions into the microcode to achieve 
        the  desired  CPU performance,  do we need a more powerful  CPU  only 
        after we have reached the final step ---- implementing a 16  megabyte 
        writable  control  store and microcode all operating systems and  all 
        applications ????
 
         
 
 
 
 
                                   2
From: William R. Soley <WRS@C39.Tymnet> 
Date: Sun, 2 Jun 85 0:39:31 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: SBE Authentication 

(begin forwarded message)
----------------------------------------------------------------
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Sat, 1 Jun 85 0:06:46 UT
From: Dick Rawson <NTD.D/RAWSON@Ontyme.Tymnet>
To: William R. Soley <NTD.WRS@Ontyme.Tymnet>
Date: 31 MAY 85 16:28:41
Message-id: I93982@Ontyme.Tymnet
Subject: SBE Authentication

 --| MAIL# RAR1564    KEY: SBE        SENT: 05/31/85 16:08:17
 --|   TO: NTD.WRS
 --|   CC: NTD.W/EUSKE
 --| FROM: DRAWSON
 --| SUBJ: SBE Authentication
 
Thanks for the forwarded messages; I will read them over the weekend.
 
John and Gazel and I were talking about the node authentication
problem:  how do we know it is the node it sez it is?  John suggested
I contribute this to the SBE design team.
 
I suggest that an engine have access to a built-in I/O device whose
purpose is to be a hard-to-counterfeit identity card.  That might be
a single IC chip that is personalized for each engine, having two
things personalized:  a systematically assigned serial number, and
a truly random (but unique) encryption key.  The chip, when it is fully
programmed, returns the serial number on request, but never returns
the key.  Instead, the chip will accept a value to be encrypted,
and return the encrypted value.  It need not be a high quality
encryption scheme (a good hash function is fine; no need to invert it),
and it can take up to tenths of a second to run!
 
You use such a device by generating a random number, asking the device
to encrypt it, encrypting the same number by the key the device
is supposed to have, and comparing the results.
 
John suggests that I ask for board space for such a facility now.
 
This isn't enough functionality for a full blown network encryption
system; it just authenticates physical identities.
----------------------------------------------------------------
(end forwarded message)
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Sat, 1 Jun 85 21:06:30 UT
From: Dick Rawson <NTD.D/RAWSON@Ontyme.Tymnet> 
Date: 01 JUN 85 13:46:37 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Message-id: I94106@Ontyme.Tymnet 
Keywords: SKYWALKER 
Subject: comments on SBE discussion 


SYNCHRONIZING CONCURRENT PROCESSORS
 
There is an alternative to test-and-set locks that is attractive for
frequently-referenced fields, assuming few collisions.  IBM S/370 has
what IBM calls "Compare and Swap" instructions that are essentially
conditional atomic store operations.  You prepare two registers before
executing the instruction; one register has the value you "think" is
in the location you're about to store into, the other register has the
value to be stored there.  As an atomic operation, the instruction
compares the first register with the value fetched from the location,
and conditionally stores the second register there, conditioned on the
first register matching.  This is good enough for updating a doublely-
linked list, since one of the instructions addresses a 64-bit-wide
field.
 
If there's a conflict, nothing hangs, but the program has to observe
the condition code and recover (usually by retrying a section of the
program) from the lost race.
 
 
For single-processor multi-programming use, there's an old, simple
technique we might consider:  a non-privileged instruction that masks
interrupts for a short time, say 20 memory cycles:  long enough for
many atomic operations, but short enough not to overrun I/O timing
constraints.
 
 
I don't think we should expect to use really intimate multiprocessing:
tightly interleaved sharing of data structures.  It interferes with
caching; it requires lots of synchronizing code, and frequently must
wait due to actual contention; and it's tricky to code correctly.  We
have it easy with our stream-oriented processing, which is the bulk of
our network processing:  it comes naturally to communicate via
circular buffers and packets.  The Supervisor may have different
design constraints, but I think network nodes would do best to
minimize how complexly processes are coupled.
 
 
PRIVATE AND SHARED MEMORY
 
I didn't understand Bill's "multi-cached distributed memory."  But one
part of the paragraph startled me:
 
Why try to free the programmer from concerns about the location of
data that might be shared between processors?  I think it's not too
much to expect the programmer to declare that data structures are to
be shared ...  the programmer had better intend sharing to occur, and
 
 
 
 
 
 
 
 
 
 
                                                                PAGE 2
 
program for correct behavior in the event of shared access.  Anything
more than shared read-only access to static values isn't likely to
work correctly without some planning.
 
Certainly I vote for automation of where shared chunks of memory are
allocated ...  but the designers and programmers have to design in
shared access.
 
 
PHYSICAL ADDRESS SPACE
 
I guess 32 bits of addresses will do awhile.  Seems like people keep
regretting their choices in a few years, tho.
 
 
VIRTUAL ADDRESS SPACE
 
If the virtual address space is comparable in size to the available
physical memory, the segment tables at least won't take a substantial
fraction of that scarce resource.
 
The cache does not eliminate the value of multiple sets of
segmentation registers.  With multiple sets, you can switch to a new
set more quickly than you can load a complete set of new values from
physical memory.  How does the cache eliminate this gain?  If you
actually need the new values, you have to load them from physical
memory upon reference; that's different in timing from doing them all
at once, but why is it any quicker?  Of course, if the slot only uses
a few segments before dismissing, then only a few registers get
loaded; but a register-set switch could be faster yet.
----------------------------------------------------------------
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 4 Jun 85 15:26:00 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: Thoughts on Memory Management 

It appears as though we mostly agree that a 32 bit physical and virtual
address space is desirable.  After a very brief talk with Tau and Jim, I
will submit the following (some of which was stated in my first message):

Our current memory management scheme supports 16 "segments" of 64K bytes.
Each segment is defined by a base address which is added to the virtual
address to yield the physical address, and a size field which must be 
compared to the low order bits of the virtual address to validate that
the reference falls within the extent of the segment.  This scheme is
fairly expensive to implement, and provides little in return.

In order to map a 32 bit virtual address space, 65,536 segment registers
would be required.  The same number of paging registers would be required
for a paging scheme supporting 64K pages.  In both cases, an MMU cache
would be required to support efficient address translation.  I propose
a paging scheme utilizing pages of a size to be specified in the design
phase (probably between 4K and 64K bytes per page).  Each page register
would look like:

	+-+-+-+-+-+     +----------------------+
	|R|W|X|F|D| ... | high-order-phys-addr |
	+-+-+-+-+-+     +----------------------+
	31                                     0

    R - read permission
    W - write permission
    X - execute permission
    F - referenced (set by uCode 1st time page is accesed)
    D - dirty (set by uCode 1st time page is written)

[ This is merely an example of what I have in mind, it is not my intent
to start a detailed design discussion.  It is my intent to properly
define enough of the required functionality so as to estimate the hard-
ware complexity and thus the bus and packaging requirements. ]

Each MMU would have a single globally addressable register which would
set the current process' page map origin and size.  From this main
memory table, the MMU would load its paging register cache as necessary.
It is important that both the map and map origin register be addressable
from the VME bus to allow for a dedicated processor to handle virtual
memory processing.  Further, it would be desirable to be able to handle
page faults by any processor, not necessarily the one making the
reference.  This is not too important, however, because the faulting
processor could always relay the data to the virtual memory server.

I want to emphasize that virtual memory does not imply that processes
will be swapped.  In most applications, all code would be locked in
memory.  The real advantage comes when exploiting the 32 bit virtual
address space by having large data structures, such as the supervisor's
master user directory (MUD), etc., be mapped into virtual memory.

As a final note, I want to address the concern for compatability.  As
long as a paging scheme such as I have described were to implement a
page size no larger than 64K bytes, it would be fully upward compatable
with some loss in the efficiency of physical memory utilization.  I
think the paging scheme is also considerably simpler and faster from a
hardware perspective than any other reasonable 32 bit MMU.

-Bill

After proofreading this message, I noticed that I glossed over an
important aspect of the MMU and CPU: page faults.  It is necessary to
be able to suspend operation of the CPU at any point that memory is
being referenced, save the state on the stack (stack?) and begin
execution of the page fault handler.  The CPU must be able to resume
execution of the instruction causing the fault, even from the middle.
This is a significant change from existing Engine/MMU behavoir.  If
it is not practical due to space considerations, it will seriously
impact the usefullness of any MMU scheme.
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 4 Jun 85 16:20:54 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: request for comments 

Due to the size of the OS sub-group and everyone's tight shedules, I do
not intend to have a meeting before tomorrow.  The sub-group should
submit their comments via Ontyme (or TUMS) before the meeting.  I'd like
to hear from everyone in the sub-group, even if only to say "I
(dis)agree".  I will have copies of all the correspondence to pass out
at the meeting.  -Bill

[ If you prefer, you may send any comments directly to me and I will
redistribute them to the group. ]
From: John Kopf <KOPFJ@C39.Tymnet> 
Date: Tue, 4 Jun 85 19:13:22 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: Still further thoughts on SBE 

1.	It should be recognized that various suggested "atomic" operations
(e.g., "Compare and Swap") are NOT integral atomic operations -- they are
particular microcode implementations providing a functionality and
capability to the programmer;  if there is not appropriate underlying
hardware read-modify-write capability, then they will not be secure in the
multiple-processor environment.  We really do need to distinguish those
capabilities which MUST be in the hardware from the particular implementations
using those hardware capabilities.


2.	We should also distinguish between the "functional modules" and the
"package";  the package is the box, power, backplane, and cooling stuff
and is essentially passive.  It is also of concern to marketing, since it
is the visible manifestation of what they sell.  If they proscribe that the
"minimum" system be contained in a box no larger than a (2-inch) ring-binder,
that imposes certain constraints, but does not of itself determine the
actual functional modules contained therein.  It is the collection of
functional modules which makes the different packages a "family".

	I could see a family of packages which consist of a 4-slot, 8-slot,
16-slot ("bin"), and multi-bin "racks".  The 4-slot would typically be
used for a SBE, memory, and I/O; the 8-slot could be used for a bigger
system, or for a dual-system (for redundancy or capacity).  The same would
hold true for the bin and rack, except that the system would not be
limited to dual processors.  Also, a particular box could contain a SBE,
a stand-alone 68K-based UNIX system, or both (as well as any other useful
combination of modules).

	Of much more concern in a communications processor is the external
cabling requirements.  One can easily envision a box the size of a 12-inch
TV with 100 RS-232 connectors;  this could easily have associated a full
closet of modems.  Are there modems which are VMS compatible, or which
could be packaged as such?  If so, they would be both smaller than the modems
currently in use, and would also be susceptible to programmed diagnostics
(a capability which we are currently sadly lacking)?

	We do NOT want the external cables to attach to the "front" edge of
the modules;  the need to remove them in order to replace the module is too
prone to errors in reconnecting.  Instead, within the modular system, these
signals shoould be brought out the back, permitting the module to be changed
without re-cabling.  I see two ways to do this:  Either use the VMX-bus pins
for this purpose on these modules (thereby precluding any possibility of their
use with VMX), or to go to the 3-high card, and use the extra space for the
I/O (thereby requiring the "standard" cards be extended for this cage, but
allowing space for non-VME signals, such as bus-parity).  I would personally
opt for the latter, since this would give us more area for circuits, at the
cost of a less standard bin (I would rather see some cards half-empty, than
see multi-card modules required because there wasn't enough space on a 
single card).


3.	I reluctantly now agree that the costs associated with redundant
VME busses and bus parity outweigh the benefits associated with their
absense.  More important than a redundant bus is the requirement that
some mechanism be provided to detect failures on the bus; a simple way
is to permit concurrent examination of the bus-signals while applying
the signals -- then we can choose to verify that the signals seen agree
with the signals sent, and detect descrepencies.

	If redundancy is required, it would be better served with duplicate
systems on independent busses, coupled by modules which insured correct
transfers from one bus to another.


4.	My personal preference would be for the SBE to be a seperate card
from memory;  we will need memory-only cards anyway, and there are some
possible applications for a SBE without local memory (e.g., special
microcode which makes it a 256-port ASYNC USART -- this may seem far-
fetched, but the major limitations on the current engine ASYNC is not
necessarily the speed, but instead the fact that the microcode is single-
threaded, and a long instruction, such as Load Multiple, may delay the
servicing of an ASYNC interrupt;  an SBE dedicated to this function might
not be as constrained, since it would NEVER be doing anything else).

	This has the additional advantage of freeing space on the SBE card,
which could be used for additional functions such as a console port,
more space for microcode (or even for writeable control store?), or such
specialized requirements as machine-id (see Dick Rawson's memo).


5.	In a multi-processor system, it is envisioned that each processor
have a local memory, and (perhaps) access to a common, global memory.  In
any event, if I/O is memory-mapped, then any module must be capable of being
addressed from an external source.  This includes each local memory, as
well as each processor.  However, each Processor Module has a need for its
private memory to map into a particular set of local addresses (e.g., the
microcode cannot realistically perform explicit memory-mapping in order
to locate an interrupt-vector element;  this MUST be provided for by the
hardware).  This suggests that we need some form of memory mapping at the
VME bus as well as for the MAC, so that a particular 1-meg SBE will always
be addressible internally in one way (e.g., interrupts, bootstrap,
and diagnostics), and another way externally to insure the ability to
distinguish between individual instances of local memory.  Memories usually
have some form of "bank-select" switch onboard (e.g., to select WHICH
Meg this memory is);  I suggest that the same capability is required
INDEPENDENTLY for the global access.

	It MAY be reasonable that each module be addressed within a 1-Meg
scope, but the CE-1000 already defines 4-Meg memory boards, and it is thus
likely that this capability will be available in the future.  Perhaps we
should discuss the "granularity" of module-mapping, to insure we don't
lock ourselves into a corner in the future.

			John Kopf
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Wed, 5 Jun 85 3:06:01 UT
From: NTD.T/HSIEH@Ontyme.Tymnet 
Date: 04 JUN 85 19:07:32 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I95543@Ontyme.Tymnet 
Subject: Some thoughts on I/O 

 
 
From   : Tao-Yang Hsieh <Taohsieh@C39.Tymnet>
Date   : 4 Jun 85, 18:56
To     : (Skywalker Task Force)
         Ralph  Brown,  Pat Driscoll,  Tao Hsieh,  Stan King,  John Kopf,
         Bill Soley, Michael Stimac, Jim Willott
 
Subject: Some thoughts on I/O
 
_____________________________________________________________________________
 
 
ASYNC and SYNC
 
The  current  ASYNC  and SYNC interface boards are actually rather  clever  and 
inexpensive ways of interfacing to the lines at the time when hardware cost was 
quite high.  If our software is very closely coupled to these two interfaces as 
I seem to hear they are, shall we try not to discard them ?  I think we can put 
an  intelligent  controller to control the ASYNC and SYNC interfaces  and  turn 
them  into  intelligent  I/O processors.  We should then be able  to  move  the 
enhanced SYNC and ASYNC functions currently implemented in the microcode out to 
the I/O controller.  This not only will recover some CPU resouces but may  also 
allow  us to support higher data rates over the ASYNC and SYNC  interfaces.  Is 
this something worth considering?
 
 
SIO
 
The SIO seems to have the greatest potential of simplifying the I/O operations. 
But the current interface does not seem to have exploited all its capabilities. 
John  Kopf  has already pointed out its lack of data-chaining.  I will  try  to 
point out some other areas where there might be room for improvement.
 
Due  to  the  limitation of the MUX bus,  the  SIO  motherboards  are  assigned 
addresses 80x - 83x.  The individual channels are identified by 4 bits sent out 
over the MUX bus with the I/O command type. With a memory mapped I/O such as in 
the  SKYWALKER,  it  seems that we should be able to assign individual  'device 
address'  to  each of the receive and transmit channels (like in the  SYNC  for 
example). This will allow each of the receive and transmit channel interrupt to 
be  easily vectored to the appropriate service routine.  This mechanism can  be 
used to signal such events as completion of data transmission,  reception, etc. 
I think this will be more efficient than the polling of the CCW that I  presume 
we currently do.
 
Assuming  that we have unique 'device address' (or memory address) for each  of 
the receive and each of the transmit channels,  we no longer need separate  I/O 
commands  to  start input and start output.  One I/O command that we  can  call 
START  I/O will point to the channel command program.  The channel program  can 
have  similar capability as IBM channel programs.  The channel command word can 
be  expanded  to  64  bits  with the lower 32  bits  used  mostly  for  address 
information.  The  high  order 32 bits contain a channel  command  and  command 
modifier  bits.  The  channel  command is a superset of the current  input  and 
output channel commands. The command modifier bits can be used to indicate data 
chaining,  command chaining,   to request CPU interruption after completion  of 
the  current CCW and prior to completion of the whole channel program,  etc.  A 
modifier bit can even indicate that the address in the low order 32 bits points 

 
 

 
 
to  another data structure.  For example,  a structure that has a pointer to  a 
list  of  buffers  to  be used for data reception  or  containing  data  to  be 
transmitted  and  a pointer to a list where the buffers are to be queued  after 
completion of the I/O operation. If there is sufficient local memory in the I/O 
processors,  a modifier bit can indicate if prefetching of CCW and data in  the 
buffer is allowed.
 
Currently,  three  I/O  commands  are required to set up  the  virtual  address 
relocation  parameters  for a channel.  We can define a SET  CHANNEL  PARAMETER 
command  to  point to a CCW string containing the relocation information for  a 
pair of receive and transmit channel.  Should it be necessary to relocated  the 
receive and transmit channels differently (???),  the relocation parameters can 
be part of the CCW string for the START I/O command. Channel attributes such as 
baud rate (if known),  channel mode (ASYNC, HDLC), channel type (input, output) 
etc. can also be sent to the channel via the CCW.
 
Current  SIO  ASYNC  input seems to require repeated issuance  of  start  input 
command each to receive one to eight characters.  A modifier bit in the CCW can 
be used to indicate generation of CPU interrupt after receiving 8 characters or 
a time-out condition. Thus, a CCW command chain can be used for SIO ASYNC input 
eliminating the need to repeatedly issuing start input I/O command.
 
We  currently  have  privileged I/O instructions to perform I/O  operations  to 
devices  on the MUX bus.   With memory mapped I/O,  presumably LOAD  and  STORE 
instructions  will be sufficient to communicate to the I/O processor.  We  will 
then need a way to detect the accessing of I/O processor so that non privileged 
users  can be prohibited from doing so.  One way is to allow each processor  to 
have up to 16 MEG of address space for its local memory, the high order address 
bits can be used to select processors on the VME bus.  The accessing of another 
processor will then require a memory address greater than 24 bits.  This can be 
used  as  a  flag to check the user class.  Another way maybe to keep  the  I/O 
instructions and require all accessing of memory belonging to another processor 
be  performed with READ/WRITE I/O instruction.  This however,  prevents us from 
performing other operations (ADD, AND, etc) using data from another processor's 
memory.
 
So much for now.
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 4 Jun 85 21:42:33 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: privileged I/O instructions 

As Tau has pointed out, it is likely that we will be using memory mapped
I/O.  I certainly see no reason for not doing so.  I believe that the
issue of protection is adaquately addressed by the MMU.  The kernel, or
its proxy, will setup a process' map to prevent access to those
locations used for I/O.  Any process which is allowed to run with paging
disabled (un-mac-ed), has ultimate power anyway by being able to corrupt
the kernel or other priveleged processes.

If more control is required, a convention could be adapted which put
various privelege levels of I/O devices in different memory address
ranges.  The kernel could then selectively allow processes to do I/O by
restricting the pages in the process' map.  Note that this also allows
a process to be given only INPUT, or only OUTPUT access.

-Bill
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Fri, 7 Jun 85 0:12:43 UT
From: NTD.T/HSIEH@Ontyme.Tymnet 
Date: 06 JUN 85 16:33:01 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I96678@Ontyme.Tymnet 
Subject: Writable Control Store for CPU microcode 

 
 
From   : Tao-Yang Hsieh <Taohsieh@C39.Tymnet>
Date   : 06 JUN 85 16:31:48
To     : Skywalker Task Force: Ralph Brown, Pat Driacoll, Tao Hsieh, Stan King, 
         John Kopf, Bill Soley, Michael Stimac, Jim Willott
CC:    : Dick Rawson
 
Subject: Writable Control Store for CPU microcode
_____________________________________________________________________________
 
Looking  over  Jim Willott's estimate of the board space required  for  various 
functions,  it  appears  that  we can trade off the on board 1 Meg  RAM  for  a 
writable control store for microcode. The current estimates are :
 
       18.75  sq. in.   Three array engine
        8.4             4K x 64 bit microcode ROM
        2.1             Map ROM
        5.5             Misc. timing & VME interface
       16.45            1 M RAM (256 x 9 SIP)
     ----------
       51.2             Stand alone engine
 
The  8.4 sq.  in.  of microcode ROM uses 8 packages of 4K x 8 PROMs for 64  bit 
wide micro-word.  These packages are 24-pin. For writable control store, we can 
use 16 packages of 4K x 4 static RAM. These are 20-pin packages. I estimate the 
space required to be less than 16.8 sq. in. (2 x 8.4)
 
If we do not put the 1 M RAM on the CPU board, the space available for writable 
control store is :
 
     16.45         from 1 M RAM (256 x 9 SIP)
      8.4          from 4k x 64 bit microcode ROM
   ----------
     24.85 sq. in.
 
     16.8  sq. in. required for 4K x 64 bit writable control store
 
 
It  therefore appears that we can have writable control store for microcode  if 
we  do  not put the 1 M RAM on the CPU board.  I am strongly in favor  of  this 
trade  off and will be willing to argue very hard for this  trade  off.  Please 
send your comments to members of the SKYWALKER task force.
 
To  load the microcode into the writable control store,  I propose  making  the 
microinstruction   pipeline   register  into  a  shift  register  and  have   a 
bidirectional port to the WCS.  Microinstruction words can be shifted into  the 
pipeline  register and then written into the WCS.  This simple procedure may be 
slow but it does not use too much hardware.  Besides,  I presume we will not be 
loading the microcode too often.
 
Looks like we can finally have writable control store for microcode !!!!!!!!!
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Thu, 6 Jun 85 19:24:58 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: More Thoughts 

1.	I really like the idea of putting some functionality onto the bus-
arbitration card.  This solves a number of nagging problems;  For example,
a bin-unique (rather than module unique) ROM could contain:

        o   "Machine" ID -- the machine is really the bin, not the specific
	    processor (s) contained therein.  Having the machine ID "stick"
	    to the bin rather than to the processor means much less hassle
	    when a board is swapped (if code requires a particular machine
	    ID, the code must be regened every time the machine ID changes;
	    it must also be regened for each such machine when intended for
	    backup).  By being at the bin level, this is avoided except when
	    the card containing it is changed (hopefully much less often).
	o   machine encryption -- same reasoning.
	o   Bootstrap -- we need a bootstrap, and centralizing it here makes
	    it available to all processors, and also frees the space otherwise
	    required on the SBE for this function.
	o   It could be used for any number of other functions we never even
	    conceived of!

	Also, a master system clock, Console port, become system resources
rather than processor resources.

	Since diagnosability requires that the arbitrator be addressable,
expanding the addressable functionality to include these functions is
much easier (i.e., less to be added than if addressing were only required
for this use).


2.	I think we have a nice match between risk and potential future
growth.  I suspect that the major drive toward expanding the engine
capabilities is to make it more "68K-ish", and a better vehicle for
multiprocessing.

	However, this view assumes that multiprocessing requires a collection
of homogeneous processors -- all the same type.  Harder to implement --
but potentially even more powerful -- is multiprocessing using a collection
of inhomogeneous processors;  not all necessarily the same type.  Then,
processors could be selected for "a good match" with a particular
application.  Just as the Engine is a good match for the node code but
a poor match for Pascal-based applications, the 68K is just the opposite.
Rather than adding a paging MMU to the Engine, a more appropriate match
may be obtained by restricting "pagability" to the 68K.  Once we have
the ability to write applications in "portable" form, the existing machine-
dependence is relieved;  five years from now, the primary processor may
well be 68K, with the Engine being used primarily for "dog-work" usage,
such as node code.  Is this significantly different from current trends
within personal computers, which have multiple inhomogeneous processors
for different applications?


3.	The MAC -- I feel that we have the license to do a number of things
with respect to the MAC.

	For example, I do not feel that we must be constrained to support
existing MAC-register formats...rather than retaining "split" fields,
reformat the register so that the Base, Range, and Access fields are each
contiguous.  The code that manages these will have to be rewritten anyway,
to allow for 32-bit physical addresses, so we might as well make it easy
to do.

	Likewise, MAC translation may be restricted to a 20 or 24 -bit
input (depending on Engine or CE1000 emulation mode), but should be
capable of generating a 32-bit physical-address output.  And if the
MAC is not disabled, the full 32-bit address supplied by the CPU should
be propagated through the MAC, to allow access of all physical memory.
The fact that we can't do this with RX3 addressing makes no difference;
interrupt routines are generally implemented with base-registers anyway,
and a 32-bit base register is a perfectly valid usage.

	One thing which would be desirable would be a simple way to "cancel"
the MAC, so that only the required number of registers need be reloaded
(rather than reloading ALL registers to insure that previous usage was
canceled).

	I am also not convinced that we must retain the "X'300" address
convention for MAC access.  Another desirable change is th change the
existing interrupt vectors from Halfword to Words (same goes for SVC's),
thereby doubling the space required for that vector.  Perhaps a re-shuffle
of assigned usage would be more appropriate:

	000-03F	Traps
	040-07F	Machine Interrupts
	080-0FF	SVC's
	100-1FF	MAC
	200-5FF	I/O Interrupts

	If nothing else, it would certainly be easier to remember.


4.	That which we currently perceive as the "low-end", competing
against the stat-mux, is really a full-blown Engine, and as such is not
likely to be able to compete against all possible stat-mux usage.

	However, we are also talking about a smart SIO card, capable of doing
things the current SIO card can't.  One such possibility IS to reprogram
the SIO as a stand-alone stat-mux (the "Async Extension Cord -- AEC").
this would really push the product line down into the low-end.  It would
also be primarily a software implementation.  The major hardware requirement
for the SIO card to be used in this service would be sufficient on-board
ROM space.

	While not a primary, critical-path usage, we should remember that
it may become a reasonable possibility, and thus avoid limiting future
migration in that direction.


5.	Sync/Async Cards -- I view the development of VMX-based Sync and
Async cards as a form of low-cost insurance, to be used in case we are
unable to make the SIO card take on these functions in a transparent
fashion within the critical time-frame.  I see the SIO card as a major
ongoing project:  Initially, we need to support the current SIO usage in
a transparent fashion.  Once that is done, the next stage is to support
the current Sync and Async usage in a fashion transparent to their normal
usage.  Next, we need to integrate these functions with new ISIS capabilities,
to permit more flexible applications.  Finally, there will be new capabilities
required.

	Each stage of this process will build upon previous stages.  I question
not that it can be done, but only that it can all be done within the specified
time-frame.  Remember that this is not the only microcode effort required.
We must also develop the SBE microcode concurrently.  I suspect that the
total effort to get first-level transparency will keep us busy enough during
the first phase -- If we can do more, fine!

	John Kopf
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 6 Jun 85 19:39:58 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: Re: Writable Control Store for CPU microcode 
In-reply-to: I96678@Ontyme.Tymnet of 06 JUN 85 16:33:01

I agree that the trade off of 1Mx8 onboard RAM for 4Kx64 WCS is desirable.
As John has mentioned, the future trend could very well be toward the 68k
for general applications.  For specific, intensive tasks such as supervisor
path selection or specialized data manipulation as in the node code, the
engine with microcode customized to the application would make an excellent,
powerful and cost effective co-processor.

Our competition has Z-80's and 68k's which make them more cost effective
in the low end market.  We can address that with some single card system.
What the competition is unable to provide in a small cost effective box
is anything that requires the power provided by custom microcode in a
configuration such as the SBE.  This is the long term advantage of the
engine (yes, long term) - customizable microcode for very intensive,
specialized tasks.

	processor	time-frame	advantages
	---------	----------	----------
	SBE w/WCS	short-term	run existing code
	SBE w/WCS	long-term	customizable data cruncher
	68k family	short-term	cost effective I/O control
	68k family	long-term	general processing

[ I appologize if this has been obvious to everyone while I've been
kicking and screaming - I was just looking for a good harmony of
long and short term goals. ]

So, regarding the issues of WCS and MMU, we should consider a system
which meets both long term and short term goals as outlined in the table
above.  I see the following issues in a new light as a result of this:

	module	issue		remarks
	------	-----		-------

	WCS	required	Absence precludes use of SBE as a
				customizable data cruncher.

	SBE	1Mx8 RAM	On board RAM is intended to address the
				very low end market.  I don't believe we
				can address this market with a multi-card
				system.  The low end is better served by
				68k based with onboard I/O (i.e. SCC's)
				such as in John's memo of 6/6.  It would
				be 'nice' to keep 1M or so on board, but
				not at the expense of WCS.

	MMU	2^24 LAS	Probably only 2^22 logical address space
				is required, but 2^24 gives us compatability
				with the 68k and leaves options open.

	MMU	2^32 PAS	In order to allow memory sharing in the
				multiprocessor system, we need a minimum
				of 16 times the per-processor address space
				(probably 2^24) or at least 2^28.  Given
				some reserve for I/O and VME memory if any,
				2^32 is the next logical step.

	MMU	architecture	A custom (gate array) MMU in the configuration
				of the existing engine is a waste of time and
				money.  It appears that there might be
				several off-the-shelf MMU chips which will
				address this issue with better cost, better
				expedience and lower risk.  This is being
				looked at as an alternative to semi-custom.

-Bill
Received: from tymix.Tymnet by C39.Tymnet; Fri, 7 Jun 85 14:42:55 PDT
Received: by tymix.Tymnet (4.12/4.7) id AA23379; Fri, 7 Jun 85 14:22:41 pdt
Return-path: <jwillott@tymix.Tymnet> 
From: jwillott@tymix.Tymnet (Jim Willott) 
Date: Fri, 7 Jun 85 14:22:41 pdt 
To: KopfJ@C39, ntd.p/driscoll@ontyme.Tymnet, ntd.r/brown@ontyme.Tymnet,
	ntd.s/king@ontyme.Tymnet, ntd.t/hsieh@ontyme.Tymnet,
	stimac@tymix.Tymnet, wrs@C39 
Message-id: <8506072122.AA23379@tymix.Tymnet> 
Subject: "More Hardware Thoughts Standard"... 



			More Hardware Thoughts

	Standard MMUs
	16082 / 32082
		Demand paging MMU.
		Maximum 25 bit physical address.
		Breakpoint and trace capability.
		Very closely coupled to 16000 / 32000.
		Multiplexed 16 bit data and 24 bit virtual address.


	Can expand physical address by routing some address lines around 
		the MMU but this reduces usefulness of breakpoint and 
		trace facility.

	May have to emulate 16000 / 32000 CPU at the interface level to 
		properly control MMU.  This could be risky because of 
		documentation level.  National sales rep. indicated that
		others have tried to use the MMU on the 68K with limited 
		success.


	Other MMUs
		National is working on a 32 bit MMU for early next year.
		Motorola is working on a 32 bit MMU for late this year to
			early next year.
		Zilog has two MMUs -- both generate 24 bit physical addresses.

  While I like the idea of a standard MMU I am concerned about the risks
	of adapting one to the engine in the required time frame.  I think
	it might be better to go with the 32 bit MAC gate array now and
	convert to a standard 32 bit MMU for model 2 or model 3.  The TSI
	schedule can not take the risk implied by the standard MMU, but
	must have a MAC gate array to work so the gate array is essentially
	free for model 1.




	Serial Drivers and Receivers
		On all of the current serial interfaces (async, sync & SIO)
			large quantities of board space are consumed by
			driver / receiver circuits.  I suggest that Tymnet
			develop a series of hybrid circuits which drive or
			receive 8 lines.  One approach would be four
			20 pin DIPs -- receive 8 V.24, receive 8 V.35,
			drive 8 V.24 and drive 8 V.35.  A second approach
			would be two 28 pin DIPs -- receive 8 lines
			selectable for V.24 or V.35 and drive 8 lines
			selectable for V.24 or V.35.
From: William R. Soley <WRS@C39.Tymnet> 
Date: Sat, 8 Jun 85 13:30:53 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <NTD.J/WILLOTT@Ontyme> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: Re: hybrid V.24/V.35 drivers/receivers 

I think this is a great idea -- this could be software selectable?

-Bill
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Mon, 10 Jun 85 11:57:11 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: More Thoughts 

1.	"SIO" card -- We need a name for the Skywalker "SIO" card in order
to distinguish it from the traditional SIO card.  I suggest "Universal
Serial I/O" (USIO) as a working name.

	I am assuming that the USIO card will have an on-board processor, at
least functionally equivalent to the M68000 (the 68000 family is a better
choice than some of the alternatives, such as the Z80, simply because it
is an alternate processor which we already support, in terms of assemblers,
etc. -- it may actually be a 68008, etc.).

	Since we are choosing a 32-bit address-space on the VME bus to
permit flexibility in configuring multi-processor systems, it would seem
reasonable to expect that the USIO card could be concurrently shared by
several processors, at least in some configurations.  Thus it is required
that the 68000 be able to generate any 32-bit address upon a transfer, in
order to be able to address the several different modules in a system.
Since each module has (potentially) an internal 16-meg address-space, it
will be necessary for the 68000 to generate arbitrary VME addresses of
more than 24-bits -- this is beyond the range of addresses that the 68000
supports internally.  The upshot is that I don't see the mapping of a
range of 24-bit internal addresses into a corresponding range of VME
addresses as a workable design;  instead, the 68000 interface will probably
consist of a pair of 32-bit register/locations, one for data, one for
address.  Then the 68000 program initiates a transfer by applying the
data to one register (if output to VME), then the address, and then a
read/write command performed explicitly.  Further, we'd need such a pair
for each set of operations which the 68000 could perform concurrently.

	We also need to be concerned with multiple SBEs trying to access the
USIO simultaneously.  Since the 68000 is going to look for unsolicited
control functions (from the SBE) either by periodic polling or by the
servicing of an interrupt, there will be in either case a delay between
the SBE access and the 68000's action in response.  How do we best insure
that concurrent requests do not "step" on one another before the 68000 can
respond?  (The simplest solution -- permitting the 68000 to be a "slave"
of only a single SBE -- is also the least-prefered solution.)

	We'd like as many SCCs on a USIO card, compatible with the required
pin-availability.  We'd also like to be able to support both V35 (balanced)
and V24 (unbalanced).  But doesn't balanced essentially mean wire-pairs?
What impact does this have on available pins?  Maybe this means that we
still have an external "daughter-board", to match the SCC with the connector.
If so, we certainly would need such interfaces for V24, V35, OCL, and also
current-loop;  perhaps in Sync and Async versions.

	A program running in the SBE will want to perform a number of
independent transactions with the USIO channel, some of which (but not all)
will be filtered through ISIS.  For flexibility, it will want to perform
independent control-functions, in the areas of:

	o	SCC control -- these should be filtered by programs within the
		USIO, in order to permit SCC independent operations as far as
		possible.  Thus "set one stop-bit" should be a generic command,
		and the USIO will map it into the appropriate SCC register.
	o	Process control -- this would include (but not be limited to)
		selection of the particular "driver" (i.e., the USIO would
		contain a collection of standard driver/interrupt routines
		for the SCCs, such as HDLC, Bysync, Sync, T201, etc.) used
		to perform the actual SCC I/O.
	o	Control-structure selection -- some (but not all) types
		of applications would use a structure similar to a channel
		program.
	o	Data-structure selection -- commonly used data-structures
		are:  rings, buffers, bufferlets.  In many cases, the most
		desirable structure for the data is separate from the form
		of the control-structure.
	o	"Other" -- the return of status indicators in various
		forms (into memory or via an interrupt) is desirable, as
		is the ability to select and perform on-board diagnostics
		upon command.  Also, the ability to down-load and/or
		execute code (i.e., special diagnostics and drivers,
		or for debugging.

	In order to implement all these requirements, I would define a set
of standard "control-blocks" within the SBE memory, so that the SBE need
only pass a control-block address to the USIO, and it in turn examines
(and perhaps copies to local memory) the individual parameters.

	Within the USIO, high-priority interrupts would dispatch to the
appropriate driver to transfer data between the SCC and local memory;
a lower-priority interrupt to service SBE requests, and a collection
of background processes which actually perform the transfers between the
SBE and local memory, for status, data, and control structures.  These
would, in addition, perform the translation between the SBE virtual
addresses (e.g., supplied in a channel-program) and the corresponding
VME addresses.  In order to reduce VME congestion, these routines should
perform primarily word (4-byte) or half-word (2-byte) transfers except in
those cases where they are clearly inappropriate (e.g., Async input).

	All of these could require a significant amount of on-board
memory, both ROM and RAM.  Assigning memory now for 1-meg of each (even
though we initially envision only needing 64K of each) will certainly
ease the transition to bigger ROM and RAM when that finally becomes
appropriate.

	(We may also want to implement the ATC on this card someday.)

	John Kopf
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 10 Jun 85 12:06:24 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: USIO (SCC) card 

Since we named the SIO card after the USART chip, we could again call this
card the SCC.

The problem of concurency in issuing "start-I/O" commands to the USIO is
easily handled by two methods that come to mind (and probably several that
don't):

1. interlock register

The USIO would have two registers or memory locations appearing on the VME
bus at 16 different addresses.  The first register would be a busy flag.
The second address would be the VME address of the channel command program
to be started.  A processor wanting to do a start-I/O would do test and set
operations on the first register until it found its old value zero.  Then
it would put the I/O channel program starting address in the second register.
Ideally, this would generate an interrupt to the USIO processor which would
read the address and clear the flag.

2. port addressing

In this scheme there would be 8 32 bit registers or memory locations def-
ined for I/O control program counters.  The first for the first device,
the second for the second device, etc.  Since someone has presumably ass-
igned devices at configuration time, or there is some external runtime
binding arbitration, there should never be any conflicts.  This is nice
because the 68K could update these registers as it processed the program
and the main processor could read them back at any time to observe the
progress.  Ideally, any write to these registers would generate an
interrupt so the 68K could handle the new address.

I would argue that the second scheme is less flexible since it sets
some small constant number of logical devices.  Even though the USIO
has a constant number of physical devices, it may be running software
that multiplexes multiple logical devices onto one physical device,
and it is likely that someone would want to assign these logical
devices to different main processors (someday).

----------------

On the issue of USIO memory management.  We should look at the expected
maximum busy time for the VME bus.  That is, how long, worst case, would
the 68K have to wait for VME bus access.  I suspect the answer is that
it will usually be granted very fast but could occasionally take long 
enough that the 68K could lose data from an SCC if it were held in
wait states for that period.  Can we consider a DMA controllr chip on
the USIO that would make transfers from USIO memory to the full 32 bit
VME bus?  Would the setup time be enough greater for this than for a
single address, data and control register that it would make small
transfers (most common?) too expensive?

-Bill
Received: from tymix.Tymnet by C39.Tymnet; Wed, 12 Jun 85 19:13:06 PDT
Received: by tymix.Tymnet (4.12/4.7) id AA17952; Wed, 12 Jun 85 19:12:27 pdt
Return-path: <jwillott@tymix.Tymnet> 
From: jwillott@tymix.Tymnet (Jim Willott) 
Date: Wed, 12 Jun 85 19:12:27 pdt 
To: kopfj@c39, ntd.d/rawson@ontyme, ntd.p/driscoll@ontyme, ntd.t/hsieh@ontyme,
	stimac@tymix.Tymnet, wrs@c39 
Message-id: <8506130212.AA17952@tymix.Tymnet> 
Subject: "More thoughts on the MAC I think"... 

More thoughts on the MAC

I think that I must have done a poor job of explaining my idea for
invalidataing all MAC regesters in one operation.  I am proposing that
I add one 24 pin chip to the design of teh TSI MAC which is not seen by
the software.  The chip works like a 256 bit register in that all
of the contents can be cleared by pulling reset -- a single write to a
single MAC address.  The output from this chip is ored with the 
present bit of the 32 bit segmetn register.  When the software writes
new segment discriptor data into a MAC register, the proper bit in
the new chip is set so that the results of testing the present bit
depends only on the present bit in the MAC register.  If a memory
access references a MAC register which was never loaded with new
discriptor data after invalidating all MAC registers, the new chip
overides the MAC register present bit and the memory segment is "not
present".  The implementation details of the new chip is that it is
a content addressable RAM into which the hardware automatically
writes the address of each updated MAC regestier as the register is
updated.

Comparing this proposal to the 16 bit register counter proposal.
The 16 bit register requires 2 8-bit register chips (20 pins each),
and a 16 way data selector using two 8 way MUXs (16 pins each).
That is 3.15 square inches as compared to 1.05 square inches.
Both proposals add a new MAC register address and require one
write operation to invalidate the MAC.  The 16 bit register proposal
may require a second write to the register to validate the MAC registers
after the new segment data is written to the MAC.  The single chip
CAM works for both 256 and 16 sets of 16 modes. One potential problem
is that in 16 of 16 mode you may not wish to invalidate the whole MAC.
This could be handeled by either hardware or software.  Software could
chose not to clear the MAC in the 16 of 16 mode or the hardware can
inhibit the CAM output in 16 of 16 mode.  Or if aboslutly necessary,
a sixth bit could be added to the context register to enable or disable
the CAM output.  Of course I like the software solution.  The CAM
permits single MAC register resilution on validateion while the 16 bit
register only permits 16 register resilution; therefore, requiring
ate least 17 writes (maybe 18) to validate a single segment where the CAM
needs only 2 writes.

I recommend that the CAM be used rather than the 16 bit register.


16 bit data path		MS 8 address lines
	|			(includes context reg. in 16 of 16 mode)
	|				|
	|				|
	|	 -----------------------+-------
	|	|			|	|
	|	|			|	|
 ---------------------		 ------------------
|      DI      ADDR   |		|      DI     ADDR |
|		      |		|		   |
| Segment Reg. Memory |		| Cashe Tag memory |
|		      |		| used as CAM      |
|		      |		|		   |
| BASE   LEN  PROT    |		|        MATCH     |
 ---------------------		 ------------------
   |      |    |                           |
   |      |    |                           |
   |      |    |   present bit             |
   |      |    +----------------------     |
   |      |    |                      |    |
   |      |    |                     --------
   |      |    |                     |  OR  |
   |      |    |                     --------
   |      |    |                        |
   V      |    |                        |
to ALU    |    |                        |
          V    |                        |
    to Compar  |                        |
               V                        V
	    to Valid Test           to Valid Test
Received: from tymix.Tymnet by C39.Tymnet; Thu, 13 Jun 85 12:36:03 PDT
Received: by tymix.Tymnet (4.12/4.7) id AA21816; Thu, 13 Jun 85 12:35:21 pdt
Return-path: <stimac@tymix.Tymnet> 
From: stimac@tymix.Tymnet (Michael Stimac) 
Date: Thu, 13 Jun 85 12:35:21 pdt 
To: jwillott@tymix.Tymnet, kopfj@c39, ntd.d/rawson@ontyme,
	ntd.p/driscoll@ontyme, ntd.t/hsieh@ontyme, wrs@c39 
Message-id: <8506131935.AA21816@tymix.Tymnet> 
Subject: re: Jim Willott's CAM reset for MAC proposal 


I think Jim's got a good suggestion here.

Michael
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 18 Jun 85 0:18:18 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER, WCS, SKYWALKER 
Subject: WCS Performance Analysis 

Here is a blurb on WCS.
Since this has not been reviewed by the task force,
I suggest it be attached as either an appendix, or separate memo,
with a disclaimer noting its lack of ratification.

-Bill

(PUB source code - cut at dotted line)
------------------------------------------------------------------------
.begin "UCODE.RPT"

Performance and flexibility is a key issue in determining the
competiveness of this family in the marketplace.  Our low end
competition is using microprocessors.  The engine processor is bigger,
and more expensive than micros and yet, with our existing software
structure, it does not hold a commensurate performance advantage.  With
56 Kbps sure to be a selling point due to DTS, we must question how the
limitation of 2 full duplex 56 Kbps channels is going to impact the
future of this product line.

With WCS, however, and careful but simple software modifications, the
engine processor (EP) could be a power-house of price performance
significantly exceeding the capability of the competition's simple micro
based systems.  This can happen in three ways:

First, by direct advantage in existing code by special instructions
specifically tailored to the individual application.  The table below
indicates the performance gain which can be realistically expected from
selective, application specific micro code.  Writable control store
(WCS) makes this kind of customization cost effective by allowing the
micro code to be downloaded along with the application software.

Second, by creation of specialized auxiliary processors for special
tasks such as intelligent I/O.  High overhead tasks which are easily
separated from existing code can be placed in off-board processors.  In
the case of simple, frequently executed tasks, the code might be written
completely in micro code.  Examples of this are ISIS dispatcher,
supervisor path selection, I/O and file system management (especially
ICOM), packet pre/post-processing, and bufferlet management to name a
few.

Third, by automated optimization of compiled code.  As the trend emerges
toward higher level languages, it should be noted that compiled code may
very well stand the most to benefit from WCS with the least effort.
Most typical compiled code spends over 30% of its time in procedure
linkage.  Micro code assist of procedure linkages and non-scalar data
accesses could easily yield a 200% increase in performance (see table)
of compiled code.  Concurrent PASCAL stands even more to benefit from
optimization of its inherently high overhead concurrency control
structures.  These benefits can be realized without any changes to the
application code itself (provided it is written in a higher level
language).

.group;
.once center;
Performance Gain due to Application Specific uCode
.nofill;


         time spent in MACRO version of translated code
Tmacro = ----------------------------------------------
                    total transaction time

         time spent in uCode version of translated code
TuCode = ----------------------------------------------
                    total transaction time

         transaction time in MACRO version
Gain   = --------------------------------- - 1
         transaction time in uCode version



    Tmacro   TuCode    Gain
    ------   ------   ------
      5 %      1 %     30 %
     10 %      2 %     60 %
     15 %      3 %     90 %
     20 %      4 %    120 %
     25 %      5 %    150 %
     30 %      7 %    180 %
     35 %      8 %    210 %
     40 %     10 %    240 %
     45 %     12 %    270 %
     50 %     14 %    300 %
     55 %     17 %    330 %
     60 %     20 %    360 %
     65 %     24 %    390 %
     70 %     28 %    420 %
     75 %     33 %    450 %
     80 %     40 %    480 %
.end "UCODE.RPT";
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Thu, 20 Jun 85 14:08:30 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: Thoughts on Skywalker System Board 

	One capability which should be included in the Skywalker system is
the ability to read, via the VMSbus, each board on the bus.  Each board
should be able to respond with its address, board-type, and status (there
may be other responses as well, but these should be a minimum).  The address
may be implicit in the query (unassigned addresses just don't respond), but
should directly correspond to the VMEbus address(es).  The board-type and
status can probably be 4-8 bits each.  Initially, board types might include:

    0	reserved for unassigned boards
    1	EP
    2	EP with WCS
    3	SCC
    4	Memory
    5	ASYNC
    6	Sync

Status would probably be unique to each board;  for EP, useful values are:

    0	Operational
    1	WCS not initialized
    2	Bootstrap required
    3	WCS failure (parity?)
    4-?	Internally detected failures

	If WCS exists, then it must be loaded;  initial loading from the
System board is proposed, and requires both EPROM (at least 32K for WCS image)
and some sort of sequencer to transfer EPROM to WCS.

	The existence of both capabilities suggests that the sequencer could,
with profit, be made intelligent (i.e., programmable).  With the addition
of a small quantity of RAM, it could then:

1.	Detect power-up;
2.	Poll all cards and build a configuration table in RAM, recording all
	active cards, their type, and status;
3.	For those cards with WCS (and only those cards) with status "WCS not
	initialized", load the standard WCS image (the basic instruction set,
	sufficient to run bootstrap and diagnostics [including "fat-finger"
	diagnostics], and load WCS from memory);
4.	For those EPs with status "Bootstrap required", down-load the
	Bootstrap;
5.	Repeat 3. and 4. for all EPs;
6.	THEN issue (bus-global) power-up interrupt to allow each processor
	to begin execution.
7.	Begin normal polling sequence:
	a.  Poll all addresses for changes (including "gone" and "newly
	    present" as well as changes in status);
	b.  For newly arrived EPs, do 3. and 4.;
	c.  Then issue (bus-global) "configuration-change" interrupt to allow
	    all active processors to read the configuration table and take
	    appropriate actions;
8.	Alternate 7 with simple, non-intensive VMEbus tests and diagnostics.
	Take entire system down if VMEbus failure detected.

	These operations need not be done at high performance (indeed, there
may be some advantages in delaying the EP bringup until power has become
stable after power failure), and a simple processor (e.g. 6502) is both
cheap and sufficiently powerful for the purpose.  The reason for using a
processor rather than a hard-wired sequencer or controller is the ease
of making changes as required.  I would suspect that 64K of EPROM would
be more than sufficient to hold both the program, and WCS and Bootstrap
images.

	Implementation of these capabilities would insure that WCS EPs would
always come up with operational microcode, capable of diagnostics and
bootstrapping.  Microcode extensions could still be loaded into WCS from
memory, as appropriate.  It would also relieve the processors of the need
to continuously poll the bus to determine configuration changes, since that
would instead be signaled by an interrupt, and the configuration table could
be read from the System board.

	In particular, we'd have a primative "diagnostic processor" capability
without the expense of a full-blown (SuperEngine-like) Diagnostic Processor.
We could still add a DP to a system if warranted, but this would be an
unwarrented expense in many configurations, especially if the Bootstrap
were smart enough to try to load from disk if present.

	John Kopf
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 20 Jun 85 14:46:15 PDT 
To: John Kopf <KopfJ@C39.Tymnet> 
Cc: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Michael Stimac
	<stimac@tymix>, Jim Willott <jwillott@tymix>, Dick Rawson
	<NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: Re: Thoughts on Skywalker System Board 
In-reply-to: your message of Thu, 20 Jun 85 14:08:30 PDT

I think the micro-diagnostic-processor capability is very powerful, and
makes much more sense than hardwired functions.  I suggest a 68008,
however, even though at the moment it is a bit more expensive, it keeps
the number of instruction sets we need to support and train to a
minimum.

-Bill
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Mon, 24 Jun 85 11:18:25 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: Skywalker Global (VMEbus) Addressing 

	It would seem reasonable to define some standards on addressing
Skywalker modules via the VMEbus (and VMXbus).  Following are my ideas
on this topic:

	The system allows 32-bit VME addresses, and the VMXbus limits us
to 24-bit addresses.  It would therefore be reasonable to map all 32-bit
addresses of the form 00xxxxxx onto the VMXbus, and reserve addresses of
the form VVxxxxxx (VV > 0) for the VMEbus.

	It is also reasonable to assume a basic VMEbus address "granularity"
of 1-Meg;  This will allow the EP (with 1-Meg) to be fully accessed as
a single "grain", and is easily extended to multi-Meg memories by using
several "grains".  It is also compatible with the expected usage of 68000
processors, such as SCC modules.

	I feel that we should allow for inter-bus communications in the
future, and thus reserve some of the address-space for this purpose.  While
inter-bus communications will be inherently less efficient than intra-bus
usage (because of the need to arbitrate usage of TWO busses as well as
the delays incurred in the transfer), it still would be much more efficient
(and cheaper) to communicate between individual bins in a cabinet via such
a mechanism than via an external medium, such as a LAN.  (For efficiency,
such transfers may be limited to "write" to avoid incuring excessive
"read" wait states.)

	I propose reserving the high-order digit (4-bits) of the VMEbus address
for this purpose, with the added convention that "0" is always the "current"
bus.

	This still leaves two digits (8-bits) for addressing individual
"grains" of "memory" (either real memory, or associated with a processor).
Thus the actual addresses would have the form:

	BGGxxxxx

where "B" selects the bus, and "GG" selects the module's 1-Meg of memory.

	Since we also should define standard addresses for configurations, I
propose the following address usage:

   00000000 - 00FFFFFF	VMXbus select (local memory)
   01000000 - 010FFFFF	System Card (Bus arbitration, etc.)
   011xxxxx - 07Fxxxxx	Processors (EP, SCC, 68000; 111d allowed)
   080xxxxx - 0FFxxxxx	Memory (Global memory, > 1-Meg of individual
			processors; 128d M-bytes allowed)
   BGGxxxxx		Access to "other" busses, with GG following the same
			rules as above.

	While this assignment does provide a reasonable "clean" assignment of
addresses, it does suffer from the fact that contiguous local memory (e.g.,
000FFFFF and 00100000, as referenced via the VMXbus) will actually be
non-contiguous via the VMEbus, and will require additional testing within
any process performing "DMA"-like operations.  This will, however, also
be true when the system is configured with global memory, and would have
to be taken into account in any case.

	Within this framework, there is still the need for "specialized"
addresses on the VMEbus for each module;  for example, the MAC area of
an EP cannot be written via VMEbus transfers;  the need to generate local
(to a particular EP) interrupts for I/O vectors; etc.  Perhaps there could
be an additional convention that maps VME "writes" to the MAC area
into the corresponding interrupt?

	A second area of concern relates to the 68000.  These have an internal
address of 24-bits (I know that the 68020 can reference more, but not all
68Ks will be 68020s).  Some standard mechanism will have to be provided to
permit the 68K to address the VMEbus;  perhaps a pair of registers, one for
address and the second for operand?

	Finally, processors with more than 1-Meg of local memory will need
some way to identify (i.e., correlate) each of its local 1-Meg "grains"
with the corresponding VMEbus "grain" (and also to verify existence without
taking a "non-existent memory" fault).  The simplest mechanism might be
an extension to the MAC which would allow the EP to specify a "MEG" to
the MAC and receive in return either "0" (non-existent) or the VME "grain"
number.  Also, if "grain"-failure can be detected, there should be some
way to take it off-line for subsequent maintenance (along with a fault-
indication, such as a red-light);  perhaps via the VMSbus?

	(The use of "grain" is derived from "granularity", and is, at best,
awkward;  What is a better name for 1-Meg of address-space?)

	John Kopf
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Mon, 24 Jun 85 18:12:54 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: Thoughts on SCC Code Structure 

	In order to provide the generality required of the SCC, as well as
the speeds, I forsee the SCC board to require certain hardware capabilities
in order to support the necessary software capabilities (software may
actually be firmware;  I really mean the body of code executed by the on-board
processor).  In particular, I see the need for priority interrupts, organized
as a hierarchy of interrupt levels, and working in concert with a collection
of background processes.

	The highest priority interrupt should be for "machine" conditions,
such as power-off.

	The next level of interrupts should be for SCC I/O;  these may all
be at the same level, or prioritized by individual ports.  If possible,
there are advantages to Output being higher priority than input (the SCC
chip can buffer 3 bytes on input, but only one on output).  These interrupt
routines should be kept short and simple, with the primary function of
transfering bytes between local memory and the chip, and only occasionally
performing specialized operations (e.g., recording "Break detected" on Async,
or signaling the chip "End-of-block" on sync output).  The obvious way to
implement this would be via queues, capable of passing both data and control
signals.

	The next (lowest?) level would be for EP requests (perhaps on a port-
by-port basis);  I see the EP passing a pointer to a descriptor within
the EP for each request, and the SCC examining that descriptor (Request
Block -- RB) to determine the nature of the request.  In general, the
processor must respond to the interrupt quickly, in order to permit additional
requests, but the interrupt routine need only read the pointer into a queue
for subsequent service by the background.

	There would also be a "background" process, performing the majority
of the activities associated with EP-SCC transfers, interpreting RB requests,
controlling the SCC chip, returning status to the EP, and generally
managing the SCC resources.  I would see here processes concerned with
copying data from the EP and placing it into the transmit queue, transfering
data from the receive queue to the EP, and managing various control
(e.g. EP channel programs) and data (e.g. rings, buffers) structures.

	Each port would have associated with it a "state"-table, wherein
would be recorded the current input- and output-interrupt state (routine
start-address;  e.g., "transmit" vs. "idle");  in/out-queue parameters;
in/out service routine addresses;  in/out control-routine address (and
control structure address);  RB service address (some requests may be done
piece-meal, to avoid data-overrun in the internal queues;  Requests really
are of a lower priority than ongoing data transfers, even though the SCC's
acceptance of the request should be of a higher priority -- the higher
priority is desirable to permit the receipt of multiple requests thereby
reducing request contention, but the servicing of the requests is not nearly
as critical.)

	Each port would also have associated with it a block of "memory I/O"
addresses to implement reads and writes to the various SCC registers.
There might also be additional addresses for non-SCC port-related control
structures, such as additional port control lines, etc.  (In the 68K,
the most reasonable addresses for such blocks are the LAST 32K-bytes of
the 16-Meg address-space, allowing for addressing by negative addresses.)
In addition, there would need be a 32-bit "register" for VMEbus addresses
so that the background could read and write via the VMEbus for both memory
operands and to generate specific EP interrupts, (and perhaps a second
register so that the EP could read status information upon interrupts).
One (or more) registers would also be needed to permit the EP to post
RB addresses for a particular port.

	The Requests (RBs) performed by the EP will be used to specify a
variety of services in addition to data transfers.  Likely requests would
include:

	Disable SCC module -- take processor off-line for maintainance.
	Upload SCC code -- place (address-independent) code block into RAM.
	Disable SCC port -- take port off-line.
	Enable SCC port -- put port on-line, select handlers (e.g., SIO, Async)
		and details such as data- and control-structures, relocation,
		etc.  (may be several different requests, since input and
		output may have different structures).
	Upload SCC port -- load the registers (and individual fields) with
		standard and/or specified values (e.g., Async may be standard
		values, except for baud-rate, bits/char, no. of stop-bits;
		Bisync may require a "transparancy-table").
	Input/Output -- Initiate one or multiple transfers.
	Sense Status -- Return status information (as opposed to making
		it readable by EP)

	Details of partitioning the functionality of requests between multiple
requests, and between EP and SCC, will depend upon the value of the work
offloaded from the EP, vs. the load imposed upon the SCC.

		John Kopf
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 25 Jun 85 15:28:42 PDT 
To: John Kopf <KopfJ@C39.Tymnet> 
Cc: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Michael Stimac
	<stimac@tymix>, Jim Willott <jwillott@tymix>, Dick Rawson
	<NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER, SKYWALKER 
Subject: Re: Skywalker Global (VMEbus) Addressing 
In-reply-to: your message of Mon, 24 Jun 85 11:18:25 PDT

Regarding your call for terminology, "page" is probably the correct
term, however, we may use it in the MMU to refer to a smaller than one
meg area of memory.  This may be confusing, so we could make the
distinction "VME page" and "MMU page" in cases where it would otherwise
be ambiguous.  I think this is less confusing than having a non-standard
term introduced, but I don't feel very strongly.  -Bill

From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 25 Jun 85 15:46:51 PDT 
To: John Kopf <KopfJ@C39.Tymnet> 
Cc: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Michael Stimac
	<stimac@tymix>, Jim Willott <jwillott@tymix>, Dick Rawson
	<NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: Re: Thoughts on SCC Code Structure 
In-reply-to: your message of Mon, 24 Jun 85 18:12:54 PDT

I agree about the interrupt structure and registers to hold the high order
address for the 68000.  I think that two addresses per port are sufficient.
One for VME -> SCC communications, and one for SCC -> VME communications,
although both could be read/written by both entities.  I suggest the first be
used for the VME master to place a VME address of a channel program.  The
second would be used by the SCC board to post a channel status.  There is no
time critical need for the SCC to service the new channel address, since it
is typically not allowed to have concurrent channel programs.  -Bill
Received: from tymix.Tymnet by C39.Tymnet; Thu, 27 Jun 85 9:04:19 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA01268;
	Thu, 27 Jun 85 09:03:56 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA00466; Thu, 27 Jun 85 09:03:54 pdt
Return-path: <jwillott@eunix> 
From: jwillott@eunix (Jim Willott) 
Date: Thu, 27 Jun 85 09:03:54 pdt 
To: kopfj@c39, ntd.d/rawson@ontyme, ntd.p/driscoll@ontyme, ntd.r/brown@ontyme,
	ntd.s/king@ontyme, ntd.t/hsieh@ontyme, stimac@eunix, wrs@c39 
Message-id: <8506271603.AA00466@eunix.Tymnet> 
Subject: Terminology for the one meg memory 

I think that using 'VME page' and 'MMU page' will ultimately lead to confusion
because the 'VME' or 'MMU' will get lost.  How about picking somthing like
'block', 'bank', 'segment', 'section' or 'piece'.
-Jim
Received: from tymix.Tymnet by C39.Tymnet; Wed, 3 Jul 85 9:50:28 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA01167;
	Wed, 3 Jul 85 09:49:59 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA14304; Wed, 3 Jul 85 09:49:41 pdt
Return-path: <jwillott@eunix> 
From: jwillott@eunix (Jim Willott) 
Date: Wed, 3 Jul 85 09:49:41 pdt 
To: kopfj@c39, ntd.d/rawson@ontyme, ntd.p/driscoll@ontyme, ntd.r/brown@ontyme,
	ntd.s/king@ontyme, ntd.t/hsieh@ontyme, stimac@eunix, wrs@c39 
Message-id: <8507031649.AA14304@eunix.Tymnet> 
Subject: Segment Registers 

	I need som input from the software side of the house.  The work I
am currently doing on the MAC for TSI is at a point where the organization
of the segment register must be defined.  I thought that here is an
oppertunity to test the organization for SKYWALKER.  TSI only needs 24
bits of address which can be the least significant bits of the SKYWALKER
segment register.  I have already implemented the single instrtuction
clear function for the MAC -- one output instruction resets all 256 segments
to not present.

I need your responses by Tuesday at the latest.



SEGMENT REGISTER

 MSB									LSB
0						|20		|28	31
<-------------- Base Address------------------->|<--- Length -->| Access

I am prepared to support the same four access tags currently supported.
	Bit 31 -- Present
	Bit 30 -- Write Protected
	Bit 29 -- Interrupt on Write
	Bit 28 -- Execute Only

I am prepared to support a length field of up to eight bits -- up to
256 blocks per segment.  That leaves up to 20 bits of base address.
However, do we need a length field?

I am prepared to add the offset field fo the address to the base address
field of the segment register.



ADDRESS INTERPRETATION

Proposal 1 -- SKYWALKER

1K byte blocks, if a length field is used then 256K byte max per segment.
Offset field limited to same size as length field.

 MODE 0 -- 16 contexts of 16 segments -- separate context register, 22 bit
 address


 MSB									LSB
0	|4	|8		|14		|22			31
Not used|  SEG  |   Not used    |    Offset     |    Byte (passed by MAC)


MODE 1 -- 1 context of 256 segments 

 MSB									LSB
0	|4	|8		|14		|22			31
<--  SEGMENT -->|   Not used    |    Offset     |    Byte (passed by MAC)


Proposal 1 -- TSI

 MODE 0 -- 16 contexts of 16 segments -- separate context register, 22 bit
 address

 MSB									LSB
0	|4	|8	 |12	|16		|22			31
Not used ------>|Not used|  SEG |    Offset     |    Byte (passed by MAC)


MODE 1 -- 1 context of 256 segments 

 MSB									LSB
0	|4	|8	 |12	|16		|22			31
Not used ------>|<-- SEGMENT -->|    Offset     |    Byte (passed by MAC)



Proposal 2 -- SKYWALKER

 MODE 0 -- 16 contexts of 16 segments -- separate context register, 28 bit
 address

 MSB									LSB
0	|4	|8		|14		|22			31
Not used|  SEG  |<-----------  Offset  --------> |    Byte (passed by MAC)


MODE 1 -- 1 context of 256 segments 

 MSB									LSB
0	|4	|8		|14		|22			31
<--  SEGMENT --> |<-----------  Offset  --------> |    Byte (passed by MAC)


Proposal 2 -- TSI

This is the same as proposal 1 for the TSI.
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Mon, 8 Jul 85 10:44:14 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: Thoughts on EP Architecture 

1.	In order to allow the EP to directly execute code from off-board
memory (e.g., Executing a Bootstrap from System-card memory, un-mapped),
we should remember to increase the PC to 32-bits.

2.	The Engine performs all memory references as 16-bit operands;  VME
allows byte, short, and long transfers.  In order to reduce risk, the initial
EP microcode will probably emulate the Engine and do 16-bit transfers;
however, we will eventually want (for reasons of efficiency) to be able
to do byte and long transfers directly.  We should insure that the EP (and
especially the Gate-arrays) contain whatever features are required in order
to support this function.  We will also want to support the read-modify-write
capability from the EP.  My concern is not so much that we can add this
capability to a future version of the EP card, as it is that the necessary
capability be included in the Gate-arrays the first time around.  (One
possible implementation might be to expand the microword size, to include
fields that implement this functionality -- 2-bits should be enough to
specify no reference, byte, short, and long references;  an additional bit
could be used to specify read-modify-write.  This implementation does have
the advantage that the presence or absence of these additional bits might
be transparent to the Gate-array;  it is not the only possible implementation.)

3.	Within the new architecture, there may no longer be any use for the
ATN1 and ATN2 interrupt levels, since these are currently dedicated to
Async and Sync usage.  This is an area that should be examined.

4.	One aspect of the Engine architecture is the limited number of
registers available to the microcode, for intermediate operands.  Since
the EP is envisioned to make greater use of microcode for extended
instructions, this is a potential bottleneck.  The alternative, using
some additional registers of the working register-set for temporary storage,
is likely to become a burden on the microcode or applications.  At least
two solutions exist:  provide additional registers for the microcode and
expand the multiplexers, or provide some mechanism for the microcode to
access the full set of 256 registers, as in the SuperEngine.  This is an
area that should be explored.  (The problem is that some desirable general-
purpose instructions require a number of intermediate operands, and in
addition should be capable of selecting any combination of registers and
preserving all others in the current register set;  e.g., queue-manipulation
instructions, including a more general GCI and WCI.)

			John Kopf
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Thu, 11 Jul 85 21:01:58 UT
From: NTD.T/HSIEH@Ontyme.Tymnet 
Date: 09 JUL 85 16:47:06 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I12209@Ontyme.Tymnet 
Subject: Response to Segment Register ONTYME from Jim Willott 

From   : Tao-Yang Hsieh
Date   : 09 JUL 85  16:27:44
To     : Jim Willott           
CC     : Ralph Brown, Pat Driscoll,  Stan King, John Kopf, Bill Soley, Michael 
         Stimac, Dick Rawson
 
Subject: Response to Segment Register ONTYME from Jim Willott
_____________________________________________________________________________
 
Here  are some comments on Segment Register.  Some of these has been  verbally 
communicated but are documented here for future reference.
 
 
1. Single instruction clear function for MAC segments present indicator
 
   All  256 segment descriptors are marked not present as a result of a single 
   write  to  a  predefined  memory  address with  PSW  bit  5  cleared  (i.e. 
   relocation  disabled).  No  output  instruction is involved  in  the  clear 
   operation.
 
 
2. I suggest that we use the terminologies SEGMENTS, PAGES, and OFFSETS rather 
   than SEGMENTS, OFFSETS, and BYTE.
 
 
3. The  segment  registers as defined in your ONTYME have only a 20  bit  base 
   address field. This 20 bit base address concatenated with the 10 bit offset 
   only  gives  a  30 bit physical address.  This does not cover  the  32  bit 
   address range that we would like.  I propose changing the segment  register 
   fields  to 22 bit base address field and 6 bit length field (yes,  we  need 
   the length field). The pages are 1K bytes.
 
                            1       1       2 2 2   2     2 2     3
    0       4       8       2       6       0 1 2   4     7 8     1
   +-------+-------+-------+-------+-------+---+---+-------+-------+
   |         22 bit BASE ADDRESS               |  6 bit    |E I W P|
   |                                           | length    |       |
   +-------------------------------------------+-----------+-------+
 
   where E , I, W, and P are as defined in your ONTYME.


4. Address interpretation:
 
   a) Virtual Address:
      For both the TSI and SKYWALKER,  virtual addresses have a maximum of  24 
      bits defined as follows:
 
      For  mode  0:  
        There  are 16 contexts of 16 segments with a separate context register 
        to select one of the 16 contexts. The virtual addresses are 20 bits in 
        length giving an address space of 1 megabyte.
 
 
                                 1       1       2   2   2       2     3
         0       4       8       2       6       0   2   4       8     1
        +-------+-------+-------+-------+-------+---+---+-------+-------+
        |  12 unused bits       | 4 bit |   6 bit   |  10 bit OFFSET    |
        |                       | SEG # | PAGE NUM  |                   |
        +-----------------------+-------+-----------+-------------------+
        
        (This  is  the same as your proposal 1 for TSI but apply to  SKYWALKER 
        also).
     


 
      For mode 1:
        There is one context of 256 segments. Virtual addresses are 24 bits in 
        length giving an address space of 16 megabytes.
 
 
                                 1       1       2 2 2   2       2     3
         0       4       8       2       6       0 1 2   4       8     1
        +-------+-------+-------+-------+-------+---+---+-------+-------+
        | 8 unused bits |  8 bits for   |   6 bit   |  10 bit OFFSET    |
        |               |   SEGMENT #   |  PAGE #   |                   |
        +---------------+---------------+-----------+-------------------+
 
        (This  is the same as your proposal 1 for TSI but apply  to  SKYWALKER 
        also).
 
   
 
   b) Real Address:
 
        The  engine processor for both the TSI and SKYWALKER has 16  megabytes 
        of local memory. To accomodate the 256 segment descriptors, I propose 
        moving  the  MAC register addresses from the current 300-3FF  to  just 
        beyond the 16 megabyte boundary.  Thus,  the 32 bit real address space 
        is allocated as follows:
 
         00000000 - 00FFFFFF   address space for engine processor local memory 
 
         01000000 - 01FFFFFF   special  addresses  local  to  each  processing 
                               element to be used to set up control  registers 
                               and other special functions.
         
         02000000  - FFFFFFFF  address space assigned to other modules on  the 
                               VME bus. The most significant 8 bits identifies 
                               the  module.  Thus  254 module identifiers  are 
                               provided (20 - FF).
 
 
        For  TSI and SKYWALKER engine processors,  the special  addresses  are 
        assigned as follows:
 
         01FFFB00 - 01FFFB03  MAC register 0 (also context 0 MAC register 0)
         01FFFB04 - 01FFFB07  MAC register 1 (also context 0 MAC register 1)
         01FFFB08 - 01FFFB0B  MAC register 2 (also context 0 MAC register 2)
         01FFFB0C - 01FFFB0F  MAC register 3 (also context 0 MAC register 3)
         01FFFB10 - 01FFFB13  MAC register 4 (also context 0 MAC register 4)
         01FFFB14 - 01FFFB17  MAC register 5 (also context 0 MAC register 5)
         01FFFB18 - 01FFFB1B  MAC register 6 (also context 0 MAC register 6)
         01FFFB1C - 01FFFB1F  MAC register 7 (also context 0 MAC register 7)
         01FFFB20 - 01FFFB23  MAC register 8 (also context 0 MAC register 8)
         01FFFB24 - 01FFFB27  MAC register 9 (also context 0 MAC register 9)
         01FFFB28 - 01FFFB2B  MAC register A (also context 0 MAC register A)
         01FFFB2C - 01FFFB2F  MAC register B (also context 0 MAC register B)
         01FFFB30 - 01FFFB33  MAC register C (also context 0 MAC register C)
         01FFFB34 - 01FFFB37  MAC register D (also context 0 MAC register D)
         01FFFB38 - 01FFFB3B  MAC register E (also context 0 MAC register E)
         01FFFB3C - 01FFFB3F  MAC register F (also context 0 MAC register F)
 
        similarly,
 
         01FFFB40 - 01FFFB7F  MAC reg 10 - 1F and context 1 MAC reg 0 - F
         01FFFB80 - 01FFFBBF  MAC reg 20 - 2F and context 2 MAC reg 0 - F
         01FFFBC0 - 01FFFBFF  MAC reg 30 - 3F and context 3 MAC reg 0 - F
         01FFFC00 - 01FFFC3F  MAC reg 40 - 4F and context 4 MAC reg 0 - F
         01FFFC40 - 01FFFC7F  MAC reg 50 - 5F and context 5 MAC reg 0 - F
         01FFFC80 - 01FFFCBF  MAC reg 60 - 6F and context 6 MAC reg 0 - F
         01FFFCC0 - 01FFFCFF  MAC reg 70 - 7F and context 7 MAC reg 0 - F
         01FFFD00 - 01FFFD3F  MAC reg 80 - 8F and context 8 MAC reg 0 - F
         01FFFD40 - 01FFFD7F  MAC reg 90 - 9F and context 9 MAC reg 0 - F
         01FFFD80 - 01FFFDBF  MAC reg A0 - AF and context A MAC reg 0 - F
         01FFFDC0 - 01FFFDFF  MAC reg B0 - BF and context B MAC reg 0 - F
         01FFFE00 - 01FFFE3F  MAC reg C0 - CF and context C MAC reg 0 - F
         01FFFE40 - 01FFFE7F  MAC reg D0 - DF and context D MAC reg 0 - F
         01FFFE80 - 01FFFEBF  MAC reg E0 - EF and context E MAC reg 0 - F
         01FFFEC0 - 01FFFEFF  MAC reg F0 - FF and context F MAC reg 0 - F
 
         01FFFF00 - 01FFFF03  MAC status register
         01FFFF04 - 01FFFF07  Last instruction fetch (absolute address)
         01FFFF08 - 01FFFF0B  Parity error address (absolute)
         01FFFF0C - 01FFFF0F  last instruction fetch (virtual address)
         
         01FFFF10             mark all 256 segment descriptors as not present
 
 
        For TSI:
          The 24 bit virtual address is relocated to a 24 bit real address  to 
          address  up to 16 megabytes of memory.  The segment descriptors  are 
          addressed using a 25 bit real address.  Addresses above 01FFFFFF are 
          invalid addresses.
 
 
        For SKYWALKER:
          The 24 bit virtual address can be relocated to a 32 bit real address 
          to  address up to 16 megabytes of local memory and to address  other 
          modules  on  the  VMEbus.  The  MAC  registers  however,  cannot  be 
          addressed with relocation enabled (PSW bit 5 = 1).  Other modules on 
          the  VMEbus  can  be  addressed with real address  or  with  virtual 
          address via relocation.
 
4. Questions
 
   a) Does clearing the segment present indicator in mode 0 results in marking 
      all 16 contexts of 16 segments as not present?
 
   b) The  MAC that you have implemented,  how does it access memory  via  the 
      VMXbus  and how does it access other modules on the VMEbus?
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Sat, 13 Jul 85 1:09:45 UT
From: NTD.T/HSIEH@Ontyme.Tymnet 
Date: 12 JUL 85 17:24:09 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I14370@Ontyme.Tymnet 
Subject: More on MAC 

From   : Tao-Yang Hsieh <Taohsieh@C39.Tymnet>
Date   : 12 jul 85 17:21:05
To     : (Skywalker Task Force)
         Ralph  Brown,  Pat Driscoll,  Tao Hsieh,  Stan King,  John Kopf,
         Bill Soley, Michael Stimac, Jim Willott
 
CC:    : Dick Rawson
 
 
Subject: More on MAC
_____________________________________________________________________________
 
I.  MAC registers at 344, 348, 34C
 
 
    The  current  MAC has three registers that can be  useful  in 
    error analysis. 
 
    1.  The register at 348 that latches the absolute address  of 
        the memory access where parity error is detected.
 
    2.  The register at 344 that latches the absolute address  of 
        the last instruction halfword fetched.
 
    3.  The  register at 34C that latches the virtual address  of 
        the last instruction halfword fetched.
 
 
    This has a problem in that if the error occurs while fetching 
    the second or third halfword of the instruction,  the  engine 
    code  does not have a convenient way of knowing where is  the 
    beginning  of  the  instruction.    I  propose  changing  the 
    function of the three registers to the following:
 
    1.  A  register to latch the virtual address of the beginning 
        of the instruction fetched.  (The address of the halfword 
        containing the op code.)
 
    2.  A  register  to latch the virtual address of  the  memory 
        access  that  results in any error  condition,  such  as, 
        parity error,  execution violation (fetching  instruction 
        from pages marked not executable), segment descriptor not 
        present,  write  protection  violation (attempt to  write 
        into  write  protected  pages),  or  interrupt  on  write 
        condition.
 
    3.  A  register to latch the absolute address of  the  memory 
        access that results in any error condition listed above.
 
 
    The  MAC status register contents can then be used  with  the 
    addresses  to  identify  the  type of error  and  the  memory 
    address  used  that  resulted  in  the  error  condition.  In 
    addition,  the  beginning  address of the  instruction  being 
    executed is available to the error handling routine.
 
 
 II. MAC resgister address space allocation
 
    My  last  ONTYME proposed allocating the 256 MAC  descriptors 
    and  the other registers in the MAC to addresses  01FFFB00  - 
    01FFFF10  in  order not to use 1K of memory at  low  address. 
    John  Kopf has indicated a preference to keep them at the low 
    1  meg  address pointing out that the 1K space that  it  uses 
    should not be a problem. 
 
    In  discussing this with Jim Willott,  he has indicated  that 
    decoding  either the high or the low address is not a problem 
    to  him.  However,  there is some hardware advantage to  have 
    more one-bits in the address to be decoded in that a 13-input 
    NAND can then be used instead of PAL.
 
    Furthermore,  even  though MAC register are  assigned  memory 
    addresses,  they  are not physically in the memory chips used 
    to  make up the 1 meg or 16 meg of  memory.  Separate  memory 
    chips  are used to hold the MAC descriptors.  If high address 
    outside the 16 meg is used for MAC registers, the full 16 meg 
    of  memory  can  be  addressed  using  virtual  and  absolute 
    addresses.  But if low address is used for MAC registers, the 
    1K  low address that the MAC registers map into can  only  be 
    addressed  using  virtual address since the  abolute  address 
    will be mapped to the MAC registers. 
 
 
    If there is no other strong objections,  I think we will keep 
    the MAC registers at 01FFFB00 - 01FFFF10.
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Mon, 15 Jul 85 14:48:45 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: MAC Addressing 


	I disagree with Tao as to the location of the MAC-register cells.

	I feel that there are three good reasons to place the MAC area into
the low area of local memory:

   o	No special addressing modes needed to manipulate the MAC --
	RX1 is suitable.

   o	The ability to access the MAC registers is desirable from off-
	card, for diagnostics, and for possible future implementations.

   o	Placing the MAC anywhere other than the low (or high) physical
	address-space will cause recuring difficulties with respect to
	un-MAC'ed (successor to DMA) references:  The problem is that
	code is written in terms of memory-Areas, each of which is described
	as one (or more) MAC segments.  Under the MAC, the program sees these
	areas as a contiguous block of memory, and will tend to use each
	area as such.  If such an area is used for DMA, it is very much
	desirable that the required number of physical memory segments be
	allocated as a contiguous block also.  If contiguous, then the DMA
	process can perform simple range-checks and address-translations;
	if not, each transfer is subject to independent relocation checks
	in order to detect discontinuities.  The result is excessive overhead
	for each such DMA-process (and that includes ISIS and the Dispatcher).

	If we were to to place the MAC registers at 01FFFB00, as recommended,
this would cause a "hole" that all processes would need to "map" around, (and
would make the MAC registers inaccessible to VME access -- for diagnostic
purposes -- as well).  In order to readily map "areas" into contiguous
physical memory, it will be desirable if all physical memory were readily
formed into a contiguous space (for reference via the VMEbus).  But if
we assign the specified addresses, we now have a discontinuity to map
around.

	My feeling is that the first preference is to place the MAC registers
in low private memory, to permit external VME access;  failing that,
map them into the highest possible addresses (FFFFFB00 = -500), in order
to assure that they are permanently "out of the way".

	I also feel that the System-card addresses should also be "out of
the way", and would recommend the use of addresses FFF00000+ (= -100000)
for this purpose.

	My main concern is the allocation of a block of addresses in a
region "where it is safe to put them because no one would EVER need to
address anything else in that range" has repeatedly burned us in the
past, and we should avoid making the same mistake again, if possible.

	(A reasonable compromise might be to place the MAC registers at
high memory [FFFFFC00 -- FFFFFFFF], and place the MAC control-registers
in low memory [with VME access].  This has the advantage of an external
processor being able to access failures, etc, by addressing a very small
range of physical [low] addresses, while not tying up the large space of
1024d bytes required for the MAC registers themselves;  the cost is two
separate areas of address-space and separate hardware range-checks for
each area.)

		  John Kopf
Received: from tymix.Tymnet by C39.Tymnet; Tue, 16 Jul 85 17:05:20 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA14620;
	Tue, 16 Jul 85 17:05:12 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA18464; Tue, 16 Jul 85 17:04:44 pdt
Return-path: <jwillott@eunix> 
From: jwillott@eunix (Jim Willott) 
Date: Tue, 16 Jul 85 17:04:44 pdt 
To: kopfj@c39, ntd.d/rawson@ontyme, ntd.p/driscoll@ontyme, ntd.r/brown@ontyme,
	ntd.s/king@ontyme, ntd.t/hsieh@ontyme, stimac@eunix, wrs@c39 
Message-id: <8507170004.AA18464@eunix.Tymnet> 
Subject: More MAC Addressing 

This is in response to the dialoge on MAC-register addressing.

I think that an overall skeem for asigning addresses must be defined first.
To do this we must deside if Skywalker is to be 24 address bits or 32
address bits and how many EPs or EP like things will be in the cage.
For example, we might say that we will use a 24 bit address with at most
16 things n the cage.  Therefore, the following address map is defined.

	00 0000 - 0f ffff = Thing 0
	10 0000 - 1f ffff = Thing 1
	20 0000 - 2f ffff = EP 1
	30 0000 - 3f ffff = EP 2
	40 0000 - 4f ffff = Memory
	50 0000 - 5f ffff = Disk controller
	60 0000 - ef ffff = not used
	f0 0000 - ff ffff = System controller

From this map it is clear that each Thing on the VME bus decodes only
the most significant 4 bit to deside if it should respond to the current
bus cycle.  I agree that the MAC registers must be addressable from the
VME bus.  As a result no matter where the registers are placed, there
is a hole in the contiguous memory space unless address modifier bits are
used to distinguish between memory and register access.  (Modifier bits
could be driven on the basis of MACed or unMACed address.)

From a hardware point of view I prefer that all memory on a Thing be
grouped together and all registers (non-memory I/O or ports) be grouped
together to simplify the address decoding.  Here an example might be to
reserve 64K for registers and decode XF 0000 - XF FFFF.

I thought that Skywalker was using the full 32 bit address of the VME bus.
I suggest that 16 Things in the cage is a reasonable limit even for the 20
slot cage.  This permits up to 64 megabytes per Thing.  I suggest that the
last 64K bytes of address within the Thing be reserved for I/O or registers.
If the "hole" is a problem then I suggest that address modifier bits be used
to clerify memory verses registers.


Jim
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Tue, 30 Jul 85 19:15:05 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: more on MAC Addressing 

	In response to Jim's memo, I have the following comments:

	First of all, a 32-bit word has 8 hex digits: HHHH HHHH (formatted as
two "short" fields);  a 24-bit address (in the same representation) must be
of the form 00AA AAAA.  I don't understand Jim's representation of addresses
using only 5 digits...these limit addressing to 20-bits (1-meg).

	My feeling is that a specific location addressed needs to have two
different representations, depending upon addressing via the VMEbus (global
address), or via VMXbus (local addressing).  Local addressing is the same
for all instances of the EP;  this is the ONLY way to insure that unmapped
interrupt routines and microcode can operate without incuring the overhead
of reloading the MAC.  For example, some fixed location within each EP will
be the "illegal Instruction Interrupt" location (for discussion, we can
assign this to local location 0000 0040).  This location is used for the
same purpose in all EPs, to insure that the microcode can always find it.
This same location is also accessible over the global VMEbus, but will
instead be referenced via some other address representation, such as
EEE0 0040 (where EEE is a bit-pattern unique to a specific instance of EP);
uses for this mode of access might be for one EP to be able to load code into
another.  There is no inconsistency here, just two different "names" for the
same physical memory cell.

	Furthermore, any EP would compute an address and pass it to the MAC;
if the MAC was enabled, the low-order 24-bits would be used to perform a
MAC lookup, and the resulting 32-bit address would then be used for actual
memory reference (either via VMX or VME);  if the MAC was disabled, the full
32-bit address presented by the EP would be used for this purpose.

	If the 32-bit physical address (the address beyond the MAC) were of
the form 00PP PPPP, this would be routed to the VMXbus (or local memory);  if
the two high-order digits were non-zero, the memory reference would be routed
to the VMEbus as a global reference.

	This allows two desirable features:

    o	Up to 16-meg of local address-space accessible via the VMXbus
	(and thus no contention for memory);
    o	Full access to global memory via VMEbus (either directly un-MAC'ed)
	or via the MAC (to implement global shared memory).

	This model makes a couple of assumptions, such as the availability of
dual-ported memories capable of independent address-selection for each port.
I don't see this as a problem, since we have been talking of EPs with 1-meg
of on-board memory, and up to 16-meg of local (on-board and VMX) memory,
which will require specific addressing for each board anyway.

	Probably the major hassle for the software will occur in the correlation
of global and local memory banks -- while it will always be possible to
include this information as assembly parameters at sysgen time, it would be
far more desirable for the software to correlate the global and local
addresses at run-time.  The existence of this information is vital to permit
one processor to request another to perform memory-transfers (e.g., I/O),
when the first processor will reference the data via local addresses, but the
second can only reference it via global addresses.

John Kopf
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 23 Aug 85 13:28:39 PDT 
To: (Skywalker Task Force) Ralph Brown <NTD.R/BROWN@Ontyme>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Tau Hsieh <NTD.T/HSIEH@Ontyme>, Stan King
	<NTD.S/KING@Ontyme>, John Kopf <KopfJ@C39>, Bill Soley <WRS@C39>,
	Michael Stimac <stimac@tymix>, Jim Willott <jwillott@tymix> 
Cc: Dick Rawson <NTD.D/RAWSON@Ontyme> 
Keywords: SKYWALKER 
Subject: GED corrections 

1. VMX bus multimaster capability?
	3.1.1.2 VMXbus states "...at most two possible bus masters." but
	6 Configurations, paragraph 3 states "...6 cards (and only one
	bus-master) per VMXbus."  At least one of these should be changed.

2. dedicated pathfinders
	6.2.1 Enhanced Supervisor, paragraph 2 states "one primary sup
	and four pathfinders to support a network five times as big."
	This assumes a linear-time path finding algorithm which does
	not exist.  The best known algorithm is N*log(N) which would
	require six pathfinders to control a 10,000 node network (or
	five times our existing 2000 node network):

		10000 * log(10000)
	      ----------------------  =  6.0587
		 2000 * log(2000)

	So, change "...and four pathfinders..." to "...and six
	pathfinders...".

Otherwise, the GED looks very good.
-Bill
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 27 Aug 85 17:17:11 PDT 
To: (Skywalker Task Force) Dave Bliss <NTD.D/BLISS@Ontyme>, Ralph Brown
	<NTD.R/BROWN@Ontyme>, John Coons <coons@eunix>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Stan King <NTD.S/KING@Ontyme>, John Kopf
	<KopfJ@C39>, Bill Soley <WRS@C39>, Michael Stimac <stimac@tymix> 
Cc: Tau Hsieh <NTD.T/HSIEH@Ontyme>, Dick Rawson <NTD.D/RAWSON@Ontyme>, Jim
	Willott <jwillott@tymix> 
Keywords: SKYWALKER 
Subject: Support for Concurency 

[[ This is intended for John Kopf to include in the GED (or GID?).  It
is being sent to everyone for comments, additions, deletions and such.
Text enclosed inside double-square-brackets is directed toward the task
force as questions or comments for further study and discussion.  -Bill
]]

## Support for Concurency

In order to support distributed algorithms efficiently and effectively
within the multiprocessor environment of Skywalker, various primative
operations must be present at the hardware and firmware levels.  This
section particularly addresses any special hardware functionality
required to generally support common distributed programming techniques.

### Atomic Operations

In order to fully exploit the speed of a bus structured sharred memory
architecture, it must be assured that software running on any processor
be able to initiate certain atomic operations on sharred memory.
Although all of these functions may be built at the software level, they
would be prohibitively slow for most applications.  Skywalker addresses
this issue by allowing a VME bus master to hold the bus for multiple
cycles (in accordance with the VME specifications), thereby locking out
other VME masters from accessing that memory for the duration of the
transaction.  In this way, EP microcode can implement arbitrarily
complex atomic transactions on sharred memory accessed through the VME
bus.  [[ question:  can we do this for non-VME bus access as well?  ]]
Any future processors or devices (in particular, the SCC card) which
intend to participate in distributed programs must have this capability
as well.

### Interrupt Structure

In addition to the global interrupts implemented by the VME bus, each
Skywalker processor or programmable device will have a set of registers
appearing at unique VME bus addresses which will generate a maskable
interrupt when written to.  These registers can be used to efficiently
implement various message passing schemes at the software level.  Each
processor will also be able to initiate standard VME bus interrupts.

-Bill
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Fri, 30 Aug 85 21:35:00 UT
From: NTD.D/RAWSON@Ontyme.Tymnet 
Date: 30 AUG 85 12:53:46 
To: NTD.W/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: NTD.W/SOLEY@Ontyme.Tymnet 
Message-id: I38730@Ontyme.Tymnet 
Keywords: SKYWALKER 
Subject: "Bill, please forward to the Task"... 
Resent-From: William R. Soley <WRS@C39.Tymnet>
Resent-Date: Mon, 2 Sep 85 18:14:39 PDT
Resent-To: (Skywalker Task Force) Dave Bliss <NTD.D/BLISS@Ontyme>, Ralph Brown
	<NTD.R/BROWN@Ontyme>, John Coons <coons@eunix>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Stan King <NTD.S/KING@Ontyme>, John Kopf
	<KopfJ@C39>, Bill Soley <WRS@C39>, Michael Stimac <stimac@tymix>
Resent-Cc: Tau Hsieh <NTD.T/HSIEH@Ontyme>, Dick Rawson <NTD.D/RAWSON@Ontyme>,
	Jim Willott <jwillott@tymix>

Bill, please forward to the Task Force.  Thanks - Dick
 
A Comment on Shared-Bus Contention
 
 
Skywalker's VME bus can be a system bottleneck, although we don't know
enough yet to say under what conditions it actually is.  But I've been
thinking about what to do about the problem if we meet it.
 
My ideas seem to fall into two categories:  minimizing usage of the
shared bus, and arbitrating among contending requests.  This note
concerns that second category.
 
I wonder if it is possible for a board to assert requests of different
priority.  For example, suppose there were three access request
priorities.  The board could assert a low-priority request, in which
case it loses to any high-priority request, but shares fair access
among contending low-priority requests.  Similarly, if it asserts a
high-priority request, it shares access with other high-priority
requests, but always wins over any low-priority request.  To be
useful, the board must be able to select a different priority for any
request that needs it.
 
Can VME support this?
 
If VME can support it, could we exploit it to much advantage?  It's
not clear to me whether we can; I'll mention one plausible use later
on.  But let me explain the principle I'm trying to use.  Then maybe
others of you can help me find how to apply it to Skywalker.
 
I'll explain the principle in terms of the only implementation I have
heard of.  ARPA Project Genie developed a timesharing system for the
SDS 930/940, which Tymshare used as the start of its business.  ARPA's
system had a drum; I don't know if Tymshare used the drum.  Drum I/O
could be overlapped with cpu execution; the drum's DMA access used a
substantial fraction of the memory's bandwidth, but drum plus cpu
memory access didn't entirely saturate the bandwidth.  The drum had a
one-word buffer (in addition to the shift register), and had higher
priority than the cpu for memory access:  it was rotating storage, and
couldn't wait arbitrarily long for memory access without losing data.
(No backpressure.)

 
 
 
 
 
Measurements showed a significant number of cpu access requests
blocked by a coincident drum request - maybe on the order of 30
percent.  I don't remember the statistics of cpu and drum demands.
But contention didn't need to be that high.  The drum's interface was
modified to make its first two memory requests at low priority, and
then if both those had failed, to make the third request at high
priority; the cpu had an intermediate priority.  The drum could always
wait three memory cycles for access with no effect on its throughput.
With this change, the cpu saw almost no memory contention.
 
 
We might be able to use similar techniques in Skywalker.  For example,
suppose the SCC card buffered input in private memory:  either on its
card, or else on its VMX bus (and the VME path to that memory is never
used).  When some amount has accumulated, the SCC tries to copy it to
shared (on VME) memory, possibly the memory of some Engine Processor.
However, the SCC uses low-priority bus requests unless and until it
accumulates too much of a backlog, or a request has been denied too
many times.  The Engine Processors use an inbetween priority level for
requests, and so don't wait for an SCC unless it gets backlogged.
 
I'll glaly admit we don't know if VME bus contention is a problem, and
if it is, we don't know if it is the kind of problem that this
technique would help.  But if the VME specification would allow this
technique, let's design things so the technique could be adopted
later.
From: John Kopf <KopfJ@C39.Tymnet> 
Date: Thu, 12 Sep 85 11:39:51 PDT 
To: (Skywalker Task Force) Dave Bliss <NTD.D/BLISS@Ontyme>, Ralph Brown
	<NTD.R/BROWN@Ontyme>, John Coons <coons@eunix>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme>, Stan King <NTD.S/KING@Ontyme>, John Kopf
	<KopfJ@C39>, Bill Soley <WRS@C39>, Michael Stimac <stimac@tymix> 
Cc: Tau Hsieh <NTD.T/HSIEH@Ontyme>, Dick Rawson <NTD.D/RAWSON@Ontyme>, Jim
	Willott <jwillott@tymix> 
Keywords: SKYWALKER 
Subject: Skywalker MAC Addressing 

	It has just come to my attention that a proposal was made to change
the SKYWALKER MAC to support a virtual address-space specified by sixteen
1-meg memory-segments, rather than 256 64K memory-segments.

	I understand that the argument for this was that 256x64 was only
required for compatibility with the Superengine;  now that that no longer
exists, we are free to make changes of this nature.

	THIS IS ABSOLUTELY FALSE!!  256x64 was selected to retain backward
compatibility with the Engine and existing applications, and only the
"256" portion was at all related to the Superengine.  The requirement for
64K segments is absolutely necessary to permit a smooth transition between
the Engine and SKYWALKER for existing code.  The reasons follow:

****	Existing applications explicitly use the Engine's 64K segment-size
for a variety of purposes, such as segregating code and different data-
areas (for protection and other considerations) into specific 64-K segments.
The corresponding addresses are defined not only by the NAD assembler (and
its SEG directive), but also by ORG and EQ statements {e.g., the ISIS
variable "FASTC" is typically defined as:

FASTC	EQ	0F0004

Using 1-meg segments, this is unconditionally at the end of segment-0
in the (virtual) address-space, and all object code will try to access
that (virtual) location for this variable;  it really exists in a different
part of memory (being maintained by ISIS, rather than the application),
and MUST be accessed via a different MAC-segment.}

****	We are not allowed to make changes that MUST be reflected in the
individual applications {changes reflected in ISIS are OK;  changes which
can be inflicted on applications via modifications to the standard ISIS
libraries are perhaps acceptable (although discouraged) -- however, major
applications (e.g., Supervisor, Node-code, Consat) DO NOT USE these
libraries}.

****	An argument for 16-sets of 16x1M segments is reduced context-
switching time;  this is not really all that critical, for several reasons:
	o   such switches occur frequently only in lightly-loaded machines,
	    and the frequency of invocation decreases as the loading increases;
	o   SKYWALKER is significantly faster than the Engine, and thus the
	    overhead imposed by context-switching is correspondingly less;
	o   The overhead of flushing 256 segments is minor (1-cycle?), and
	    is more than made up for by the need for ISIS to only load those
	    segments actually used (typical applications only use 5 or 6
	    segments);
	o   Since it is very likely that the MAC registers will be outside
	    SKYWALKER's memory address-space (i.e., accessed via different
	    address modifier bits), special microcode will probably be required
	    to load the MAC anyway, and can be designed to do an efficient copy
	    operation;
	o   Applications which do require more than 16 64K segments are very
	    likely to have that machine dedicated to their use anyway.
Thus it is not clear that this argument is really important.

****	In a multiple-processor environment, it may be highly desirable to
permit an application more than 16 segments for uses such as shared memory
and inter-processor communications.


	I feel that this issue is still in question, and must be resolved
before we actually commit to a particular implementation.

		John Kopf
Received: from tymix.Tymnet by C39.Tymnet; Mon, 16 Sep 85 8:49:03 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA01715;
	Mon, 16 Sep 85 08:48:26 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA09675; Mon, 16 Sep 85 08:48:33 pdt
Return-path: <jwillott@eunix> 
From: jwillott@eunix (Jim Willott) 
Date: Mon, 16 Sep 85 08:48:33 pdt 
To: kopfj@c39, ntd.d/bliss@ontyme, ntd.d/rawson@ontyme, ntd.p/driscoll@ontyme,
	ntd.r/brown@ontyme, ntd.s/king@ontyme, ntd.t/hsieh@ontyme,
	stimac@eunix, wrs@c39 
Message-id: <8509161548.AA09675@eunix.Tymnet> 
Keywords: SKYWALKER 
Subject: Re: Skywalker MAC Addressing 

I believe that the information you have received may be incorrect.
A change in the TSI MAC was made.  The old TSI MAC was going to support
two modes -- one mode of 256 segments of 64 pages of 1024 bytes and a 
second mode of 16 contexts of 16 segments of 64 pages of 1024 bytes.
The change is to limit it to a single mode of 16 contexts of 16 segments 
of 64 pages of 1024 bytes.  It was further indicated that if this proved
unacceptable to Skywalker a different MAC gate array would be defined.


-jim

Received: from tymix.Tymnet by C39.Tymnet; Wed, 31 Jul 85 12:58:09 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA29850;
	Wed, 31 Jul 85 12:57:51 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA05375; Wed, 31 Jul 85 12:57:42 pdt
Return-path: <jwillott@eunix> 
From: jwillott@eunix (Jim Willott) 
Date: Wed, 31 Jul 85 12:57:42 pdt 
To: kopfj@c39, ntd.d/rawson@ontyme, ntd.p/driscoll@ontyme, ntd.r/brown@ontyme,
	ntd.s/king@ontyme, ntd.t/hsieh@ontyme, stimac@eunix, wrs@c39 
Message-id: <8507311957.AA05375@eunix.Tymnet> 
Subject: A Proposal for Addressing 



	With this Ontyme I am trying to pull together all of the
various thoughts on addressing into an addressing proposal.  Within
a short time the addressing question will start impacting TSI.
I want to be reasonably sure that I understand what is needed and 
that everybody else understands what will be inexpensive to do 
in hardware.

WITHIN AN INTELLIGENT MODULE

	These modules all have some kind of processor on board --
a 68000, a 68020 or a bit slice.  A single module may be made up of
from one to six boards and may use the VMXbus to interconnect the
members of the module.  All local (on board) resources must be 
accessed through the least significant 24 bits of the address.
That is the most significant 8 bits are all zero.  Within this 
16 megabyte address space the last 64K bytes are reserved for 
non-memory  registers and I/O ports.

	0000 0000 - 00FE FFFF	Local memory, on board or VMX
	00FF 0000 - 00FF FFFF	Local I/O (MAC registers, Console...)
	0100 0000 - FFFF FFFF	Global  resource -- VMEbus


WITHIN A SINGLE VME CARD CAGE

	Limit the maximum the maximum number of intelligent modules
and global memory to 15.  Each of these items can support up to
16 megabytes of address space.  Within the 16 megabyte address space
only the last 64K bytes may be used for I/O and may impose special
requirements for access such as specific address modifier codes to
signal appropriate privilege level.  In the case of the global memory
all 16 megabytes would probably be available.  Each module will have
a means of defining the base address for access from the VMEbus.
This will permit any module to address memory and possibly I/O 
on any other module.  There is no guarantee that the global 
address space is contiguous.

	The following list is only a suggestion of how module
base address might be assigned.  I suggest that the system controller
be assigned a base address at one end of the address range and I
have am suggesting the high end.

	0100 0000 - 01FF FFFF	EP1
	0200 0000 - 02FF FFFF	EP 2
	0300 0000 - 03FF FFFF	SCC 1
	0400 0000 - 04FF FFFF	SCC 2
	0500 0000 - 05FF FFFF	SCC 3
	0600 0000 - 06FF FFFF	SCC 4
	0700 0000 - 07FF FFFF	SCC 5
	0800 0000 - 08FF FFFF	SCC 6
	0900 0000 - 0CFF FFFF	Reserved for expansion
	0D00 0000 - 0DFF FFFF	Shared Global Memory
	0E00 0000 - 0EFF FFFF	Disk Controller
	0F00 0000 - 0FFF FFFF	System Controller



THE LAST NIBBLE

	The most significant four bits of the address are reserved
for future use.  One use that has been suggested is for decoding
up to 15 card cages communicating over some kind of additional
bus.  This feature could be included in the custom designed system
controller board.

Jim
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 04 MAR 87 17:07:42 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I01635@Ontyme.Tymnet 
Subject: Skywalker Thursday Reviews 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
                                 M E M O R A N D U M
 
          McDonnell Douglas                                          Tymnet
          Network Systems Company            Network Technology Development
 
          DATE>          March 4, 1987  17:02
 
          TO>            Skywalker Core Team
                         Mike Rebmann
 
          COPIES>        Al Fenn
 
          FROM>          Ken Holcomb
 
 
          SUBJECT>       Skywalker Thurs. Status Review Mtg.
          -----------------------------------------------------------------
 
          Due  to  a  conflict  with  the IOC Design Review Checkpoint, our
          Weekly Status Meeting needs to be re-scheduled to  3:00PM through
          the merry month of March, beginning with our mtg on 3/5/87.
 
          Thanks......Ken
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 30 MAR 87 16:39:42 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I13358@Ontyme.Tymnet 
Subject: Skywalker Port Signals 

 
 
 
 
 
 
 
                                 M E M O R A N D U M
 
          McDonnell Douglas                                          Tymnet
          Network Systems Company            Network Technology Development
 
          DATE>          March 30, 1987  16:34
 
          TO>            Dick Rawson              BJC
                         Dan Lasater              Bill Euske
                         Gazel Tan                John Kopf
                         Bill Soley               Mike Rude
                         Pat Driscoll             Michael Stimac
                         Dave Alcorn              Dave Bliss
                         John Coons
 
          COPIES>        Al Fenn
 
          FROM>          Ken Holcomb
 
 
          SUBJECT>       Signal requirements for Port Distribution
          -----------------------------------------------------------------
 
          The following  is my understanding of the signalling requirements
          for Phase 1 Skywalker.  While the other signals specified  in the
          IOC HFS  are nice,  none of the software currently available uses
          these signals.  Also,  in your  design, we  probably should allow
          that some  time in  the future,  those additional  signals may be
          supported.
 
          Async                              Sync
 
          TD                                 TD
          RD                                 RD
          RTS                                RTS
          CTS                                CTS
          DSR                                DSR
          DTR                                DTR
          CD                                 TC
          BSY                                RC
 
          Should anyone on this distribution list have a problem with this,
          please get  to me ASAP with your objections. This is an important
          issue in the port distribution design.
 
          Thanks....Ken
           
 
 
 
 
                                          1
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 31 MAR 87 17:07:14 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.D/RAWSON@Ontyme.Tymnet, NTD.D/LASATER@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.P/DRISCOLL@Ontyme.Tymnet, NTD.D/ALCORN@Ontyme.Tymnet,
	NTD.J/COONS@Ontyme.Tymnet, NTD.BJC@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.J/KOPF@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet, NTD.M/STIMAC@Ontyme.Tymnet,
	NTD.D/BLISS@Ontyme.Tymnet, NTD.A/FENN@Ontyme.Tymnet 
Message-id: I14005@Ontyme.Tymnet 
Subject: RODNEY COMMENTS 

TO:    Ken Holcomb

FROM:  Mike Rebmann

COPY:  Mary Arnett
       Mike Cady
       Al Fenn
       Rick Sprenkel
       Jeff Stern

SUBJ:  RODNEY Review, Your OnTyme #I12869 (Appended)


Thanks for your OnTyme.  I appreciate the need for thorough, 
timely review input on key development documents by all relevant 
organizations.  That's why I got verbal approval from Jeff last 
week and sent a followup OnTyme requesting he make it formal by 
sending you a message.  Inasmuch as you have not yet received the 
message, NTD should consider the document approved - both because 
that's our agreed "default," and also because of the verbal 
approval Jeff conveyed.

I am, however, aware of comments on the document by Mary Arnett 
and her team.  These were expressed in #M76775, sent March 19.  
In case you did not receive the comments, I am resending them 
separately.  I am unaware of any other comments from anyone, and 
it is Jeff's job to get any such comments to you.

To avoid any future problems in the review/approval area, I 
intend to get the reviewers/approvers clearly defined by document 
as a result of the program planning meeting April 1, and 
following.  As input to that, will you please prepare a list of 
documents for which external approval is required, plus their 
associated planned release dates, and bring 20 copies to the 
meeting.  Thanks, Ken!

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                       M E M O R A N D U M
 
McDonnell Douglas                                          Tymnet
Network Systems Company            Network Technology Development
 
DATE>          March 30, 1987  8:33
 
TO>            Mike Cady
 
COPIES>        Mike Rebmann             Al Fenn
 
FROM>          Ken Holcomb
 
 
SUBJECT>       Rodney Review Comments
-----------------------------------------------------------------
 
As of  this morning,  I have still not received any comments from
your  organization  regarding  Rodney.     I   can  only  assume,
therefore,  that  none  are  forth  coming.  At this point, if we
receive   any   substantive   request   to   change   the  Rodney
specification, it is likely that it could be accommodated only by
slipping the schedule.  
 
Thanks...Ken

TO:    Ken Holcomb

FROM:  Mike Rebmann

COPY:  Rick Sprenkel
       Jeff Stern

SUBJ:  QSATS RODNEY Review, March 19


As I stated in my response to your #I12689, Mary Arnett has 
forwarded review comments on RODNEY on behalf of her team.  In 
order to assure you receive them with no further delay, I have 
appended her OnTyme.  Regards.


    MSG#:M76775
     IN#:  198
      TO: M/REBMANN       
    FROM: QSATS    M/ARNETT        
    SENT: 19 MAR 87 12:33:30  
    READ: 20 MAR 87 09:41:53  
    SUBJ: 

    TO:    Jeff Stern/Mike Rebmann
    FROM:  Mary Arnett
    DATE:  March 19, 1987

    CC:    NETS.M/CADY,NETS.R/SPRENKEL,NETS.M/REBMANN,TYM.J/STERN
    QSATS.B/NOVAK QSATS.J/GOMES,QSATS.D/JOHNSON,QSATS.P/MISTRY
    QSATS.T/KOCHMANN QSATS.D/ELLENBERG



                      RODNEY ARCHITECTURAL SPECIFICATIONS REVIEW
                                         from
                              QSATS Skywalker Test Team


    The following is a list of the Skywalker Test Team's concerns, 
    questions and recommendations for your consideration.  Alterna-
    tives and/or implementation ideas presented here are given to 
    share with you some of our thoughts.


    FILE SYSTEM

    File system protection

    If the client has write access to the handle it seems that he can 
    access any disk file, thus presenting a security risk, as well as 
    being able to modify or corrupt files.

    Questions:  How much information is in the file handle?  In which 
    address space is the file handle resident?  What protection does 
    the handle have?  Can access be gained to something else by manip-
    ulating the file handle?  Are there any file system attributes?  
    Where?  What could modify it?

    Recovery of disk space on client crash:  How is this accomplished?


    BUS-DRIVER FAILURES

    Failed drivers asserting (disabling/negating) VMEbus signals.

    Question:  Is there protection from this possibility?


    DISTRIBUTED DISPATCHER

    Buffering limitations

    Existing MP dispatcher may NOT be the best model for a Skywalker 
    dispatcher due to buffering limitations.  MP dispatcher implements 
    in S/W the Engine bufferlet scheme and is limited to only 32K.  
    Both are thought to be inadequate due to processor/throughput 
    demands.

    A possible alternative might be Ring-lets (circular buffers with 
    chaining capabilities), and buffer space whose size is dependent 
    upon the system configuration, i.e. max. port no., max. users, 
    etc.

    A second alternative might be to take advantage of the 68000 
    processor series "loop mode" for messaPotential


    IPCF

    Potential for a late processor reading the wrong message.

    Once the recipient has modified it's translation descriptor tree, 
    the sender has no control over the receiver's access and should 
    not free memory until ALL recipients have read the message.  A 
    possible alternative to a timeout to free memory might be to count 
    the number of rings in which we place a pointer to a message.  The 
    recipient can be allowed to lock and decrement the count field.  
    (CAS can be used for decrementing.)

    Message solicitation

    IPCF comments imply that some process (OS?) must "poll" to see if 
    messages are present.  Why not include this in periodic operating 
    system processing, such as the scheduler?  Mail-box interrupts 
    would be an even better solution.  We are assuming that an opera-
    ting system "relays" messages to certain tasks.

    If a write process times out before successfully sending a message 
    and the recipient has no knowledge of who the sender was, can the 
    message get lost?  Does the sender have any way of knowing he was 
    unsuccessful in sending the message?  A sender should know enough 
    to retry.

    Overhead

    When messages are sent, enough address information must be sent to 
    allow the receiver to add the message pointer to his logical 
    address space; therefore we have to add or modify and later remove 
    part of the translation descriptor tree for each message.

    A possible alternative would be to pass the physical address of 
    the message and VME base.  We could pass the physical address of 
    the page descriptor for the buffer and add this to the receiver's 
    descriptor tree (68851 INDIRECTION), allowing the sender some 
    later access control.

    Another possibility might be to use small pages and define the 
    message as a set of contiguous pages resulting in the need to only 
    pass the page descriptor address.  This would allow the recipient 
    processor to "allocate" a local buffer with correct mapping.  No 
    second IPCF mapping would be necessary.

    Questions:  Can multiple processes on a single processor use IPCF, 
    or do we have a "single handler"?  Is there any way to trace the 
    sender of a message?  (This could be very important for debugging 
    purposes.)


    MMU

    Concurrence Problems

    It is unclear if both RODNEY and the Operating System maintain the 
    translation tables.  If both do, what protection do we have that 
    concurrence problems do not result in mapping to an incorrect 
    address.


    I/O COMMUNICATION

    Definition of access to another CP's memory

    Questions:  Is this a DMA?  Do we have a VMEbus DMA protocol 
    controller?  Do we have to pass MMU access information and manip-
    ulate MMU's?  Can one CP access another CP's buffers?


    VMEbus

    Lack of 32 bit operations across the VMEbus requires 32 bit trans-
    fers to be executed as two 16 bit operations.  A later 32 bit 
    upgrade may involve software changes to take advantage of 32 bit 
    atomic R/W operations.  Dual ported RAM for the VMEbus seems to 
    imply no need to limit ourselves to only 16 bit displacements on 
    the bus, since the bus interface could handle the difference.


    SYSTEM LOGS

    Page 34 offers a choice of initials or username.  Username should 
    be used to avoid ambiguity, and to aid in debugging.

    Logger information will be written to the disk when the log buffer 
    fills up.  There should be a timer added to update the disk 
    periodically, even if no events are written into the log buffer.

    The boot code should ALWAYS write out the last current log buffer 
    to the disk in the event of a card failure.

    Record the initial relative time stamp at system bring up, as well 
    as the last relative time stamp before UTC is recorded.


    SECURE UID

    Non-socketed EEPROM

    It should be impossible to erase certain information on the 
    EEPROM, namely the Manufacturers serial number, the H/W model 
    number, and the PC board art work version.

    Nonerasable information can be stored into the high address field 
    of the EEPROM prior to installation on the individual cards.  The 
    PC board can then provide only traces to the lower order address 
    lines, allowing future upgrades to be input.  If it is desirable 
    to install the EEPROM prior to wave soldering the populated board, 
    then a fixture can simply be provided at the point of assembly 
    and/or the depot for repairs to allow writes to the entire address 
    field initially.  When removed again only the lower addresses will 
    be able to be rewritten.


    DEBUG

    Page tables

    The debug facility should have access to the page table areas.  
    This would be very useful for tracking and identifying bus errors.

    Debugger Output to TTY problem

    Output to the TTY port appears to be the same as in the original 
    XRAY design.  Past experience has shown this can create a problem 
    due to the slow baud rate,i.e. 300 baud, and the need for very 
    careful buffering.


    BACKUP

    We would like to emphasize that failure information MUST be logged 
    before reinitialization.

    We are concerned about the Single Point of Failure with the 
    arbiter and possibly the IOCP, depending upon the design.


    BOOTSTRAPPING

    3 second window

    We feel that the 3 second window may not be enough time to allow a 
    DLL BOOT command in to change the load file name, if the system 
    used is very busy.
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 03 APR 87 08:28:33 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.BJC@Ontyme.Tymnet, NTD.K/HOLCOMB@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.P/DRISCOLL@Ontyme.Tymnet, NTD.M/STIMAC@Ontyme.Tymnet,
	NTD.D/ALCORN@Ontyme.Tymnet, NTD.D/BLISS@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.J/COONS@Ontyme.Tymnet, NTD.J/KOPF@Ontyme.Tymnet,
	NTD.B/SOLEY@Ontyme.Tymnet, NTD.D/RAWSON@Ontyme.Tymnet 
Message-id: I15436@Ontyme.Tymnet 
Subject: SKYWALKER NODE CODE SUPPORT 

TO:     Ken Holcomb

CC:     Mike Rebmann
        Rick Sprenkel
        Jeff Stern
        Phil Kreter
        Al Fenn

FR:     Mike Cady

RE:     Skywalker Node Code Support

------------------------------------------------------------------------

To respond to your question:

YES, Release 1 of Skywalker must support sync lines in node code.

Do we have a problem?  If so, why?  We need Skywalker to talk to
our Micro family.  This is a Skywalker agreed-to requirement.

Regards.

Mike
Received: from EMSBUB.Ontyme.Tymnet by B39.Tymnet; Fri, 23 Jan 87 17:26:10 UT
From: BUB.BJC@EMSBUB.Ontyme.Tymnet 
Date: 23 JAN 87 07:55:29 
To: BUB.B/SOLEY@EMSBUB.Ontyme.Tymnet (BUB.WRS) 
Cc: BUB.M/STIMAC@Ontyme.Tymnet, BUB.D/BLISS@Ontyme.Tymnet,
	BUB.J/COONS@Ontyme.Tymnet, BUB.D/LASATER@Ontyme.Tymnet,
	BUB.G/TAN@Ontyme.Tymnet, BUB.P/DRISCOLL@Ontyme.Tymnet,
	BUB.K/HOLCOMB@Ontyme.Tymnet, BUB.W/EUSKE@Ontyme.Tymnet,
	BUB.D/ALCORN@Ontyme.Tymnet, BUB.M/RUDE@Ontyme.Tymnet,
	BUB.A/FENN@Ontyme.Tymnet, BUB.J/SARGENT@Ontyme.Tymnet,
	BUB.B/SOLEY@Ontyme.Tymnet, BUB.J/KOPF@Ontyme.Tymnet 
Message-id: Y01752@Ontyme.Tymnet 
Subject: SKYWALKER project meeting minutes. 

  22 January 1987     skymin.122
 
  To: Distribution
  From: BJ
  Subject: SKYWALKER project meeting minutes.
 
  Here are the minutes from today's meeting.
 
  AGENDA:
  1. OLD ITEMS.
  2. NEW BUSINESS.
     sub-committee meeting reports
     distribution
     FASB
  3. Other new business
 
 
  OLD ITEMS
  1. REVIEW AND REFINE SCHEDULE.
 
  1A.  Diagnostics:   put on disk
  and delivered to Ken.
  ==> John C.  Deliver disk to Ken mid next week
 
  1B.  Translator:  details provided on hardcopy.
  outstanding questions to be handled off line
  between Stimac and Holcomb. Superproject on order.
 
  1C.  Mech. Eng:updates to be provided by end of day.
  ==> Dave B. Get updates to Ken by 23 January
 
  1D.  Xcom, Node code translator changes:  meeting to
  be held 22 January.
  ==> Pat D, Gazel. Get updates following meeting to Ken.
 
  1E. Multiple node code:
  ==> Gazel, get schedules to Ken next week.
 
  1F. ISIS: additional changes.
  ==> Dan. Get disk with changes to Ken next week.
 
  1G.  Other projects: Load facilities need to be further
  defined. additional meeting scheduled for friday am.
  ==> BJ. Report results of meeting.
  ==> Mike. Get project schedules for requirements as defined
  in meeting.
 
  1H.  Source level debugger evaluation is needed as well as
  qualification of external vendor if appropriate. Selection of
  development system tools is needed.
  ==> Mike. Write up proposed solution for Engine/68K debugger
  distribute for review.
 
  ==> NEXT MEETING. is there an outstanding issue regarding
  compilers/assemblers/linkers? Do we have organization wide
  agreement on which tools we will be using?
 
  1I. Design document deliveries need to be put into schedules,
  included as milestones.
  ==> Everyone. Define GED/GID/ERS/IMS etc. design documents
  which will be prepared for each project component, get dates
  and documents to Ken.
 
  1/22 Status: Document schedule provided by Bliss and Lasater.
  ==> Everyone else. Incorporate documents into schedule
  updates to be provided to Ken.
 
  1J.  Documents due to Mktg: Monterey meeting will be internal
  NTD doc. BJ to distribute proposed outline for "Architectural
  spec" for comments and work assignments. BJ to distribute
  "Proposal document" (per methodology) for comment, then
  distribution to marketing.
 
  1/22 Status: Draft Proposal distributed.
        includes draft Architecture summary
  ==> Dan. Fill in SW pieces, review and comment.
  ==> Dave. Fill in HW pieces, review and comment.
  ==> Everyone else. review and comment.
 
  1K. Project documentation storage. File naming convention and
  document directories need to be established. Procedures
  for moving stuff from PCs and PUBs to UNIX need definition.
 
  1/22 status: Les has some macros and document procedures.
  Elliot is working on macros.
  ==> John C. Get Elliot in touch with Les, distribute
  resolution.
 
  1L.  Training: Training programs, reference material and
  documentation lists for Basic UNIX, C-Roff etc. are needed.
  ==> Mike Stimac. put together a training course outline
  recommendation and  reference material list. Get together
  with Lasater to discuss.  Training plan should be completed
  by end of January. See BJ if admin help is needed.
 
  1/22 status: Lists of staff for training have been provided
  by Dan, Gazel, Dave, and Gomes.
  ==> ANYONE else who needs staff training, get names to Ken.
 
  2. MANPOWER FORECASTS.
 
  Xcom and node code. Additional detail requires further
  specification.
  ==> Pat D, Gazel. prepare and give to Ken after meetings
  with Mike Stimac.
 
  3. PROJECT PLAN.
 
  Distributed for review. Comments to be provided to Ken.
  ==> Everyone. Review and comment to Ken.
 
  1/22 STATUS. Dave B and BJC have provided comments.
  ==> Everyone else. Do it.
 
  4. FACILITIES.
 
  Pre-prototype system quote received and reviewed,
  Dan is submitting rec.
 
  Printer recommendations have been made.
  ==> Bill E. Resolve printer issue with Al.
 
  Tymcoms needed for intra-bldg access to VAX/SUN
  systems - 3 MACIII minis with SIO network connections.
 
  1 additional CMF backplane needs modification for use
  by Diagnostics.  John to follow up with Dave.
 
  X-roff license issue deferred until printer issue
  is resolved.
 
  5. FASB.
 
  Activity code lists distributed. Set up projects where
  needed in each group. Guideline for how many projects to
  set up should be what level of granularity you would like
  to see when the project is over in terms of forecasts,
  estimates, actuals.  Remind staff that implementation doesn't
  start until design reviews are completed.
 
  NEW ITEMS
 
  - SKYWALKER sub-committee meetings need to have
  1 person assigned as note taker. Notes need to
  written and distributed to Project Notebook.
 
  - DISTRIBUTION list for SKYWALKER distribution has
  Kopf and Soley added to it.
 
  - FASB.
    a. Use 1915-1467 for general SKYWALKER planning activities
    and meetings not directly associated with a specific
    project component.
 
    b. Get projects defined and to Rosie under PIR 1915 for
    the SKYWALKER projects in your groups. Put schedule dates
    on them.
    ==> Pat D, Mike S, Dan, John.
 
    NOTE: SKYWALKER is THE MAJOR source of capitalized
    development expenses for 1987. It is important that the
    audit trail is clean on this project.  Make sure staff
    is aware of correct project numbers and activity codes.
    see BJ if you have ANY questions about this.
 
  - Multi-Mac svcs will not be implemented in Release 1
    SKYWALKER.
 
  - Dan has assigned a core team to do the Release 1 software
    design. Design to be distributed 9 February. Update meeting
    with team on 29 Jan and 5 Feb 9am. Review meeting on
    Design document 12 Feb 9am.
    Team is to use current ISIS and Marketing requirements
    as Design requirement input. They are also to identify
    but defer any issues associated with future releases.
 
  - Marketing queries. Requests by marketing for discussions
    on SKYWALKER project should be referred to Ken or BJ.
    Marketing management would like the project and project
    communication to be handled in a formal fashion.

  - A mechanical engineering Concept review will be appropriate
    soon and will be scheduled in Feb.
    ==> Dave, let Ken know when to schedule review.
 
  NEXT MEETING.
 
  Regular core team meetings 1 hour each week.
  Regular time to be THURSDAYS at 1:30 pm.
 
  NEXT MEETING THURSDAY , JANUARY 29 1:30.
 
  Distribution:        cc:
  Stimac         Holcomb
  Bliss          Euske
  Coons          Alcorn
  Lasater        Rude
  Tan            Fenn
  Driscoll       Soley
  Kopf
Received: from tymix.Tymnet by B39.Tymnet; Fri, 20 Feb 87 13:40:46 PST
Received: by tymix.Tymnet (5.51/4.7) id AA16744; Fri, 20 Feb 87 13:40:10 PST
Received: by eunix.Tymnet (5.51/4.7) id AA18895; Fri, 20 Feb 87 13:39:37 PST
Return-path: <kopfj@eunix> 
From: kopfj@eunix (John Kopf) 
Date: Fri, 20 Feb 87 13:39:37 PST 
To: skywalker@eunix 
Message-id: <8702202139.AA18895@eunix.Tymnet> 
Subject: VMdebug 

	Recommendation for Rodney
		John Kopf

	RODNEY provides distributed management of a Skywalker system's
resources, and each instance of RODNEY (i.e., each card) provides some
(limited) set of capabilities.

	One such capability is the determination of the card's resources,
such as memory available;  another is the provision of a software USART.

	A function provided by RODNEY is loading the initial code to be
executed into the card ("bootstrapping").  For want of a better name, I
will refer to this initial code as a "Monitor" -- it may be ISIS-kernel,
IOC-kernel, or Diagnostic-kernel.  Thus RODNEY is -- to some extent --
providing a "virtual machine" to the monitor being loaded.  In the future,
RODNEY must be expanded to provide multiple virtual machines within a
single card (e.g., the "ATC" configuration, where the single processor
must be capable of support of both ISIS and IOC).

	In other instances, RODNEY may be controlled by additional
mechanisms, for activity such as DLL or Debugging.

	One area where debugging is particularly painful is that of
debugging "system" code -- kernels and RODNEY itself.  The pain would
be greatly eased if the tools available for debugging applications
cound be brought to bear, permitting high-level debugging in real-time
code.

	Such a capability could be provided relatively simply by the
development of a VM debugger -- a special Monitor which would control
the MMU and emulate privledged capabilities while executing the code in
slave-mode.  Except for the interception of privledged operations and
MMU violations, such a debugger would otherwise be transparant to normal
debugging.  Hopefully, the MAXsbug code is sufficiently modular that
a small amount of development would permit recasting it into the role
of such a Monitor, perhaps with the addition of some Emulator capability
(assuming that Emulator source-code is available so we can "steal"
appropriate sections of code), and with the (hopefully) standard system
modules (such as MMU support).

	The effort expended in developing VMdebug would be more than
paid back in the effort saved for ongoing development and mantanance
of system code, especially in those cases where the code being developed
must interface with other processors running existing code.

	I strongly recommend that the concept of VMdebug be given further
consideration, because I feel that immediate diversion of effort toward
it's development will reduce the effort required for subsequent development,
and thus reduce the risks associated with RODNEY and Monitor(s) development
and enhancements.

Received: from tymix.Tymnet by B39.Tymnet; Mon, 23 Feb 87 13:16:27 PST
Received: by tymix.Tymnet (5.51/4.7) id AA01371; Mon, 23 Feb 87 13:16:07 PST
Received: by eunix.Tymnet (5.51/4.7) id AA17462; Mon, 23 Feb 87 13:15:31 PST
Received: from EMSTUMS.Ontyme.Tymnet by B39.Tymnet; Mon, 23 Feb 87 21:14:21 UT
Return-path: <NTD.J/BURKE@EMSTUMS.Ontyme.Tymnet> 
From: NTD.J/BURKE@EMSTUMS.Ontyme.Tymnet 
Date: 23 FEB 87 19:58:28 
To: skywalker@eunix 
Message-id: I96912@Ontyme.Tymnet 
Keywords: TUMS 
Subject: Rodney comments 


Betty Jo, here is my set of comments on Rodney.
        - John Burke.
 
 
4. FILE SYSTEM
 
 
    Lock: a timeout value must be spec'd. Violators should be prosecuted.
 
    Appends:
 
      o Somewhere it was said that a caller will do a two-step operation
        to do an append: first, allocate the page, and, second, write.
        I see no reason why a board should have to call the File System
        to reserve a block before the board is ready to write it.
 
      o Three blocks must physically written every time you do a block
        append: home (advance the Free pointer), the old Last, and the new
        block.  You can't get around writing the Home; but the last and
        the new could be written in one operation.  And if the append
        function can accept several extension blocks, then it need write
        the Home only once.
 
        If the append function says "write k blocks starting at last -n",
        then it's nice and straightforward to the calling board, and the
        service can get the above advantages.
 
        Order of write and disk consistency: the following order works
        okay for "Rewrite the last plus n more blocks":
 
         1. Lock
 
         2. Read Home and n-1 acquired blocks, to get links.
 
         2. Write new Home pointing beyond the acquired free blocks.
 
         3. Write the old Last.
 
         4. Write all the acquired blocks, using the remembered links.
 
         5. Unlock.
 
        More robust 3rd and 4th steps avoid ever having the file chain
        point to the free list:
 
         3. Write the last acquired block (break the chain on disk.)
 
         4. Write the old Last [and the other n-1 acquired blocks].
 
        The Disk Clean should recognize that one block or chain might point
        into the Free List, should log this event and just cut that link.
        More than one of these, though, would be pathological.
 
 
    Multi-block functions:
 
      o If read and write calls are set up so you can accept/present
        several blocks at once, then the File System has a potential to
        serve long reads and writes quickly and the callers can move
        large hunks with greater ease.
 
        If the File Server abandons the function in the middle, it must
        indicate where it stops.
 
    All calls to the server should return a Device Unavailable status if
    the server isn't talking.
 
 
    Overlapped Operations: boards must be able to initiate a read or write,
    go off to process and check later, especially for log writes, which are
    a regular event.  The document should express this somehow.
 
 
    Important functionalities which I think we need in a lowest-level
    service:
 
      o Read Errors: Data should be retrievable when there is a read error.
 
      o Writes: a failure status should be returned; esp. Volume Full.
 
      o Server should tell calling boards what its blocksize is (in Open,
        I guess).
 
    We should state function and parameters for these operations:
 
      o File Delete.
 
      o Delink front blocks off of the file.
 
      o Create the FEFS Volume.
 
 
    Utilities:
 
      o Clean: should not discard pages when links conflict; save the bad
        chains under special filenames.
 
      o Copy:
 
 
5. LOADING
 
 
  o Device names for CPU's: I think we must allow that a CPU lower on the
    bus can have a higher number, in the case where the lower CPU does not
    come up till later.
 
    Also, two processors must have different hard names if they have ANY
    difference which means they can't execute each others' code, thus
    different names for different size memory.  And look how much variation
    we can want in memory size.  Using actual memory size in the name
    doesn't sound reasonable to me.  Maybe they should have thumbwheels to
    set names, so the installer can hand-set them as "CPUA", CPUB", etc.
 
    Maybe all boards should have several extra thumbwheels or dip switches
    so we can be well covered for unanticipated stuff like this.
 
  o Crash on bad message?????  We have to tolerate new messages;
    especially in ROM code.  This would give us a method of telling new
    boards from old ones by their response.
 
 
7. IPCF
 
  o Nack: a generic NAK should be defined, so that any processor can
    always reply "I don't know WHAT you're talking about".  The
    processor which receives an unexpected message must not be
    responsible for it.  The sending process must deal with the situation
    (maybe we got restarted out from under him); if it is misbehaving
    we must rely on the judicial process.
 
 
9. BACKUP
 
    State the scope of the Judicial System.  I would think it is
    available to all components which use inter-system processes,
    whenever they discover non-response to any basic process.  Though
    the processor's heart beats, it might be a vegetable.
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Mon, 23 Feb 87 22:10:47 UT
From: NTD.BJC@EMSNTD.Ontyme.Tymnet 
Date: 23 FEB 87 13:40:41 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I97029@Ontyme.Tymnet 
Subject: RODNEY WALKTHROUGH MEETING NOTES 
RODNEYWALKTHROUGHMEETINGNOTES: 

 
	 20 February 1987
 
	 To:Distribution
	 From: BJ
	 Subject: RODNEY WALKTHROUGH MEETING NOTES
 
	 Here are the notes from yesterday's meeting.
 
	 CONTENTS
 
	 1.Procedural issues from meeting:  Agenda, players, etc.
 
	 2.Problem areas/issues for which action items were defined.
 
	 3.Detailed notes from walkthrough on section by section basis.
 
 
	 SECTION 1 - PROCEDURAL
 
 
	 PLAYERS:
 
	 Dan Lasater, Bill Soley, Dick Rawson, Steve Feldman, Dave
	 Smith, Kevin Sinclair, Bill Euske, Michael Stimac, Pat
	 Driscoll, John Kopf, John Coons, Mike Rude, Dave Bliss, John
	 Burke, Jim Willott, Dennis Ernst, BJ
 
	 AGENDA ITEMS:
 
	 I. Rules:
	 - Purpose of meeting is walkthrough for clarification
	 - Lasater is responsible for keeping us on track
	 - Design must be confined to meeting market window
	 - We should identify areas where further completeness in spec
	   is required to initiate other design work.
	 - We will contract an editor to deal with editorial
	   changes in document required prior to marketing
	   distribution.
	 - We will not defer distribution of agreed design document
	   to necessary NTD personnel to wait for this editing.
 
	 II. Scope of work:
 
	 This discussion resulted in 1 action item - to review and
	 rework introduction to document describing who Rodney is and
	 what the intention of the document is.  Basic concept here is
	 that the level of completeness expected of the document is
	 that sufficient for Design Objectives to be defined for all
	 SKYWALKER system components, and for project plans to be
	 developed for each of these system components.  Any
	 shortcomings in the design which preclude these two results
	 are to be identified as action items and assigned.  The
	 concept is further refined to state that this architectural
	 specification need not be detailed enough to permit detailed
	 design of each system component, nor is it necessary for it
	 to include internal design detail for specific RODNEY
	 components.
 
	 Since RODNEY appears to becoming some form of entity in and
	 of itself, we anticipate that detailed design issues for
	 RODNEY will have to be addressed in follow-on RODNEY design
	 documents. Definition of who Rodney is has been assigned
	 to Dick for presentation on Tuesday.
 
	 III. Action items
 
	 (Note, some of these references are rather cryptic.
	 They are listed in the order they were raised, so for
	 further clarification	you might want to reference the
	 detailed discussions notes (in the next section) where you
	 might find explication, or at least, other, related
	 cryptic references.)
 
 
	 1. Proposed rules for Tuesday
	    - Alternative proposals in writing by Monday
	    - Send to reviewers and rodney group mailname on EUNIX
 
	 2.  Eliminate unnecessary non-RODNEY design details from
	 document, make sure they are incorporated as input to the
	 appropriate OTHER SKYWALKER design activities.
 
	 3. Changes in board capabilities and functions - list
	 fault recovery issues (state tables) in RODNEY pathology
	 BY: DETAILED RODNEY DESIGN
	 ==> RODNEY DETAIL DESIGN TEAM
 
	 4. LOCAL vrs VME addressing - Update document with
	 diagrams/description.
	 BY: Tuesday
	 ==> Soley and Kopf
 
	 5. Review model numbering issues with marketing when
	 the Architectural spec is provided to them.
	 BY: Mid march
	 ==> Project manager
 
	 6. Longword boundary concerns. Assign team to do research
	 on issues - gate, MICROTECH linker, Compiler, NADA.
	 BY: Wed 25th
	 ==> Willot (assigned by Core team)
 
	 7. Add state tables to RODNEY detailed design
	 BY: DETAILED RODNEY DESIGN
	 ==> RODNEY DETAIL DESIGN TEAM
 
	 8. Block Diagram of system.
	 BY: Tuesday
	 ==> Soley and Coons
 
	 9. File sharing - consideration of alternatives, and
	 additional detail
	 BY: Tuesday
	 ==> Feldman presents alternatives.
	 BY: after Tuesday
	 ==> Soley deals with detail
 
	 10.  Evaulate console loading - develop section of RODNEY
	 document to address this.
	 BY: Tuesday
	 ==> RODNEY ARCH TEAM (assigned by core team)
 
	 11. ASYNC I/O. Needs study of alternatives and impact of
	 alternatives on various schedules/groups.
	 BY: Thursday 26th
	 ==> Ernst, Kopf, Chu, Jonas (assigned by core team)
 
	 12. SIO interrupts. Additional interrupts desired by
	 XCOM. Issues deferred to Detail FIRMWARE design
	 for ISIS driver functionality. Future phases might include
	 development of enhanced slot features for using interrupts.
	 BY: FIRMWARE DETAIL DESIGN
	 ==> Coons/Lasater
 
	 13. IOC addressing (section 2.2, section 2.4) more
	 information and clarity.
	 BY: Tuesday
	 ==> Kevin
 
	 14. Additional pseudo code and state machine for init IPCF
	 and defibrillate.
	 BY: RODNEY DETAIL DESIGN
	 ==> RODNEY detail design team
 
	 15. Zero memory (sec. 3.4) more detail
	 BY: RODNEY DETAIL DESIGN
	 ==> RODNEY detail design team
 
	 16. When to do system test?
	 BY: RODNEY DETAIL DESIGN
	 ==> RODNEY detail design team
 
	 17. *max cage=?  Defer to marketing after the ME concept
	 review.
	 BY: RODNEY detail design
	 ==> Project manager
 
	 18. Paranoia - data structures
	 BY:RODNEY detail design and ISIS detail design
	 ==> RODNEY detail design team
	 ==> ISIS detail design team
 
	 19. Bus errors - define, document responses (state tables)
	 BY: Rodney and ISIS detail design
	 ==> RODNEY detail design team
	 ==> ISIS detail design team
 
	 20. DISK CONTROLLER, Utilities for file and disk system.
	     Define requriments, evaluate options for development
	     or purchase of controller and file system and utilites.
	 BY: Thursday 26
	 ==> Soley and John Coon's guy (assigned by core team)
 
	 21. Filenames and strings (case/parity) need more definition
	 after evaluation of alternatives. Also, volume integrity
	 issues need to be addressed in more detail.
	 BY: RODNEY detail design
	 ==> Rodney detail design team
 
	 22.  Real time monitor of log.  a) Modify document to include
	 in log section, requriement for this capability.  b) Include
	 detail on this issue in RODNEY detail design.
	 A) BY: Tuesday
	 ==> Kevin
	 B) BY: Rodney detail design
	 ==> RODNEY detail design team
 
	 23. Alternative back up options are NOT to be supported in
	 phase 1. Update document.
	 BY: Tuesday
	 ==> Kevin
 
	 24. Clarify reload roles
	 BY: Tuesday
	 ==> Kevin
 
	 25. List file system primatives after Steve's proposal is
	 presented.
	 BY: RODNEY detail design
	 ==> Soley
 
	 26. Down line loading protocol requriements and definition.
	 BY: Boot and SKYLOADER detail design
	 ==> Dave Comstock and Mike Rude's guy (to be specified by
	     Tuesday) (core team assignment).
 
	 27. Command /response state tables - is IPCF reply message
	 required?
	 ITEM FOR DEBATE ON TUESDAY
 
	 28. Administration of IPCF msg types, Penal codes, and log
	 msg types.
	 BY: ongoing
	 ==> Ken
 
	 29. Description for resetting "conviction"  and pardon
	 definitions.
	 BY: RODNEY detail design
	 ==> Rodney detail design team
 
	 30. Do we run standalone debugger concurrently with other
	 software?
	 NO!
 
	 31. How to handle rewrite at last block of log
	 BY: RODNEY detail design
	 ==> Rodney detail design team
 
	 32. ISIS may add additional fields in log to identify
	 sources of messages. Need description of messages.
	 BY : Tuesday
	 ==> KEVIN
 
	 33.a) Need more elaborate alternatives on port numbers b)
	 need to check out mechanical limits on paddle,  IOC &
	 Breakout design.
	 Need to insure that accounting can be accomodated in design.
	 BY: ?
	 ==>Mike Rebmann to create accounting task force
	 ==> Willot - verify that designs can accomodate reuqriements
 
	 34. Port addressing - last 4 ports defining device address
	 may be a problem.
	 BY: Rodney detail design
	 ==> Detail design team
 
	 35. Problem handling witnesses and accusations when we only
	 have two processors. (not a phase 1 requirement)
	 BY: Later
 
	 36. Need a proposal to lock and unlock SW uarts.
	 BY: Tuesday
	 ==> Rawson
 
	 37. Debug of a virtual machine. This is a non-Rodney issue.
	 BY: Later
	 ==> Kopf
 
	 38. Who is RODNEY and what is the scope and purpose of the
	 document. WE need to write something.
	 BY: Tuesday
	 ==> Rawson
 
 
	 Section IV.- Detailed notes from discussion and walkthrough
	 RODNEY
 
	 (Note - this contains comments and discussion. Issues that
	 appeared to be problems or concerns are flagged with "==>".
	 Some, but not all of these appeared on the lists above.
 
	 Section 1.
 
	 IOC 68010s.  Concerns:  16 bit automacy (bus width of 68010 -
	 16bit).  Test and set instructions reuqired.  local use of
	 Test and set not sufficient with dual porting.  Requires
	 simulation of compare and swap.  Concensus appears to be that
	 the 68010 is a pain to have to deal with in design.
	 Discussion of options for elminiating it by a) redesign of
	 IOC using 68020, or b) purchase of 68020 IOC.	Conclusion
	 appears to be that these are really not options if we are to
	 meet either cost or time targets.
 
	 Distributed processes proposed to minimize single point of
	 failure.
 
	 Serial numbers set in PROM, not set by SW. SW will prohibit
	 write.
 
     ==> ALFRED is new debugger for engine to be used as model for
	 SKYWALKER. ALFRED needs better definition/description in
	 document.
 
     ==> No hot swap of cards in phase 1. We need to insure that
	 we can add hot swap in phase 2. Issue here regarding futures.
	 How much backward compatibility between phase 1 and 2 does
	 marketing want/need?
 
     ==> What about case of adding new boards with different
	 capabilities? (like swapping out an 8 mg board for a 4 meg
	 board). Can the software handle this case?
 
	 Section 2
 
	 Different vendors use difference VME addressing schemes.
	 Types of addresses affect difference address modifiers.
 
	 Motorola and Heuricon boards have some overlap in addressing,
	 Tadpole and Heuricon have no overlap.
 
      ==> The use of the term GLOBAL MEMORY in the document is
	 unclear. All memory is accessible globally but it wont be
	 used in that fashion. Memory is owned by specific processors.
	 Memory on board is used only to describe things on that
	 board. All memory is readable and writeable by processors
	 other than the owner processor.
 
	 Issue of local and VME addressing is still unclear.  Mapping
	 from VME into local RAM is handled by switches on boards.
	 Not all boards permit SW to read switches.  If an oeprator
	 miss-sets vME address we need to be able to figure that out.
 
	 If different cards can't talk to each other because of
	 incompatiblities in space addressing do we have a problem? We
	 can`t mix card types that are incompatible.
 
	 <Here there were two diagrams>
 
	  In this section there is no description of the utility
	 required to put VME addresses into EROM. IOCs. get their
	 address from port number.
 
     ==>  Section 2.2 needs more detail on local addressing.
	  Section 2.6 Extended memory - design documents. Scope of
	  architectural spec is components where interface is needed.
	 ISIS design issues need to be addressed in isis design.
	 Extended memory is not a proboem except for PASCAL and SNA.
 
      ==>  RODNEY  needs definition. RODNEY is a multiprocess
	 definition. IThis document defines an architecture that
	 underlys ISIS. RODNEY is the interface between ISIS and
	 system components. RODNEY manages global resources, ISIS
	 manages each machine.
 
	 Section 2.5 Serial#, Model#, HW SW FW Vrs#.  Only Model # and
	 Serial # actually make up UID.  Some capabilities
	 identification derivable from model #, however this doesn't
	 necesarily include clock and memory size.  Kopf thinks it
	 might be handy to know and tell software about memory, clock
	 or other characteristics.  If HW vers# is stored in EPROM, is
	 that affected by ECOs?  Not on CPs for phase 1 but yes for
	 our own built boards.	IOC has EEPROM.  Can change version #
	 when ECO is applied.
 
     ==> Make the utility that updates EEPROM for ECO revision a part
	 of the board diagnostics so when MFG does the revision, they
	 then run the diagnostics and change the rev.
 
      ==> Marketing has new model numbering scheme.The question is
	  whether or not this has any relevance to our use of model
	 numbers here, and whether they might have some preferences on
	 how these numbers are used to conform to their own schemes.
 
     ==>  2.7 IOC gatearray. Long word boundary issue needs to be
	 addressed.
 
	  Bootstrap - all cards: CP, IOC, Disk Controller.
 
	  GLOB is a data structure held by each processor in its own
	 local memory. All processors have Read access to everyone
	 else GLOB, but can't write into anyone elses glob.
 
    ==>   We need additional description of I/O initialization.
	  "INIT IPCF" also needs process description. Pseudo code
	 probably not necessary.
 
	  How is memory to be managed in cards. Boot allocates what
	 memory it needs, GLOB table and defined structures are all
	 that need to exist after boot phase.
 
    ==>   Global memory management within a card to be addressed as an
	 ISIS design issue.
 
    ==>   Defibrillation code or process description should be added.
	 Code especially desireable.
 
 
    ==>    System configuration test needs to be clarified - for
	 system as a whole and for specific components.
 
    ==>   Add statement that initialization continues even if system
	 fails. component Sysfail turns off lights on all boards.
 
     ==>   Zeroing memory after a dump. On crash, if magic # is bad?,
	  What if disk goes down on power fail?. Do we choose a
	  failure mode when memory might still be good or keeping it
	  desireable.?
 
	  Page 7 GDS - reference to GLOB on arbiter should be
	 deleted.
 
	   Pointers - Device has address, idelly the same address in
	 local as well as VME addressing but vendor cards don't
	 implement it this way. So, pointers become ambiguous, hence
	 clear definition of kind of pointer is needed to know whether
	 it's local or VME address. Implementation must be careful.
	 All VME addresses may be/should by/probably will be via VME
	 subroutine call.
 
    ==>    Add notes about issues of pariticular sensitivity such as
	 global indirections. Paranoia on Data strucutres, Paranoia on
	 bus error handline, paranoia on addressing pointers.
 
     ==>   MAXCAGE - should this be 4 or 8 or 1. ISsue for further
	 study/clarification.
 
	 Section 3 is  part of system initialization. Not really
	 bootstrapping.
 
	 Discuss issue of board death and recovery during expansion of
	 PATHOLOGY section and constrints.
 
    ==> Potential design improvement by including  state
	 tables/diagrams and cases for addressing pathology - in
	 addition to pseudo code.
 
    ==>   Possibly error issues may be "to be adddressed" in detail
	 design.
 
	  Section 4 - Front end file system
 
	 Partition disk into logical volumes.  1 partition for FEFS, 1
	 for ISIS, 1 for ????.	What ISIS thinks is the whole disk is
	 1 logical volume.
 
	  FEFS is for loading, dumping, error log.
 
     ==>  Specify in document that the discussion of the file system
	 is based on a byte stream model.
 
	  ISIS sees a logical volume differenly from this file system.
 
     ==>  Completion issue - Determination of disk and file system
	 distribution ?
	 - Selection of controller?
	 - legal characters in file system?
	 - diagnostic and utility programs - where do they reside?
	 - System convention - what is meaning of character strings?
	 - Define file names and file strings.
	 - How far into file system should redundancy go - disk and
	 file sytem back up?
	 - what is checking for volume integrity taking place?
 
     ==>  Write without pointer advance option may be requried for
	 log. (for periodic writes of event log information)
	 "Frontal truncate " means "circular wrap"?
 
     ==>  Problem area - file system structure, data content,
	 concurent access to logs, etc.
 
	 Comment:   We need a better description of the physical
	 Response: We make no assumptions in the file system about
	 physical media.
 
	  4.2 Loading files onto disk. DL load facility has primative
	 but complete set of file manipulation capability.
 
 
      ==>  Additional discussion required regarding FORCE BOARD. It
	  could meet requirements but tradefoffs required are not
	  desireable, even though alternatives are more expensive.
	  We need further refinement of requriements (HW especially)
 
	  Section 5 - loading.
 
	  Section 4 para2 issue to be addressed in IOC development.
 
	  DL loading algorithm is in ROM
 
	   5:1 Device name - some characteristics in common with UID -
	 e.g. Model #. IOC is associated with physical port number it
	 is connected to. (lowest port # in group of 8)
 
	  Device name used to determine filenmae to look for on disk
	 includes: Filetype, load (for boot), dump, log.
	 Version number has meaning for load files ()=current file, 1
	 and 2 = backup version of code.
 
	  Device name requred (rather than using UID) isnce device
	 name doesn't depend on physlical serial # and can be put into
	 ISIS tymfile.
 
	 Page 22 end - "version " indicates "instance" not to be
	 confused with "version # as in Version, revision.
 
      ==> Loading - need to be able to load from console ports. Define
	 requirements. When ISIS is up it can do loads to update disk
	 while processors are up.
 
      ==> Why is only 1 downline load to be allowed at one time?
	  1 DL load is permitted per IOC not per system?
	  Needs clarification.
 
      ==> Issue of TYMTUNNELS.
 
	  Conclusion seems to be that for first run, ability to load
	  from local disk or from console might permit tymtunnels
	  to work ok even without ability to load across an X.25 link?
 
	  1 copy of ISIS on all CPS instead of multiple copies of
	 ISIS. This is a host sw issue (SKYWALKER UNIX load facility,
	 and CMF).
 
	  Engineering notes that hardware won't support off protection
	  for off card read only references.
 
	  Clarify differences between current boot DLL and this
	 proposal. I.e., add scenarios of load options. E.g., ISIS
	 can be up without node code being up.
 
      ==> Revisit DL loading protocol. Do we need a link level
	 protocol.
 
	 Define command/response sequence (5.5, 5.6 - state machine of
	 possible responses.  State descriptions are important here.
 
	 Access through DDT OR through DDL mechanism are intended to
	 be as symmetrical as possible.
 
	 Section 6 - Configuration
 
     ==>  Configuration detail to be moved to APPENDIX.
 
	  ISIS needs to talk to RODNEY to find out physlica unit (i.e.
	 device names).
 
      ==>  Note in GLOB section that GLOB will be used for
	 configuration.
 
	 Slot - licenses, LU-PU messages, CPH primary, [CPH backup].
	 Configuration changes on the fly allowed through load of new
	 glob configuration file.
 
	 Section 7 - Interprocessor communication
 
	 Device level communication, not slot communication.  Solves
	 concurrency and reace conidtion issues.  Ring structure
	 allowing more than 1 writer into a single ring.  Each
	 processor has only an input ring into which processors write
	 pointers to their rings.  Each entry in ring is 4 bytes long
	 (1 pointer).
 
	 OWNER ID = high order 12 bits of base VME address.
 
	 Must have some polling by reader at least often enough to
	 time out lock.
 
	 When reader checks he needs to know that lock hasn`t been
	 locked and unlocked and locked again since the last check.
	 Does he need to look at a pointer to see if its been update?
 
	 It's important that writer not be interrupted. Can this be
	 done?
 
	  This is an area where we made tradeoffs for performance
	 instead of fault tolerance.
 
	   What about using CAS . Limitation of 68010 need not
	 necessarily be adhered to for operations involving only
	 68020s.
 
	  VME limitations impact us as well. TYMNET IOC design option
	 in gate array, locking bus and local master ' buys you
	 compare and swap but locks local proccessor from access to
	 bus. Simulating CAS by locking bus is trading 1 limitation
	 for another. All boards (ours and others) will hold bus
	 through RMW cycle.
 
	  7.3 correct.
	  7.5 delete.
 
	  Pointer is full VME address (see pseudle code IOC send page
	 35.)
 
      ==>  Define IPCF reply message protocol.
 
      ==>  Message types for IPCF must be administrered.
 
	   Mailbox facility will be used if available.
 
	   Section 8 - I/O communication
 
	   8.1 SIO emulation. IOC would emulate both SIO and some of
	 the functionality in ISIS driver.
 
     ==> Async I/O section needs work.
     ==> Interrpts and ISIS SIO driver require further design work
	 (FIRWARE).
 
	 Sync emulation REQURIED for phase 1.
 
	 8.2 IOCs need to know VME address and physical port number.
	 RODNEY proposes that the backpanel include a way of sending
	 which port group it is back to the IO*c.
 
     ==> Port naming conventions are a problem area.
 
	 <Diagram here>
 
	 Thumb wheel switch on backpanel to set.  Physical port
	 relationships will be defined, this then is used for device
	 name.	Is there a problem with this?
 
	 If breakout panel thumb wheel switches are set wrong, is
	 there any way to tell?
 
	 RODNEY requires the physical port ID for device ID
	 derivation.  Needs help from breakout panel, paddleboard and
	 IOC.
 
     ==> Need more elaboration/alternatives on port numbers including
	 mechanical limits inherent to the IOC/Breakout design (if
	 any). Mapping of IOC names to group of 4 when IOC supports
	 more than 4 might cause a problem if FE's reconnect other
	 4 to another IOC by error.`
 
	  page 37 last paragraph unclear. 4 reads to poll 4 ports (2
	 bits each).
 
	  Section 9 - back-up
 
    ==>  12.2 Clocking relationship of system clock to SUP clock, real
	 time interrups at least once very 16 sec is required for
	 every RODNEY component.  Some of the stuff in this section
	 12.2 should be moved back up to section 9.
 
   ==>	 Should longer locks have crash count?
 
	  9.2 paragraph 1. Conviction is cleared on reset.
 
    ==>   Need more detail of conviction re-setting. I.e., need
	  pardon defintion.
 
    ==>   What about Conviction on 1 accusation in case of only 2
	 processors (no bk-up, only CP and IOC case)
 
	 What of case where there is only 1 witness, for example, CP
	 sees IOC is bad, but no other CP notices or cares or
	 witnesses against it?
 
	  All boards except Plessy can be reset across the VME bus so
	 this is probably what conviction results in - the convict
	 goes back to initialization.
 
	 What to do at initialization? This is decided for prcoessors
	 by ISIS.
 
	 Does convict know why he was reset? Other processors know
	 he's been convicted.
 
     ==>  Need to establish penal codes (reason codes for conviction).
 
	  9.2 ISIS topic for slots to be moved individually - similar
	 mechanisms for slot and card back up.
 
	  On conviction convict code picked up by various back up
	 processors who will load rele4vant slot code.
 
    ==>  Slot backup.  needs forther explication here, as well as more
	 detail in ISIS design.
 
	 Section 10 - Debuggers
 
	 2 debuggers - standalone (like MACSBUG, resident in ROM.
 
	 No separate console board. TTY port on each CP. Any TTY port
	 used to access any standalone or ISIS debugger on any system.
 
	 Implementation through SW uarts.
 
	 Local TTY port woudl have in ROM a soft abort button
	 permitting abort-like functions to occur on another
	 processor.
 
	 Has to scan for data from TTY port
 
	 Assumption here is that standalone debuggers can't work while
	 somthing else is going on.
	 Not a necesary assumption- MACs bug was modified for TTY to
	 permit concurrent functioning of MACSBUG and other things.
	 Can we run standalone debug with other processes? We should
	 evaluate this, it might be nice to have.
 
     ==> <Aside...  at end of day we decided that we should not go
	 looking for trouble, and that we would not pursue this.  We
	 did decide that one could access the standalone debugger on
	 one processor card through the TTY port on another card
	 without disabling the current functioning of the accessed
	 card.>
 
	 10.1 Note: TTY emulation (like engine) is not required
	 because ISIS and diagnositcs are new.
 
 
     ==>  How to handle SW UART lock-out option for notification of
	 lock owner (on TTY port into standalone debugger) that you
	 are taking lock away.
 
	  ISIS debugger is the second debug facility available. Most
	 design of this is planned for the ISIS design activity.
 
	  Source level debugger input/output. Issues of definition
	  regarding slot debugging. Interpreter and Translator issues.
 
	  John would like a future ability to debug a virtual machine.
 
	  Section 11.
 
    ==>   How to handle rewirte of last block in logger requires
	 clarification.
 
	 Log entry:
 
	 time/ UID / code (prod type) / code (event type)/severity
	 code / text .
 
	 Option for part of the message field to be reserved for and
	 used by ISIS for some further identification.
 
	 Section 11.1 Log file handling.
 
	 All log messages are ID'd by UID (paragraph 2).
 
	 Concept here is that the primary purpose of the log is HW.
	 It is NOT a replication of msgs to SUPS etc.
 
     ==> Issue of real time access to HW log - display of last page,
	 last "n" messages etc.
 
	 WHO is RODNEY? Is he a series of PROM routines? (ROMNEY).
     ==> Need RODNEY defintion.
 
	 Message types for logger need administration.
 
	 Section 12 Operating considerations
 
	  Medium performance lock.
 
     ==>  Lock use identification and evaluation needed.
	  Suggestion is made that when programming locks
	  state diagrams be included in design (another paranoia
	  issue). Locks are RODNEY System functions that live in ROM
	 and can be called by ISIS's and node codes.
 
	  (Locks, clocks, heartbeats, interprocess communication are
	  ISIS tools.
 
     ==> Future. For inhouse 68030 - desirable to be able to lock up
	 dual ported memory access to VME bus to allow safe use of
	 CAS.
 
	  Section 13 - Mis. Considerations
 
	  13.2 Dispatcher - single vrs multiple dispatcher. Does
	 affect/impact BACK-UP sceme, but should be addresed from
	 detail design perspective by ISIS design.
 
	   13.4 ISIS library upgrade.
 
	 If we don't rewrite isis libs then we lose abiliyt for
	 hierarchical recovery.  (i.e.	we can`t have option to back
	 up by going to back up code (reload) instead of switch to
	 backup processor.  But..  This is a new and better feature
	 not a replication of existing back up capability, so,...  New
	 library packages are nice but not essential and might be
	 considered to be optional development.  (There is some issue
	 of compatiblity with all interfaces if we do do new
	 libraries).
 
	 13.6 Potential problem with 2 card systems as we can have
	 cofessions but not accusations.
 
	 Interchassis communciation.
 
    ==>  Issue here is what form of backward compatiblity to support
	 interchassis communication developed in phase 2 is required?
	 Is it ok, for example, to require that new phase 2 CPU cards
	 be installed to permit interchassis comm? It is prob'ly NOT
	 ok to require that the chassis be replaced, or that all cards
	 become obsolete when interchassis comm is required.
 
	 We need to take this up with marketing during spec review.
Received: from tymix.Tymnet by B39.Tymnet; Wed, 25 Feb 87 19:58:52 PST
Received: by tymix.Tymnet (5.51/4.7) id AA10781; Wed, 25 Feb 87 18:41:02 PST
Received: by eunix.Tymnet (5.51/4.7) id AA06034; Wed, 25 Feb 87 14:41:38 PST
Return-path: <kopfj@eunix> 
From: kopfj@eunix (John Kopf) 
Date: Wed, 25 Feb 87 14:41:38 PST 
To: skywalker@eunix 
Message-id: <8702252241.AA06034@eunix.Tymnet> 
Subject: "Async Study for Skywalker" 

this file may be found in /kopfj/sky_doc/async.


			Async Study for Skywalker


			    February 23, 1987
				  by:
				Paul Chu,
				Dennis Jonas,
				John Kopf,
				Kevin Sinclair

Objective:  To determine the most expedient method to provide conventional
	(non-SIO) ASYNC support to slots running in the Skywalker Environment,
	without requiring modifications to those slots regarding I/O driver
	protocols supported.

Constraints imposed:  The facility MUST be available by Feb '88;  it MUST
	use the IOC and the SCC-chip (i.e., no new card development).

Acceptible options:  Requiring new code be developed, for both IOC and ISIS,
	is expected.

Results:

	ISIS will provide the necessary capabilities to translate between
"normal" and new I/O driver protocols.

	A desirable goal of any new Async-I/O Driver protocol is to provide
a complete set of functionality (both normal and SIO) in such a way that, in
the future, products could migrate toward using the new protocol without a need
for retaining driver-dependent protocols -- THIS IS NOT, HOWEVER, A FORMAL
SKYWALKER GOAL;  it is merely highly desirable from the view of application
support.

	The SCC's inability to support independent input- and output-clock
rates means that split baud-rates can NOT be supported (e.g., 75/1200).


Assumptions and Reasoning leading to these Results:

	It is assumed that the the IOC will be based upon the existing
TSI code;  in particular, drivers for SIO/ASYNC emulation already are
defined and exist.  However, these do not provide either the auto-baud
detect or the output-delay capabilities required of "normal" Async.  It
is not clear if they support Break-generation either.  These are all
capabilities which are required of normal Async, and thus necessitat
that an alternative set of drivers be developed for IOC.  However, TSI
-- and thus hopefully IOC -- support alternative drivers.  The particular
problems associated with auto-baud detection in association with a USART
have been successfully addressed in the ATC product, and thus many of those
algorithms can be readily incorporated into new normal-Async drivers.

	A more complex problem is associated with the IOC's communication
with ISIS and the slot.  Neither the current normal nor SIO Async
implementation is general enough for the capabilities required;  each
provides some of the requirements, but these do not overlap enough.

	Normal Async is based upon input and output rings, but incorporate
both bit-arrays, and incrementable counters -- these are not safe in a
multi-processor environment, since they require "atomic" (read-modify-write)
operations.

	SIO/Async suffers from CCW-oriented output, and a strange form
of input (where characters are input to half-words in an 8-hw buffer, but
the buffer is timeout-driven).  It also lacks the special features required.

	The most reasonable implementation for a new Async is for each
port to communicate to IOC via a ring of 16-bit "characters" for each
direction of communications;  each ring would have a 16-bit fill and
empty cursor, and a data-area whose length is defined by the ISIS-end.
Eack 16-bit data-field would contain two bytes of information; the first,
if non-zero, would specify a control-function, the second the control-value,
or the data if the first was zero (this is similar to SIO-input, where
<0,0> is data-zero, but <1,0> indicates Break).  This communication mechanism
is "safe", in that only one processor writes each field, and the other only
reads that field;  it does suffer in that bit-wise polling is not available,
and instead each such ring must be polled by the receiver in turn, to determine
if there is anything to do.

	It does have the advantage that such polling could be performed by
the ISIS Foreground (in place of the normal Async polling), and then data
could be transfered between the ISIS/IOC rings and the conventional Async
rings of the slot for normal Async Emulation (such activity between ISIS
Foreground and the Slot IS safe, as long as only a single processor is
involved).

	It also has the advantage that a slot could use the mechanism
directly, in future implementations (retrofitting this functionality to
Engine-ISIS would permit full portability of the capability, without the
need for seperate driver-protocols for Async and SIO within products such
as CONSAT and TYMCOM).

	With respect to split baud-rate support, the committee concluded
that while such support is necessary, it is precluded using the SCC chip.
Alternatives require a new card:  one possibility is the use of the "cirrus"
chip proposal;  a second is to use SCC, with one channel for each direction
of I/O;  a third is to develop a card with an Engine, executing the existing
microcode, and performing strictly as a USART!


Async Control Information:

	The existing implementation of ISIS Async currently supports the
following functions:

    o	Generate delay (units of 1/30th second)...leave output in marking-
	state for that long).
    o	Set input baud-rate;
    o	Set output baud-rate:
	(these both present a code which currently selects:
	75 Baud Ascii (i.e., 8-bit);
	10 CPS Ascii;
	15 CPS Ascii;
	30 CPS Ascii;
	40 CPS Ascii;
	60 CPS Ascii;
	120 CPS Ascii;
	240 CPS Ascii;
	480 CPS Ascii;
	960 CPS Ascii;
	10 CPS Ascii (at 300 baud...generate delay between characters);
	10 CPS Ascii (at 1200 baud...generate delay between characters);
	14.7 CPS (2741..7-bit);
	30 CPS (3767..7-bit);
	50 Baud (Baudot..5-bit);
	75 Baud (Baudot..5-bit).
    o	Enter TID mode.
    o	enter data-mtde.
    o	enable.
    o	RS up.
    o	RS down.
    o	Generate 0.083 seconds of output Break-state.
    o	TID mode detected char at assumed baud-rate specified (char observed
	at that baud-rate follows).
    o	Break-begin detected.
    o	Break-end detected.

	Desirable enhancements:
    o	Generate N units of output Break.
    o	Detected N units of input Break.
    o	Echo-control.
    o	Parity-control.

	In addition, it will be desirable to define the Async environment
such that the full set of RS232 signals can be individually manipulated.
The individual pin can be accessible without sharing the pin among different
applications like the existing implementation.  This is especially useful
in the applications where software flow-control is not possible and hardware
handshaking is required to secure the traffic without losing data.

	It would also be desirable if the new Driver-protocol imposed no
hardware-dependent constraints, such as a need to explicitly write control-
registers;  such constraints already are proving awkward in the migration
from SIO to SCC-chips.



Received: from tymix.Tymnet by B39.Tymnet; Fri, 27 Feb 87 17:35:27 PST
Received: by tymix.Tymnet (5.51/4.7) id AA23714; Fri, 27 Feb 87 17:34:49 PST
Received: by eunix.Tymnet (5.51/4.7) id AA09535; Fri, 27 Feb 87 17:34:33 PST
Return-path: <kopfj@eunix> 
From: kopfj@eunix (John Kopf) 
Date: Fri, 27 Feb 87 17:34:33 PST 
To: skywalker@eunix, tsi@eunix 
Message-id: <8702280134.AA09535@eunix.Tymnet> 
Subject: Potentail problem with current IOC implementation 


I have encountered a problem-areain the design of the IOC.  It is something
that, for now, we can live with, but it reduces robustness, and should thus
be taken into account reasonably soon.

The IOC firmware currently has a number of different "drivers" (e.g.,
Sync-emulation, SIO/Async-emulation, etc.).  The selection of driver is
predicated upon the "protocol" value specified in an Engine-originated
CCW.  The problem is that the dirver is selected at the time that the
CCW is interpreted...it is possible, for example, that an error in a
HDLC-intended CCW might instead specify "bisync", and thus cause the
Sync-emulation driver to be installed -- if ISIS does not have Sync-
support, none of the expected data-structures will be in the Engine's
memory, with probable catastropic results.

The solution would be to specify protocol(s) as part of the initial
"connect" operation (perhaps by ISIS passing the SVC-number), and not
allowing certain protocol-changes.

Since TSI is not expected to be a debugging environment (instead running
only released code) this is unlikely to be a problem there initially.
However, the possibility of an alternate Async (or other) protocols would
certainly necessitate this capability.

Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 3 Mar 87 16:07:51 UT
From: NTD.BJC@EMSNTD.Ontyme.Tymnet 
Date: 03 MAR 87 07:55:22 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.BJC@Ontyme.Tymnet, NTD.K/HOLCOMB@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.P/DRISCOLL@Ontyme.Tymnet, NTD.M/STIMAC@Ontyme.Tymnet,
	NTD.D/ALCORN@Ontyme.Tymnet, NTD.D/BLISS@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.J/COONS@Ontyme.Tymnet, NTD.J/KOPF@Ontyme.Tymnet,
	NTD.B/SOLEY@Ontyme.Tymnet, NTD.D/RAWSON@Ontyme.Tymnet,
	NETS.M/REBMANN@Ontyme.Tymnet, NTD.J/SARGENT@Ontyme.Tymnet 
Message-id: I00520@Ontyme.Tymnet 
Subject: SKYWALKER MEETING MINUTES 
SKYWALKERprojectmeetingminutes.: 

   2 March    1987         skymin.302
 
  To: Distribution
  From: BJ
  Subject: SKYWALKER project meeting minutes.
 
  Here are the minutes from the status review meeting
  of 26 February.
 
  Agenda:
 
  I.  Old items
  II. New items
  III.Other new items
 
 
  I. OLD ITEMS
 
  Old items were reviewed, including action items from previous
  meeting.
 
  1b. Project schedule updates for firmware and diagnostics
  will be available next week (diagnostics) and after final
  Rodney review (firmware).
 
  1c. Engine/68k debugger proposal will be provided on 5 March,
  incorporated into the Translator GED.
 
  1e. Mechanical engineering  concept review was held. Marketing
  has expressed renewed interest in Pegasus, and concern regarding
  cabling plan. Dave requested that if there is a serious problem
  with the ME concept, Marketing should notify Engineering at once.
  Mike Rebmann took action item to get Marketing input back to
  NTD.
 
  <On Friday 27 Feb, a conference call with Marketing/Engineering
  took place.  Marketing agreed to deliver to NTD by Tuesday,
  February 3rd, a written statement of marketing interests regarding
  packaging and cabling>
 
  1f.  IOC/HW functional specification review took place
  Agreement on principle achieved.  Document will be updated
  for new distribution 3 march. Goal to approve for Marketing
  distribution 5 March.
 
  2a. Update to draft project plan (first submitted for
  core team review in early January) is distributed for
  review. Note that there have been a number of changes to
  this document. Last chance review comments due 2 March.
 
  2b.  SCCS class will be prepared by Michael's staff. Michael
  will send out class description, 1 rep from each of the
  development groups should attend class. Description of class
  to be distributed 2 March, class has been scheduled for
  Friday, 13 March, Bldg. F conference room.
 
  2f.  Trade shows.  QA needs to be involved.  Marketing has asked
  us to build 12 machines over and above our own use .  QA has
  asked for a SKYWALKER machine from pilot as well.  Ken is to
  request that Rebmann specify what level of functionality is
  required for the trade shows.
  Action to Mike Rebmann to explore this issue with Marketing.
 
  RODNEY issues are action items defined in the
  meeting that need resources allocated to them.
 
  3.  ASYNC I/O We need resource allocation for ASYNC I/O
  study.  This study includes evaluation of one or more
  strategies for handling autobaud, 19.2, and SIO async
  emulation in SKYWALKER, and evaluation of implications of
  strategies on CONSAT, SIO Emulation, and Firmware
  development.
  ASYNC I/O study report distributed.
 
  4.  DISK Controller We need to expeditiously make a decision
  on a controller for SKYWALKER because the controller
  characteristics impact design and hence schedule issues.
 
  Controller/disk/file system study report distributed.
 
  5.  Utilities for File and disk system We need resource
  allocation to define utilities required to support file and
  disk system for RODNEY.
 
  ==> Same as 4 above.
 
  6.  Down line load protocol We need resource allocation to
  evaluate and define a DLL protocol for SKYWALKER.
 
  This was incorporated into the Rodney design effort.
 
  7.  PORT numbers/accounting We need Mike Rebmann to put
  together a taskforce to define accounting requirements for
  SKYWALKER.
 
  This requires further definition of what we need from Mike.
 
  9.  We need to address MAXCAGE and Model# use with Marketing
  During review process we can discuss model number use with
  Marketing.
 
  10. Availability/requirements for test beds need to be defined
  for Engineering.
 
  Project schedules for each group should identify what test
  facilities they require and WHEN they are required such that
  we can share resources.
 
  ==> Everyone. Include this issue in project schedule updates.
 
  11. Resources have been allocated to SKYWALKER design teams.
 
  Remember, project schedules due monday 2 march.
 
  Next week's meeting is scheduled for Thursday, 5 March, 1:30 pm.
Received: from B36.Tymnet by B39.Tymnet; Tue, 10 Mar 87 12:22:06 PST
Received: from tymix.Tymnet by B36.Tymnet; Tue, 10 Mar 87 12:21:30 PST
Received: by tymix.Tymnet (5.51/4.7) id AA17235; Tue, 10 Mar 87 12:10:19 PST
Received: by eunix.Tymnet (5.51/4.7) id AA26020; Tue, 10 Mar 87 12:09:52 PST
Return-path: <kopfj@eunix> 
From: kopfj@eunix (John Kopf) 
Date: Tue, 10 Mar 87 12:09:52 PST 
To: skywalker@eunix 
Message-id: <8703102009.AA26020@eunix.Tymnet> 
Subject: Unaddressed problem area 

	Unaddressed Problem within Skywalker
		  John Kopf
		March 10, 1987

A potential problem area within Skywalker has to do with a processor
(and its memory) "disappearing" from the VME.  While attention has been
paid to detecting this condition (and eventually removing that processor
from access), no corresponding attention has been paid to the recovery
required from the process which made the (legal, but unsuccessful)
access in the first place.

Consider the following simple scenario:  processor A passes a message-
pointer to processor B (using IPCF), but fails before processor B can
make use of this pointer.  When B eventually gets around to processing
that pointer (remember, it might have a queue of such pointers to process
before it gets to the critical one, so that some delay is inevitable),
it encounters the buss-error.  However, it has likely climbed deep into
its stack before making te attempt.

How does processor B recover from this situation?  There are likely to
be a number of objects that need "clean-up" in addition to the stack
(e.g., locks).  Processor B probably should avoid crashing the process
which made the reference, if possible, but this is only possible if there
exists some sort of "audit-trail" to permit recovery.  Is it the processes'
responsibility to perform this recovery?  Will there be some way to even
inform the process that the condition has occured?

This is a significant issue, since there will be a number of arbitrary
processes which will reference VME (e.g., All IOC drivers, Dispatcher,
other I/O such as disk, and even products such as split Node-code and
Supervisor).

I believe that this is a significant issue, which should be discussed.

Received: from B36.Tymnet by B39.Tymnet; Wed, 11 Mar 87 10:17:11 PST
Received: from tymix.Tymnet by B36.Tymnet; Wed, 11 Mar 87 10:16:40 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24587; Wed, 11 Mar 87 10:16:06 PST
Received: by eunix.Tymnet (5.51/4.7) id AA01676; Wed, 11 Mar 87 10:15:38 PST
Return-path: <jwillott@eunix> 
From: jwillott@eunix (Jim Willott) 
Date: Wed, 11 Mar 87 10:15:38 PST 
To: skywalker@eunix 
Message-id: <8703111815.AA01676@eunix.Tymnet> 
Subject: Observation on IOC Addressing 









                              MEMORANDUM
                                         TYMNET INC.(NTD)
                                         DATE: March 11, 1987

       TO: Skywalker Design Teams

     FROM: Jim Willott       Ext. 6023 Bldg. H
                                  ONTYME: NTD.J/WILLOTT
                                          BUB.J/WILLOTT
                                  DEPT: CPU -- VLSI Group

     SUBJECT: Observation on IOC Addressing
     -----------------------------------------------------------------

     This is the formalization and retransmition  of  an  earlier
     note which seems to have become lost.  In section 2.2 of the
     current RODNEY document (there  does  not  seem  to  be  any
     revision  assigned  to  this  document) there is a statement
     about "an area suitable for a common offset for all  boards,
     e.g.  (base)+2000x".  I read this as an example.  In section
     3 of the same document the references to offset 2000 seem to
     be   stating  this  as  not  an  example  but  as  a  design
     requirement.  However, Table 1 --  IOC  Memory  Map  clearly
     shows  that  there  is no usable memory (RAM or ROM) at this
     address.  I recommend that RODNEY specify clearly the offset
     for  this pointer / table and that it be consistent with the
     current IOC design and as many of the current set of CPUs as
     possible.































From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 12 Mar 87 16:03:15 PST 
To: kopfj@eunix (John Kopf) 
Cc: skywalker@eunix 
Subject: Re: Unaddressed problem area 
In-reply-to: <8703102009.AA26020@eunix.Tymnet> of Tue, 10 Mar 87 12:09:52 PST

The point John brings out about processes needing to be able to recover
from the failures of other processors, particularly from bus errors is
indeed very important.  It was discussed in detail in by the architectural
design team, but it appears to have escaped being clearly documented.  Here
is a summary of what we feel the policy should be on handling bus errors:

Bus errors must be expected to occur from time to time due to heavy bus
utilization resulting in a local bus timeout while waiting for arbitration
of the VME bus.  This is not necessarily an error condition.  The transfer
should be retried some number of times, possibly with some delay in
between.  If after several attempts (about 3 I should think) without
success, the event should be logged and an accusation should be issued for
the processor being accessed.  If the transfer succeeds after some number
of tries, the event should also be logged as a statistic indicating
excessive, possibly pathalogic, bus utilitzaion.

When Rodney makes VME transfers, it will take over the bus error trap
before the transfer and restore it following the transfer so it can
implement the above logic.  It will be up to ISIS (or whatever) to
implement a similar strategy that is compatible with its management of the
MMU and user mode processes.  In particular, the method for ISIS (or
whatever) to notify the user mode process that a non-recoverable bus error
has occured is not obvious - perhaps a software interrupt.  The user code
could make use of the setjmp() and longjmp() routines to recover from
errors which occur deep inside nested code.

-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 12 Mar 87 16:30:33 PST 
To: jwillott@eunix (Jim Willott) 
Cc: skywalker@eunix 
Subject: Re: Observation on IOC Addressing 
In-reply-to: <8703111815.AA01676@eunix.Tymnet> of Wed, 11 Mar 87 10:15:38 PST

I am confused by your statement about "no usable memory ( RAM or ROM )" at
base + 2000x.  The table 4. IOC memory map of the March 6 IOC Hardware
Functional Specification shows ROM (presumably EPROM) at base + 0000x
extending through base + 1FFFFx.  Rodney addresses the possibility of the
location being in ROM as opposed to RAM by allowing one level of
indirection.

All other processors being considered have RAM at base + 0 for at least
one magabyte.  We chose the 2000x offset to allow room for interrupt
vectors and such.

Please let us know if we need to change the offset, it was not included
as an example.  Thanks -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 1 Apr 87 11:56:25 PST
Received: by tymix.Tymnet (5.51/4.7) id AA05909; Wed, 1 Apr 87 11:55:53 PST
Received: by eunix.Tymnet (5.51/4.7) id AA11382; Wed, 1 Apr 87 11:55:46 PST
Return-path: <alanlo@eunix> 
From: alanlo@eunix (Alan Lo) 
Date: Wed, 1 Apr 87 11:55:46 PST 
To: alanlo@eunix, dlasater@eunix, mikemo@eunix, wrs@eunix 
Message-id: <8704011955.AA11382@eunix.Tymnet> 
Subject: "MEMO TO> Dan Lasater Mike Mo Bill"... 


				MEMO


TO>	Dan Lasater
	Mike Mo
	Bill Soley

FROM>	Alan Lo

DATE>	April 1, 1987

SUBJ>	Arbitration Test Plan For CPU Board Evaluation

======================================================================

After talking with the various people involved in the
68K board evaluation, I came up with the following
list of concern areas for arbitration testing.  They 
are in decending order of importance.

 1.  Verify read-modify-write protocol for the
     Plessy board (i.e. read modify cycle holds
     adress strobe).

 2.  Check all cases of the CAS instruction
        CAS.B
        CAS.W even/odd byte
        CAS.L even/odd byte, even/odd word
        CAS2

 3.  Show VME bus access to RAM can lock out
     local processor RAM access.

 4.  Test VME bus arbitration logic where two
     processors request for the bus simultaneously.

 5.  Verify VME bus capture capability (i.e.
     a processor can hold the VME bus indefinitely).

Please review the above list and give me feedback before
Friday, 4/3/87.  Testing will be done in their order of 
importance.

From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 1 Apr 87 23:46:30 PST 
To: alanlo@eunix (Alan Lo) 
Cc: alanlo@eunix, dlasater@eunix, mikemo@eunix 
Subject: Re: "MEMO TO> Dan Lasater Mike Mo Bill"... 
In-reply-to: <8704011955.AA11382@eunix.Tymnet> of Wed, 1 Apr 87 11:55:46 PST

Your test plan looks complete and reasonable.  #5 might be worded more
accurately as:
  5.  Document effect of holding the VME bus through multiple data
      transfer cycles (i.e. can local processor sneak in between cycles?).

This may be difficult since the Plessey board is incapable of holdin the
bus in this way.  You could test using the Huerikon boards, however, as
bus master and the Plessey as slave.

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 8 Apr 87 13:52:00 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19406; Wed, 8 Apr 87 13:50:41 PDT
Received: by eunix.Tymnet (5.51/4.7) id AA02239; Wed, 8 Apr 87 13:50:28 PDT
Return-path: <kopfj@eunix> 
From: kopfj@eunix (John Kopf) 
Date: Wed, 8 Apr 87 13:50:28 PDT 
To: skywalker@eunix 
Message-id: <8704082050.AA02239@eunix.Tymnet> 
Subject: Notes on Rodney GED 

	Notes on RODNEY External Specification (April 6, 1987)
		John Kopf
		April 8, 1987

Section 1.2:  Since the Skywalker model is so different from the
conventional model of Tymnet (where "application" refers to the set
of processes (interface) in a slot), while using the same words, it
might be appropriate to expand upon this in an appendix, so that all
readers can understand the distinctions between tho old and new meanings
of these words.  In particular, a "slot" is in reality a "client" of
ISIS, and ISIS itself is the "application".  (See also p.16, where
"patron" is defined to get around this usage).  Perhaps an ISO model of
current ISIS and slot could be compared with an ISO model of a slot
within Skywalker, with real benefit.

Also, it should be stressed that system integrity is a joint effort
of all the applications, but not of the slots;  a fault caused by a
slot is actually the responsibility of the controlling application,
since it is the responsibiility of these applications (e.g., ISIS) to
validate client requests before commiting them to the system, (or at
least commiting them with additional restriction information -- e.g.
Addressing-limits on Channel-programs executed by the IOC).  A slot
CANNOT cause a fault unless permitted by the controller, since a slot
does NOT have direct access to ANY system functions.

Section 1.3.4:  Since I can find no reference to the ISIS Dispatcher
within Section 1.3, I recommend it be listed here (unless this is what
is meant by "interprocess Communications Facility -- and if it is, I
doubt that anyone else will identify it as such either).

Section 1.4.2:  There appears to be a discrepency between Rodney and
IOC with respect to "bootstrap" loading from IOC ports...this must be
resolved (I'd recommend a "Boot-driver" in IOC, and Rodney/Skywalker
invoke these drivers when necessary).

Section 2.1:  I am opposed to passing arguments -- upon traps -- via
registers and in-line parameters, having long experience with such
usage in ISIS;  I recommend instead passing a pointer, on the stack,
to a descriptor which specifies the request.  Exceptions should, by
definition, occur infrequently, and thus the minor optimization engendered
by the proposed usage should be insignificant, while potentially causing
problems in the future.  By the same rights, I feel all registers should
be preserved across a Trap;  the specific descriptor can reserve space
for values returned.

Section 2.1.2:  Figure 6 is awkward, especially as it differs from the
usage in 2.1.3 for expounding source code.  I recommend eliminating
"Figure 6" entirely, and retain the 2.1.3 convention for displaying
code.

Section 2.3.1:  There needs to be a capability akin to the ISIS Restart-
SVC so a Patron can tell Rodney where to "trap" to in the event of such
a condition (and possibly others).  Processes such as the Dispatcher
can recover from the condition in the event of such a trap, whereas
"crashing" the dispatcher can leave all sorts of problems dangling.
Also, there is no mention of recovery when Rodney, acting on behalf of
a Patron, engenders such a trap.  There needs to be a mechanism defined
so RODNEY can clean up after itself in this event, and to tell the
Patron if he did it.

Section 2.3.3:  I am still concerned about a desirable future usage
where both ISIS and IOC share the same processor (the "ATC" case).
This would be very difficult if each thought it had the MMU, whereas
arbitration by Rodney would greatly simplify future extensions.  Since
the initialization of the various MMU partitions occurs infrequently
(compared with partition switching), perhaps a compromise could be
worked out wherein Rodney does the initialization, and the patron can
do the switching (of those partitions he has access to).  Such usage
would also permit Rodney to initially map itself before transfering
control to the 
patron.

GENERAL COMPLAINTS:

THERE IS NO INDEX!!!  This is unacceptable on a document of this size.
A desirable index would further identify and distinguish Definitions
from References.

There exist a number of declarations which are apparently based upon
implicit assumptions -- any such assumptions should be explicitly declared
along with enforcement criteria.  An example is the universal usage
of "space"-ASCII (high-order bit off).  Is this really the standard
usage?  If so, whose responsibility is it to convert "Mark"-ASCII to
the standard form (such as might arrive from the network or terminals)?
This is certainly critical for file-names, and a universal convention
would certainly eliminate the need to manipulate characters anywhere
other than at the origination.  (Also, if space-ASCII is the convention
for characters, is there any system meaning for the alternate mark_ASCII
(e.g., special usage such as EOF, EOT, etc)?
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Thu, 7 May 87 14:32:36 UT
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 07 MAY 87 07:19:51 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I31037@Ontyme.Tymnet 
Subject: Skywalker Status Review Minutes 4/30/87 
SkywalkerStatusReviewMinutes4/30/87: 

 
 
 
 
 
 
 
                                  MCDONNELL DOUGLAS
                              INFORMATION SYSTEMS GROUP
                               INTEROFFICE MEMORANDUM
 
          Network Systems Company            Network Technology Development
          Tymnet                                           No:SKY870507.001
                                                     Date:May 7, 1987  7:16
 
          SUBJECT>       Skywalker Status Review Minutes 4/30/87
 
          TO>            Dick Rawson              BJC
                         Dan Lasater              Bill Euske
                         Gazel Tan                John Kopf
                         Bill Soley               Mike Rude
                         Pat Driscoll             Michael Stimac
                         Dave Alcorn              Dave Bliss
                         John Coons
 
          COPIES>        Al Fenn
 
          FROM>          Ken Holcomb
          -----------------------------------------------------------------
 
          PLEASE NOTE:  THE REGULAR WEEKLY STATUS MEETING WAS SCHEDULED FOR
          FRIDAY, MAY 9 AT 1:30, INSTEAD OF THURSDAY.
 
          The FASB sign off procedure was discussed. Dave Bliss stated that
          hardware feasibility has not been established yet.
 
          CSC has asked whether Engineering would be willing to provide PLC
          release materials to accelerate production.  Dave Bliss is to get
          back to Ken.
 
          A Skywalker  performance meeting  was scheduled with Chung Lam on
          5/8/87 from 9:30 to 11:30.
 
          A meeting with Mary Arnett was scheduled for 5/4/87 to discuss QA
          test plans and coordination issues with NTD.
 
          Skywalker cost  estimates are  to be  received from Dave Bliss on
          5/4/87 ( BJ, I still need a copy).
 
          The respective team leaders will be requested to  keep a  list of
          coordination topics and to meet weekly to discuss them.
 
          It  was   agreed  that   for  the  software  engineering  effort,
          intermediate results produced by  the  individual  teams  will be
          distributed  to  improve  communication  and  reduce  the  review
          effort.
 
 
 
                                          1

 
 
 
 
          The following distribution  for  project  released  documents was
          agreed upon:
 
               Dan Lasater         16 copies
               Dave Bliss           2 copies
               John Coons           9 copies
               Gazel Tan            3 copies
               Pat Driscoll         8 copies
               Michael Stimac       5 copies
 
          This is in addition to the Core Team distribution.
 
          All documentation for Skywalker will be placed on MINIMAC. Ken to
          set up a directory with the appropriate protection.
 
          The Indian Hills coding convention was adopted for the C programs
          with one exception as to the position of the {.
 
          It was agreed that all C code must pass LINT before compilation.
 
          SCCS  was  adopted  and  proposed  to  be  included in a division
          programing practices for C.
 
          Ken is to put together a release procedure for Skywalker.
 
          All future  change  control  documents  will  require  a revision
          history.
 
          There was  a brief  discussion on the Motorola board.  Due to the
          sensitivity of the issue, it was  decided  to  raise  it  at Al's
          staff  meeting.  At  the  staff  meeting  it was decided that the
          board's design was inferior to the existing selected board.
 
          There was a discussion on VME monitors. Engineering is evaluating
          some and it was decided to table this issues until the evaluation
          is complete.
 
          Mike Rude is to write a PIR to provide for Standard Documentation
          macros.
 
          There was  a request  to Dave Bliss to provide some documentation
          for the current selected board (Plessy).
 
          There is an immediate need  to  define  the  format  for  the DDL
          record (  X.409?).   Michael Stimac, Dan Lasater and Pat Driscoll
          are to get together and resolve.
 
 
 
 
 
 
 
                                          2

 
 
 
 
          There is a requirement for a statement to the effect  of reducing
          or  eliminating  the  use  of  vendor  specific  features  in the
          software implementation.  When use of these features is required,
          those features  should be  contained in modules which are clearly
          identified as non-portable. John Coons, Dan Lasater, Pat Driscoll
          and Bill Euske to provide an update on this item at next meeting.
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
                                          3
7:e	