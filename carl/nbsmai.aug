11-Aug-80 00:23:40-EST,7135;000000000000
Mail from Host MIT-AI at 0023-EST on  11 Aug 1980
Date: 11 AUG 1980 0023-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

Date: 10 August 1980 00:39-EDT
From: Gail Zacharias <GZ at MIT-MC>
   This is a pretty long message with comments on the proposal found in
AI:USERS2;HRC AMBER, intermixed with text from that file (spoiler
warning, spoiler warning...)
  --------------------------------
           .....    The cardinal rule for this first pass is  simpli-
    city:  with an appropriate set of tools, it should be possible to
    write an intepretter for this language is fairly little time.
  While this language would indeed be simple to implement, I think it is more
important that the adventures be simple to write.  After all, once an
interpreter is written, it can be transported to other sites.  I bet all you
need is no more than 4 interpreters to cover the ARPANET... Say Lisp, C,
Fortran and maybe one more.  Add Cobol and Basic and you've got the rest of
the world covered as well.  So it's not like you are going to have hundreds of
people writing their own interpreters.  So the important thing is the
adventure writing, where you indeed hope to have hundreds of people doing it.
      The   command   parser   returns   a   triplet   of   the    form
    <verb>/<object>/<with>,  where  <verb>  is  the action to be per-
    formed, <object> is the object to which the action  is  directed,
    and  <with>  is  a second object to use in performing the action.
    Thus,  the  command  "hit  dwarf  with   sword"   would   produce
    hit/dwarf/sword; ....  
  Say in the future you decide to include things like "hit dwarf over head with
sword".  You might then use things like hit/dwarf/head/sword, but how do you
decide the order? It is better to INCLUDE THE PREPOSITION.  The general rule
would be that the parser returns <verb>/<object>/<with-1>/..../<with-n> where
<with-i>=<preposition>/<what>.  A simple parser might never supply more than
one, and a simple interpreter might ignore all but the first one, and ignore
the preposition.  But you insure the ability to extend the language and not
have to rewrite any existing adventures.  (Note that the parser would have to
parse off the preposition ANYWAY, so this doesn't increase the minimum
complexity of the parser. All it says is that it should include the parsed off
preposition in the result)
                                      ...              A distinction is
    made between nouns (any word which can serve in the  <object>  or
    <with>  portion  of  a  command) and objects (which correspond to
    portable objects within the adventure). .....
    Object descriptions have the form "OBJECT <word> <word>  <string>
    <string> ... ;" where the first <word> is the word used to refer-
    ence this object, and the second <word> is the name of the  state
    (room)  where  this  object initially resides (see below for more
    info on state names).
In general, portability is just one attribute of an object.  If you give it
this special position, down to making a syntactic distinction, you are
practically guaranteeing that future extensions to the language will be
incompatible.  I would propose instead some syntax whereby each noun specifies
if it is portable, and its initial position, arranged so that in the future it
can be extended to each noun specifying a number of properties, without making
old adventure descriptions (the ones specifying only portability) obsolete.
      Actions take the form "<action-function> ( <parameter>,  <parame-
    ter>, ... )".  Available actions are ...
There should be variables other than booleans, and "actions" should include
ability to set them to arbitrary expressions. This is necessary for
keeping score, strength or other measurements.  Also even boolean variables
should be settable to arbitrary expressions.  Note that you are already
requiring an arbitrary-boolean-expression parser, (for the conditions !),
so this is not much of an extra requirement.  I would say all variables
are integers. An expression is as usual, with +,-,*,/,&,|,= etc.
"=" returns  0 or 1, and & (and) and | (or) treat 0 as false, non-zero as
true.  The evaluation can stay strictly left to right for simplicity of
parser..  Add an action SETVAR(<word>,<expression>), overriding SETTRUE etc.
              PRINT(<string>) - Print the string.  Note: there is no facility for
                              inserting newline characters in strings; it is up
                              to the interpreter to split strings across lines
                              at appropriate points.  This allows an adventure
                              to be played on different line length terminals.
NO FACILITY? That's simply wrong.  How about an extra action,
PICTUREPRINT(<string>) which simply prints out <string> as is?  Can you imagine
the puzzle in Zork with NO FACILITY for inserting newline....
Also now we also need a way to print numbers.    
  One general comment: You don't seem to have a facility for putting in
comments!  That is absolutely necessary... { comment  would be fine.
    
    In addition to the game extensions outlined  below,  I  think  it
    might  be  worth while defining a simpler description format: one
    with all <word>'s and <key words>'s replaced with numbers.   This
    would  be  simpler  to parse, allowing interpreters to be written
    more quickly ...
Again, while I can understand the desire of a language designer to see
his language implemented and quick, I don't think that is the main point.
However, I do agree it might be useful to precompile the adventures,
for the sake of efficiency.
    
    One of the simplest, tho quite interesting extensions would be to
    allow  multiple adventures to be linked together, thus creating a
    (potentially) huge game.  This can be done easily by  having  the
    interpreter  maintain  the object locations and boolean variables
    separately for each adventure; a "context  switch"  can  then  be
    performed  via  a  new action (NEWADVENTURE). 
I don't think it is that simple.  The main problem is you don't just want a
bunch of adventures, you want one huge one.  That can't be done without the
interpreter taking on some extra burdens.  Otherwise EVERYTHING will change as
you step from one adventure to another.  Can you imagine stepping North, and
suddenly, it's not "turn on light" anymore, now it's "light lamp".  I think it
would become neccessary for more things to be built into the interpreter, the
way you have "look" and object locations built in now, so some continuity can
be maintained.  Maybe the interpreter would have some basic built-in
vocabulary, ("look","get","n",..)  with some built in synonyms.  That would
probably do it, because once the words exist, most people would use them "as
intended".  But if they have to think up the words from scratch, they will
often come up with different ones.
  Overall, congratulations on a job well done! 


11-Aug-80 00:27:21-EST,3137;000000000000
Mail from Host MIT-AI at 0027-EST on  11 Aug 1980
Date: 11 AUG 1980 0027-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

Date:  5 Aug 1980 1023-PDT
From: Chesley at SRI-KL
  	Are we decided to pursue this idea via the INFO-MUD mailing list?
If so, full speed ahead; if not, let's get another list going.
	I wonder if the various people who've mentioned existing programs
could summarize the approach they take and/or supply pointers to where they
can be found on the net.  I for one would just as soon start using an
existing program rather than design a new one.  However, I think that the
design must have the following attributes in order to succeed: 1) It must
be simple enough that people can write interpretters for several machines
(primarily, I suppose, Unix and TOPS-20/TENEX/...) without spending huge
amounts of time on it.  2) It must be fairly easy to write Adventures (i.e.,
you shouldn't have to be a LISP hacker in order to write something).  And
3) it should be possible to write decently interesting adventures.
	Let me describe what I've been thinking:  The following design is
not intended to cover all possible adventures; rather, it covers a lot of
them, and can get us some "field" experience with which to design an even
better one.
	The Universe is viewed as a large state machine, each room being a
state.  To further qualify things, boolean variables are introduced; no
more sophisticated data structures are used.  For each room, there is a
list of conditionals and associated actions.  One special conditional is
the input of a particular command from the player.  A set of predicates
and actions are also available.
	Syntacticly, the adventure specification is divided into three
sections: a) a list of booleans to be used; b) a list of words and their
synonyms; and c) the list of states.
	Semanticly, it is assumed that a preprocessor inputs player commands,
strips out all punctuation, throws away all unrecognized words, and then
selects a verb, object, and indirect object (one or all may be null), which
we'll write here as verb/object/indirect-object (e.g., "hit dwarf with axe"
would produce hit/dwarf/axe (I'm stretching the meaning of indirect object,
but you get the idea)).  The program then starts at the current state and
searches for true conditionals; there is also a list of conditionals which
is always searched, regardless of what room you're in.
	Actions include newroom(<room name>) (which also simulates the input
of a "look" command after going to the new room), settrue(<boolean>), and
setfalse(<boolean>).
	Thus, a room description might look like "office: (look, "This is
my office.") (e, newroom(corridor)) (w & winbroke, newroom(parkinglot))
(w & winbroke, "You break the window as you climb thru."; settrue(winbroke);
newroom(parkinglot))".
	Of course, many more details need to be added, but the important
thing about this design is that it's simple: you need an input parser and
an interpretter which can recognize simple conditionals and actions, and
that's all.
	--Harry...
-------


11-Aug-80 00:32:06-EST,3023;000000000000
Mail from Host MIT-AI at 0032-EST on  11 Aug 1980
Date: 11 AUG 1980 0030-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 4 August 1980 21:34-EDT
From: Gail Zacharias <GZ at MIT-MC>
  Sounds like exactly the right thing for this group to get into as we all
wait for our respective chances at a full ARPANET->GATEWAY->ESSEX->MUD
connection.  If the group as a whole is not interested, I'd be interested
in forming a splinter group for this purpose.
  Here is some first thoughts (these are truly off-the-cuff, but we gotta
start somewhere).
  1. The exact physical format doesn't really matter at this point and
   can be left for last.  Any fixed format can be decoded by any computer.
   (what's a comma here and a parenthesis there matter anyway...)
   So let us just concentrate on the conceptual components and layout.
  2. If we can define the "components" of an adventure, then relationships
   will be easy to specify.  There are ways of doing that, for
   example take some primitive relationships and then form predicate
   calculus statements.... I.e. <relationship>=<relationship> and
   <relationship>|<relationship> or <relationship> etc...  Whatever..
   These things can be done.  So the first thing would be to get a firm
   hold of the "components", the nouns so to speak...
  3. So on to components...
   Room name.  Now right here you have a problem already...  You might
     want to have different names depending on the players vantage point.
     (to wit Tea Room and Posts Room in Zork).  Hmm, how about a set of
     pairs, [(condition,room name) (condition,room name) .... ].  The
     conditions would have to be mutually exclusive.  The conditions
     would of course be sentences made up of "nouns", the thing I put off
     for later in 2.  Recurse, recurse.
  Description.... Hmm, I guess that should be like Room Name, with
     the pairs.... Actually I guess Room Name is part of description
     so forget Room name, just have description. Pairs
     [(condition,description)... ].  For example could  have something like
     (verbose-flag-off or room-visited-within-last-2-moves, "Foo Room")
     (not have-lamp, "Some dark room")
     etc.
  Unique identification - this is internal to the game description, not for
     the player.  It could be just a number assigned to a room as it is
     added to the game.  This is needed so the room can be referred to, as in
     if-have-been-in-room(1745980)... Of course each adventure chunk would
     not be aware of other adventure-chunks' room numbers, but it would
     know all the room numbers within the single sub-adventure so internal
     relationships can be established.	There needs to be ways for
     adventure-chunks to connect to other ones without
     really knowing what the others are...  'My east exit should go to
     a room preferably not in my chunk, preferably with no windows...'
    ..... that's enuff from me...



11-Aug-80 00:32:39-EST,1574;000000000000
Mail from Host MIT-AI at 0032-EST on  11 Aug 1980
Date: 11 AUG 1980 0031-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 28 Jul 1980 1604-PDT (Monday)
From: Mike at UCLA-SECURITY (Michael Urban)
     Well, the problem in the case of MUD (which I haven't
gotten into yet) doesn't seem to be so much the killing
of abstract entities, so much as the mode of interaction
between (real live) players.  The observation was that
certain players were more eager to "kill" (hinder the
play of) other players than to cooperate with them.
   Fantasy Role Playing games in general, including 
Zork and Adventure, seem to end up with a somewhat
disturbing moral infrastructure, in which the goal
of the player, the hero, the "good guy", seems to be
to amass gold and wealth and power and step on whatever
gets in his/her way.  One person has described this
as "almost a parody of Capitalism".  In Zork, you
rob banks and steal grails from temples.  In Adventure,
you swipe Ming vases, etc.  In Dungeons and Dragons,
the worst examples are to be found..."We hit the door,
throw 50 dice worth of fireballs, and kill whatever was
there.  What was it?"
   The whole point of this flame is to encourage the
creative people who referee in D & D, and who build
Adventure-like programs, to consider the underlying
morality that you're encouraging in your game.  It 
ISN'T just abstract game-playing, after all; the reason
people play these games is precisely the "color" that
is put into them.
  	Mike
-------



11-Aug-80 00:33:08-EST,790;000000000000
Mail from Host MIT-AI at 0033-EST on  11 Aug 1980
Date: 11 AUG 1980 0032-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: baltrunas at NBS-10

    From: obrien at Rand-Unix (Mike O'Brien)
Date: 25 Jul 1980 at 1053-PDT
  1) killing but no bartering. - PLATO
2) bartering but no killing. - PLATO
3) MUD - Essex, Tops-10
  The second became much more interesting when new features were inserted which
made it necessary to go in with a team in order to survive some of the more
horrendous perils, as well as allowing groups to stake out territory (the maze
was stupendous in size).  Both of these were simple mazes with walls, monsters
and treasure, not the simulators seen on the net.
  So, "kill first and plunder later" is not the universal method of play in these
games.


11-Aug-80 00:34:46-EST,2377;000000000000
Mail from Host MIT-AI at 0034-EST on  11 Aug 1980
Date: 11 AUG 1980 0033-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date:  5 AUG 1980 0016-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
  Hmmmm.  You find that you will run into some interesting situations
when you start working with relationships, conditional names, or
conditional properties.   I have been working on such a standardized
format for ADVENTURE-like gaming.  Like MUD, it is to allow multi-user
interaction, but it must also be able to be described completely as a
sort of text file, similar to adventure's, but much more comprehensive.
  I began with relationships, and how you might parse them.
  Then I worked with program structure and data-description structure.
  Then I learned LISP, and found a whole world of thinking different from
other upper level languages.
  I now have a medium sized set of syntax rules which I'm trying to follow
which will allow me to describe objects, rooms, monsters, treasures, magical
spells, and traps all using the same genre.  If you imagine the closeness
that each of these items has to each othe, you can see that they are not
very different at all, but that they only have different properties.  A
property might be STRENGTH, WEIGHT, DISTANCE, CAPACITY, BOYANCY, (ROUTINE),
or any combination of the above, not to mention DESCRIPTION, INFO-DATA, or
usage statistics.  I've found that the 'property' idea works best, 'cause
it allows for sparse arrays.
  I'd like anyone who has suggestions for MUD to also mail me a copy, so that
I can incorporate the KIND-OF-SYMPTOM that a room, or object might exude
into the generalized syntax that I'm developing.  If it works out, then
once the program is initilized, it would begin to grow by itself, and
export copies would merely be later versions of the same dungeon or adventure
with additional rooms and capabilities.
  I'm interested in talking to anyone else interested in working on this.
(In fact, between Aug 20 and Sept 20 whilst I am on vacation, {sigh, away
from arpa, I will be spending an inordinate amount ofeffort on this.)
{If anyone knows of a 300 baud Dialin or TIP in the Tuscon, Arizona area,
I may not be gone for a month. <alas, the trials of vacation>
  			Rooms and Ideas WANTED
  			    Inquire Within!
  \cb\ Carl.


11-Aug-80 00:34:57-EST,694;000000000000
Mail from Host MIT-AI at 0034-EST on  11 A80
Date: 11 AUG 1980 0034-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: baltrunas at NBS-10

    Date: 5 Aug 1980 09:20 PDT
From: JimDay.ES at PARC-MAXC
In-reply-to: GZ's message of 4 August 1980 21:34-EDT
  I think that "room" standardization is a fine idea.  But I think that the game
environment should be largely file-driven, to save RAM space and to enhance
portability.  So I suggest an ASCII format for room attributes, creatures, objects,
etc.  The local implementation of the game could then be in any language and
for any machine.  But the implementation would be more or less transparent to
the player(s).
  Jim Day


11-Aug-80 00:42:27-EST,5366;000000000000
Mail from Host MIT-AI at 0042-EST on  11 Aug 1980
Date: 11 AUG 1980 0037-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 16 Jul 1980 1250-EDT
From: FRP LIST
  Date of review:			Name:			Type:
User programmable:		When written:
Writer:				Language:
Parser:
End game:			Save/restore:
Character attributes:
Description:
Good points:
Bad points:
Comments:
Access info:
-------
Name: EMPIRE	Type: conquest, management of a country, simulation
Date of review: Thursday, 17 July 1980
Writer: Peter Langston			When written: Mid 1978
User programmable: no
Language: C, Empire runs only on Unix
Parser: Simple commands with multiple arguments and 
		operators similar to the Unix shell itself
End game: Not applicable	Save/restore: The database is constantly
				updated and resides in its own directory(s)
				on the system
Character attributes: actually country attributes, too complicated to explain
Description: (taken from the empire manual)
The game empire is the most recent in  a  series  of  territorial
conquest  games  inspired by a board game of the same name played
at Reed College (Portland, Oregon).  Earlier versions  have  been
written at Reed by Peter Langston and at The Evergreen State Col-
lege (Olympia, Washington) by Chuck Douglas, Peter Langston,  Ben
Norton and Mike Rainwater.  The current version was written part-
ly on the HRSTS Unix system at the Harvard Science Center,  (Cam-
bridge,  Mass.),  partly  on  the Unix system at Commercial Union
Leasing Corporation, (New York, N. Y.) and  partly  on  the  Unix
system  at  Davis  Polk  &  Wardwell,  (New York, N. Y.) by Peter
Langston with invaluable goading from Joe Stetson,  Robert  Brad-
bury, Nat Howard and others.
  Empire falls into the broad category of simulation games, involv-
ing  both military and economic factors.  Although no goal is ex-
plicitly stated, players rapidly derive their own,  ranging  from
the  mundane  desire  to be the biggest, mightiest country in the
game and conquer all others to the more refined goals  of  having
the most efficient land use possible or the lowest ratio of mili-
tary to civilians while still surviving, etcetera.
  The role of the computer  in  Empire  is  that  of  modeling  the
physical/economic  system.  Players interact through the computer
rather than with the computer.  The games is played in  a  "real-
time"  environment; players log on and allocate resources, attack
neighbors, send diplomatic communiques, etc. whenever  they  have
time  and  the  program keeps track of their activities such that
when they are not logged on the time accumulates  until  they  do
log  on; (accumulated time is expressed in "bureaucratic time un-
its" or B.T.U.s).
  The purpose of the B.T.U. Concept is three-fold:
I) The fact that commands use up B.T.U.s  limits  the  amount  of
time that any player can spend developing his/her country so that
the insomniacs won't necessarily out-play (or perhaps  over-play)
the players with less free time;
II) The build up of B.T.U.s not being dependent on  being  logged
on  at  any particular time allows players to participate when it
is convenient rather than at some fixed time (as in the  case  of
monopoly, the stock market, etc);
III) The B.T.U. arrangement helps compensate for the fact that in
concept,  the  governments  of  each country are always "playing"
although the player  representing  that  country  may  only  play
periodically.
  The geography of the game is embodied in a rectangular map parti-
tioned  into  M x N  sectors (where M and N are typically but not
necessarily powers of two, usually 32, 64 or 128) that is approx-
imately  50%  sea,  45% habitable land and 5% uninhabitable moun-
tains.  This "map" is generated by  a  program  (the  "creation") that
places volcanoes pseudo-randomly forming land masses,  (continen-
tal  drift  was too complicated), and then pseudo-randomly places
veins of gold and iron ore.
	
New countries may join the game at any time; upon entry into  the
game  a new country is given two adjacent sectors.  These sectors
are initially designated "sanctuaries" and are  inviolable.   The
new  nation  may stay in these two sectors for any length of time
and thereby be safe from attack.  However, in order to  build  or
expand it is necessary to leave the safety of the sanctuary.  The
sector of land that was a sanctuary can then be  redesignated  as
one  of  a multitude of other land-use types ranging from weather
stations to gold mines to munitions plants.
  Good points:for people who like extremely complicated games,
this is one of the ones that should satisfy them. Typical games
consist of 5-15 players and can continue over a period of
2-5 months.
  Bad points: a few minor misfeatures, but nothing major
Access info: 
the empire manual is available via ANONYMOUS login from
[SRI-KL]<MCLURE>EMPIRE.DOC. Interested parties can
contact the author:
	Peter Langston
	127 W. 26th St.
	New York, NY  10001
	[Final note: this program has been certified to be dangerous
	to your health and sanity. It has turned former close friends
	into vicious enemies. It has also been known to lower student
	players' GPAs by 2 full points and has been the complete downfall
	of others.]
-------


17-Aug-80 01:22:17-EST,2748;000000000000
Mail from Host MIT-AI at 0122-EST on  17 Aug 1980
Date: 17 AUG 1980 0117-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

Date: 16 AUG 1980 1324-EDT
From: KWH at MIT-AI (Kenneth W. Haase,  Jr.)
  	I thought you might be interested in the following-
     I am presently working on a MUD type system for interactive
space games on a PDP at NBS (UNIX operating system --- 
It's not on the net though, sigh...)
  Jocularly called the UNIXVERSE, it is a kludge of an n-body simulation
which presently simulates the solar system and three ships.  It is
vaguely based on Larry Niven's Known Space, with ships zipping
around between planets.  It is newtonian (maybe in a few years
I'll rewrite it for Einsteinian relativistic simulation) and has
two interesting aspects.  First, it continuously runs at N times real time
as a background process.  Secondly, all ship data is written in
raw binary in a random access file, and all ship commands are taken
from a file in the same format.  These numbers are pure x,y,z co-ordinates
velocities or accelerations, and there are simple programs to print them
out for you.  Of course, the clever thing is to write an autopilot
(if you want to fly around the solar system using only control of
x,y,z accelerations and guts estimation of gravity, be my guest....
just let me off at Ceres-) and this makes the game unique in that
programming skill is a powerful tool here.  You should be able
to write your own autopilot to do lots of things (scanning for obstructions
obstacle avoidance, intruder detection and destruction).  The system
also has a number of scanning options, radio transmissions, comm lasers,
and weapons. (This allows for multi-user interaction, friendly or otherwise)
You program your ship to do x,y, and q, to call you if z happens,
or to blow up if m passes by.  It is possible to build robot "bolo" ships,
cruising around, wreaking havoc.  In fact, missles are entities like ships,
and can either be preset or given programs (if you want a guided missle,
build one!)
  Details of energy levels, ship abilities, etc, are in a special file
accesible only by a super-user "Protector" (if you don't recognize it,
read some Niven) who sets such parameters (he can also steer planets,
and thereby makes the game "fair" while still allowing individuality.
Special projects can be made, such as Kempler Rosettes or massive
gravity slings (incidentally, you can sling off of Jupiter right now)
based on non-relativistic black holes.  In the future, I want
to add solar sails, and other interesting things.
  	Any questions, requests, advice, or pointers to similar
systems would be appreciated. 
  				Cheers,
					Ken


17-Aug-80 01:22:37-EST,751;000000000000
Mail from Host MIT-AI at 0122-EST on  17 Aug 1980
Date: 17 AUG 1980 0120-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    SGR@MIT-MC 08/15/80 22:06:14 Re: FRL as an FDL (these acronyms have got to stop!)
    I read the paper ZVONA referred to about frames about a year ago.
    I was considering a dungeon language based on Minsky frames, but
    didn't understand them well enough.  A better treatment is given
    in the new Lisp book by Winston & Horn. (Copyright is 81, so it's
    pretty new. (How did the Coop get it a year early? Who knows?
    (Ask POURNE, he should know something about publishers (me, I just
    read the stuff...))))

    It looks like a win to me...

    	-$teve


17-Aug-80 01:22:59-EST,618;000000000000
Mail from Host MIT-AI at 0122-EST on  17 Aug 1980
Date: 17 AUG 1980 0121-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 15 Aug 1980 1249-PDT
    From: Chesley at SRI-KL

    	Oops.  I err'd in describing the internal format.  Erase the reference
    to the array wordptr (which is actually an array internal to the compiler),
    and replace it with wordvar.
    	As I look over the description, it doesn't seem very clear.  Perhaps
    I should write up a better one, and make it a part of the compiler documenta-
    tion.
    	--Harry..
    -------


17-Aug-80 01:23:27-EST,2555;000000000000
Mail from Host MIT-AI at 0123-EST on  17 Aug 1980
Date: 17 AUG 1980 0122-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 15 Aug 1980 1159-PDT
    From: Chesley at SRI-KL

    	When I said I was working on a parser, what I had at the time was
    just something that parsed the adventure and either said "Yep, that's an
    adventure written in Amber", or complained about syntax errors.  Since
    then, I've started adding internal details.  I've been assuming that the
    output of the compiler (input of the interpreter) is specific to the
    particular implementation, and thus not subject to standardization, and
    that an interactive adventure writer would output an Amber source language
    description of the adventure (maybe plus an internal format description).
    	Anyway, let me summarize the format that I've evolved (I'm ashamed
    to admit that it wasn't designed exactly, but arose as I encountered
    various problems in parsing and generating "code" (i.e., I designed it as
    I went)): Starting from the printstring end, there are two character
    arrays, containing contiguous strings, each string ending with a zero byte
    (C convention): (1) the cmdstring array (printstrings for commands); and 
    the output string array (actually a file, but it can be read into an array
    if you've got the space) (strings within the state descriptions).  The "value"
    of an output string is an index into the output string file for the first 
    character of the string. For command parsing in the interpreter, an array 
    csw contains the index into the wordptr array for the cmdstring, and an 
    array cmdtype tells whether it's a VERB or a NOUN.  There's a 2-D array 
    wordvar which contains all variables (<word> variables have all been 
    translated into either constants or indexes into this array); the first 
    dimension is the variable number, and the second is the subvariable number.
    	I haven't gotten around to doing states yet, but I envision the
    following: One array is a set of expression trees describing the states
    and expressions within them; a second is indexed by the state number and
    points into the first where the state expression starts.
    	All of the arrays are written to a file in binary, to be read by
    the interpreter.

    	Re manual: of course, once we get something written, a simple, easy
    to read, adventure writer's manual will be done.
    	--Harry...
    -------


17-Aug-80 01:26:04-EST,578;000000000000
Mail from Host MIT-AI at 0126-EST on  17 Aug 1980
Date: 17 AUG 1980 0125-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 15 AUG 1980 0107-EDT
    From: ZVONA at MIT-AI (David Chapman)

    In ai:zvona;frlfrp > are some ideas about using FRL as an
    implementation language for FRPs. FRL is a language that manipulates
    frames, which are sort of augmented property lists. (The frame idea is
    due originally to Marvin Minsky.) FRL would also be a good author
    language. Sample chunks of program are given. 


17-Aug-80 01:26:32-EST,2074;000000000000
Mail from Host MIT-AI at 0126-EST on  17 Aug 1980
Date: 17 AUG 1980 0126-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 14 August 1980 22:35-EDT
    From: Gail Zacharias <GZ at MIT-MC>

    	ATN's are a much too complex and totally unneeded concept here.  Their
    basic purpose is to implement natural language processing, and have happened to
    be used in the context of data bases.  But there are much simpler ways to
    access data bases.  So I vote against that.
    	But I do feel that at this point we absolutely need to discuss the data
    base.  The main point is that if we ever want to allow interactive AUTHOR
    interface the data base will need to be dynamic.  We'll need to add and remove
    properties, rules, rooms etc.  These things need to be standardized so that the
    author interfaces can be transportable.  While they are not part of Amber the
    adventure description language, they are part of Amber the adventure-
    interpreter input standard.  Harry, you said you are writing the Amber parser -
    what in the world are you parsing it into?  We need to discuss that.  It has to
    have the capability of becoming dynamic eventually.  Any data base hackers out
    there care to come up with some good primitives?

    Another thing which needs to be transportable is macro packages, so Amber
    should include macros as part of the standard.  Especially since Amber itself
    is likely to be the de facto author interface for a while.  Once Amber has
    macros, that basically covers the DEFINE feature of DL1.  I think Amber is more
    powerful.  The basic problem is that DL1 will do actions independently on
    individual words in the user input, while Amber allows you to act on the whole
    structure of the input.  Basically saying that the word "foo" has a property of
    arbitrary-piece-of-code is like saying in Amber 
    (inputverb="foo"; arbitrary-piece-of-code) but in Amber you can also do more
    complicated matching.






17-Aug-80 01:27:37-EST,747;000000000000
Mail from Host MIT-AI at 0127-EST on  17 Aug 1980
Date: 17 AUG 1980 0127-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 14 Aug 1980 1741-EDT
    From: JHENDLER at BBNA
    In-reply-to: Your message of 14-Aug-80 0405-EDT

    in re. the ATN proposal I have one comment...
       "good luck. You'll need it!"

     -Jim
    p.s.  (to be more exact, some time take a look at how complex the LUNAR program
    was. also note that at least 2 and perhaps 3 people got doctorate thesis'
    out of that program..also note that the only way to run this efficeintly will
    be on pretty hefty computers.. (ok, so a little more than
    one comment, no one is perfect...))
    -------



17-Aug-80 01:28:20-EST,2728;000000000000
Mail from Host MIT-AI at 0128-EST on  17 Aug 1980
Date: 17 AUG 1980 0128-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 14 Aug 1980 1002-PDT
    From: Chesley at SRI-KL

    	Re running Amber on micro's: Yes, certainly, but it's easier to get
    it up on the big machines.  It is my feeling that eventually, using much
    cleverness and text compression, it should be possible to run a decent
    Adventure on a micro without mass storage.  For larger micro's, a Pascal
    (UCSD perhaps) implementation would cover a lot; Basic even more, but the
    dialects vary more.
    	Re rights: I consider the Amber language to be in the public domain.
    I suppose I could retain rights, but I prefer not to for the following
    reasons: (1) Since much of the design work is being done on SRI and DoD
    computers and networks, legal questions could be raised if I tried to make
    a profit off it.  (2) At this stage of the game, I prefer to see maximum
    use of the language in order to better understand the problems and potentials
    of this sort of thing (I may later design an even better language which I
    will sell).  And (3) it is very difficult to inforce rights on a language
    design; the better approach is to use the experience gained here to develop
    an edge in writing a good Adventure generator/player since programs are
    easier to protect.
    	Re attributes:  The variables in Amber allow the DM to keep info
    such as candle burn, or to establish links between an action here and a
    consequence there (I had the sluice gates in mind, and tried to show an
    equivalent example with the light switch in the Amber manual).
    	DL1: Having looked at the DL1 stuff, I rather like it.  It strikes
    me as more complex to implement, and a bit more complicated to use, but
    also more powerfull.  Therefore, we need to decide on one of several courses:
    (a) forget about Amber, and concentrate on DL1; (b) forget Amber, and work
    on a modified DL1 (i.e., extend and/or simplify the existing DL1); (c) forget
    DL1, and continue with Amber; or (d) fold some of the ideas of DL1 into
    Amber.  Are there any strong opinions on the subject?  Any weak opinions?
    If we go to DL1, the first thing needed is a good manual for it.  If we
    fold DL1 into Amber, I think this implies generalizing the property lists
    of Amber, and perhaps treating user defined functions (forms) in the same
    manner as strings, numbers, etc.  
    	--Harry...
    P.S.	I've put some effort into Amber, but I'm still not adverse to dropping
    it in favor of DL1 or some offspring of DL1.
    -------


17-Aug-80 01:30:15-EST,2024;000000000000
Mail from Host MIT-AI at 0130-EST on  17 Aug 1980
Date: 17 AUG 1980 0130-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 13 AUG 1980 2240-EDT
    From: PCR at MIT-AI (Phillip C. Reed)

         Re: Amber comments

    	I have been reading with interest the comments and design
    spec here, and have decided to throw in my two bits worth.
    	First, I would like to suggest that we not be so restrictive
    in our view of where the language will be run. It's all very
    nice to write Amber for the monster machines, but
    let's keep the personal type computers in mind. Remember, there's lots
    more of them than of us, and we really don't want to shut those people
    out (there's a lot of imagination out there).
    	Second, has anybody given any thought to who will have the rights
    to Amber? Since this discussion is taking place partly on a public-owned
    medium, does this mean that Amber will be in the public
    domain? Since part of this discussion is taking place over government
    funded equiptment, does this mean that Amber will be owned by the U.S.
    Gvmnt? (Imagine the implications!!)  My thought here is: after we get
    the bugs out, and a decent Author Implementation Language designed (AIL?),
    we might try putting it someplace like MicroNET, with the idea of letting
    the users there write and play adventures, and use that as a testbed.
    	Third, some suggestions: we will need to be able to
    specify durations in the attributes list (candles burn n turns), and
    attach descriptions to partial durations ('There are candles here.'/
    'There are partly burned down candles here'/'There are some
    used candlesticks here. There are no candles in them, only some burned
    wax.'). We also need to be able to schedule events to occur at a later
    time (an example here would be in Zork, having the water level lower n turns
    after turning the bolt).
    		Phil(PCR@MIT-AI)



17-Aug-80 01:31:02-EST,1464;000000000000
Mail from Host MIT-AI at 0131-EST on  17 Aug 1980
Date: 17 AUG 1980 0131-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 13 Aug 1980 1912-PDT
    From: Chesley at SRI-KL

    	The new version is out: you'll find it in "mc:users2;hrc amber2".
    	However, I had to make another extension: I wanted to be able to
    associate variables with objects (yes, we're back to the original property
    list idea), in such a way that you could make statements like 

    	(take$anyobject$nil AND (inputobject.location = thisroom),
    		SETVAR(inputobject.location,carrying))

    	That is, use a general "inputobject" so that you can write catch-alls
    like the above.  But this implies that every object (noun even) have every
    type of subvariable.  Clearly no good.  So I changed the VERB/NOUN
    definitions to include a "class": "NOUN axe portableobject "axe"".  All
    words of the same class have all the subvariables for that class, thus
    restricting subvariable proliferation, and the classes can be used like
    the previous "anyword".  For instance, the above condition/action would become:

    	(take$portableobject$nil AND (inputobject.location = thisroom),
    		SETVAR(inputobject.location,carrying))

    	This gives the catch-alls more power since you can no longer say
    "take my hand" and have it be caught by the catch-all above.
    	--Harry...
    -------


17-Aug-80 01:31:41-EST,3664;000000000000
Mail from Host MIT-AI at 0131-EST on  17 Aug 1980
Date: 17 AUG 1980 0131-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    SGR@MIT-MC 08/13/80 22:11:29 Re: long msg about dl1

    DL1 allows the dungeon-creator to define dungeons in a machine-independent,
    high-level fashion.  The types of statements in DL1 are:

    	1. ARTICLE: define several words as being articles,
    	   to be ignored by the command parser:
    	
    	   ARTICLE a, an, the;


    	2. PREPOSITION: define several words as being synonomous
    	   propositions:

    	   PREPOSITION with, using;
    	   PREPOSITION in, into, inside;


    	3. DEFINE: define an identifier to have a value.  The value 
    	   can be an integer, a string, or a form that describes
    	   executable code:

    	   DEFINE Foo = 5,
                      Bar = "Foobar!",
    	          Baz = ($if (Lightp ($loc Me))
    			     ($say "There is light here." Nl)
    			     ($say "It's mighty dark in here!" Nl));


    	4. INCLUDE: Yank in another DL1 source file.  Good for 
    	   maintaining standard parameters for multiple worlds:

    	   INCLUDE "dsk:sgr;donjon defs",
              	   "dsk:sgr;donjon consts";


    	5. VERB: define a word to be a command verb, and its
    	   associated actions:

    	   VERB take, get HAS
      	        PROPERTY Preact = (Availp ($dobj)),   #dir obj available?
                    PROPERTY Action = (Take_obj ($dobj)); # take it


    	6. NOUN: define a word to be a noun, and its associated 
    	   actions:

    	   NOUN [red axe] PLURAL axes,
    		hatchet,  PLURAL hatchets
    		IN Room5 HAS
                    PROPERTY Sdesc = ($say "a little red axe"),
    	        PROPERTY Ldesc = ($say "There is a little red axe here." Nl),
                    PROPERTY Weigh = 2;

    	   NOUN Garden IN All HAS
                    PROPERTY Sdesc = ($say "Pleasant Garden"),
    	        PROPERTY Ldesc = ($say
    				"You are in a pleasant garden.  Tall flowers" Nl
    				"bloom all about you." Nl),
    	        PROPERTY Light = True,
                    PROPERTY Holds = Infinity,
                    PROPERTY Has   = 0,
                    PROPERTY Action = ($match ($verb)  #define transitions to rooms
    					(M House) (M Barn) (M cave) (M garage));

    The property lists are unstructured, so the DM can define whatever
    sort of properties please him.  Some properties are used by the system:
    Sdesc = short description, Ldesc = long description, Preact = something
    to be done in preparation for Action = code associated with a word.

    Also, the DM can define demons, which wake up and decide what to do
    at the beginning of each turn.  Usually there is a demon which describes
    the room condition at the beginning of a turn.  Fuses are like demons,
    except they go to sleep for a specified number of turns, and then execute.

    During a turn, the currently active demons are run first, then the fuses
    are checked, then a sentence is parsed.  The output of the parser is
    a verb, a direct object, an indirect object, and a preposition. Then we
    run the Preact property of the verb, the Action property of the indirect
    object, the Action property of the direct object, the Action property of
    the verb, and  finally the Action property of the room.

    For some examples and comparison with DDL, consult the DDL reference manual
    in MC:ARCHIV;DDL MANUAL.

    Syntax diagrams, for those who are fans of such, are in the file 
    MC:ARCHIV;DL1 SYNTAX.

    	-$teve



17-Aug-80 01:32:15-EST,1782;000000000000
Mail from Host MIT-AI at 0132-EST on  17 Aug 1980
Date: 17 AUG 1980 0132-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 13 Aug 1980 1720-PDT
    From: Chesley at SRI-KL

    	Working on the Amber language parser, a couple interesting ideas
    have come up: (1) If there is an internal string table, and a statement
    like "PRINT("...")" enters the string in the table, and effectively
    replaces "..." with the table index, then string can be manipulated as
    numbers, assigned to variables, etc. (the concept is essentially the same
    as in C, where a string produces a pointer to the text); this allows
    things like "SETVAR(s1,"hi there")" and then later "PRINT(s1)".  It also
    points in a potential direction for expansion: allowing dynamic creation
    of strings, and various operations (compare, concatenate, etc.) on them.
    And (2) as long as we have numerical variables, we may as well provide a
    limited form of C's unary * and & operators (indirect and address-of,
    respectively); these operators would return pointers or follow pointers,
    but not necessarily abs pointers (they may use table indexes).  The proper
    use of these operators is somewhat subtle, but adds some power to the langauge.
    	To answer a couple points raised earlier that I neglected:  Yes,
    we do need comments; how about using C's /* ... */ convention?  And by the
    way, I've been trying to keep to upper-case ASCII for generality when picking
    characters.  (Oh no! I've forgotten what other point I wanted to answer.
    Sigh.)
    	I'll redo the Amber manual with all the changes discussed so far,
    and let you know when it's residing at AI.
    	--Harry...
    -------


17-Aug-80 01:33:00-EST,2366;000000000000
Mail from Host MIT-AI at 0132-EST on  17 Aug 1980
Date: 17 AUG 1980 0132-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 13 Aug 1980 1146-PDT
    From: Chesley at SRI-KL

    	My justification for entering into language design at this stage has
    two parts: (1) To a large extent, I know what I want the language to do (tho
    I guess I haven't stated it (I will below)); this comes from experience with
    several Adventures, and from having written a crude Adventure language before,
    some time ago.  (2) To another large extent, I don't think we'll really know
    everything that the language should do until we start using it; thus, I hope
    we're describing a language which can be extended without great pain.
    	What the first pass language is intended to allow is: (a) room
    descriptions, including variable portions such as "the lights are on"; (b)
    movement between rooms; (c) portable objects which can have simple
    attributes such as "on" and "off" or "empty" and "full"; (d) actions performed
    on and with these objects, or on and with pseudo-objects (such as "hands",
    which is not a real object, but implicitly always present).  Further, the
    language should be portable, and it should be convenient to write adventures
    in it (even more convenient with a good adventure writer interface program).
    	I'm already coming up with ideas for extensions, but I want a decent
    base to build upon first.
    	And talking about extensions, here's one: some way to refer to a
    sentence that was said; that is, if I say "tell george 'hit that dwarf'",
    I should be able to get at hit$dwarf$nil.  This would allow the adventure
    writer to populate the adventure with usefull or interesting people.  We
    might take some ideas from Eliza to allow general conversations.  Another
    way to add pseudo-people would be to allow adventure scripts to issue
    commands, then implement a multi-player game, with some of the players being
    scripts...  But as I say, I want something to build on first; I've known too
    many people who designed the niftiest program ever but never quite got
    around to implementing it because they kept coming up with new ideas on how
    to make it even niftier.
    	--Harry...
    -------


17-Aug-80 01:33:25-EST,1364;000000000000
Mail from Host MIT-AI at 0133-EST on  17 Aug 1980
Date: 17 AUG 1980 0133-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date:  13 August 1980 12:42 edt
    From:  Janofsky.Tipi at RADC-Multics

    I had a very long, rambling flame on language designing and putting the
    wagon train before the horse.  I trashed it this morning in favor of
    three questions to this eager group.
     1. Does it make any sense to design a language before we write down
    (agree on?) what we want to DO with it?
     2. Doesn't the player interface (parser, etc.) dictate the complexity
    of the interpreter and, by implication, the sophistication of the
    language it will interpret?
     3. If we're talking about a language from "describing" CFS (or FRP if
    you prefer) games, isn't the author interface a critical  and driving
    issue in specifying the features of the defininition language
    (constructs allowed, problem types, scoring, combat, etc.)?
     I would suggest that the player interface and the author interface be
    well defined before we specify the language.  The implemntation of
    interpreters could be staged so the set of capabilities evolve through
    several versions; but this only works if you know where you want to go
    before you start.
    	Bill Janofsky


17-Aug-80 01:38:57-EST,2825;000000000000
Mail from Host MIT-AI at 0138-EST on  17 Aug 1980
Date: 17 AUG 1980 0134-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 12 August 1980 21:36-EDT
    From: Gail Zacharias <GZ at MIT-MC>

    	C and LISP were brought up as possible languages in which to
    implement the adventure interpreter, not as the languages in which the game
    author would describe his game.
    	I agree however that a language like Amber would be TERRIBLE to
    write adventures in.  Writing state machine descriptions is notoriously
    difficult even for an experienced programmer, as it requires one to think
    in a sort of inverted logic.  However, it would be always possible to write
    a user oriented front end.
    	I think an adventure system would have to have a multi-layer
    implementation, for the same reason that most compilers compile languages
    into assembly language rather than directly into machine language.  There
    is only so many conceptual levels you can support in a single program. So
    as I see it, we would have
     Author <--> Author-interface <--> Internal description <--> implementation
    	Amber would fit in the internal description category, although of
    course one could write a description in Amber just as one could toggle in a
    machine language program into a computer.  But the main point is that it
    defines the capabilities of the system, and the meaning of various concepts
    and constructs which occur.  There is no reason the Author-interface can't
    be interactive, IF Amber and the underlying implementation is well
    designed.  It's easy to say `oh, I'd like to be able to say "King Kong will
    leave a trail of blood when he jumps out the window" and have that become
    part of the adventure', but what does "become part of the adventure" mean?
    If we have an Amber, we know that the job of the author interface is to
    translate that into something like 
    (King!Kong/jump/out!window
    			SETVAR(Blood.trail,True),
    			PRINT("A Trail of Blood appears") );
    and add it to the data base.  There is no reason Jerry Pournelle or Larry
    Niven should ever write the latter, ugly form.  But we need one so we know
    what we are talking about.
    	Anyhow I would be interested in hearing more discussion on
    the user (author) interface.  This list should be able to support both
    sides of the problem!  In fact the two discussions SHOULD happen at the same
    time.  The external interface discussion would make sure the internal
    language (Amber) supports most of the concepts we are likely to come up
    with, and the Amber discussion should make us realize which features would
    be very difficult to support and which would be easy.



17-Aug-80 01:39:20-EST,1293;000000000000
Mail from Host MIT-AI at 0139-EST on  17 Aug 1980
Date: 17 AUG 1980 0135-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 12 Aug 1980 14:01 PDT
    From: JimDay.ES at PARC-MAXC
    In-reply-to: Chesley's message of 12 Aug 1980 1304-PDT

      There is an obvious need for global and local identifiers, but I think that the
    room designer shouldn't have to worry about that.  Room definition should be
    entirely conversational.  What the internals look like is up to the implementor.  I
    would prefer local identifiers to be prefixed with a unique "room number"
    assigned from a master list.  That way, no conflicts could arise between local and
    global identifiers.
      In any case, I suspect that each room will turn out to be a different game
    unless a hierarchical structure is imposed on room design parameters.  In effect,
    rooms defined at a lower level would be able to add features that didn't conflict
    with features/restrictions already defined at a higher level.  This needn't be too
    restrictive on room design, though, since a room could always be added to some
    other part of the game-definition tree where no conflict existed -- at the top, if
    necessary.

    Jim Day



17-Aug-80 01:39:55-EST,2572;000000000000
Mail from Host MIT-AI at 0139-EST on  17 Aug 1980
Date: 17 AUG 1980 0137-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 12 Aug 1980 1304-PDT
    From: Chesley at SRI-KL

    	Oops.  By adding numerical operators I made the language ambiguous:
    a/b/c could be a command or a divided by b divided by c.  Thus, I suggest
    using dollar sign ($) rather than slash(/).
    	How to qualify words is a good question.  I suggest the following
    general syntax: <item-list>$<item-list>$<item-list>... where <item-list> is
    <word>!<qualifier>!<qualifier>...  For instance, "hit the red dwarf hard
    with the rusty sword" would be hit!hard$dwarf!red$sword!rusty.  The initial
    interpreter would ignore all <qualifier>'s and any <word>'s past the first
    three.  Now, does this cover all the cases (where's my high school grammer
    book?)?
    	As to multi-adventure adventures:  If we take the view that we'll
    eventually add high level language type procedures, etc., I think we can
    safely ignore this for the moment.  The eventual concept would be that
    you surround individual adventures with a "subadventure <adventurename>
    BEGIN ... END" structure.  Anything not enclosed is global; that is, global
    words, variables, and states can be refered to from any adventure unless
    that word/var/state has been declared within the adventure in which case
    the local definition is used until the adventure is exited.  You could
    have subadventures within adventures.  Whether we want a subroutine call as
    well as a GOTO is unclear, but it might be nice.
    	As to an interactive adventure writer:  Great idea!  But I think
    we still need an adventure description languages for trading adventures,
    and it might as well be possible for humans to use the language too.  So
    the interactive version would build the adventure, then write it out in
    the format we're describing here.
    	As to C on 10's/20's: I should have said I'd write one in Yacc/Lex
    (much easier than C; in fact, I've already got one started, with the
    description parser all written, tho not debugged).  These programs eventually
    produce a C program, but there may be problems in porting.  We'll see.
    	One other change to the language: I suggest removing the variable
    definition section.  It doesn't really simplify the parser much to have
    variables predeclared, and they can be initialized in the "start" state.
    	--Harry...
    -------


17-Aug-80 01:40:35-EST,4589;000000000000
Mail from Host MIT-AI at 0140-EST on  17 Aug 1980
Date: 17 AUG 1980 0137-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 12 Aug 1980 0721-PDT
    Sender: ROUNDS at OFFICE-1
    From: ROUNDS at OFFICE-1

    Gentlepeople:

    I saw some of your recent discussions regarding user-defined
    adventures, and was told about it by Rich Zellich.  I thought
    that I'd drop you a note outlining a few ideas which you may wish
    to consider in creating such a situation.

    I like the idea of "weight"; that is, the old concept that a
    player can carry so many objects is expanded to the concept that
    a player can carry n pounds (or kilograms, to be modern about
    it).  Each item is assigned a realistic weight, and a player can
    carry up to, say, 75 lbs.  This would mean that a gold treasure
    might weigh 50 lbs, and you can carry very little else.  A
    trasure of diamonds may weigh .5 lbs, and it has very little
    effect.  This will give the benefit of making players selective
    and add an element of judgment.

    Players should be able to "hide" things.  If I have gold, and
    find silver, and cannot carry both, I should be able to "bury"
    one, by saying something like "Hide Silver with Password" and
    only someone in that area who subsequently states that "Password"
    will see the silver.  (As you can tell, this is based on a
    limited knowledge of these games, mostly "Adventure", but I'm
    trying to think multi-user.)

    At the start of the game, a player should be able to define his
    own gear, which would add against his weight allowance.  This can
    get tricky.  I'm unsure to say which would be better: a menu of
    things to pick from, which limits the player to the imagination
    of the creator, or a free-form definition concept which entails
    very fierce programming challenges, but really lets the
    user/player participate.  For example, the former would be
    something like:
    	
    PICK ITEMS FROM THIS LIST TO CARRY:

    ITEM                                         WEIGHT
    M16 Rifle, cal .223                          7 lb
    Heckler & Koch G3 Rifle, cal .308            10 lb
    Remington 870 12-ga Riot Shotgun             9 lb
    .223 Ammo                                    25 rounds = 1 lb
    .308 Ammo                                    16 rounds = 1 lb
    12 Ga Shells                                 10 rounds = 1 lb

    And there would be a table of equivalents to give the player some realistic
    tradeoff -- For example:

    KILL FACTORS
    Dwarf can be killed by 1 axe, 3 rounds .223, 1 round .308, 1 12-ga shell
    Man can be killed by 1 axe, 5 rounds .223, 2 rounds .308, 1 12 ga shell
    Dragon can be killed by 20 rounds .308, 15 12 ga shells, 1 anti-vehicle mine
    Griffin cannot be killed except by silver bullet

    And so forth.
    		
    Letting the user do the definitions can encourage cheating (e.g.,
    defining a 40 mm grenade launcher to be able to kill anything
    with one round but only weigh one lb and ammo defined as light as
    .223).  But it does encourage inventiveness and clever ideas
    (e.g., defining a metal detector which will uncover
    password-hidden metal treasures, as mentioned above, or defining
    an anti-gravity harness to enable carrying more weight [could
    this be cheating?  hard to say -- I like cleverness too much --
    its different from just lying about weights]).

    Of course, I don't mean this to just be weapons; that's just an
    area of familiarity for me.  The equipment list should have food,
    drink, maps, lamps, ropes, and all sorts of stuff.  It would be
    nice to let a frequent player set up a definition for himself
    which would have a set of standard equipment he could call up by
    saying "Take John Smith's Gear".

    This lets the player really participate.  He has to account for
    weight of weapons and ammo, and trade off ammo quantities for
    capabilities to carry other stuff.  Weapon is useless without
    ammo.  Some things are used up (ammo, food, drink) and the weight
    they took is then available.  Overcoming obstacles costs
    capabilities (ammo expended cannot be replenished until return to
    base, so you can only zap so many dwarves) and its all more real
    to the player.

    How does this fit into your concepts?  Anything of value to your
    efforts?  Hope it is of interest.

    Regards, Will Martin


17-Aug-80 01:41:07-EST,1251;000000000000
Mail from Host MIT-AI at 0141-EST on  17 Aug 1980
Date: 17 AUG 1980 0139-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    POURNE@MIT-MC 08/12/80 05:43:15 Re: the lord hath delivered him into my hands
        KLH@MIT-AI 08/12/80 05:10:40 Re: weep, wail
        	With all this talk of plots and such, what of those
        like me who, alas, may have some skill in imagining stories and
        perhaps even characters and settings and adventures; but who
        know none of these marvy concepts (flow, description etc) or at least do
        not know them well, and haven't time to do all the interminable
        practice and re-writes that publishable stories require?
        	Is there to be a way people like A. Hacker and myself can
        contribute to inventions of new novels?
        	K. L. Harrenstien
    __________________________________
    	I sincerely hope not.
    	Presumably the fallacy is clear.  I certainly shall
    survive, however, if KLH's views are those of most on the
    system.  I don't have to give away what I can sell for a lot of
    money; nor does he.
    	Welcome to the novelist trade, KLH.  I look forward to
    seeing your next publication.


17-Aug-80 01:41:29-EST,730;000000000000
Mail from Host MIT-AI at 0141-EST on  17 Aug 1980
Date: 17 AUG 1980 0140-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    POURNE@MIT-MC 08/12/80 05:03:17 Re: weep, wail
    	With all this talk of languages and such, what of those
    like me who, alas, may have some skill in imagining stories and
    perhaps even characters and settings and adventures; but who
    know none of these marvy languages (C LISP etc) or at least do
    not know them well, and haven't time to do all the interminable
    checkouts that compiled languages requrie?
    	Is there to be a way people like Niven and myself cna
    contribute to inventions of new adventures?
    	J. E. Pournelle


17-Aug-80 01:41:55-EST,2159;000000000000
Mail from Host MIT-AI at 0141-EST on  17 Aug 1980
Date: 17 AUG 1980 0141-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 12 AUG 1980 0058-EDT
    From: JBARRE at MIT-AI (Julie A. Barrett)

         It seems that science fiction is not the only medium in which it is 
    ok to go about killing people (or aliens) just for the sake of doing it.
    How many cop shows have you watched lately?  There are plenty of producers
    and writers who want to put out decent science fiction (and TV shows in
    general) but the networks, studios, etc. are the ones who insist on 
    putting out the crap we see day after ever loving day on the tube.  
    You might read Harlan Ellison's THE STARLOST because he talks about 
    that kind of activity by studios, networks, etc.  Also his GLAS TEAT
    series might be  interesting reading.

         But in a nutshell, the reason that this garbage permiates into
    ourliving rooms  is because the executives (praise A.C. Nielsen) think
    that we want to see that kind of drivel.    There's not much you can do
    esxept turn off the set.  If you want to join a group that is opposed
    to violence on TV, go to your local PTA.  They sponsor a national list
    of "good" and "bad" programs.  Of course, that is just "good" and
    "bad" in the view of the PTA, but they have genuienley tried to do 
    something about the violence on TV.Also try local church groups
    in your area.  These may be somewhat of a turnoff to some of you, but the
    fact is, they seem to be the only ones who are trying to do anything 
    about the issue.

         I think that right now the reason that science fiction is getting 
    a lot of attention in the violence area, is because that is the current 
    trend (or a current trend) in TV.  BS Dyslexia and other Star Wars/Close
    Encounters clones are on the air to make a buck, and let's face it, gang,
    violence is what sells.  It's a sad, but true commentary  on
    our way of life.
    --julie barrett


    P.S.  That's what makes certain games sell, also--violence.


17-Aug-80 01:57:25-EST,3171;000000000000
Mail from Host MIT-AI at 0157-EST on  17 Aug 1980
Date: 17 AUG 1980 0143-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 11 August 1980 22:37-EDT
    From: Gail Zacharias <GZ at MIT-MC>

    For your information: There is a C compiler available both for ITS and
    TOPS-20.  The compiler exists only at selected sites and is not available
    for random distribution, due to residual Bell Labs control over it, but
    of course once compiled you can transport the object code.  So I guess
    that leaves TENEX and TOPS-10.
    I still don't like the incongruity which will arise in the future when we
    will have with-phrases being only the noun, while other modifying phrases
    will include the preposition, but I agree it's not worth arguing about, so
    let's just leave it as is.  Now, what do we do about adjectives?  I.e.
    "get the green axe".  Again, the question is not necessarily should we
    include them in the minimal protocol, but how do we set up the minimal
    protocol so they can be included later in an upward compatible manner.

    Re combining adventures:
    Of course the auxilary variables used to describe the adventures are "local",
    since that is part of the adventure PROGRAMMING enviournment.  However,
    as I see it, the vocabulary is global.  That is part of the playing
    enviournment, which should be as isolated as possible from any knowledge
    of context switching, so that what you have is one big game, not just many
    small ones which happen to be invokable with the same system command.
    The result of having a global vocabulary is that you HAVE TO have adjectives.
    When you do have them, then an author could make reasonably sure that the
    object being referred to is his own, by qualifying it sufficiently, e.g.
    /big/warped/rusted/greenish/axe.  On the other hand, he might not care,
    and just say "axe", thus allowing axes to be brought in from other
    adventures.  greenish/axe would allow any greenish axe, whether warped,
    rusted, big or not. You do have the problem of a player/hacker who just
    can't find the big warped rusted greenish axe, so he writes a little
    adventure which all it has is a room with such an axe, and adds it into the
    game...   However, using the before: and after: mechanism, the author
    could make sure that the user has picked up HIS axe.  Note that I am
    considering the whole state-machine to be a global concept.  In fact,
    everything is global.  The state variables are local only in the sense
    that the interpreter will assure that there is no name conflicts, by,
    say, prefixing each variable with <adventure ID>$ in the symbol table.
    The interpretter-assigned unique adventure ID is available to the author
    at definition time as value of the variable MYADVENTURE.  There is
    also a variable CURRENTADVENTURE which at run time will hold the ID
    of the adventure the player is in.  Thus one can have conditionals on
    MYADVENTURE=CURRENTADVENTURE etc.  Some other global conventions might
    be needed too.


17-Aug-80 01:58:16-EST,1756;000000000000
Mail from Host MIT-AI at 0158-EST on  17 Aug 1980
Date: 17 AUG 1980 0144-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date:  11 August 1980 22:31 edt
    From:  SSteinberg.SoftArts at MIT-Multics (SAS at SAI-Prime)
    Sender:  COMSAT.SoftArts at MIT-Multics
    Original to:  info-mud at mit-ai

    Why a language?  Why not make it interactive?  You say NEWROOM
    and it creates a new room and asks Which way?  If there is already
    a room their it asks if you made a mistake, want to make it a random
    choice or require a password or object to make the room accessible.
    Then you are in the room.  There is nothing here.  You can then
    enter a description with the DESCRIBE verb which lets you describe
    things.  It might default to knowing about the passageway in.
    OLDROOM allows you to get into old rooms.  Rooms which you want
    "private" can be PROTECTED (maybe with a spell?)  so people
    can't create shortcuts.  You can CREATE a dragon or dwarf and
    TRAIN it.  IF DRAGON SEES EXTINGUISHER SAYS "That won't stop me."
    IF DRAGON HIT BY EXTINGUISHER DIES LEAVING Scattered wreckage and COKE
    BOTTLE.  There is a hairy language but one can write a "Magic Book"
    which describes incantations.  Perhaps you need the name of the Dragon
    Lord to create dragons.

    Internally you will need a state machine with hair for multiple
    "actors".  I wrote a dungeon language (single user) three or four
    years ago and found that a language was nice but static.  It really
    needed an easier way to grow, especially for scripting other creatures.
    I never quite got King Kong to leave foot prints after he escaped from
    you.




17-Aug-80 02:15:50-EST,3509;000000000000
Mail from Host MIT-AI at 0215-EST on  17 Aug 1980
Date: 17 AUG 1980 0145-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 11 Aug 1980 1127-PDT
    From: Chesley at SRI-KL

    	Re name: I know the S1 people are using Amber (and the names of all
    the major characters in the series).  However, it seems to me that there's
    enough distance between OS and MAD (multi-author Dungeon) work to keep the
    confusion at bay.  And Amber really is an appropriate name for such a language.
    	Re GZ's comments: Some excellent points.  My concern over simplicity
    is more than just a wish to get something up quickly; and I agree that LISP,
    C, FORTRAN, and BASIC implementations would cover just about everyone.  I am
    also concerned about the momentum of this discussion dying before we get
    something going: I'll do a C implementation, but we really need something
    for the 10's/20's if this is going to become a widely used system on the net,
    and I don't really have the experience with 10's/20's to do one (nor really
    do I have the time).  But this concern shouldn't stand in the way of doing
    a good and complete design.
    	I agonized for some time over numerical variables and property lists
    before deciding to take the simpler approach.  The point that an expression
    parser is already required is a good one (and expression parsers aren't
    really that hard to implement anyway).  So let's change that: SETVAR(<word>,
    <expression>) rather than SETTRUE and SETFALSE.  Given that, it's possible
    for the user to implement property lists: We can define room names as
    constants (the intepreter assigns unique numbers to each), and they can be
    used in expressions.  If we add dot (.) to the valid characters for a name
    (for mnemonic purposes), it becomes quite natural to define, say, axe.location
    as containing the room number where the axe is.  Thus, DROP(axe,thisroom)
    becomes SETVAR(axe.location,thisroom); using negative numbers for special
    locations, DROP(axe,neverneverland) becomes SETVAR(axe.location,-2), and
    TAKE(axe) becomes SETVAR(axe.location,-1).  To standardize things, we define
    two interpreter constants: "carrying" (-1) and "neverneverland" (-2).  In
    order to expand expressions to include integers, we need to add the operators
    +, -, *, /, % (modulo), and unary -; and we might as well add parentheses
    for grouping.  For realtional operators, we add =, >, <, >=, <=, and # (not
    equal to).  CARRYING(axe) then becomes (axe.location = carrying).
    	With the above changes, the OBJECT type in the word list goes away,
    and all objects are listed as NOUN's.
    	As to <preposition>/<word>, I more or less agree.  However, since
    "with x" is the most common usage, let's just say that anything beyond the
    <verb>/<object>/<with> is ignored; this allows for future expansion without
    extending or complicating the current definition.

    	I'd like to see more discussion of how to combine multiple adventures.
    I've described local adventures and how one might "call" another at the
    same level.  What I didn't talk about is global information (a global word
    list, for instance), or the idea of a tree structure (or tangled hierarchy).
    Obviously, many ideas can be taken from high level language design.
    	--Harry (apologizing for the longish message)...
    -------


17-Aug-80 02:16:23-EST,3509;000000000000
Mail from Host MIT-AI at 0216-EST on  17 Aug 1980
Date: 17 AUG 1980 0146-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    Date: 11 Aug 1980 1127-PDT
    From: Chesley at SRI-KL

    	Re name: I know the S1 people are using Amber (and the names of all
    the major characters in the series).  However, it seems to me that there's
    enough distance between OS and MAD (multi-author Dungeon) work to keep the
    confusion at bay.  And Amber really is an appropriate name for such a language.
    	Re GZ's comments: Some excellent points.  My concern over simplicity
    is more than just a wish to get something up quickly; and I agree that LISP,
    C, FORTRAN, and BASIC implementations would cover just about everyone.  I am
    also concerned about the momentum of this discussion dying before we get
    something going: I'll do a C implementation, but we really need something
    for the 10's/20's if this is going to become a widely used system on the net,
    and I don't really have the experience with 10's/20's to do one (nor really
    do I have the time).  But this concern shouldn't stand in the way of doing
    a good and complete design.
    	I agonized for some time over numerical variables and property lists
    before deciding to take the simpler approach.  The point that an expression
    parser is already required is a good one (and expression parsers aren't
    really that hard to implement anyway).  So let's change that: SETVAR(<word>,
    <expression>) rather than SETTRUE and SETFALSE.  Given that, it's possible
    for the user to implement property lists: We can define room names as
    constants (the intepreter assigns unique numbers to each), and they can be
    used in expressions.  If we add dot (.) to the valid characters for a name
    (for mnemonic purposes), it becomes quite natural to define, say, axe.location
    as containing the room number where the axe is.  Thus, DROP(axe,thisroom)
    becomes SETVAR(axe.location,thisroom); using negative numbers for special
    locations, DROP(axe,neverneverland) becomes SETVAR(axe.location,-2), and
    TAKE(axe) becomes SETVAR(axe.location,-1).  To standardize things, we define
    two interpreter constants: "carrying" (-1) and "neverneverland" (-2).  In
    order to expand expressions to include integers, we need to add the operators
    +, -, *, /, % (modulo), and unary -; and we might as well add parentheses
    for grouping.  For realtional operators, we add =, >, <, >=, <=, and # (not
    equal to).  CARRYING(axe) then becomes (axe.location = carrying).
    	With the above changes, the OBJECT type in the word list goes away,
    and all objects are listed as NOUN's.
    	As to <preposition>/<word>, I more or less agree.  However, since
    "with x" is the most common usage, let's just say that anything beyond the
    <verb>/<object>/<with> is ignored; this allows for future expansion without
    extending or complicating the current definition.

    	I'd like to see more discussion of how to combine multiple adventures.
    I've described local adventures and how one might "call" another at the
    same level.  What I didn't talk about is global information (a global word
    list, for instance), or the idea of a tree structure (or tangled hierarchy).
    Obviously, many ideas can be taken from high level language design.
    	--Harry (apologizing for the longish message)...
    -------


17-Aug-80 02:16:52-EST,1840;000000000000
Mail from Host MIT-AI at 0216-EST on  17 Aug 1980
Date: 17 AUG 1980 0146-EDT
From: CB at MIT-AI (Carl A. Baltrunas)
To: cb at SRI-TSC, baltrunas at NBS-10

    SGR@MIT-MC 08/05/80 20:44:10 Re: dungeon languages.
    Last spring I implemented a language based on DDL, which
    Mike Urban mentioned earlier.  I changed the syntax to be
    somewhat more LISPy, and played with a few new features.

    There is no official manual, but if you want to look at the
    programs, you're quite welcome.

    Source code for the compiler:  MC: ARCHIV; DL1 1
    Source code for the interpreter:  MC: ARCHIV; DL1RUN 1
    Source code for a simple-minded scenario: MC: ARCHIV; DANDUN DL1

    Object code for the compiler:  MC: ARCHIV; TS DL1
    Object code for the interpreter:  MC: ARCHIV; TS DL1RUN
    Object code for a simple-minded scenario:  MC: ARCHIV; DANDUN DL1RUN

    To try out the scenario, do
    :archiv;dl1run
    When it asks you for the dungeon filename, type
    archiv;dandun dl1run

    The programs are heavily documented, although somewhat cretinous
    in places.  Keep in mind that I did this for fun, not for a living,
    and invested only about a month's worth of spare time.

    As far as acknowledgements, I should say that Daniel Weise
    (DANIEL@ML) introduced me to DDL, which I modified somewhat (not much)
    into DL1.  The scenario is originally due to Mike Urban, I think.

    For the future, I'm designing DL2, which at the moment is somewhat
    like SCHEME (a lexically scoped dialect of LISP due to GJS & GLS @ AI).

    	-$teve

    p.$.  The behavior of this set of programs has a few bugs...  Also, it
    takes a LOOOOONNG time to load, due to some simple-minded implementation
    details. I wanted to get it working soon, and performance suffered.



 @{á