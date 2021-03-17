
PEAK MACRO SYSTEM						Page 1
Introduction


  Introduction

    The macro system built into the PEAK editor provides a very
  powerful extension capability.  If a command doesn't do exactly
  what is wanted, a list of commands may be combined together to
  perform that function.  This command list is called a MACRO.

    Macros are formed in two ways:  by typing a list of commands
  enclosed between two delimiters (for example, a quoted string)
  or by making an example of the actions desired and storing the
  commands as they are typed.  In either case, the end result is
  a string of command characters which is remembered by PEAK as the
  current Keyboard Macro.

    PEAK then allows you to execute this command string, regardless of
  length, by typing a single command (Execute Macro).  This facility
  is quite basic and remembers only a single macro at a time.  As soon
  as the user decides to create another list of commands, the previous
  macro is forgotten.



  Binding and UnBinding

    Additional comands allow the user to save these macros before they
  are forgotten.  They may be bound to a key (Bind Macro) or given a
  suitable name (Name Macro) at the discretion of the user.  The act
  of binding or naming the Keyboard Macro makes a copy of the macro in
  a safe place which will be remembered until the user un-binds the key
  or un-names the macro.

    When a macro is bound to a key, the previous binding of that key is
  remembered.  When the key is to be unbound, if the key is bound to a
  macro the most previous binding is replaced.  If the key is bound to
  an internal PEAK function, it may not be unbound and an error message
  is displayed.

    When a name is bound to a macro, the given name is pre-pended to a
  list.  This list is checked whenever the internal PEAK function name
  table is searched.  In this sense, named macros operate like internal
  PEAK functions.  That is, they are executed via the Execute Function
  command and they may be bound to keys with the Bind Function command.

    However, there is one major difference.  When a macro is named, any
  named macros previously defined or internal PEAK functions with the
  same name become "stacked".  Any new references to the same function
  name will refer to this named macro instead.  When the most recent
  macro with a specific name becomes un-named, it is "unstacked" and the
  next previously defined macro or internal PEAK function of the same
  name will be available.  Note that creating a new macro name does not
  affect any existing key bindings.



PEAK MACRO SYSTEM						Page  2
The Functions


  The Functions

    Define Macro		defines a macro as a delimited string
    ( Xmeta-D )			of commands.  The first character that
				is entered is used as the delimiter.
				All command characters entered until a
				second delimiter is entered become part
				of the macro.


    Define Macro By Example	defines a macro as the user edits the
				current buffer.  All commands entered
				until an "Abort Command" (which is bound 
				to Control-G) is entered become part of
				the macro.  This definition mode is very
				useful because the user can watch what
				each of the commands do while they are
				entered.


    Execute Macro		executes the currently defined Keyboard
				Macro.  If no macro is defined, an error
				message is displayed.


    Bind Macro			makes a copy of the currently defined
    ( Xmeta-M )			Keyboard Macro and binds the copy to the
				specified Key.  To execute the macro,
				simply type the specified command key.


    Name Macro			makes a copy of the currently defined
				Keyboard Macro and gives it the name as
				entered by the user.  If this name is
				the same as any internally defined PEAK
				function, then any future binding or
				execution of the function name will use
				the new function.  (See above for more
				about this.)


    Bind Function		replaces the current binding of the key
				specified, with the function specified
				by the user.  If the name of a macro is
				given, then the named macro is copied
				and bound to the specified key.  (See
				above for more about this.)


PEAK MACRO SYSTEM						Page  3
The Functions


    UnName Macro		removes the macro with the specified
				name if one exists.  If more than one
				named macro or an internal PEAK function
				has the same name, then the first macro
				found becomes un-named and is forgotten.
				Future references to that name will find
				the most previous macro with the same
				name or the internal PEAK function (if
				there are no matching macros).  If no
				macro is found to match, then an error
				message is displayed.


    UnBind Key			removes the most current macro binding
				from the specified Key and remembers
				the most recent previous binding of that
				Key.  If the key is bound to a function,
				then the key cannot be unbound and an
				error message is displayed.  (To un-bind
				a key bound to a function, the user must
				rebind the key to another function or a
				macro.)


    Query Function		displays the name of the function which
    ( Xmeta-? )			is executed when the specified Key is
				entered.  If the Key is bound to a macro,
				then the name (if any) is displayed and
				as much of the macro command as will fit
				in the remainder of the message line.


