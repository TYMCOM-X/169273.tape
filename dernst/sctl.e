/* -------------------------------------------------------------------------
   This file contains external declarations for routines of SCR_CTRL.OBJ.
   ------------------------------------------------------------------------- */
#define  up    0
#define  down  1
#define  right 2
#define  left  3
 
extern	 int   sc_movcur (/* move_dir, move_cnt*/);
 
extern	 int   sc_up (/* num_up */ );
extern	 int   sc_down ( /* num_down */ );
extern	 int   sc_right ( /* num_right */ );
extern	 int   sc_left ( /* num_left */ );
extern	 int   sc_gotoxy ( /* column, line */ );
 
extern	 int   sc_bcspac (/* num_back */);
extern	 int   sc_wait ();
extern	 int   sc_beep ();
 
extern	 int   sc_clrscr ();
extern	 int   sc_clreol ();
extern	 int   sc_clrlin ( /* col, line1, line2 */);
 
extern	 int   sc_savcsr ();
extern	 int   sc_rstcsr ();
   