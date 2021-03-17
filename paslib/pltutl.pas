$TITLE pltutl

$PAGE graphics type definitions
$include pltutl.typ


const
  zero_pt: point := ( 0, 0, 0 );

  ident_matrix: matrix := 
     (	1, 0, 0, 0,
	0, 1, 0, 0,
	0, 0, 1, 0,
	0, 0, 0, 1  );


$PAGE external entries into pltpac[52260,255]

external procedure origin ( x,y: real );
external procedure movep ( x,y: real );
external procedure drawp ( x,y: real );
external procedure ploton ( t: plotters );
external procedure plotoff;
external procedure plotpen ( c: pen_colors );
type int = 0..10000;
external procedure plotspeed ( s: int );
$PAGE plotter name array
type pltnms = array [hpa..tek] of string[3];

const pltr_names: pltnms :=
	(  'hpa',
	   'hpb',
	   'tek'    );
$PAGE plt_init and static data
public var
  plt: plt_state;		(* stackable state infomation *)

static var
  xorg, yorg: real;		(* virtual origin of output device *)
  xscale, yscale: real;		(* scaling to map data window to device coordinates *)

  plotter_on: boolean;		(* current state of plotter *)



(* INIT initializes all static data (above) required by this package. *)

public procedure plt_init ( pltr_name: string; var errinname: boolean );
 var nm: plotters;
 begin
  rewrite (output, 'plt.dmp');
  plotter_on := false;
  plt.window := ident_matrix;		(* initial state, no transformations *)
  plt.transform := ident_matrix;
  plt.cursor := zero_pt;
  plt.color := none;
  plt.plotter_type := ask;
  for nm := hpa to tek
    do exit if pltr_names [nm] = lowercase (pltr_name) do plt.plotter_type := nm;
  errinname := plt.plotter_type = ask;
  xorg := 0; yorg := 0;		(* default to no scaling or mapping on layout *)
  xscale := 1; yscale := 1;
  origin (0,0);			(* pltpac does not initialize this *)
 end;
$PAGE plt_on, plt_off

(* These routines turn the plotter on and off, and remember the state of the 
   plotter.  In the event of an error, we always try to turn the plotter off. *)

public procedure plt_on;
 begin
  ploton (plt.plotter_type);				(* ask user what type he wants *)
  plotter_on := true;
 end;


public procedure plt_off;
 begin
  if plotter_on then begin		(* only issue commands to the plotter if needed *)
    plotoff;
    plotter_on := false
  end
 end;
$PAGE plt_color, plt_speed
(* COLOR simply sets the current pen color. *)

public procedure plt_color ( color: pen_colors );
 begin
  plt.color := color;
  plotpen (color);
 end;



(* SPEED sets the pen movement speed for interested devices. *)

public procedure plt_speed ( speed: real );
 begin
  plotspeed ( trunc (speed) )
 end;
$PAGE mul4x4, ptxmat, radians
(* MUL4X4 multiplies two 4x4 matrices *)

external function mul4x4 (a,b: matrix): matrix;	(* assembly language helper *)


(* PTXMAT multiplies a point by a matrix.  There is a assumed fourth component
   of the point vector, which is one;  this makes the translation come out right. *)

function ptxmat (pt: point; mat: matrix): point;
 begin
  with ptxmat do begin
    x := (pt.x * mat[1,1]) + (pt.y * mat[2,1]) + (pt.z * mat[3,1]) + mat[4,1];
    y := (pt.x * mat[1,2]) + (pt.y * mat[2,2]) + (pt.z * mat[3,2]) + mat[4,2];
    z := (pt.x * mat[1,3]) + (pt.y * mat[2,3]) + (pt.z * mat[3,3]) + mat[4,3];
  end
 end;



(* RADIANS simply converts degrees to radians *)

function radians (r: real): real;
 begin
  radians := (r * 3.1415923) / 180;
 end;
$PAGE plt_layout
(* LAYOUT performs a mapping from the windowed data onto the actual plotter
   surface.  The user specifies a box to receive the plotted data by giving
   its bottom-left and upper-right corners.  Scaling factors and the x-y 
   origin are computed from this. *)

public procedure plt_layout ( blx, bly, urx, ury: real );
 begin
  xorg := (blx + urx) / 2;	(* get origin of box *)
  yorg := (bly + ury) / 2;

  xscale := (urx - blx) / 2;	(* scaling factors *)
  yscale := (ury - bly) / 2;
 end;
$PAGE plt_window
(* WINDOW designates the portion of the data space which is to be viewed.  It
   causes all data outside of the window to be clipped.  It also introduces
   a transformation which translates and scales the data into a box whose
   x, y, and z extents are -1 to 1. *)

public procedure plt_window ( blf: point;		(* bottom, left, front vertex *)
		       urb: point  );	(* upper, right, back vertex *)

 const minus2 = -2.0;			(* can't have this directly in prog *)
 var mat1, mat2: matrix;

 begin
   writeln ('w', blf.x, blf.y, blf.z, urb.x, urb.y, urb.z);
  (* Derive scaling factor for box. *)

  mat1 := ident_matrix;
  mat1[1,1] := 2 / (urb.x - blf.x);
  mat1[2,2] := 2 / (urb.y - blf.y);
  mat1[3,3] := 2 / (urb.z - blf.z);

  (* Derive the required translation to center window in box *)

  mat2 := ident_matrix;
  mat2[4,1] := (urb.x + blf.x) / minus2;
  mat2[4,2] := (urb.y + blf.y) / minus2;
  mat2[4,3] := (urb.z + blf.z) / minus2;

  (* Create initial transformation matrix *)

  plt.window := mul4x4 (mat2, mat1);		(* compute compound transformation *)
  plt.transform := plt.window;			(* make it the current transform *)
 end;
$PAGE plt_scale plt_translate
(* SCALE appends a scaling transformation to the master transformation matrix,
   and updates the master copy. *)

public procedure plt_scale (sc: xyz);
 var mat: matrix;			(* new scaling matrix *)
 begin
  mat := ident_matrix;			(* initialize most components *)
  mat[1,1] := sc.x;
  mat[2,2] := sc.y;
  mat[3,3] := sc.z;
  plt.transform := mul4x4 (mat, plt.transform);	(* apply transformation *)
 end;



(* TRANSLATE concatenates a translation mapping to the current transformation
   matrix. *)

public procedure plt_translate (tr: xyz);
 var mat: matrix;			(* new transformation matrix *)
 begin
  mat := ident_matrix;
  mat[4,1] := tr.x;
  mat[4,2] := tr.y;
  mat[4,3] := tr.z;
  plt.transform := mul4x4 (mat, plt.transform)
 end;
$PAGE plt_rotate
(* ROTATE concatenates a rotation mapping to the current transformation matrix. *)

public procedure plt_rotate (rot: xyz);
 var mat: matrix;
     r: real;
 begin
  if rot.x <> 0 then begin		(* need only do below, if non-zero *)
    mat := ident_matrix;
    r := radians (rot.x);
    mat[2,2] := cos (r);
    mat[3,3] := mat[2,2];
    mat[2,3] := sin (r);
    mat[3,2] := - mat[2,3];
    plt.transform := mul4x4 (mat, plt.transform)
  end;
  if rot.y <> 0 then begin
    mat := ident_matrix;
    r := radians (rot.y);
    mat[1,1] := cos (r);
    mat[3,3] := mat[1,1];
    mat[3,1] := sin (r);
    mat[1,3] := - mat[3,1];
    plt.transform := mul4x4 (mat, plt.transform);
  end;
  if rot.z <> 0 then begin
    mat := ident_matrix;
    r := radians (rot.z);
    mat[1,1] := cos (r);
    mat[2,2] := mat[1,1];
    mat[1,2] := sin (r);
    mat[2,1] := - mat[1,2];
    plt.transform := mul4x4 (mat, plt.transform)
  end
 end;
$PAGE plt_move, plt_draw
public procedure plt_move (pt: point);
 var p: point;
 begin
  writeln (pt.x, pt.y, pt.z);
  plt.cursor := pt;
  p := ptxmat (pt, plt.transform);
  writeln (p.x, p.y, p.z);
  movep ( (p.x * xscale) + xorg, (p.y * yscale) + yorg );
  writeln ('m', (p.x * xscale) + xorg, (p.y * yscale) + yorg );
 end;


public procedure plt_draw (pt: point);
 var p: point;
 begin
  plt.cursor := pt;
  p := ptxmat (pt, plt.transform);
  drawp ( (p.x * xscale) + xorg, (p.y * yscale) + yorg );
  writeln ('d', (p.x * xscale) + xorg, (p.y * yscale) + yorg );
 end.
