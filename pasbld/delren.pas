$TITLE Delete_File and Rnam_File

$OPTIONS nocheck, special

module delren;

type
  fname_str = string [60];

external procedure openfile  ( fname_str; var integer; var boolean;
			       boolean; boolean );
external procedure closefile ( integer; boolean );
external procedure renamfile ( integer; fname_str );




public procedure del_file ( name: fname_str );

var channel: 0 .. 15;
    aok: boolean;

begin
  openfile (name, channel, aok, false, false);
  if aok then
    closefile (channel, true);
end;




public procedure rnam_file ( old_name, new_name: fname_str );

var channel: 0 .. 15;
    aok: boolean;

begin
  del_file (new_name); (* Can't rename to an existing name. *)
  openfile (old_name, channel, aok, false, false);
  if aok then
    renamfile (channel, new_name);
end.
   