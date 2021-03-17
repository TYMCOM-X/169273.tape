Program SCAN;

Const ArrayDim = 300;
      NumOfUsersToList = 10;

Type
    SquareArray = array[1..ArrayDim,0..ArrayDim] of Integer;
    Nam_Rec =  record
                 Name : string;
                 Counter : Integer;
                 indexPTR : Integer;
               end;
    Vect = array[1..ArrayDim] of Nam_Rec;

Var i,
    Number_Of_Progs,
    Number_Of_Users : Integer;
    NumOfScans : Integer;
    Currentline,
    DirName,
    ProgName,
    UserName,
    Str,S : String;
    PFl,
    report,
    input,
    TermIn  : text;
    MainArray : SquareArray;
    Prog_Table,
    User_Table : Vect;
    Continue : char;
    InFile,ProgFile,OutFile  : Packed Array[1..10] of char;


Procedure Blanklines(var Which_File : Text;
                         How_Many   : Integer);
var
       I : Integer;
begin
  For I := 1 to How_Many do
    Writeln(Which_File);
end; (* procedure blanklines *)


Procedure Initialize_arrays;
var i,j : Integer;
begin
  writeln(tty,'Initializing arrays.');break(tty);
  for i := 1 to ArrayDim do
    begin
      Prog_Table[i].Name := '';
      Prog_Table[i].Counter := 0;
      Prog_Table[i].indexPTR := 0;
      User_Table[i].Name := '';
      User_Table[i].Counter := 0;
      User_Table[i].indexPTR := 0;
      for j := 1 to ArrayDim do
        MainArray[i,j] := 0;
    end;  (* for i *)
end;  (* initialize_arrays *)

Function Get_A_Line:String;
var
       Str : String;
Begin
  Readln(input,Str);
  Get_A_Line := Str;
end;

Function Get_DirName(OneLine : String):String;
var
       Str : string;
       S   : char;
       i,j : Integer;
Begin
  S := ' ';Str := '';
  i := 1; (* bypass first char *)
  While not (S in [ '(','[' ] ) do
     (* move to beginning of Directory Name *)
    begin
      i := i + 1;
      S := OneLine[i];
    end; (* while *)
  While not (OneLine[i] in [ ')',']' ] ) do
    begin
      Str := Str || S;
      i := i + 1;
      S := OneLine[i];
    end; (* while *)
  Str := Str || ')';
  Get_DirName := Str;
end; (* function Get_DirName *)

Function Get_UserName(OneLine : String):String;
var
       Str : string;
       i,j : Integer;
Begin
  Str := '';
  if OneLine[1] in [ '(','[' ] then
    i := 1
  else
    i := 0;
  Repeat
    i := i + 1;
    Str := Str || OneLine[i];
  Until OneLine[i+1] in [ ')',']' ];
  Get_UserName := Str;  
end;

Function Get_ProgName(OneLine : String):String;
var
       TempS : string;
       i     : Integer; 
Begin
  i := 1;
  Repeat
    i := i + 1;
  Until OneLine[i] in [ ')',']' ];
  Repeat
    i := i + 1;
  Until OneLine[i] in [ ')',']' ];
  i := i + 1;
  TempS := '';
  While i <> length(oneline) do
    begin
      TempS := TempS || OneLine[i];
      i := i + 1;
    end; (* while *)
  Get_ProgName := TempS || OneLine[i];
end;

Function Search_Table(    Item     : string ;
                      Var Table    : Vect   ):Integer;
var
       Not_Found : boolean;
       i : Integer;
begin
  i := 1;Not_Found := true;
  while Not_Found do
    begin
      if Item = Table[i].Name then
        begin
          Not_Found := false;
          Search_Table := i;
          Table[i].indexPTR := i;
          Table[i].Counter :=Table[i].Counter + 1;
        end (* if Item *)
      else
        if Table[i].Name = '' then
          begin
            Table[i].Name := Item;
            Not_Found := false;
            Search_Table := i;
            Table[i].indexPTR := i;
            Table[i].Counter := 1;

          end  (* if Table[i].Name *)
        else
          i := i + 1;
    end; (* while Not_Found *)
end; (* Search_Table *)

Function Count_Elements(Table : Vect):Integer;
var
       Count : Integer;
begin
  write(tty,'Counting elements, ');break(tty);
  Count := 1;
  if Table[ArrayDim].Name = '' then
    begin
      while Table[Count].Name <> '' do
        Count := Count + 1;
      Count_Elements := Count - 1;
    end
  else
    Count_Elements := ArrayDim;
  writeln(tty,count - 1,' counted.');break(tty);
end; (* Count_Elements *)



Procedure Post_To_MainArray(Var MainArray  : SquareArray;
                                 Vertical   : Integer;
                                 Horizontal : Integer);
begin
  MainArray[Vertical,Horizontal] := MainArray[Vertical,Horizontal] + 1;
end; (* post_to_MainArray *)


Procedure ReadInLog;
var
       User_Location,
       Prog_Location,
       Num_Of_Users,
       Max_Loc     : Integer;
       UN,DN,PN    : string;
       done        : boolean;

Begin (* ReadInLog *)
  write(tty,'Reading ',InFile,',');break(tty);
  NumOfScans := 0;Max_Loc := 0;Num_Of_Users := 0;done := false;
  CurrentLine := Get_A_Line;
  writeln(tty);break(tty);
  repeat
      NumOfScans := NumOfScans + 1;
      if NumOfScans MOD 300 = 0 then
        begin
          write(tty,NumOfScans:6,' ');break(tty);
          if NumOfScans MOD 3000 = 0 then
            writeln(tty);
        end; (* if *)
      PN := Get_Progname(CurrentLine);
      DN := Get_DirName(CurrentLine);
      PN := PN || DN;
      Prog_Location := Search_Table(PN,Prog_Table);
      UN := Get_UserName(CurrentLine);
      User_Location := Search_Table(UN,User_Table);
      if User_Location > Num_Of_Users then
        Num_Of_Users := User_Location;
      if Prog_Location = ArrayDim then
      begin
        writeln(tty,'!!! Number of Progs is now at max !!!');
        done := true;
        break(tty);
      end; (* if *)
      if User_Location = ArrayDim then
      begin
        writeln(tty,'!!! Number of Users is now at max !!!');
        done := true;
        break(tty);
      end; (* if *)
      Post_To_MainArray(MainArray,Prog_Location,User_Location);
      CurrentLine := Get_A_Line;
    Until EOF(input) or done;
  writeln(tty,'  ',NumOfScans,' lines read.');break(tty);
end; (* ReadInLog *)

Procedure Sort(Var r  : Vect;
                   lo,
                   up : Integer);
var
       i : Integer;
       TempR : Nam_Rec;


Procedure SiftUp(Var r  : Vect;
                     i,
                     n  : Integer);
var
       j,temp_i : Integer;
       TempR : Nam_Rec;
begin (* SiftUp *)
  temp_i := i;
  while 2 * temp_i <= n do
    begin
      j := 2 * temp_i;
      if j < n then
        if r[j].counter < r[j+1].counter then
          j := j + 1;
      if r[temp_i].counter < r[j].counter then
        begin
          TempR := r[j];
          r[j] := r[temp_i];
          r[temp_i] := TempR;
          temp_i := j;
        end (* if *)
      else
        temp_i := n + 1
    end (* while *)
end; (* SiftUp *)

begin (* Sort *)
  (* construct heap *)
  for i := (up DIV 2) downto 2 do
    SiftUp(r,i,up);
  (* repeatedly extract maximum *)
  for i := up downto 2 do
    begin
      SiftUp(r,1,i);
      TempR := r[1];  
      r[1] := r[i];
      r[i] := TempR;    
    end; (* for *)
end; (* Sort *)

Procedure AlphaSort(Var r  : Vect;
                        lo,
                        up : Integer);
var
       i : Integer;
       TempR : Nam_Rec;


Procedure SiftUp(Var r  : Vect;
                     i,
                     n  : Integer);
var
       j,temp_i : Integer;
       TempR : Nam_Rec;
begin (* SiftUp *)
  temp_i := i;
  while 2 * temp_i <= n do
    begin
      j := 2 * temp_i;
      if j < n then
        if r[j].Name < r[j+1].Name then
          j := j + 1;
      if r[temp_i].Name < r[j].Name then
        begin
          TempR := r[j];
          r[j] := r[temp_i];
          r[temp_i] := TempR;
          temp_i := j;
        end (* if *)
      else
        temp_i := n + 1
    end (* while *)
end; (* SiftUp *)

begin (* AlphaSort *)
  (* construct heap *)
  for i := (up DIV 2) downto 2 do
    SiftUp(r,i,up);
  (* repeatedly extract maximum *)
  for i := up downto 2 do
    begin
      SiftUp(r,1,i);
      TempR := r[1];  
      r[1] := r[i];
      r[i] := TempR;    
    end; (* for *)
end; (* AlphaSort *)



Procedure List_Users(    Table_Line  : Integer;
                     var Report_File : Text   );
Type Pointer = ^Node;
     Node    = Record
                 Name       : String;
                 Counter    : integer;
                 Next       : Pointer;
               end;
var
       ListHead : Pointer;
       i,Space,Num_Of_Usrs : Integer;

Procedure Insert(Rec : Nam_Rec);
var
       Entry,PriorEntry,NewEntry : Pointer;
       Searching : boolean;
       i : integer;
begin (* Insert *)
  Entry := ListHead;
  Searching := true;
  While Searching and (Entry <> nil) do 
    if Rec.Counter > Entry^.Counter then
      Searching := false
    else
      Begin
        PriorEntry := Entry;
        Entry := Entry^.Next;
      end; (* else *)
  New(NewEntry);write(tty,'N');break(tty);
  NewEntry^.Name := Rec.Name;
  NewEntry^.Counter := Rec.Counter;
  NewEntry^.Next := Entry;
  if Entry = ListHead then
    ListHead := NewEntry
  else
    PriorEntry^.Next := NewEntry;
end; (* insert *)

Procedure WriteList;
var
       Entry,D : Pointer;
       More    : Boolean;
       i,count : integer;
begin (* writeList *)
  More := true;
  Entry := ListHead;
  Count := 0;
  While (Entry <> nil) and More do
    begin
      write(report_File,Entry^.Name);
      for i := 1 to (10 - Length(Entry^.Name)) do
        Write(report_File,' ');
      writeln(report_File,Entry^.Counter:1);
      Count := Count + 1;
      if Count = NumOfUsersToList then
        More := False;
      D := Entry;
      Entry := Entry^.Next;
      Dispose(D);write(tty,'D');break(tty);
    end; (* While *)
  While Entry <> nil do
    begin
      D := Entry;
      Entry := Entry^.Next;
      Dispose(D);write(tty,'D');break(tty);
    end; (* While *)
end; (* WriteList *)

Function Count_Usrs(Line_Num : Integer):Integer;
var
       i,Count : integer;
begin
  Count := 0;
  For i := 1 to ArrayDim do
    begin
      if MainArray[Line_Num,i] > 0 then
        Count := Count + 1;
    end; (*for *)
  Count_Usrs :=  Count;
end; (* function Count_Users *)

begin (* List_Users *)
  ListHead := nil;
  for i := 1 to ArrayDim do
    User_Table[i].Counter := MainArray[Table_Line,i];
  Num_Of_Usrs := Count_Usrs(Table_Line);
  if Num_Of_Usrs > NumOfUsersToList then
    writeln(Report_File,'                           Top Ten Users:');
  for i := 1 to ArrayDim do
    if User_Table[i].Counter > 0 then
      begin
        Insert(User_Table[i]);(* ,, *)
      end;
  WriteList;
  writeln(tty);break(tty);
end; (* list_users *)


Procedure Write_A_File(Prog_Recs   : Vect;
                     Num_Of_Recs : Integer;
                     Direction   : char;
                 var Which_File  : text);
Var
       i,j,linecount : Integer;
Begin
  Writeln(tty,'Write_A_File');
  linecount := 0;
  if Direction = 'B' then
    writeln(Which_File,' USER NAMES')
  else
    writeln(Which_File);
  if Direction = 'R' then
    for i := Num_of_Recs downto 1 do
        begin
          linecount := linecount + 1;
          if Which_File = Report then
            (* no line numbers unless File = Report *)
            write(Which_File,linecount:3,'. ');
          with Prog_Recs[i] do
            begin
              write(Which_File,Name);
              if Which_File = Report then
                (* spaces not needed in this file *)
                for j := 1 to (18 - (length(Name))) do
                  write(Which_File,' ');
              write(Which_File,counter);
            end; (* with Prog_Recs *)
          writeln(Which_File);
        end (* for *)
  else
    if (Direction = 'B') or (Direction = 'F') then
      begin
        writeln(Which_File);
        for i := 1 to Num_Of_Recs do
          begin
            write(Which_File,'--------------------------');
            writeln(Which_File,'--------------------------');
            write(Which_File,i:3,'. ');
            with Prog_Recs[i] do
              begin
                write(Which_File,Name);
                for j := 1 to (18 - (length(Name))) do
                  write(Which_File,' ');
                write(Which_File,counter);
                if  Direction = 'B' then
                  begin
                    writeln(Which_File);
                    List_Users(indexPTR,Report);
                    writeln(Which_File);
                  end (* if *)
                else
                   writeln(Which_File);
              end; (* with Prog_Recs *)
          end; (* for *)
      end; (* if *)
end; (* Write_A_File *)





Begin  (**** MAIN ****)
  open(tty); Rewrite(tty);
  writeln(tty,'Begin p1');break(tty);
  writeln(tty);break(tty);
  Write(tty,'Enter file to be scanned = ');break(tty);
  i := 1;
  readln(tty);
  while not EOLN(tty) do
    begin
      Read(tty,InFile[i]);
      i := i + 1;
    end; (* while not EOLN *)
  for i := i to 10  do
    InFile[i] := ' ';
  Reset(input,InFile);
  open(report);Rewrite(report);
  open(PFl,ProgFile);Rewrite(PFl);
  If EOF(Input) then
    Writeln(tty,'   ***  FILENAME ',InFile,' NOT FOUND  ***')
  else
    begin
      Initialize_arrays;
      ReadInLog;

      Number_Of_Progs :=  Count_Elements(Prog_Table);
      Number_Of_Users := Count_Elements(User_Table);

      Sort(Prog_Table,1,Number_Of_Progs);

      writeln(tty,'Writing Prog List to Report.PIO.');break(tty);
      writeln(report,'Prog Table after sort : ');
      BlankLines(report,2);
      writeln(report,'  PROGRAM             TIMES');
      writeln(report,'   NAME                USED       ');
      Write_A_File(Prog_Table,Number_Of_Progs,'R',report);
      writeln(tty,'Writing to PFL.PIO.');break(tty);
      Write_A_File(Prog_Table,Number_Of_Progs,'R',PFl);

      AlphaSort(Prog_Table,1,Number_Of_Progs);
      writeln(tty,'writing Alpha-ProgNames to Alpha.PIO.');break(tty);
      BlankLines(Report,10);
      writeln(Report,'Prog Table after AlphaSort : ');
      writeln(Report);writeln(Report);break(tty);
      writeln(Report,'  PROGRAM             TIMES');
      write(Report,'   NAME                USED       ');
      Write_A_File(Prog_Table,Number_Of_Progs,'B',Report);


    end; (* if EOF(Input) *)
  Writeln(tty,'End Scan.PAS');
end. (* Main  program *)
                 
                     