$PAGE line sequence abstraction
(*  The LineSequence abstraction allows manipulation of sequences of text
    lines.  The operations in the abstraction are:

	CreateLineSequence : -> LineSequence
		creates and returns an empty line sequence
	AppendLineToSequence : LineSequence x Line -> LineSequence
		adds a line to the end of an existing line sequence
	ResetLineSequence : LineSequence -> LineSequence
		enables reading of lines starting with the first line
		in a sequence
	EndOfLineSequence : LineSequence -> Boolean
		returns True if all lines in a sequence have been read,
		or if the sequence has never been reset
	ReadLineFromSequence : LineSequence -> LineSequence x Line
		returns the "current" line in a sequence, and advances a
		cursor to the "next" line in the sequence
	DestroyLineSequence : LineSequence ->
		frees any storage used by a line sequence
	SaveLineSequence : File x LineSequence -> File x Location
		saves a line sequence on a binary file, returning its
		starting cursor position
	RestoreLineSequence : File x Location -> File x LineSequence
		reads and returns a line sequence from a binary file,
		starting at a specified cursor position			*)

type
   line_in_sequence_pointer = ^ line_in_sequence_record;

   line_sequence = record
	first_line, last_line, cursor: line_in_sequence_pointer
   end;

   line_in_sequence_record = record
	next: line_in_sequence_pointer;
	text: packed array [1..*] of char
   end;

$PAGE create_line_sequence - in line sequence abstraction
(*  CREATE LINE SEQUENCE will return an new, empty LineSequence.  *)

procedure create_line_sequence (var seq: line_sequence);

begin
   seq := (nil, nil, nil)
end;
$PAGE append_line_to_sequence - in line sequence abstraction
(*  APPEND LINE TO SEQUENCE will modify an existing line sequence by adding
    a specified line at its end.  *)

procedure append_line_to_sequence (var seq: line_sequence;
				   line: packed array [1..*] of char);

var
   temp: line_in_sequence_pointer;

begin
   with seq do begin
      new (temp, length (line));
      temp^.next := nil;
      temp^.text := line;
      if first_line = nil then
	 first_line := temp
      else
	 last_line^.next := temp
      last_line := temp
   end
end;
$PAGE reset_line_sequence - in line sequence abstraction
(*  RESET LINE SEQUENCE resets the internal cursor of a line sequence so
    that the next ReadLineFromSequence operation will read the first line
    of the sequence.  *)

procedure reset_line_sequence (var seq: line_sequence);

begin
   seq.cursor := seq.first_line
end;
$PAGE end_of_line_sequence - in line sequence abstraction
(*  END OF LINE SEQUENCE returns true if all lines in a sequence have been
    read, or if the sequence has never been reset.  *)

function end_of_line_sequence (seq: line_sequence): boolean;

begin
   end_of_line_sequence := (seq.cursor = nil)
end;
$PAGE read_from_line_sequence - in line sequence abstraction
(*  READ LINE FROM SEQUENCE will return the text of the line indicated by
    the internal cursor in a line sequence, and will advance the cursor to
    the next line in the sequence.  *)

procedure read_line_from_sequence (var seq: line_sequence; var line: string [*]);

begin
   assert (seq.cursor <> nil);
   line := seq.cursor^.text;
   seq.cursor := seq.cursor^.next
end;
$PAGE destroy_line_sequence - in line sequence abstraction
(*  DESTROY LINE SEQUENCE frees any storage used by a line sequence.  *)

procedure destroy_line_sequence (var seq: line_sequence);

begin
   with seq do begin
      while first_line <> nil do begin
	 cursor := first_line;
	 first_line := first_line^.next;
	 dispose (cursor)
      end
   end
end;
$PAGE save_line_sequence - in line sequence abstraction
(*  SAVE LINE SEQUENCE will write a line sequence to a binary file,
    returning a cursor which can be used to retrieve the sequence from
    the file.  *)

procedure save_line_sequence (var lsf: file of *; seq: line_sequence; var loc: integer);

var
   len: integer;
   scan: line_in_sequence_pointer;

begin
   loc := cursor (lsf);
   scan := seq.first_line;
   while scan <> nil do begin
      len := length (scan^.text);
      write (lsf, len, scan^.text: size (scan^.text, len));
      scan := scan^.next
   end;
   len := -1;
   write (lsf, len)
end;
$PAGE restore_line_sequence - in line sequence abstraction
(*  RESTORE LINE SEQUENCE will read a line sequence back in from a binary
    file, starting at a specified cursor.  *)

procedure restore_line_sequence (var lsf: file of *; loc: integer; var seq: line_sequence);

begin
   seq := (nil, nil, nil);
   seek (lsf, loc);
   read (lsf, len);
   while len <> -1 do begin
      new (temp, len);
      read (lsf, temp^.text: size (temp^.text, len));
      temp^.next := nil;
      if seq.first_line = nil then
	 seq.first_line := temp
      else
	 seq.last_line^.next := temp;
      seq.last_line := temp
   end
end;
   