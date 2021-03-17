
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 303*)
(*TEST 6.9.4-10, CLASS=QUALITY*)
(* This program checks that data written appears on the output
  file regardless of the omission of a line marker. The common
  error is to buffer output and fail to flush the buffers at
  end of job. *)
program t6p9p4d10;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #303');
   write(' OUTPUT IS FLUSHED AT END_OF_JOB...6.9.4-10')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

