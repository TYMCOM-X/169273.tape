program djw7  options dump(noro);

type
  one_longword_rec = record
			f1: integer
		     end;
  two_longword_rec = record
			f1: integer;
			f2: integer
		     end;
  three_longword_rec = record
			f1, f2, f3: integer
		       end;
  one_longword_set = set of 16..47;
  two_longword_set = set of 0..63;
  three_longword_set = set of 32..127;

var
  r1: one_longword_rec;
  r2: two_longword_rec;
  r3: three_longword_rec;
  s1: one_longword_set;
  s2: two_longword_set;
  s3: three_longword_set;
  s3a: set of 39..136;
  i, j: integer;
  i1: 10..maximum (integer);
  j1: minimum (integer)..50;

procedure proc1 (var p1: one_longword_rec);
  begin
    p1 := r1
  end;

procedure proc2 (p1: set of 18..47);
  begin
    s1 := p1
  end;

procedure proc3 (p1: one_longword_rec; p2: set of 16..40);
  begin
    r1 := p1;
    s1 := p2
  end;

procedure proc4 (p1: set of 18..31; p2: one_longword_rec);
  begin
    s1 := p1;
    r1 := p2
  end;

procedure proc5 (var p1: two_longword_set; var p2: two_longword_rec;
		 var p3: three_longword_set; var p4: three_longword_rec);
  begin
    p1 := s2;
    p2 := r2;
    p3 := s3;
    p4 := r3
  end;

procedure proc6 (p1: two_longword_rec; p2: set of 8..47;
		 p3: three_longword_rec; p4: three_longword_set);
  begin
    r2 := p1;
    s2 := p2;
    r3 := p3;
    s3 := p4
  end;

function func1 (p1: two_longword_rec): two_longword_set;
  begin
    func1 := s2
  end;

function func2 (p1: set of 8..55): two_longword_rec;
  begin
    func2 := r2
  end;

function func3: three_longword_set;
  begin
    func3 := s3
  end;

function func4: three_longword_rec;
  begin
    func4 := r3
  end;



begin
  proc1 (r1);
  proc2 (s1);
  proc2 ([]);
  proc2 ([48..96]);
  proc2 ([18..40]);
  proc2 ([i..49]);

  proc3 (r1, s1);
  proc3 (r1, []);
  proc3 (r1, [16..47]);

  proc4 (s1, r1);
  proc4 ([], r1);
  proc4 ([0..i], r1);
  proc4 ([i..j], r1);
  proc5 (s2, r2, s3, r3);
  proc6 (r2, s2, r3, s3);
  proc6 (r2, [0..63], r3, []);
  proc6 (r2, s3, r3, s1);
  s2 := func1 (r2);
  r2 := func2 (s2);
  r2 := func2 (s3);
  r2 := func2 ([i..j]);
  r2 := func2 ([i1..j]);
  r2 := func2 ([i1]);
  r2 := func2 ([i..j1]);
  r2 := func2 ([j1]);

  s3 := func3;
  s3a := func3;
  r3 := func4;
end.
   