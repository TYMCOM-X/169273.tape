program heat2d;

 const
   dt = 0.1;  d2dt = 20.0; pm4 = 0.0001; one = 1.0; two = 2.0;
   jmax = 43; kmax = 43;

 var
  nmax, nyt, nxt, nym, nxm, jx, kx, j, k, nc, ncl, itb, ite, itrun: 0..16000;
  tlow, thih, deltt, delt, time, ta, edtime, eddt: real;

 type matrix = array [1..jmax, 1..kmax] of real;

 var
  dens, temp, cap, ts, c, cbb, al, dbb, a, b, s, cb, dum, db: matrix;


begin
  (* Generate problem. *)

  for k := 1 to kmax do begin
    for j := 1 to jmax do begin
      dens [j,k] := 0.0;
      temp [j,k] := 0.0;
      cap [j,k] := 0.0;
      ts [j,k] := 0.0;
      c [j,k] := 0.0;
      cbb [j,k] := 0.0;
      al [j,k] := 0.0;
      dbb [j,k] := 0.0;
      a [j,k] := 0.0;
      b [j,k] := 0.0;
      s [j,k] := 0.0;
      cb [j,k] := 0.0;
      dum [j,k] := 0.0;
      db [j,k] := 0.0;
    end;
  end;

  nc := 0;
  nyt := 22;
  nxt := 42;
  nmax := 100;
  edtime := 1.0;
  eddt := 0.5;
  nym := nyt - 1;
  nxm := nxt - 1;
  jx := nym - 1;
  kx := nxm - 1;

  for k := 1 to nxt do begin
    for j := 1 to nyt do begin
      dens [j,k] := 1.0;
      temp [j,k] := 10.0;
      ts [j,k] := 1.0;
      cap [j,k] := 0.1;
    end;
  end;

  ncl := nc;
  time := 0.0;
  ta := max (pm4, dt);

  (* Computation begins. *)

  while (nc < nmax) do begin

    nc := nc + 1;
    time := time + dt;
    for k := 1 to nxt do begin
      for j := 1 to nyt do begin;
	ta := sqrt (temp [j,k]);
	al [j,k] := dens [j,k] * ta * temp [j,k];
	c [j,k] := pm4 * ta * temp [j,k];
      end;
    end;

    (* Couplings *)

    for k := 1 to nxm do begin
      for j := 1 to nym do begin
	cb [j,k] := two * sqr (max (c [j,k], c [j,k+1])) / (c [j,k] + c [j,k+1]);
	ta := cb [j,k] * abs (temp [j,k] - temp [j,k+1]) / max (al [j,k], al [j,k+1]);
	cbb [j,k] := cb [j,k] / (one + ta);
	db [j,k] := two * sqr (max (c [j,k], c [j+1,k])) / (c [j,k] + c [j+1,k]);
	ta := s [j,k] * abs (temp [j,k] - temp [j+1,k]) / max (al [j,k], al [j+1,k]);
	dbb [j,k] := db [j,k] / (one + ta);
	s [j,k] := dens [j,k] * cap [j,k] * d2dt;
	ts [j,k] := temp [j,k];
      end;
    end;

    (* Boundary conditions for free boundary *)

    for j := 1 to nyt do begin;
      a [j,1] := 0.0;
      b [j,1] := 1.0;
      temp [j,1] := 1.0;
      temp [j,nxm] := 1.0;
      ts [j,1] := 1.0;
      ts [j,nxm] := 1.0;
    end;
    for k := 1 to nxt do begin
      a [1,k] := 0.0;
      b [1,k] := 1.0;
      temp [1,k] := 1.0;
      temp [nym,k] := 1.0;
      ts [1,k] := 1.0;
      ts [nym,k] := 1.0;
    end;

    (* Z sweep *)

    for j := 2 to jx do begin
      for k := 2 to kx do begin
	dum [j,k] := s [j,k] + cbb [j,k] + cbb [j,k-1] * (one - a [j,k-1]);
	a [j,k] := cbb [j,k] / dum [j,k];
	b [j,k] := ( s [j,k] * temp [j,k] + cbb [j,k-1] * b [j,k-1]
		  + dbb [j,k] * (temp [j+1,k] - temp [j,k])
		  - dbb [j-1,k] * (temp [j,k] - temp [j-1,k])       ) / dum [j,k];
      end;
      for k := kx downto 2 do
	ts [j,k] := a [j,k] * ts [j,k+1] + b [j,k];
    end;

    (* R sweep *)

    for k := 2 to kx do begin
      for j := 2 to jx do begin
	dum [j,k] := s [j,k] + dbb [j,k] + dbb [j-1,k] * (one - a [j-1,k]);
	a [j,k] := dbb [j,k] / dum [j,k];
	b [j,k] :=
	      ( s [j,k] * ts [j,k] + dbb [j-1,k] * b [j-1,k]
		  + cbb [j,k] * (ts [j,k+1] - ts [j,k])
		  - cbb [j,k-1] * (ts [j,k] - ts [j,k-1])	) / dum [j,k];
      end;
      for j := jx downto 2 do
	temp [j,k] := a [j,k] * temp [j+1,k] + b [j,k];
    end;

    (* Back substitution R direction *)

    if time > edtime then begin
      writeln (nc, time);

      for k := 2 to kx do begin
	for j := 2 to jx do begin
	  deltt := abs (temp [j,k] - ts [j,k]);
	  if delt < deltt then delt := deltt;
	  if tlow > temp [j,k] then tlow := temp [j,k];
	  if thih < temp [j,k] then thih := temp [j,k];
	end;
      end;

      edtime := edtime + eddt;
      ncl := nc;
      writeln (delt, tlow, thih);
    end (* if *) ;

  end (* while *) ;

  for j := 2 to jx do
    writeln (j, temp[j,2], temp[j,3], temp[j,kx-1], temp[j,kx]);

end.
    