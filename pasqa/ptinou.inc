	  open(tty); rewrite(tty); writeln(tty);
	  write(tty,' give an input file name: '); break(tty);
	  readln(tty); read(tty,inp_flnm);
	  write(tty,' give an output file name: '); break(tty);
	  readln(tty); read(tty,out_flnm);
   