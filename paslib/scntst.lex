scanner scan_lex;

alphabet is ascii;

(*  Character Classes  *)

eol = /13;
quote = """";

(*  Token Classes  *)

string = quote (any - quote - eol | quote quote)* quote;

(*  The Scanner  *)

string { };

end
 