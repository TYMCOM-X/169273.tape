scanner scan_lex;

eof = /28;
quote = """";

(*  Token Classes  *)

name = alpha (alpha|digit)* - any* ".." any*;
number = digit+;
char = "/" number;
string = quote (any - quote - eol | quote quote)* quote;
action = "{"  (any - "}")*  "}" |
	 "<<"  (any* - any* ">>" any* ) ">>";

(*  The Scanner  *)

ignore (" "|eol)+;
ignore "(*" (any* - any* "*" ")" any* ) "*" ")";
symbols  { insymbol := &a }
    scannersy = "SCANNER",
    alphabetsy = "ALPHABET",
    readersy = "READER",
    ignoresy = "IGNORE",
    symbolssy = "SYMBOLS",
    endsy = "END",
    issy = "IS",
    semicolonsy = ";",
    commasy = ",",
    equalsy = "=",
    colonsy = ":",
    andsy = "&",
    orsy = "|",
    dashsy = "-",
    ellipsissy = "..",
    lparensy = "(",
    rparensy = ")",
    lbracketsy = "[",
    rbracketsy = "]",
    starsy = "*",
    timessy = "@",
    plussy = "+",
    notsy = "'",
    eofsy = eof;

name { };

number { };

char { };

string { };

action { };

end
