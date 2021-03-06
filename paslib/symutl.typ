(*SYMUTL.TYP, last modified 10/18/83, zw*)
$IFNOT symutltyp

TYPE
    namtyp = tkntyp;
    symtyp = ^namtabrec;
    namtabtyp = ^namtabrec;
    namtabrec = RECORD
      nam: namtyp;
      nxt: ^namtabrec
    END;
    settyp = ^setrec;
    setrec = RECORD
      sym: symtyp;
      nxt: ^setrec
    END;
    lsttyp = ^lstrec;
    lstrec = RECORD
      sym: symtyp;
      nxt: ^lstrec
    END;
    prosymtyp = RECORD
      sym: symtyp;
      id: INTEGER
    END;
    prolsttyp = ^prolstrec;
    prolstrec = RECORD
      prosym: prosymtyp;
      nxt: ^prolstrec
    END;
    protyp = RECORD
      sym: symtyp;
      lst: prolsttyp
    END;
    prosettyp = ^prosetrec;
    prosetrec = RECORD
      pro: protyp;
      nxt: ^prosetrec
    END;
$PAGE
    gmrtyp = RECORD
      nonterset: settyp;
      terset: settyp;
      proset: prosettyp;
      startsym: symtyp
    END;
    tretyp = ^trerec;
    trerec = RECORD
      sym: symtyp;
      ister: BOOLEAN;
      lst, nxt: tretyp
    END;

$ENABLE symutltyp
$ENDIF
  