;This is to define the CONSP predicate used below in AIP.  Is X a list?
(DE CONSP (X) (NOT (ATOM X)))

;To define specials:
(DF SPECIAL (X)
    (MAPC (FUNCTION (LAMBDA (%L)
			    (PUTPROP %L T 'SPECIAL)))
	  X))

;To make things unspecial:
(DF UNSPECIAL (X)
    (MAPC (FUNCTION (LAMBDA (%L)
			    (REMPROP %L 'SPECIAL)))
	  X))

;Does every element of a list satisfy a predicate?
(DM EVERY (BODY)
    (CONS 'AND (MAPCAR (CADR BODY) (EVAL (CADDR BODY)))))

;Return subsets of a list that satisfy a predicate:
(SPECIAL PRED)

(DE SUBSET (PRED LIST)
    (MAPCAN (FUNCTION (LAMBDA (X) (COND ( (APPLY PRED (NCONS X)) X )
				        ( T NIL ))))
	    LIST))

(UNSPECIAL PRED)

;Find first element of list satisfying a predicate and return it
;(along with the rest of the list to the right):
(DE SOME (PRED LIST)
    (PROG NIL
	L (COND ( (NULL LIST) (RETURN NIL) )
		( (PRED (CAR LIST)) (RETURN LIST) ))
	  (SETQ LIST (CDR LIST))
	  (GO L)))
;Chapter 3: MACROS

(DM POP (BODY)
    (LIST 'PROG1
	  (LIST 'CAR (CADR BODY))
	  (LIST 'SETQ (CADR BODY) (LIST 'CDR (CADR BODY)))))

;exercise 3.1:
(DM PUSH (BODY)
    (LIST 'PROG2
	  (LIST 'SETQ (CADDR BODY)
		      (LIST 'CONS (CADR BODY) (CADDR BODY)))
	  (LIST 'CAR (CADDR BODY))))

(DM LET (BODY)
    (PROG (VARS&VALS VARS VALS SVAR SVAL)
	  (POP BODY)
	  (SETQ VARS&VALS (REVERSE (POP BODY)))
     LOOP (COND ( (NULL VARS&VALS)
		  (COND ( VARS
			  (RETURN (CONS (APPEND (LIST 'LAMBDA VARS) BODY)
					VALS)) )
			( (RETURN (CONS 'PROGN BODY)) )) ))
	  (SETQ SVAL (POP VARS&VALS))
	  (SETQ SVAR (POP VARS&VALS))
	  (COND ( (GET SVAR 'SPECIAL)	;SPECIALs done with SETQ
		  (SETQ BODY (CONS (LIST 'SETQ SVAR SVAL) BODY)) )
		( T
		  (PUSH SVAL VALS)
		  (PUSH SVAR VARS) ))
	  (GO LOOP)))

;This is the stuff to do QUASI-QUOTE as in AIP p.42 ff
(DE QUASI-QUOTE (SKEL)
    (COND ( (NULL SKEL) NIL )
	  ( (ATOM SKEL)
	    (COND ( (OR (NUMBERP SKEL)	;a self-defining atom?
			(EQ SKEL T)
			(EQ SKEL NIL))
		    SKEL )
		  ( T (LIST 'QUOTE SKEL) )) )
	  ( (EQ (CAR SKEL) '*UNQUOTE*) (CADR SKEL) )
	  ( (AND (CONSP (CAR SKEL))
		 (EQ (CAAR SKEL) '*SPLICE-UNQUOTE*))
	    (LIST 'APPEND (CADAR SKEL) (QUASI-QUOTE (CDR SKEL))) )
	  ( (COMBINE-SKELS (QUASI-QUOTE (CAR SKEL))
			   (QUASI-QUOTE (CDR SKEL))
			   SKEL) )))

(DE COMBINE-SKELS (LFT RGT SKEL)
    (COND ( (AND (ISCONST LFT) (ISCONST RGT)) (LIST 'QUOTE SKEL) )
	  ( (NULL RGT) (LIST 'LIST LFT) )
	  ( (AND (CONSP RGT) (EQ (CAR RGT) 'LIST))
	    (CONS 'LIST (CONS LFT (CDR RGT))) )
	  ( (LIST 'CONS LFT RGT) )))

(DE ISCONST (X) (OR (NULL X) (EQ X T) (NUMBERP X)
		    (AND (CONSP X) (EQ (CAR X) 'QUOTE))))

;The read macros needed:
(DRM /@ (LIST '*UNQUOTE* (READ)))	;return (*UNQUOTE* var)
(DRM /|
    (PROG (TEMP)
	   (COND ( (EQ (SETQ TEMP (READCH)) '/@)
		  (RETURN (LIST '*SPLICE-UNQUOTE* (READ))) )
		( (EQ TEMP '/")
		  (RETURN (QUASI-QUOTE (READ))) )
		( (ERR (PROG2 (PRINT TEMP)
			      '"
UNKNOWN CHARACTER FOLLOWS BAR - READ")) ))))

;SELECTQ defined as a MACRO:
(SPECIAL *TYPE*)

(DM SELECTQ (BODY)
    (LET (*TYPE* (GENSYM))
	 (POP BODY)
	 (LIST 'LET (LIST *TYPE* (POP BODY))
		(CONS 'COND
		      (MAPCAR (FUNCTION *SELECTQ-CLAUSE*) BODY)))))

(DE *SELECTQ-CLAUSE* (EXP)
    (COND ( (CONSP (CAR EXP))
	    (CONS (LIST 'MEMQ *TYPE* (LIST 'QUOTE (CAR EXP))) (CDR EXP)) )
	  ( (MEMQ (CAR EXP) '(T *OW* *OTHERWISE*))
	    (CONS 'T (CDR EXP)) )
	  ( (CONS (LIST 'EQ *TYPE* (LIST 'QUOTE (CAR EXP))) (CDR EXP)) )))

;defining new function definition functions allows macros to be
;expanded at function definition time (see p.46):
(DM DEX (BODY) (CONS 'DE (EXPANDMACROS (CDR BODY))))
(DM DFX (BODY) (CONS 'DF (EXPANDMACROS (CDR BODY))))

(DE EXPANDMACROS (L)
    (COND ( (OR (ATOM L) (EQ (CAR L) 'QUOTE)) L )
	    ;We do not expand anything that is quoted.
	  ( (ISMACRO (CAR L))
	    ;If L is a macro call, apply the macro body to L.
	    (EXPANDMACROS ((GET (CAR L) 'MACRO) L)) ) ;our AND is different
	  ( T (EXPANDREST L) )))

(DE EXPANDREST (L)
    (COND ( (ATOM L) L )
	  ( T (CONS (EXPANDMACROS (CAR L)) (EXPANDREST (CDR L))) )))

(DE ISMACRO (A) (AND (ATOM A) (NOT (NUMBERP A)) (GET A 'MACRO)))
;Chapter 4: DATA TYPE DEFINITION

;section 4.3 p.51 (includes code for exercise 4.3):
(DM RECORD-TYPE (L)		;(RECORD-TYPE name [flag] list-of-slots)
    (LET (*TYPE* (CADR L)		;a SPECIAL: the name given
	  FLAG (CADDR L)		;shall we make an IS- predicate?
	  SLOTS (CAR (LAST L)))		;the actual data slots

     (COND ( (EQ (LENGTH L) 3)	;no flag specified
	     (SETQ FLAG *TYPE*) ))	;so default to name of the type
     (COND ( FLAG
	     (EVAL |"(DE @(READLIST |"(I S /- |@(EXPLODE *TYPE*)))
			 (L)
			 (EQ (CAR L) (QUOTE @FLAG)))) ))
     (LIST 'DE *TYPE*
	   (SLOT-FUNS-EXTRACT SLOTS (COND ( FLAG '(D) )))
	   (COND ( FLAG
		   (LIST 'CONS |"(QUOTE @FLAG) (STRUC-CONS-FORM SLOTS)) )
		 ( (STRUC-CONS-FORM SLOTS) )))))

(DE SLOT-FUNS-EXTRACT (SLOTS PATH)
    (COND ( (NULL SLOTS) NIL )
	  ( (ATOM SLOTS)
	    (EVAL |"(DM @(READLIST |"(|@(EXPLODE SLOTS) : |@(EXPLODE *TYPE*)))
			(L)
			(LIST '@(READLIST |"(C |@PATH R)) (CADR L))))
	    (LIST SLOTS) )
	  ( (NCONC (SLOT-FUNS-EXTRACT (CAR SLOTS) (CONS 'A PATH))
		   (SLOT-FUNS-EXTRACT (CDR SLOTS) (CONS 'D PATH))) )))

(DE STRUC-CONS-FORM (STRUC)
    (COND ( (NULL STRUC) NIL )
	  ( (ATOM STRUC) STRUC )
	  ( (LIST 'CONS (STRUC-CONS-FORM (CAR STRUC))
			(STRUC-CONS-FORM (CDR STRUC))) )))
;Chapter 5: FLOW OF CONTROL FUNCTIONS

;The LOOP macro from section 5.4 p.59 (including exercises 5.1 & 5.2):
(DM LOOP (L)
    |"(PROG @(VAR-LIST (GET-KEYWORD 'INITIAL L))
	   |@(SUBSET 'CADDR
		     (SETQ-VARS (GET-KEYWORD 'INITIAL L)))
      LOOP |@(APPLY 'APPEND (MAPCAR 'DO-CLAUSE (CDR L)))
	   (GO LOOP)
      EXIT (RETURN |@(GET-KEYWORD 'RESULT L))))

(DE GET-KEYWORD (KEYWORD CLAUSES)
    (LET (ITEM (ASSOC KEYWORD CLAUSES))
	 (COND ( ITEM (CDR ITEM) ))))

(DE DO-CLAUSE (CLAUSE)
    (SELECTQ (CAR CLAUSE)
	     ( (INITIAL RESULT) NIL )
	     ( NEXT |"((SETQ @(CADR CLAUSE) @(CADDR CLAUSE))) )
	     ( WHILE |"((COND ( (NOT @(CADR CLAUSE)) (GO EXIT) ))) )
	     ( DO (CDR CLAUSE) )
	     ( UNTIL |"((COND ( @(CADR CLAUSE) (GO EXIT) ))) )
	     ( *OTHERWISE*
	       (ERR (PROG2 (PRINT (CAR CLAUSE))
			   (TERPRI '"Unknown LOOP keyword"))) )))

(DE PAIR-UP (L)
    (COND ( (NULL L) NIL )
	  ( (NULL (CDR L)) (ERR '"Odd number of INITIAL elements") )
	  ( (CONS (LIST (CAR L) (CADR L)) (PAIR-UP (CDDR L))) )))

(DE VAR-LIST (L) (MAPCAR 'CAR (PAIR-UP L)))

(DE SETQ-VARS (L)
    (MAPCAR (FUNCTION (LAMBDA (PAIR) (CONS 'SETQ PAIR)))
	    (PAIR-UP L)))

;Section 5.5 including exercises 5.3 & 5.4:
(DE LINEREAD ()
    (LOOP (INITIAL PTR NIL)
	  (UNTIL (EQ (LOOP (WHILE (EQ (PEEKC) 40))
			   (DO (READCH))
			   (RESULT (PEEKC))) 15))
	  (NEXT PTR (NCONC PTR (LIST (READ))))
	  (RESULT PTR)))
;The FOR macro from section 5.8 p.64:
(DM FOR (L)
    (LET (VARS (VARS:FOR L)
	  ARGS (ARGS:FOR L)		;<exercise 5.5>
	  TEST (TEST:FOR L)
	  TYPE (TYPE:FOR L)
	  BODY (BODY:FOR L))		;<exercise 5.6>
	 (CONS (MAKE-MAPFN VARS TEST TYPE BODY)
	       (CONS (LIST 'FUNCTION
			   (MAKE-LAMBDA VARS
				(ADD-TEST TEST
				     (MAKE-BODY VARS TEST TYPE BODY))))
		     ARGS))))

(DE VARS:FOR (L)
    (MAPCAN (FUNCTION (LAMBDA (X)
			      (COND ( (IS-VAR-FORM X) (VAR:VAR-FORM X) ))))
	    L))

;Exercise 5.5:
(DE ARGS:FOR (L)
    (MAPCAN (FUNCTION (LAMBDA (X)
			      (COND ( (IS-VAR-FORM X) (ARG:VAR-FORM X) ))))
	    L))

(DE IS-VAR-FORM (X) (AND (EQ (LENGTH X) 3) (EQ (CADR X) 'IN)))

(DE VAR:VAR-FORM (X) (CAR X))

(DE ARG:VAR-FORM (X) (CADDR X))

(DE TEST:FOR (L)
    (LET (ITEM (ITEM:FOR '(WHEN) L))
	 (COND ( ITEM (CADR ITEM) ))))

(DE TYPE:FOR (L)
    (LET (ITEM (ITEM:FOR '(DO SAVE SPLICE FILTER) L))
	 (COND ( ITEM (CAR ITEM) )
	       ( (ERR '"No body in FOR loop") ))))

;Exercise 5.6:
(DE BODY:FOR (L)
    (LET (ITEM (ITEM:FOR '(DO SAVE SPLICE FILTER) L))
	 (COND ( ITEM (CADR ITEM) )
	       ( (ERR '"No body in FOR loop") ))))

(SPECIAL ITEM L)

(DE ITEM:FOR (KEYWORDS L)
    (LET (ITEM NIL)
	 (SOME (FUNCTION (LAMBDA (KEY) (SETQ ITEM (ASSOC KEY (CDR L)))))
	       KEYWORDS)
	 ITEM))

(UNSPECIAL ITEM L)

(DE MAKE-MAPFN (VARS TEST TYPE BODY)
    (COND ( (EQUAL TYPE 'DO) 'MAPC )
	  ( (NOT (EQUAL TYPE 'SAVE)) 'MAPCAN )
	  ( (NULL TEST) 'MAPCAR )
	  ( (SUBSET-TEST VARS BODY) 'SUBSET )
	  ( 'MAPCAN )))

(DE SUBSET-TEST (VARS BODY)
    (AND (EQ (LENGTH VARS) 1) (EQUAL (CAR VARS) BODY)))

(DE MAKE-BODY (VARS TEST TYPE BODY)
    (COND ( (EQUAL TYPE 'FILTER)
	    (LIST 'LET (LIST 'X BODY) '(COND ( X (LIST X) ))) )
	  ( (OR (NOT (EQUAL TYPE 'SAVE)) (NULL TEST))
	    BODY )
	  ( (SUBSET-TEST VARS BODY) NIL )
	  ( (LIST 'LIST BODY) )))

(DE ADD-TEST (TEST BODY)
    (COND ( (NULL TEST) BODY )
	  ( (NULL BODY) TEST )
	  ( T (LIST 'COND (LIST TEST BODY)) )))

;Exercise 5.7:
(DE MAKE-LAMBDA (VARS BODY)
    (COND ( (EQUAL VARS (CDR BODY)) (CAR BODY) )
	  ( (LIST 'LAMBDA VARS BODY) )))
;Chapter 9: DATA-DRIVEN PROGRAMMING

;{ redefined per chapter 9 p.105 (to allow easy expansion) }
(DRM /|
     (LET (CHARACTER (READCH))
	  (COND ( (GET CHARACTER 'BAR-MACRO)
		  (APPLY (GET CHARACTER 'BAR-MACRO) NIL) )
		( (ERR (PROG2 (PRINT CHARACTER) '"
ILLEGAL USE OF | - BAR MACRO")) ))))

(DEFPROP /@ BAR-ATSIGN-MACRO BAR-MACRO)
(DE BAR-ATSIGN-MACRO () (LIST '*SPLICE-UNQUOTE* (READ)))
(DEFPROP /" BAR-QUOTE-MACRO BAR-MACRO)
(DE BAR-QUOTE-MACRO () (QUASI-QUOTE (READ)))

;Section 9.2 p.106: Defining the := macro
(DM := (EXPRESSION)
    (LET (LFT (CADR EXPRESSION) RGT (CADDR EXPRESSION))
;exercise 9.3:
	 (COND ( (ISMACRO (CAR LFT)) (SETQ LFT (EXPANDMACROS LFT)) ))		 
	 (COND ( (ATOM LFT) |"(SETQ @LFT @RGT) )
;exercise 9.2:
	       ( (GET (CAR LFT) 'SET-PROGRAM)
		 (CONS (GET (CAR LFT) 'SET-PROGRAM)
		       (APPEND (CDR LFT)
			       (LIST (SUBST LFT '*-* RGT)))) ))))

(DEFPROP CAR RPLACA SET-PROGRAM)
(DEFPROP CDR RPLACD SET-PROGRAM)
(DEFPROP GET GET-SET-PROGRAM SET-PROGRAM)
(DE GET-SET-PROGRAM (ATM PROP VAL) (PUTPROP ATM VAL PROP))

;exercise 9.1:
(DEFPROP CADR RPLACAD SET-PROGRAM)
(DE RPLACAD (OLD NEW) (RPLACA (CDR OLD) NEW))
;This is at the end so that all the defined MACROs can be done.
;section 3.4 p.45
;to avoid expanding macros every time they are encountered:
(SPECIAL *MACLOBSW*)
(SETQ *MACLOBSW* T)

(DE RENAME-MACRO (NAME)
    (LET (*TYPE* (READLIST (APPEND (EXPLODE NAME) '(/- E X P A N D E R))))
	(EVAL |"(PUTPROP (QUOTE @*TYPE*)
		         (GET (QUOTE @NAME) 'MACRO)
		         'EXPR))
	(EVAL |"(DM @NAME (OLD-CODE)
		    (MACLOBBER OLD-CODE (@*TYPE* OLD-CODE))))))

(DE MACLOBBER (OLD NEW)
    (COND ( (AND *MACLOBSW* (CONSP NEW))
	    (RPLACA OLD (CAR NEW))
	    (RPLACD OLD (CDR NEW))
	    OLD )
	  ( NEW )))

(COND ( *MACLOBSW*
	(MAPC (FUNCTION RENAME-MACRO) '(PUSH POP LET SELECTQ LOOP FOR))
	'(MACROs Redefined)
      ) )

(UNSPECIAL *TYPE* *MACLOBSW*)
   