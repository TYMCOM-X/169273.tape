(* QLDPARSE extracts a line designator from an input line. It returns a count
   of line addresses seen, and a representation of the LD.  The parse begins
   with the first token at or following a caller supplied index.  If no LD at
   all appears, a zero count and nil LD chain are returned. There is no error.
   If the start of an LD appears, then the parse continues until the first
   token, not a part of an LD, is seen or an error is detected. If a valid LD
   is found, the count of address (one for a single address, two for a
   range) and the LD chain built up are returned with a null error code. An
   error occurs if there is a syntactic error in a contained string predicate,
   star is used improperly, or a line address or offset is out of range. In
   such a case, the count and LD chain parameters are not returned; and a
   nonnull error code is returned. The index is set to the start of the first
   token following the LD if there is no error; otherwise is set to point to
   the start of the erroneous token.  The caller should insure that the LD
   chain has been initialized by QLDDISPOSEing any records on it, and then
   setting LD to nil.   *)


EXTERNAL PROCEDURE QLDPARSE
	    (	LINE: CMDLINE;			(* LINE TO BE PARSED *)
		VAR IDX: CMDLINEIDX;		(* PARSING CURSOR *)
		VAR NLD: LDCOUNT;		(* NUMBER OF LINE ADDRESSES SEEN *)
		VAR LD: LDCHAIN;		(* REPRESENTATION *)
		WILDSWITCH: BOOLEAN;		(* SWITCH FOR WILDCARDING *)
		VAR ERR: QERRCODE	);	(* INDICATES SUCCESS OR NATURE OF FAILURE *)



(* QLDEVAL resolves a line designation within the range of lines specified,
   and returns the line numbers of the addressed lines.  The rules governing
   the evaluation of an LD are given in the 'Definition of QED'.  If an error
   is detected, the error code is set nonnull, and the range left undefined. *)

EXTERNAL PROCEDURE QLDEVAL
	    (	VAR BUF: QBUFFER;		(* BUFFER TO SEARCH *)
		LD: LDCHAIN;			(* PARSED LD TO EVALUATE *)
		SEARCHRANGE: LDRANGE;		(* LIMITS OF SEARCH *)
		VAR RANGE: LDRANGE;		(* CONTAINS RETURNED LINE ADDRESSES *)
		VAR ERR: QERRCODE	);	(* INDICATES SUCCESS OR NATURE OF FAILURE *)



(* QLDDISPOSE frees the storage associated with an LD chain. It is passed a
   pointer to the start of the chain to free. This pointer may be nil. *)

EXTERNAL PROCEDURE QLDDISPOSE (LD: LDCHAIN );
