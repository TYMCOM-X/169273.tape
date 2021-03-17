integer forward external procedure RESTRE( reference integer base; 
						integer maxLength );
COMMENT	Transfer the tree stored in consecutive words starting with
"base" into the TREE array, and verify it is in the proper format.
If the entire tree is not in the stated area, return the negative of
the number of words needed to store the tree.  If the tree is 
improperly formed, return 0.  If the tree can be expanded, do so,
and return the number of words that were required to represent it.
;

integer forward external procedure SAVTRE( reference integer base; 
						integer maxLength );
COMMENT	Save a compressed copy of the tree in consecutive words 
starting with "base". If the compressed version of the entire tree will
not fit in the stated area, don't write anything, and return the 
negative of the number of words needed to store the tree.  If the tree 
is improperly formed, return 0 (some data may have been written). If 
the compressed tree is valid and fits, store it, and return the number 
of words which were required to represent the tree.
;

external integer array TREE[0:'1100];	
COMMENT dont trust the size stated here, it is at least this big.;

COMMENT external integer TREE!0;
COMMENT this is just TREE[0], so why not access it that way.;

