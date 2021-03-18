.frmopDef 1,!FOCFM,create frame;

.b!ex
	MOVSI	AC,!FOCFM
	FRMOP	AC,addr
	  error return	; AC/ error code from {TabRef FOCFM}
	normal return	; @C/ child frame desc

addr:   flags,,CNT
	JBTPPN
	JBTPRV		; {TabRef JBTPRV}
	JBTAUN		; AUN for new frame
	JBTUNM		; sixbit username for given AUN
        JBTUN1		; 
	<license>	; iff CF.LIC set

where CNT is the number (0-5) of login data words that follow the header
	word; this number does NOT include the license word, if any

and <license> is the license bits to pass to child frame; license can be
	the same or less than caller, but not more; non-privileged caller
	can set    req.release (rsname)
            end
         until exit;

         if (err_code=0)  then
          begin
            if (next = end_flst) and (count = o_file_dir_limit) then
             begin
		useradr:= userid;
                get_rec (tmp,err_code,second_work,rsname2,'A');
                if (err_code=0) then
                 begin
                  previous := cur_block;
                  cur_block := tmp;
		  second_work.userid:= useradr;
                  update_rec (second_work,tmp,end_flst,rsname2,err_code,hdr,'3');
                  if (err_code=0) then 
                   begin
                    next := tmp;
                    dskio (write,previous,top,err_code,work,'3')
                   end
                 end;
                req.release (rsname)
              end   "next=end_flst"
             else update_rec (work,cur_block,work.next,rsname,err_code,hdr,'3');
         end;
         ok := (err_code=0);
         if (err_code<>0) and (err_code<>5)  then recovery(err_code,work,true)
         end
end;   "add"


!
procedure entry delete (var  hdr : ontyme_file_hdr ;
                   var  ok  : boolean ;
                   var work : univ o_file_dir_block ) ;

var
   second,
   found        : boolean ;
   rsname2: string16;
   err_code : integer;

begin
   found := false ; second := false ;
   if dir_open then with work do
      begin
      next := top;  
      repeat                     " getting each dir. block "
         begin
         previous := cur_block;  cur_bloc