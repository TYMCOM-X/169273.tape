  envmodule pt007e options special(word,coercions);

		 const 	mem_cst = 2;
		 	len_cst = 32;

	external const	max_pr_mem: integer;
			mchwrd_ar_len: integer;
			out_txm_flnm_cst: file_name;

	type	sclr_type=(zero,one,two,three,four,five);
		subsclr_type=one..four;
		ind_type=(blue,rose,red,green,brown);
		stsclr_type=set of sclr_type;
		bl_ar_type=packed array[sclr_type] of boolean;
		stsub_type=set of subsclr_type;
		st_char=set of char;
		wrd_type=packed array[1..7] of char;
		sng_type=string[256];
		mchwrd_ar_type=array[1..len_cst] of machine_word;
		blar_pr_mem_type=packed array[1..mem_cst] 
							of boolean;
		rl_type=-10e6..10e6 prec 6;
		drl_type=-10e6..10e6 prec 13;
		subrcd_type=packed record
				intar: packed array
				    [ind_type,subsclr_type] of integer;
				stindar: packed array[ind_type]
						of stsub_type
			    end;
		rcd_type=packed record
				srlnum: rl_type;
				rlnum: drl_type;
				subrcd: subrcd_type;
				wrd: wrd_type;
				numb: sclr_type;
				stsclr: stsclr_type;
				lngst: st_char;
				bl_ar: bl_ar_type
			 end;
		ptr_main_rcd_type=^main_rcd;
		main_rcd=record ptr: ptr_main_rcd_type;
				case boolean of
				  true:(main_ar: mchwrd_ar_type);
				  false:(elmt: rcd_type)
			 end;
		ptr_mchwrd_ar_type=^mchwrd_ar_type;

	external const	blar_pr_mem_ct: blar_pr_mem_type;

    external procedure print( string[50] );

    external procedure p_integer( sng_type; integer);

    external procedure p_char( sng_type; char);

    external procedure p_boolean( sng_type; boolean);

    external procedure p_sclr_type( sng_type; sclr_type);

    external procedure p_ind_type( sng_type; ind_type);

    external procedure p_stsclr_type( sng_type; stsclr_type);

    external procedure p_bl_ar_type( sng_type; bl_ar_type);

    external procedure p_st_char( sng_type; st_char);

    external procedure p_sng_type( sng_type; sng_type);

    external procedure p_rl_type( sng_type; rl_type);

    external procedure p_drl_type( sng_type; drl_type);

    external procedure p_rcd_type( sng_type; rcd_type);

    external procedure jump_error;

    external procedure pr_mem(var ptr_mchwrd_ar_type; 
                             integer; integer; string[35]);

    external procedure rd_inp( file_name );

    external procedure tp_fl_ops( file_name );

    external procedure bin_fl_ops(var mchwrd_ar_type;
			           file_name; var text);
  end.
