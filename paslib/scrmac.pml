PMF VERSION 3.0 LIB FILE
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
M 10 685 #BEGINITEM
),#literal(#after_list,"#skip_after_last_item))$INDENT LEFT +#item_indent#if("#gen("#num("#before_list - #before_each_item),0),$SKIP #num("#before_list - #before_each_item),$SKIP 0&)~A%,#literal(#after_list,~A%),#literal(#before_list,"#skip_before_first_item))#if(~A$,#literal(#before_list,~A$),#literal(#item_number,"#save_next_number)),#if("#gen("#start_number,0),#literal(#item_number,"#start_number),#literal(#item_number,"#save_next_number)))#undef(#save_next_number)#if(~A#,0),#literal(#item_number,~A#,#if("#gen(~A#),#literal(#before_each_item,"#skip_before_each_item))#if(~A",#literal(#before_each_item,~A"),#literal(#item_indent,"#item_indentation))#if(~A!,#literal(#item_indent,~A!#if(
E
M 10 303 #ENDMARGIN
#if("#gen("#num("#left_indenture),0),$INDENT LEFT  -#left_indenture  ,$INDENT LEFT  +#substr("#left_indenture,2)  )#if("#gen("#num("#right_indenture),0),$INDENT RIGHT  -#right_indenture ,$INDENT RIGHT  +#substr("#right_indenture,2) )$SKIP #lines_after_mar #undef(#left_indenture)#undef(#right_indenture)
E
M 8 311 #TAB_POP
#if("#gtn("#tab_default_level,0),#assign(#tab_default_level,"#num("#tab_default_level - 1))#undef(#tab_1)#undef(#tab_2)#undef(#tab_3)#undef(#tab_4)#undef(#tab_5)#undef(#tab_6)#undef(#tab_7)#undef(#tab_8)$TABS #tab_1<:,:>#tab_2<:,:>#tab_3<:,:>#tab_4<:,:>#tab_5<:,:>#tab_6<:,:>#tab_7<:,:>#tab_8 ,#pop_error(TAB))&
E
E
E
M 10 56 #ENDFIGURE
$JUS$IND -#figure_indentation$SKIP #lines_after_figure
E
E
M 11 224 #MARGIN_POP
#if("#gtn("#margin_default_level,0),#assign(#margin_default_level,"#num("#margin_default_level - 1))#undef(#left_indent)#undef(#right_indent)#undef(#lines_before_margin)#undef(#lines_after_margin),#pop_error(MARGIN))&$SKIP 0
E
E
E
E
M 11 203 #FIGURE_POP
#if("#gtn("#figure_default_level,0),#assign(#figure_default_level,"#num("#figure_default_level - 1))#undef(#figure_indent)#undef(#skip_before_figure)#undef(#skip_after_figure),#pop_error(FIGURE))&$SKIP 0
E
E
E
M 8 167 #PAR_POP
#if("#gtn("#par_default_level,0),#assign(#par_default_level,"#num("#par_default_level - 1))#undef(#lines_before_par)#undef(#first_line_indent),#pop_error(PAR))&$SKIP 0
E
M 4 0 #COM

E
E
M 8 269 #TOC_POP
#if("#gtn("#toc_default_level,0),#assign(#toc_default_level,"#num("#toc_default_level - 1))#undef(#toc_more_pad)#undef(#toc_indent)#if("#gen("#level,3),#assign(#tocpad,"#substr("#blanks,1,"#num("#toc_indent * (#level - 2)))),#assign(#tocpad,)),&#pop_error(TOC))&$SKIP 0
E
E
M 4 102 #PAR
,$PARAGRAPH #first_line_indent)~A",$PARAGRAPH  ~A",$SKIP #lines_before_par)$NEED 2  #if(~A!,$SKIP ~A!#if(
E
E
E
L 4 181 #TOC
$MARGIN 10$PAGE$FOOTNOTES OFF$TITLE LEFT <:':>#fix_title("#title)<:' LEFT ':>#date<:':>$SKI 3$CEN^UTable of Contents|U$SKI 2$VER$TABS 10,60SECTION		PAGE$TABS 10,62$TOC
E
E
M 13 418 #TITLE_PAGE_1
,\)$TITLE LEFT <:':>#fix_title("#title)&#pad&Page <:\' LEFT ':>#date<:':> $NUMBER 1$JUSTIFY&$PAGE~A&,~A&,\)$PARAGRAPH 0#if(~A%,~A%,\)$PARAGRAPH 0#if(~A$,~A$,\)$PARAGRAPH 0#if(~A#,~A#$SKIP #skip_to_feet$JUSTIFY LEFT$PARAGRAPH 0#if(~A"$SKIP 1~A!#literal(#one_third_down,"#num("(#page_length/3)-7))#literal(#skip_to_feet,"#num("#page_length-(7+#one_third_down+3+4+9)))$TITLE ''&$PAGE$CENTER$SKIP #one_third_down
E
E
E
E
E
M 13 525 #ITEM_DEFAULT
),#literal(#skip_after_last_item,"#skip_after_last_item))#assign(#item_default_level,"#num("#item_default_level + 1))$SKIP 0~A%,#literal(#skip_after_last_item,~A%),#literal(#skip_before_first_item,"#skip_before_first_item))#if(~A$,#literal(#skip_before_first_item,~A$),#literal(#start_number,"#start_number))#if(~A#,#literal(#start_number,~A#),#literal(#skip_before_each_item,"#skip_before_each_item))#if(~A",#literal(#skip_before_each_item,~A"),#literal(#item_indentation,"#item_indentation))#if(~A!,#literal(#item_indentation,~A!#if(
M 13 532 #SECT_DEFAULT
),#literal(#skip_after_l3,"#skip_after_l3))#assign(#sect_default_level,"#num("#sect_default_level + 1))$SKIP 0~A&,#literal(#skip_after_l3,~A&),#literal(#skip_before_l3,"#skip_before_l3))#if(~A%,#literal(#skip_before_l3,~A%),#literal(#skip_after_l2,"#skip_after_l2))#if(~A$,#literal(#skip_after_l2,~A$),#literal(#skip_before_l2,"#skip_before_l2))#if(~A#,#literal(#skip_before_l2,~A#),#literal(#skip_after_l1,"#skip_after_l1))#if(~A",#literal(#skip_after_l1,~A"),#literal(#skip_before_l1,"#skip_before_l1))#if(~A!,#literal(#skip_before_l1,~A!#if(
E
M 12 637 #BEGINMARGIN
),#assign(#lines_after_mar,"#lines_after_margin)))~A#,#assign(#lines_after_mar,~A#),#if(~A$,#assign(#lines_after_mar,~A$),#literal(#right_indenture,"#right_indent)#if("#gen("#num("#right_indent),0),$INDENT RIGHT  +#right_indent,&$INDENT RIGHT   #right_indent)&)#if(~A",&$INDENT RIGHT  ~A"),0),$INDENT RIGHT +~A")#if("#gen("#num(~A",#literal(#right_indenture,~A"   ),#literal(#left_indenture,"#left_indent)#if("#gen("#num("#left_indent),0),$INDENT LEFT  +#left_indent  ,$INDENT LEFT   #left_indent  ))#if(~A!  ,$INDENT LEFT   ~A!),0),$INDENT LEFT  +~A!)#if("#gen("#num(~A!,#literal(#left_indenture,~A!  ,$SKIP #lines_before_margin  )#if(~A#,$SKIP ~A##if(
E
E
E
E
M 12 310 #BEGINFIGURE
),#assign(#lines_after_figure,"#skip_after_figure)))~A#,#assign(#lines_after_figure,~A#),#if(~A$,#assign(#lines_after_figure,~A$), #assign(#figure_indentation,#figure_indent))$IND +#figure_indentation$VERBATIM #if(~A", #assign(#figure_indentation,~A" )#if(~A!,$NEED ~A! ,$SKIP #skip_before_figure )#if(~A#,$SKIP ~A##if(
E
E
E
E
E
E
E
E
E
E
M 9 138 #ENDLEVEL
#assign( #level,"#num("#level - 1 ))#if("#gen("#level,3),#assign(#tocpad,"#substr("#tocpad,"#toc_indent + 1)),#assign(#tocpad,))&$LEVEL -1
E
M 10 259 #FOOTNOTES
),off),#assign(#footnote_type,OFF)$FOOTNOTES OFF))))~A!),right),#assign(#footnote_type,RIGHT),#if("#eqc("#lwc(~A!),left),#assign(#footnote_type,LEFT),#if("#eqc("#lwc(~A!),center)),#assign(#footnote_type,"#CENTER),#if("#eqc("#lwc(~A!),"#eqc("#lwc(~A!#if("#or("#not(
E
E
E
E
E
E
E
M 10 295 #FIX_TITLE
))#literal(#index_apos,"#search("#old_title,'))#if("#index_apos,#literal(#after_apos,"#substr("#old_title,"#num("#index_apos + 1)))#substr("#old_title,1,"#num("#index_apos))'#fix_title(#after_apos)#undef(#after_apos),#old_title)&#undef(#old_title)#undef(#index_apos)~A!#literal(#old_title,"#eval(
E
E
M 12 635 #TAB_DEFAULT
),#literal(#tab_8,"#num("#tab_7 + 1)))$TABS #tab_1<:,:>#tab_2<:,:>#tab_3<:,:>#tab_4<:,:>#tab_5<:,:>#tab_6<:,:>#tab_7<:,:>#tab_8&#assign(#tab_default_level,"#num("#tab_default_level + 1))~A(,#literal(#tab_8,~A(),#literal(#tab_7,"#num("#tab_6 + 1)))#if(~A',#literal(#tab_7,~A'),#literal(#tab_6,"#num("#tab_5 + 1)))#if(~A&,#literal(#tab_6,~A&),#literal(#tab_5,"#num("#tab_4 + 1)))#if(~A%,#literal(#tab_5,~A%),#literal(#tab_4,"#num("#tab_3 + 1)))#if(~A$,#literal(#tab_4,~A$),#literal(#tab_3,"#num("#tab_2 + 1)))#if(~A#,#literal(#tab_3,~A#),#literal(#tab_2,"#num("#tab_1 + 1)))#if(~A",#literal(#tab_2,~A"),#literal(#tab_1,1))#if(~A!,#literal(#tab_1,~A!#if(
M 9 186 #APPENDIX
&|U$SKIP 2$PARAGRAPH #first_line_indent~A!)<:	\':>$ENTRY ''^U&~A!<:':>&)$PAGE$SKI 2$ENTRY ''$ENTRY <:':>#fix_title(~A!#if("#nec("#footnote_type,OFF),$FOOTNOTES  #footnote_type  <:':>
E
E
E
E
E
M 15 402 #MARGIN_DEFAULT
),#literal(#lines_after_margin,"#lines_after_margin))&#assign(#margin_default_level,"#num("#margin_default_level + 1))$SKIP 0 ~A$,#literal(#lines_after_margin,~A$),#literal(#lines_before_margin,"#lines_before_margin))&#if(~A#,#literal(#lines_before_margin,~A#),#literal(#right_indent,"#right_indent))#if(~A",#literal(#right_indent,~A"),#literal(#left_indent,"#left_indent))#if(~A!,#literal(#left_indent,~A!#if(
E
M 9 3893 #DOCUMENT
),#literal(#page_width,75))$WIDTH #page_width#if("#title,#literal(#blanks,                                                                                                  )#literal(#pad_count,"#num("#page_width-#left_margin-#length("#title)))#literal(#pad,"#substr("#blanks,1,"#num("#pad_count-7)))$TITLE LEFT <:':>#fix_title("#title)&#pad&Page <:\' LEFT ':>#date<:':> )#com(	Set up standard tab positions with literals, emit	Scribe commands for these positions and to turn the	default page numbering off. Also initialize some	internal 'bookeeping' macros.				)#literal(#tab_1,09)#literal(#tab_2,17)#literal(#tab_3,25)#literal(#tab_4,33)#literal(#tab_5,41)#literal(#tab_6,49)#literal(#tab_7,57)#literal(#tab_8,65)$TABS #tab_1<:,:>#tab_2<:,:>#tab_3<:,:>#tab_4<:,:>#tab_5<:,:>#tab_6<:,:>#tab_7<:,:>#tab_8$NUMBER off&#literal(#level,1)#literal(#first_sect_flag,first)#com(	The following literals are used in #sect(defined below) spacing:	  #skip_before_l1  :  # lines to skip BEFORE level 1 titles	  #skip_after_l1   :  # lines to skip AFTER  level 1 titles	  #skip_before_l2  :  # lines to skip BEFORE level 2 titles	  #skip_after_l2   :  # lines to skip AFTER  level 2 titles	  #skip_before_l3  :  # lines to skip BEFORE level 3 titles  	  #skip_after_l3   :  # lines to skip AFTER  level 3 titles   )#literal(#skip_before_l1,"#page_length)#literal(#skip_after_l1,2)#literal(#skip_before_l2,3)#literal(#skip_after_l2,2)#literal(#skip_before_l3,2)#literal(#skip_after_l3,1)#com(	The following literals are used in #BEGINFIGURE and	#ENDFIGURE for spacing around the figure and	indenting the figure. Their default values are set	here.							)#literal(#skip_before_figure,1)#literal(#skip_after_figure,1)#literal(#figure_indent,5)#literal(#figure_indentation,)#literal(#lines_after_figure,)#com(	Two literals are defined here that will be used by #PAR	as default values for  1.the number of spaces to skip	before the beginning of a new paragraph, and  2.the number	of spaces to indent the first line of the paragraph.	    )#literal(#lines_before_par,1)#literal(#first_line_indent,0)#com(	The default values used by the ITEM macros and	some working literals are initialized here.		)#literal(#item_indentation,4)#literal(#skip_before_each_item,0)#literal(#start_number,1)#literal(#skip_before_first_item,1)#literal(#skip_after_last_item,1)#literal(#save_next_number,1)#literal(#count,0)#literal(#backslashes,\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\)#com(  Default values for indenting with #BEGINMARGIN       are initialized here. 				)#literal(#left_indent,5)#literal(#right_indent,5)#literal(#left_indenture,0)#literal(#right_indenture,0)#literal(#lines_before_margin,1)#literal(#lines_after_margin,1)#literal(#lines_after_mar,)#com(	Literals used by the #FOOTNOTES macro are declared	and initialized here. #center is assigned two blanks	because the default for footnoting is centering and	there is no actual "CENTER" directive as there is for 	left or right justifying footnotes. Footnotes start	turned off.						)#literal(#center,  )#literal(#footnote_type,OFF)#com(	  The default value for the table of contents	padding of level 3 ... level n section titles	is initialized here.			      )#literal(#tocpad,)#literal(#toc_more_pad,   )#literal(#toc_indent,3)#com(	Literals are declared and initialized here that	insure that the user does not use a POP macro	if it has no corresponding DEFAULT macro used	previously.					)#literal(#sect_default_level,0)#literal(#par_default_level,0)#literal(#figure_default_level,0)#literal(#item_default_level,0)#literal(#toc_default_level,0)#literal(#margin_default_level,0)#literal(#tab_default_level,0)~A%,#literal(#page_width,~A%),#literal(#left_margin,10))$MARGIN #left_margin#if(~A$,#literal(#left_margin,~A$),#literal(#page_length,66))$LENGTH #page_length#if(~A#,#literal(#page_length,~A#)#if(~A")#literal(#date,~A!#nopascal#literal(#title,
E
E
E
M 15 329 #FIGURE_DEFAULT
),#literal(#skip_after_figure,"#skip_after_figure))&#assign(#figure_default_level,"#num("#figure_default_level + 1))$SKIP 0~A#,#literal(#skip_after_figure,~A#),#literal(#skip_before_figure,"#skip_before_figure))&#if(~A",#literal(#skip_before_figure,~A"),#literal(#figure_indent,"#figure_indent))&#if(~A!,#literal(#figure_indent,~A!#if(
E
E
E
M 12 238 #PAR_DEFAULT
),#literal(#first_line_indent,"#first_line_indent))#assign(#par_default_level,"#num("#par_default_level + 1))$SKIP 0~A",#literal(#first_line_indent,~A"),#literal(#lines_before_par,"#lines_before_par))#if(~A!,#literal(#lines_before_par,~A!#if(
E
E
M 10 315 #POP_ERROR
&_DEFAULT ***********************************************************************************************************************************************$JUSTIFY~A!&_POP USED WITHOUT A MATCHING ~A!&_POP USED TOO MANY TIMES*************************************************************************** ~A!$VERBATIM$ 
E
M 12 257 #TOC_DEFAULT
)#literal(#toc_more_pad,"#substr("#blanks,1,"#num("#toc_indent)))#if("#gen("#level,3),#assign(#tocpad,"#substr("#blanks,1,"#num("#toc_indent * (#level - 2))))))&#assign(#toc_default_level,"#num("#toc_default_level + 1))$SKIP 0~A!,#literal(#toc_indent,~A!#if(
E
E
E
E
M 11 141 #BEGINLEVEL
#assign( #level,"#num("#level + 1 ))#if("#gtn("#level,2),#literal(#temp,#tocpad)#assign(#tocpad,"#temp&#toc_more_pad)#undef(#temp))&$LEVEL +1
E
E
E
E
E
E
E
E
E
E
E
M 8 186 #ENDITEM
$INDENT -&#item_indent$SKIP #after_list&#literal(#save_next_number,"#item_number)#undef(#item_indent)#undef(#before_each_item)#undef(#item_number)#undef(#before_list)#undef(#after_list)
E
E
E
E
E
E
M 9 255 #ITEM_POP
#if("#gtn("#item_default_level,0),#assign(#item_default_level,"#num("#item_default_level - 1))#undef(#item_indentation)#undef(#skip_before_each_item)#undef(#start_number)#undef(#skip_before_first_item)#undef(#skip_after_last_item),#pop_error(ITEM))$SKIP 0
M 9 255 #SECT_POP
#if("#gtn("#sect_default_level,0),#assign(#sect_default_level,"#num("#sect_default_level - 1))#undef(#skip_before_l1)#undef(#skip_after_l1)#undef(#skip_before_l2)#undef(#skip_after_l2)#undef(#skip_before_l3)#undef(#skip_after_l3),#pop_error(SECT))&$SKIP 0
E
E
E
E
M 5 245 #MEMO
$JUSTIFY&)$SKIP 3  $INDENT -10 ~A%,$JUSTIFY LEFT$SKIP $PARAGRAPH -10CC:\\\\\\\~A%#if(~A$$SKIP $PARAGRAPH -10 Subject:\\~A#$SKIP $PARAGRAPH -10Date:\\\\\~A"$SKIP $PARAGRAPH -10From:\\\\\~A!$PAGE$SKIP 2  $INDENT +10$PARAGRAPH -10To:\\\\\\\
E
M 5 453 #ITEM
) + 1))#assign(#count,"#num("#item_indent - #count))#if("#gtn("#count,0),#substr("#backslashes,1,"#count)))&~A!&\#assign(#count,"#num("#count + #length(~A!,~A!#assign(#count,0)$SKIP #before_each_item   $PARAGRAPH -&#item_indent#if("#nec("#item_number,0),#if("#and("#len("#item_number,9),"#gen("#item_indent,4)),\#assign(#count,1))#item_number&.\#assign(#count,"#num("#count + #length("#item_number) + 2))#assign(#item_number,"#num("#item_number + 1)))#if(
M 5 848 #SECT
$SKIP #skip_after_l3 ))$PARAGRAPH #first_line_indent~A!)<:	\':>^#\~A!$SKIP #skip_after_l2,#com( Level >=3 ??? )$SKIP #skip_before_l3$NEED #num("#skip_after_l3 + 3)$SECTION$ENTRY <:'^#	:>#tocpad&#fix_title(~A!)<:	\':>^#\~A!<:&:>$SKIP #skip_after_l1,#com( Level 2 ??? )#if("#eqn(2,"#level),$SKIP #skip_before_l2$NEED #num("#skip_after_l2 + 3)$SECTION$ENTRY <:'^#	:>#fix_title(~A!))<:	\':>$ENTRY ''^#\\<:&:>~A!)<:':>&)#if("#eqc("#first_sect_flag,first),#assign(#first_sect_flag,not_first)#if("#ltn("#skip_before_l1,"#page_length),$SKIP #skip_before_l1),#if("#gen("#skip_before_l1,"#page_length),$PAGE,$SKIP #skip_before_l1))$NEED #num("#skip_after_l1 + 3)$SECTION$ENTRY ''$ENTRY <:'^#	:>#upc("#fix_title(~A!$PARAGRAPH 0#com( Level 1??? )#if("#eqn("#level,1 ),#if("#nec("#footnote_type,OFF),$FOOTNOTES   #footnote_type  <:':>#fix_title(
E
 