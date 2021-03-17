Message 282 (2518 chars): New Read
Received: from B36.Tymnet by B39.Tymnet; Thu, 24 Jul 86 19:21:47 PDT
Return-path: <JMARCIN@B36.Tymnet> 
From: JMARCIN@B36.Tymnet 
Date: Thu, 24 Jul 86 19:18:05 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: how to hash a username into a block number 
In-reply-to: your message of Thu, 24 Jul 86 19:08:13 PDT


The following are the Pascal routines from (netvalcode:36)cudent.ntv
that are used to compute the hash on the username.


(************************************************************************)
(*      Return the next name character, incrementing name_pointer.	*)
(************************************************************************)

procedure get_next_name_character(
      name		: username_string;
  var name_pointer	: username_text_index;
  var name_char		: char);

begin
  if name_pointer < name.size
  then
    begin
      name_char := name.text[name_pointer];
      name_pointer := name_pointer + 1
    end
end (* get_next_name_character *);



(************************************************************************)
(*      Return the next three characters from the user name,		*)
(*	right justified in a word; increment name_pointer		*)
(*	as appropriate.							*)
(************************************************************************)

procedure next_name_word(
      name		: username_string;
  var name_pointer	: username_text_index;
  var name_word		: univ word_in_bytes);

begin
  get_next_name_character(name, name_pointer, name_word[1]);
  get_next_name_character(name, name_pointer, name_word[2]);
  get_next_name_character(name, name_pointer, name_word[3])
end (* next_name_word *);



(************************************************************************)
(*      Return the hashed CUD block number for the name.		*)
(************************************************************************)

function hashed_block(
      name	: username_string) : normal_cud_block_index;

var
  name_pointer : username_text_index;
  name_word,
  sum : integer;

begin
  name_pointer := 0; sum := 0;
  repeat
    name_word := 0;
    next_name_word(name, name_pointer, name_word);
    sum := sum + name_word
  until name_pointer >= name.size;
  hashed_block := (sum mod BIT7) mod MAXIMUM_NORMAL_CUD_BLOCK
end (* hashed_block *);



Let me know if you need additional definitions...they are in files like:
(netvalcode:36)globl.ntv,(netvalcode:36)cuddef.ntv.

No, its not written up anywhere.

jill
