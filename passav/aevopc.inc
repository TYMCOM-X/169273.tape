(* Opcode representation for the Eclipse cross-compiler, assigned more or
   less alphabetically, except that extended forms immediately follow the
   short form of an instruction.

   The opcode is isolated by performing a "MOD OPC_FIELD" *)

const
  opc_field: opc_range := #H100;

(* Shift, carry, noload, and skip subfields for ALC instructions are added
   to the appropriate opcode and isolated by "DIV ALC_FIELD" *)

  alc_field: opc_range := #H100;

(* Skip subfield. *)

  skip_field; opc_range := alc_field;
  skp: opc_range := 1 * skip_field;
  szc: opc_range := 2 * skip_field;
  snc: opc_range := 3 * skip_field;
  szr: opc_range := 4 * skip_field;
  snr: opc_range := 5 * skip_field;
  sez: opc_range := 6 * skip_field;
  sbn: opc_range := 7 * skip_field;

(* Noload field. *)

  noload: opc_range := #H8 * alc_field;

(* Carry field. *)

  carry_field: opc_range := #H10 * alc_field;
  carry0: opc_range := 1 * carry_field;
  carry1: opc_range := 2 * carry_field;
  carryc: opc_range := 3 * carry_field;

(* Shift field. *)

  shift_field: opc_range := #H40 * alc_field;
  shift_left: opc_range := 1 * shift_field;
  shift_right: opc_range := 2 * shift_field;
  swap_bytes: opc_range := 3 * shift_field;

(* Machine instructions. *)

  adc: opc_range := 0;
  add: opc_range := 1;
  addi: opc_range := 2;
  adi: opc_range := 3;
  anc: opc_range := 4;
  and: opc_range := 5;
  andi: opc_range := 6;
  bam: opc_range := 7;
  blm: opc_range := 8;
  bto: opc_range := 9;
  btz: opc_range := 10;
  clm: opc_range := 11;
  cmp: opc_range := 12;
  cmt: opc_range := 13;
  cmv: opc_range := 14;
  cob: opc_range := 15;
  com: opc_range := 16;
  ctr: opc_range := 17;
  dhxl: opc_range := 18;
  dhxr: opc_range := 19;
  divu: opc_range := 20;
  divs: opc_range := 21;
  dlsh: opc_range := 22;
  dspa: opc_range := 23;
  dsz: opc_range := 24;
  edsz: opc_range := 25;
  fab: opc_range := 26;
  fad: opc_range := 27;
  famd: opc_range := 28;
  fams: opc_range := 29;
  fas: opc_range := 30;
  fcle: opc_range := 31;
  fcmp: opc_range := 32;
  fdd: opc_range := 33;
  fdmd: opc_range := 34;
  fdms: opc_range := 35;
  fds: opc_range := 36;
  fexp: opc_range := 37;
  ffas: opc_range := 38;
  ffmd: opc_range := 39;
  fhlv: opc_range := 40;
  fint: opc_range := 41;
  flas: opc_range := 42;
  fldd: opc_range := 43;
  flds: opc_range := 44;
  flmd: opc_range := 45;
  flst: opc_range := 46;
  fmd: opc_range := 47;
  fmmd: opc_range := 48;
  fmms: opc_range := 49;
  fmov: opc_range := 50;
  fms: opc_range := 51;
  fneg: opc_range := 52;
  fnom: opc_range := 53;
  fpop: opc_range := 54;
  fpsh: opc_range := 55;
  fsa: opc_range := 56;
  fscal: opc_range := 57;
  fsd: opc_range := 58;
  fseq: opc_range := 59;
  fsge: opc_range := 60;
  fsle: opc_range := 61;
  fslt: opc_range := 62;
  fsmd: opc_range := 63;
  fsms: opc_range := 64;
  fsnd: opc_range := 65;
  fsne: opc_range := 66;
  fsner: opc_range := 67;
  fsno: opc_range := 68;
  fsnod: opc_range := 69;
  fsnu: opc_range := 70;
  fsnud: opc_range := 71;
  fsnuo: opc_range := 72;
  fss: opc_range := 73;
  fsst: opc_range := 74;
  fstd: opc_range := 75;
  fsts: opc_range := 76;
  ftd: opc_range := 77;
  fte: opc_range := 78;
  hlv: opc_range := 79;
  hxl: opc_range := 80;
  hxr: opc_range := 81;
  inc: opc_range := 82;
  ior: opc_range := 83;
  iori: opc_range := 84;
  isz: opc_range := 85;
  eisz: opc_range := 86;
  jmp: opc_range := 87;
  ejmp: opc_range := 88;
  jsr: opc_range := 89;
  ejsr: opc_range := 90;
  lda: opc_range := 91;
  elda: opc_range := 92;
  ldb: opc_range := 93;
  eldb: opc_range := 94;
  lef: opc_range := 95;
  elef: opc_range := 96;
  lob: opc_range := 97;
  lrb: opc_range := 98;
  mov: opc_range := 99;
  msp: opc_range := 100;
  mul: opc_range := 101;
  muls: opc_range := 102;
  neg: opc_range := 103;
  pop: opc_range := 104;
  popb: opc_range := 105;
  popj: opc_range := 106;
  psh: opc_range := 107;
  pshj: opc_range := 108;
  pshr: opc_range := 109;
  rtn: opc_range := 110;
  save: opc_range := 111;
  sbi: opc_range := 112;
  sge: opc_range := 113;
  sgt: opc_range := 114;
  sta: opc_range := 115;
  esta: opc_range := 116;
  stb: opc_range := 117;
  stbn: opc_range := 118;
  sub: opc_range := 119;
  xch: opc_range := 120;
  xct: opc_range := 121;
  xor: opc_range := 122;
  xori: opc_range := 123;

(* Some useful skip mnemonics. *)

  seq: opc_range := sub + noload + szr;
  sne: opc_range := sub + noload + snr;
  usge: opc_range := sub + carry0 + noload + sez;
  usgt: opc_range := sub + carry0 + noload + szc;
  usle: opc_range := sub + carry0 + noload + snc;
  uslt: opc_rnage := sub + carry0 + noload + sbn;
  seqz: opc_range := mov + noload + szr;
  snez: opc_range := mov + noload + snr;
  slez: opc_range := add + carry1 + noload + sez;
  sgtz: opc_range := add + carry1 + noload + sbn;
  sltz: opc_range := mov + shift_left + noload + snz;
  sgez: opc_range := mov + shift_left + noload + szc;
