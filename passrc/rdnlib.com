;rdnlib.com, last modified 5/31/83, zw
;link RDNLIB.REL
:
r (upl)fudge2
rdnlib= run, autoru, lookup, filutl, query, infpac, pasdir, diratr, dirlok,
 daytim, dcext, ecext, nst1, dcdtim, ecdtim, ecdcti, ecdcda,
 ectsda, dctime, ectime, nsd1, nsd2, dcdate, ecdate, dayofw,
 dcmont, chars2, cvbins, dcdays, ecdays, dcsecs, ecsecs, dtime,
 cmdutl, douuo, prgdir, tempfi, jobnum, mmmfls, blokio/a
rdnlib= rdnlib/x

  