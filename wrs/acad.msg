From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 8 Dec 87 14:26:24 PST 
To: <jwillott@opus> 
Subject: Re: autocad to postscript 
In-reply-to: <8712082025.AA12948@opus.Tymnet.com> of Tue, 8 Dec 87 12:25:33 PST

dfx2ps doesn't convert AutoCad files directly, only Drafix1+ files.  But
fear not, there is a pair of programs that will convert from AutoCad to
Drafix1+...

1.  create an AutoCad drawing exchange file using the DXFOUT command:
	Command: DXFOUT File name: figure.dxf

2.  convert it to a Drafix1+ drawing file with the OTTO program:
	otto figure.dxf -cD -v2.5
    where 2.5 is the version of AutoCad which created the DXF file.

3.  convert the Drafix1+ drawing file to Drafix interchange format:
	dfxport -efigure.dwg -ofigure.dfx

4.  convert the Drafix1+ interchange file to postScript:
	dfx2ps -t figure.dfx > figure.Dx

5.  cleanup intermediate files:
	rm figure.dxf figure.dwg figure.dfx

The figure may now be included in PostScript documents produced by psdit
by including the command
	.so figure.Dx
in the psroff source file.

-Bill

If you get this message before you hear from me, give me a call at home:
408-866-0443
  