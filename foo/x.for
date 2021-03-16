	integer x(12),y(12),dat(18),text(12),txt(8)
	integer gan,uun,g,u,page,file,p,f
	integer t(6)
	data t/' ',' ','No Uf','d.','No Us','er.'/

1	read(21,2,end=20) gan,uun,x,dat
2	format(o6,1x,o6,1x,12a1,1x,18a1)
	read(26,3,end=20) page,file,text
3	format(13x,i,i,12a1)
	read(27,4,end=20) g,u,p,f,txt
4	format(o6,1x,o6,1x,i,i,8a1)
	i = 1
	do 6 k=1,8
	if ( txt(k) .ne. ' ' ) i = 3
	if ( text(k) .ne. txt(k) ) i = 5
6	continue
	write(28,5) gan,uun,x,dat,p,f,t(i),t(i+1)
5	format(o6,1x,o6,1x,12a1,1x,18a1,i6,i6,2x,a5,a3)
	go to 1

10	read(20,11,end=20) x,gan,uun
11	format(12a1,1x,o6,1x,o6)
	read(22,12,end=20) y,dat
12	format(12a1,4x,18a1)
	do 15 i=1,12
	if ( x(i) .ne. y(i) ) go to 18
15	continue
	write(21,13) gan,uun,x,dat
13	format(o6,1x,o6,1x,12a1,1x,18a1)
	go to 10
18	type 19,x,y
19	format(1x,'mismatch: ',12a1,1x,12a1)
20	end
