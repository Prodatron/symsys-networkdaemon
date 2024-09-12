
			; TCP receive example for M4 Board
			; Requires firmware v1.0.9b8 upwards
			; Will write incoming data to testfile.bin
			; Duke 2016
			
			org	#4000
			nolist
DATAPORT		equ #FE00
ACKPORT		equ #FC00			
C_OPEN		equ #4301
C_READ		equ #4302
C_WRITE		equ #4303
C_CLOSE		equ #4304

C_NETSOCKET	equ #4331
C_NETCONNECT	equ #4332
C_NETCLOSE	equ #4333
C_NETSEND		equ #4334
C_NETRECV		equ #4335
C_NETHOSTIP	equ #4336
			
start:		ld	a,2			
			call	#bc0e		; set mode 2
get_server_ip:	ld	hl,msgserverip
			call	disptextz
			ld	hl,buf
			call	get_textinput
			
			cp	#FC			; ESC?
			ret	z
			xor	a
			cp	c
			jr	z, get_server_ip
		
			; convert ascii IP to binary, no checking for non decimal chars format must be x.x.x.x
			
			ld	hl,buf	
			call	ascii2dec
			ld	(ip_addr+3),a
			call	ascii2dec
			ld	(ip_addr+2),a
			call	ascii2dec
			ld	(ip_addr+1),a
			call	ascii2dec
			ld	(ip_addr),a
			

			push	iy
			push	ix

			ld	a,(m4_rom_num)
			cp	#FF
			call	z,find_m4_rom	; find rom (only first run)
							; should add version check too and make sure its v1.0.9
			cp	#FF
			call	nz,tcpclient
			
			pop	ix
			pop	iy
			ret
	
tcpclient:	ld	hl,#FF02	; get response buffer address
			ld	e,(hl)
			inc	hl
			ld	d,(hl)
			push	de
			pop	iy
			
			; get a socket
			
			ld	hl,cmdsocket
			call	sendcmd
			ld	a,(iy+3)
			cp	255
			ret	z
			
			; store socket in predefined packets
			
			ld	(csocket),a
			ld	(clsocket),a
			ld	(rsocket),a
			ld	(sendsock),a
			
			
			; multiply by 16 and add to socket status buffer
			
			sla	a
			sla	a
			sla	a
			sla	a
			
			ld	hl,#FF06	; get sock info
			ld	e,(hl)
			inc	hl
			ld	d,(hl)
			ld	l,a
			ld	h,0
			add	hl,de	; sockinfo + (socket*4)
			push	hl
			pop	ix		; ix ptr to current socket status
			
			; connect to server
			
			ld	hl,cmdconnect
			call	sendcmd
			ld	a,(iy+3)
			cp	255
			jp	z,exit_close
wait_connect:
			ld	a,(ix)			; get socket status  (0 ==IDLE (OK), 1 == connect in progress, 2 == send in progress)
			cp	1				; connect in progress?
			jr	z,wait_connect
			cp	0
			jr	z,connect_ok
			call	disp_error	
			jp	exit_close
connect_ok:	ld	hl,msgconnect
			call	disptextz
			
			; open file to save received data in.
			
			ld	hl, filename
			ld	c,#8A		; #80|FA_WRITE|FA_CREATE_ALWAYS
			ld	a,13
			
			ld	de, C_OPEN
			call	send_command2	; will do cmd(2, DE), size(1, A), mode (1, C) followed by data in HL with A size.
			ld	b,(iy+3)		; fd
			ld	a,(iy+4)		; res
			
			cp	0
			jp	nz, exit_close
			ld	a,b
			ld	(fd),a
			
			;
			
		

mainloop:		call	#bd19		; check once per frame, remove to make it faster
			
			ld	bc,2048	
			call	recv
			cp	#FF
			jr	z, exit_close	
			cp	3
			jr	z, exit_close
			xor	a
			cp	c
			jr	nz, got_data
			cp	b
			jr	z,mainloop	; no data
got_data:	
					
			
			ld	a,#2E
			call	#bb5a
			push	iy
			pop	hl
		
			ld	de,#6
			add	hl,de		; received text pointer
			ex	de,hl
			ld	h,b
			ld	l,c
			ld	a,(fd)
			call	fwrite		; remove write for full speed!
			jp	mainloop
			


exit_close:
			ld	hl,cmdclose
			call	sendcmd			
			ld	hl,cmdclosefile
			call	sendcmd
			ret
			
			; recv tcp data
			; in
			; bc = receive size
			; out
			; a = receive status
			; bc = received size 

			
recv:		; check if anything in buffer ?
			ld	a,(ix+2)
			cp	0
			jr	nz,recv_cont
			ld	a,(ix+3)
			cp	0
			jr	nz,recv_cont
			ld	bc,0
			
			ld	a,(ix)			; move check here, because there may still be data in buffer, when connection is closed
			cp	3				; socket status  (3 == remote closed connection)
			ret	z
			
			ret
recv_cont:			
			; set receive size
			ld	a,c
			ld	(rsize),a
			ld	a,b
			ld	(rsize+1),a
			
			ld	hl,cmdrecv
			call	sendcmd			; no point in checking receive. We are only after the received size.
			ld	c,(iy+4)
			ld	b,(iy+5)
			ret
			
			
			;
			; Find M4 ROM location
			;
				
find_m4_rom:
			ld	iy,m4_rom_name	; rom identification line
			ld	d,127		; start looking for from (counting downwards)
			
romloop:		push	de
			;ld	bc,#DF00
			;out	(c),d		; select rom
			ld	c,d
			call	#B90F		; system/interrupt friendly
			ld	a,(#C000)
			cp	1
			jr	nz, not_this_rom
			
			; get rsxcommand_table
			
			ld	a,(#C004)
			ld	l,a
			ld	a,(#C005)
			ld	h,a
			push	iy
			pop	de
cmp_loop:
			ld	a,(de)
			xor	(hl)			; hl points at rom name
			jr	nz, not_this_rom
			ld	a,(de)
			inc	hl
			inc	de
			and	#80
			jr	z,cmp_loop
			
			; rom found, store the rom number
			
			pop	de			;  rom number
			ld 	a,d
			ld	(m4_rom_num),a
			ret
			
not_this_rom:
			pop	de
			dec	d
			jr	nz,romloop
			ld	a,255		; not found!
			ret
			
			;
			; Send command to M4
			;
sendcmd:
			ld	bc,#FE00
			ld	d,(hl)
			inc	d
sendloop:		inc	b
			outi
			dec	d
			jr	nz,sendloop
			ld	bc,#FC00
			out	(c),c
			ret
					
			; display text
			; HL = text
			; BC = length

disptext:		xor	a
			cp	c
			jr	nz, not_dispend
			cp	b
			ret	z
not_dispend:
			ld 	a,(hl)
			push	bc
			call	#BB5A
			pop	bc
			inc	hl
			dec	bc
			jr	disptext

			; display text zero terminated
			; HL = text
disptextz:	ld 	a,(hl)
			or	a
			ret	z
			call	#BB5A
			inc	hl
			jr	disptextz

			;
			; Display error code in ascii (hex)
			;
	
			; a = error code
disp_error:
			push	af
			ld	hl,msgsenderror
			ld	bc,9
			call	disptext
			pop	bc
			ld	a,b
			srl	a
			srl	a
			srl	a
			srl	a
			add	a,#90
			daa
			adc	a,#40
			daa
			call	#bb5a
			ld	a,b
			and	#0f
			add	a,#90
			daa
			adc	a,#40
			daa
			call	#bb5a
			ld	a,10
			call	#bb5a
			ld	a,13
			call	#bb5a
			ret
			; ------------------------- fwrite
			; -- parameters: 
			; -- A = fd
			; -- HL = size
			; -- DE = addr
			; -- return:
			; -- A = 0 if OK
fwrite:
			push	hl
			push	de
			push	bc
			ld	c,a				; fd
			
write_loop:
			; get chunk size (<=#FC)
	
			ld	a,h
			cp	a,0
			jr	nz,wfull_chunk
			ld	a,l
			cp	#FC
			jr	c,fwrite_cont
wfull_chunk:
			ld	a,#FC
fwrite_cont:
			cp	a,0
			jr	z,write_done		; size is 0?
	
			; A = size
			; HL = data
			; DE = COMMAND
			; C = fd
			push	hl				; total size
			ex	de,hl
			; HL = addr
			
			ld	de,C_WRITE		; write cmd
			
			call send_command2
			; todo, add error check!
			pop	de
			push	bc
			ld	b,0
			ld	c,a				; size
			add	hl,bc			; increase address
			ex	de,hl			; DE = addr
			or	a				; clear carry
			sbc	hl,bc			; and substract chunksize
			pop	bc
			jr	write_loop
write_done:
			pop	bc
			pop	de
			pop	hl
			ret			
			;
			; Get input text line.
			;
			; in
			; hl = dest buf
			; return
			; bc = out size
get_textinput:		
			ld	bc,0
			call	#bb81	
inputloop:
re:			call	#bd19
			call	#bb09
			jr	nc,re

			cp	#7F
			jr	nz, not_delkey
			ld	a,c
			cp	0
			jr	z, inputloop
			push	hl
			push	bc
			call	#bb78
			dec	h
			push	hl
			call	#bb75
			ld	a,32
			call	#bb5a
			pop	hl
			call	#bb75
			pop	bc
			pop	hl
			dec	hl
			dec	bc
			jr	inputloop
not_delkey:	
			cp	13
			jr	z, enterkey
			cp	#FC
			ret	z
			cp	32
			jr	c, inputloop
			cp	#7e
			jr	nc, inputloop
			ld	(hl),a
			inc	hl
			inc	bc
			push	hl
			push	bc
			call	#bb5a
			call	#bb78
			push	hl
			ld	a,32
			call	#bb5a
			pop	hl
			call	#bb75
			pop	bc
			pop	hl
			jp	inputloop
enterkey:		ld	(hl),0
			ret
crlf:		ld	a,10
			call	#bb5a
			ld	a,13
			jp	#bb5a

ascii2dec:	ld	d,0
loop2e:		ld	a,(hl)
			cp	0
			jr	z,found2e
			cp	#2e
			jr	z,found2e
			; convert to decimal
			cp	#41	; a ?
			jr	nc,less_than_a
			sub	#30	; - '0'
			jr	next_dec
less_than_a:	sub	#37	; - ('A'-10)
next_dec:		ld	(hl),a
			inc	hl
			inc	d
			dec	bc
			xor	a
			cp	c
			ret	z
			jr	loop2e
found2e:
			push	hl
			call	dec2bin
			pop	hl
			inc	hl
			ret
dec2bin:		dec	hl
			ld	a,(hl)
			dec	hl
			dec	d
			ret	z
			ld	b,(hl)
			inc	b
			dec	b
			jr	z,skipmul10
mul10:		add	10
			djnz	mul10
skipmul10:	dec	d
			ret	z
			dec	hl
			ld	b,(hl)
			inc	b
			dec	b
			ret	z
mul100:		add	100
			djnz	mul100
			ret
			; DE = COMMAND			
			; A = size 
			; HL = DATA
			; C = fd
send_command2:
			push	af
			push	hl
			push	de
			push	bc
			add	3
			ld	bc,DATAPORT					; data out port
			out	(c),a						; size
			out	(c),e						; command lo
			out	(c),d						; command	hi
			pop	de
			push	de
			out	(c),e						; fd
			sub	3
			; send actual data
			
sendloop2:
			ld	d,(hl)
			out	(c),d
			inc	hl
			dec	a
			jr	nz, sendloop2
			
			; tell M4 that command has been send
			ld	bc,ACKPORT
			out (c),c
			pop	bc
			pop	de
			pop	hl
			pop	af
			ret

			
msgconnclosed:	db	10,13,"Remote closed connection....",10,13,0
msgsenderror:	db	10,13,"ERROR: ",0
msgconnect:	db	10,13,"Connected!",10,13,0
msgserverip:	db	10,13,"Input TCP server IP:",10,13,0

cmdsocket:	db	5
			dw	C_NETSOCKET
			db	#0,#0,#6		; domain, type, protocol (TCP/ip)

cmdconnect:	db	9	
			dw	C_NETCONNECT
csocket:		db	#0
ip_addr:		db	0,0,0,0		; ip addr
			dw	#1234		; port
cmdsend:		db	0			; we can ignore this byte (part of early design)	
			dw	C_NETSEND
sendsock:		db	0
sendsize:		dw	0			; size
sendtext:		ds	255
			
cmdclose:		db	#03
			dw	C_NETCLOSE
clsocket:		db	#0

cmdrecv:		db	5
			dw	C_NETRECV		; recv
rsocket:		db	#0			; socket
rsize:		dw	2048			; size
cmdclosefile:	db	#03
			dw	C_CLOSE
fd:			db	0


m4_rom_name:	db "M4 BOAR",#C4		; D | #80
m4_rom_num:	db	#FF
filename:		db	"testfile.bin",0
buf:			ds	2048	
