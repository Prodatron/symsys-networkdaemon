nolist

org #C000

WRITE "c:\Program Files (x86)\#emus\WinApe\ROM\m4test.rom"

			db	#01
			db	2,0,0
	
			dw	rsx_commands

			; RSX jump block
			jp 	nothing
rsx_commands
			db "M4 BOAR","D"+#80
			db 0

nothing     ret
;---------------------------------------
; Helper functions
			
			
hsend       ;A=source bank, HL=source address, D-1,E=length, IYL=network daemon bank, IX=return address, BC=#7F00
   			out	(c),a			;switch to application bank
   			ld	b,#fe
hsend_loop
			inc	b
			outi				;copy data from application memory to m4 dataport
			dec	e
			jr	nz,hsend_loop
			dec	d
			jr	nz,hsend_loop
			ld	b,#7f
			db #fd:ld a,l
			out	(c),a			;switch back to network daemon bank
			jp	(ix)			;return to network daemon

hreceive    ;A=destination bank, DE=destination address, IYH,C=length, HL=M4 buffer, IYL=network daemon bank, IX=return address, B=#7F 
			out	(c),a			;switch to application bank
			db	#fd
			ld	b,h
			ldir				;copy data from m4 buffer to application memory
			ld	b,#7f
			db #fd:ld a,l
			out	(c),a			;switch back to network daemon bank
			jp	(ix)		    ;return to network daemon 			


rom_response
			ds	#800+3

sock_info	ds	20
			; socket 0  status		[1]
			; socket 0  notused		[1]
			; socket 0  recv buf size[2]
			; socket 1  status		[1]
			; socket 1  notused		[1]
			; socket 1  recv buf size[2]
			; etc...
			
helper_functions
			dw hsend
			dw hreceive

ds #FF00-$

			dw	#0109	; rom version
			dw	rom_response
			dw	0
			dw	sock_info
			dw	helper_functions
