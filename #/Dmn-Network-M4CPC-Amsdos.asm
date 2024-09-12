;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@                  M4 BOARD (CPC) MIDLEVEL DRIVER ROUTINES                   @
;@                                                                            @
;@             (c) 2016-2016 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

org #4000
nolist

main    ld a,"S"
        call #BB5D
        xor a
        call m4cini

;### M4CTOP -> M4CPC TCP open connection
;### Input      A=socket number (0-3), E=mode (0=client, 1=server), IX=structure (2byte local port [, 4byte remote IP, 2byte remote port])
;### Output     CF=0 ok, CF=1 error (A=error code)
;### Destroyed  AF,BC,DE,HL,IY
        ld a,"I"
        call #BB5D
        xor a
        ld e,0
        ld ix,condat
        call m4ctop
        jr c,main2
        ld a,"C"
        call #BB5D

;### M4CTST -> M4CPC TCP test connection status
;### Input      A=socket number (0-3)
;### Output     A=status (0=in process, 2=established, 3=close_wait, 4=close, +128=data received)
;###            BC=received bytes, IX,IY=remote IP, DE=remote port
;### Destroyed  F,HL
main1   xor a
        call m4ctst
        bit 7,a
        jr nz,recv
        cp 2+1
        jr c,main1
main2   xor a
        call m4ctcl
        ld a,"E"
        call #BB5D
        ret

;### M4CTRX -> M4CPC TCP receive data from connection
;### Input      A=socket number (0-3), HL=memory address, E=memory bank, BC=length (<= last received length)
;### Output     BC=remaining bytes
;### Destroyed  AF,DE,HL,IYH
recv    push bc
        ld a,"R"
        call #BB5D
        pop bc
        xor a
        ld hl,#0000
        ld e,1
        call m4ctrx
        ld a,c
        or b
        jr nz,recv
        jr main1

condat  dw 50000:db 94,142,241,111:dw 23

;### ERROR CODES
neterrnhw   equ 1   ;no hardware setup
neterrnip   equ 2   ;no IP configuration
neterrfnc   equ 3   ;function not supported
neterruhw   equ 4   ;unknown hardware error

neterrsfr   equ 8   ;socket - no more free socket
neterrsex   equ 9   ;socket - does not exist
neterrstp   equ 10  ;socket - wrong type
neterrspr   equ 11  ;socket - is in use for another process

neterrdiv   equ 16  ;dns - invalid domain string
neterrdto   equ 17  ;dns - timeout
neterrdrc   equ 18  ;dns - recursion not supported
neterrdtr   equ 19  ;dns - truncated answer
neterrdln   equ 20  ;dns - package too large

neterrtes   equ 24  ;tcp - connection not yet established

bnk16c  di
        ld l,(ix+1)
        ld h,(ix+2)
        jp (hl)

pck_buffer  ds 600
App_BnkNum  db 0

read "Dmn-Network-M4CPC.asm"
read "Dmn-Network-M4CPCBanking.asm"
list
endadr
