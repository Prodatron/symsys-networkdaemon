;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@                  M4 BOARD (CPC) LOWLEVEL DRIVER ROUTINES                   @
;@                                                                            @
;@             (c) 2016-2016 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- M4CPC SUBROUTINES (BANKING) ----------------------------------------------
;### M4CROM -> search ROM, get pointers, initialize data and routines
;### M4CRED -> read data from M4 rom
;### M4CRCV -> receive data from the M4 and store into application memory


;==============================================================================
;### M4CPC SUBROUTINES (BANKING) ##############################################
;==============================================================================

m4cromena   db #84+1,#7f        ;upper rom enabled
m4cromdis   db #8C+1,#7f        ;upper rom disabled
m4cromnum   db 0,#DF            ;M4 rom number

m4cromver   dw 0                ;M4 rom version
m4crombuf   dw 0                ;M4 rom response buffer address
            dw 0                ;M4 rom config (not used)
m4cromsoc   dw 0                ;M4 socket table address

m4cromhsn   dw 0                ;M4 helper function "send" address
m4cromhrc   dw 0                ;M4 helper function "receive" address

m4crspbuf   ds 16               ;buffer for response data


m4creladr   dw m4crom2+3,m4crom5+3,m4crom6+2,m4crom8+2,m4crom9+2,m4croma+2,m4cred1+3,m4cred2+2,m4cred3+3,m4crcv1+3,m4crcv2+3
m4creladr0
m4crelanz   equ m4creladr0-m4creladr/2

;### M4CROM -> searches for ROM and gets basic data
;### Output     CF=1 -> error, rom not found
;### Destroyed  AF,BC,DE,HL,IX,IY
m4cromnam   db "M4 BOAR","D"+#80

m4crom  ld a,(App_BnkNum)   ;set bank number in banked-routine pointers
        ld (m4cromp+0),a
        ld (m4credp+0),a
        ld (m4crcvp+0),a
        ld hl,m4cred
        ld e,a
        call m4cadr
        ld (m4crcv6+2),a
        ld hl,m4creladr     ;relocate #4000-banked addresses
        ld b,m4crelanz
m4crom1 ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld a,(de)
        res 7,a
        set 6,a
        ld (de),a
        djnz m4crom1
        ld ix,m4cromp       ;search for M4 rom
        ld hl,jmp_bnk16c
        rst #28
        ei
        ld a,(m4cromnum)
        or a
        scf
        ret z
        ld (m4credx+1),a    ;set rom numbers
        ld (m4crcvx+1),a
        ld hl,(m4cromhrc)   ;set receive routine jumps
        ld (m4crcv8+1),hl
        ld hl,(m4crombuf)   ;set rom buffer address
        ld bc,3
        add hl,bc
        ld (m4crombuf),hl
        add hl,bc
        ld (m4crcv7+1),hl
        or a
        ret
m4cromx 
m4crom2 ld bc,(m4cromena)       ;*** search for M4 rom
        out (c),c
        ld d,#7f
m4crom3 push de
        ld b,#df
        out (c),d
        ld a,(#c000)
        dec a
        jr z,m4crom6
m4crom4 pop de
        dec d
        jr nz,m4crom3
m4crom5 ld bc,(m4cromdis)
        out (c),c
        ei
        ret
m4crom6 ld de,m4cromnam
        ld hl,(#c004)
m4crom7 ld a,(de)
        cp (hl)
        jr nz,m4crom4
        inc de
        inc hl
        rla
        jr nc,m4crom7
        pop af              ;rom found
m4crom8 ld (m4cromnum),a
        ld hl,M4C_ROMINFO   ;get rom version, response buffer address, socket info address
m4crom9 ld de,m4cromver
        ld bc,2*4
        ldir
        ld e,(hl)           ;get helper function table address
        inc hl
        ld d,(hl)
        ex de,hl
m4croma ld de,m4cromhsn
        ld bc,4
        ldir
        jr m4cred3

;### M4CRED -> read data from M4 rom
;### Input      IY=address, E=length (16 bytes max)
;### Output     (m4crspbuf)=data, DE
;### Destroyed  AF,BC,DE,HL,IX,IY
m4cred  ld ix,m4credp
        ld hl,jmp_bnk16c
        rst #28
        ret
m4credx ld bc,#df00
        out (c),c
m4cred1 ld bc,(m4cromena)
        out (c),c
        ld c,e
        ld b,0
        push iy:pop hl
m4cred2 ld de,m4crspbuf
        ldir
m4cred3 ld bc,(m4cromdis)
        out (c),c
        ret

;### M4CRCV -> receive data from the M4 and store into application memory
;### Input      HL=address, E=bank, BC=length
;### Output     DI
;### Destroyed  AF,BC,DE,HL,IX,IY
m4crcv  ld a,m4c_sta_delay
        ld (m4c_sta_rx),a
        ld a,c
        ld (m4crcv5+1),a
        ld a,b
        ld (m4crcv6+3),a
        call m4cadr
        ld (m4crcv4+1),hl
        ld (m4crcv3+1),a
        ld ix,m4crcvp
        ld hl,jmp_bnk16c
        rst #28
        ret
m4crcvx ld bc,#df00
        out (c),c
m4crcv1 ld bc,(m4cromena)
        out (c),c
m4crcv2 ld ix,m4cred3
m4crcv3 ld a,0
m4crcv4 ld de,0
m4crcv5 ld bc,#7f00
m4crcv6 ld iy,0
m4crcv7 ld hl,0
m4crcv8 jp 0                ;hreceive -> A=destination bank, DE=destination address, IYH,C=length, HL=M4 buffer, IYL=network daemon bank, IX=return address, B=#7F 

m4cromp db 0:dw m4cromx
m4credp db 0:dw m4credx
m4crcvp db 0:dw m4crcvx
