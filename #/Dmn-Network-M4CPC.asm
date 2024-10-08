;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@                  M4 BOARD (CPC) MIDLEVEL DRIVER ROUTINES                   @
;@                                                                            @
;@             (c) 2016-2016 by Prodatron / SymbiosiS (J�rn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo
;! crash bugfix
;- multiple connections testen
;- settings (ssid, name, dhcp, dns)



;--- M4CPC CONTROL ROUTINES ---------------------------------------------------
;### M4CINI -> init the M4CPC and setup network configuration

;--- M4CPC TCP ROUTINES -------------------------------------------------------
;### M4CTOP -> M4CPC TCP open connection
;### M4CTCL -> M4CPC TCP close connection
;### M4CTST -> M4CPC TCP test connection status
;### M4CTRX -> M4CPC TCP receive data from connection
;### M4CTTX -> M4CPC TCP send data to connection
;### M4CTDC -> M4CPC TCP disconnect connection
;### M4CTSK -> M4CPC TCP skip received data from connection
;### M4CTFL -> M4CPC TCP flush outgoing data to connection

;--- M4CPC UDP ROUTINES -------------------------------------------------------
;### M4CUOP -> M4CPC UDP open
;### M4CUCL -> M4CPC UDP close
;### M4CUST -> M4CPC UDP test status
;### M4CURX -> M4CPC UDP receive data
;### M4CUTX -> M4CPC UDP send data
;### M4CUSK -> M4CPC UDP skip received data

;--- M4CPC SPECIFIC ROUTINES --------------------------------------------------


;--- M4CPC SUBROUTINES --------------------------------------------------------
;### M4CCMD -> send command to the M4
;### M4CADR -> calculates banking code and new address
;### M4CSCK -> get new socket from M4
;### M4CSCT -> translate daemon socket to M4 socket
;### M4CSTA -> get socket status

;--- M4CPC HIGHLEVEL INTERFACE EQUS -------------------------------------------


low_sockmax     equ 4

low_vermaj      equ 0   ;version 0.1
low_vermin      equ 1


m4c_sta_delay   equ 2       ;delay for RX/TX status
m4c_sta_tx      db 0        ;TX "led" status
m4c_sta_rx      db 0        ;RX "led" status


;==============================================================================
;### M4CPC CONSTANTS ##########################################################
;==============================================================================

M4C_PORTDATA    equ #fe00
M4C_PORTACK     equ #fc00

M4C_BUFSIZE     equ #800

M4C_CMDSOCKET   equ #4331
M4C_CMDCONNECT  equ #4332
M4C_CMDCLOSE    equ #4333
M4C_CMDSEND     equ #4334
M4C_CMDRECV     equ #4335
M4C_CMDHOSTIP   equ #4336
M4C_CMDRSSI     equ #4337
M4C_CMDBIND     equ #4338
M4C_CMDLISTEN   equ #4339
M4C_CMDACCEPT   equ #433A

M4C_ROMINFO     equ #ff00


;==============================================================================
;### M4CPC CONTROL ROUTINES ###################################################
;==============================================================================

;### M4CINI -> init the M4CPC and setup network configuration
;### Input      A=type (0=init
;###                    1=set MAC address    -> HL=6byte MAC address [*IGNORED*]
;###                    2=set IP address     -> HL=4byte IP, 4byte Subnet mask, 4byte Gateway
;###                    3=set NETBIOS name   -> HL=name (0-terminated)
;###                    4=set DNS server     -> HL=4byte primary IP, 4byte secondary IP
;###                    5=set SSID, password -> HL=ssid (0-terminated), DE=password (0-terminated)
;###                   )
;### Destroyed  AF,BC,DE,HL,IX,IY
m4cini  cp 1
        ret z               ;mac address setup -> not supported
        jr nc,m4cini1
        call m4crom         ;init -> search rom and get base values
if 0
        ld hl,statxtrom0
        ld a,(m4cromnum)
        call clcn08
        ex de,hl
        ld hl,statxtrom1
        ld bc,11
        ldir
        ex de,hl
        ld a,(m4cromver+1)
        call clcn08
        ld (hl),"."
        inc hl
        ld a,(m4cromver+0)
        call clcn08
        ld (hl),")"
endif
        ret
m4cini1 cp 3                ;##!!##
        ;...
        ret


;==============================================================================
;### M4CPC TCP ROUTINES #######################################################
;==============================================================================

;### M4CTOP -> M4CPC TCP open connection
;### Input      A=socket number (0-3), E=mode (0=client, 1=server), IX=structure (2byte local port [, 4byte remote IP, 2byte remote port])
;### Output     CF=0 ok, CF=1 error (A=error code)
;### Destroyed  AF,BC,DE,HL,IY
m4ctopcmdc  db 9:dw M4C_CMDCONNECT:ds 1+4+2     ;socket, ip, port
m4ctopcmdb  db 9:dw M4C_CMDBIND   :ds 1+4+2     ;socket, ip (=0.0.0.0), port
m4ctopcmdl  db 3:dw M4C_CMDLISTEN :ds 1         ;socket
m4ctopcmda  db 3:dw M4C_CMDACCEPT :ds 1         ;socket

m4ctop  push de
        push ix
        call m4csck
        pop ix
        pop de
        ret c
        dec e
        jr z,m4ctop1
        ld (m4ctopcmdc+3),a     ;** client mode
        ld l,(ix+5)
        ld h,(ix+4)
        ld (m4ctopcmdc+4+0),hl
        ld l,(ix+3)
        ld h,(ix+2)
        ld (m4ctopcmdc+4+2),hl
        ld l,(ix+6)
        ld h,(ix+7)
        ld (m4ctopcmdc+4+4),hl
        ld hl,m4ctopcmdc
m4ctop0 push ix
        call m4ccmd
        ld iy,(m4crombuf)
        ld e,1
        call m4cred
        ei
        pop ix
        ld a,(m4crspbuf)
        inc a
        scf
        ld a,neterruhw
        ret z
        xor a
        ret
m4ctop1 ld (m4ctopcmdl+3),a     ;** server mode
        ld (m4ctopcmdb+3),a
        ld (m4ctopcmda+3),a
        ld l,(ix+0)
        ld h,(ix+1)
        ld (m4ctopcmdb+4+4),hl
        ld hl,m4ctopcmdb
        call m4ctop0
        ret c
        ld hl,m4ctopcmdl
        call m4ctop0
        ret c
        ld hl,m4ctopcmda
        jr m4ctop0

;### M4CTCL -> M4CPC TCP close connection
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
m4ctcl  equ m4ctdc

;### M4CTST -> M4CPC TCP test connection status
;### Input      A=socket number (0-3)
;### Output     A=status (0=in process, 2=established, 3=close_wait, 4=close, +128=data received)
;###            BC=received bytes, IX,IY=remote IP, DE=remote port
;### Destroyed  F,HL
m4ctst  call m4csta         ;get socket status (A=socket status [0=ok, 1=connect in progress, 2=send in progress, 3=close, >3=error], BC=received size)
        ld e,a
        ld a,4
        ret c               ;no socket -> closed (=4)
        ld a,e
        ld e,0
        cp 4
        jr z,m4ctst1        ;4=incoming connection in process (=0)
        cp 1
        jr z,m4ctst1        ;1=outgoing connection in process (=0)
        ld e,2
        jr c,m4ctst1        ;0=connection established (=2)
        cp 3
        jr c,m4ctst1        ;2=sending, connection established (=2)
        ld e,4              ;3=connection closed, >3=error (both results in a 4=close status)
m4ctst1 ld a,c
        or b
        jr z,m4ctst2
        ld a,128
m4ctst2 add e
        ld de,(m4crspbuf+6)
        db #dd:ld l,d
        db #dd:ld h,e
        ld de,(m4crspbuf+4)
        db #fd:ld l,d
        db #fd:ld h,e
        ld de,(m4crspbuf+8)
        ret

;### M4CTRX -> M4CPC TCP receive data from connection
;### Input      A=socket number (0-3), HL=memory address, E=memory bank, BC=length (<= last received length)
;### Output     BC=remaining bytes
;### Destroyed  AF,DE,HL,IYH
m4ctrxcmd   db 5:dw M4C_CMDRECV:db 0:dw 0

m4ctrx  push ix
        push iy
        ld (m4ctrxcmd+4),bc
        push hl
        push de
        push bc
        call m4csct
        ld (m4ctrxcmd+3),a
        ld hl,m4ctrxcmd
        call m4ccmd
        ld iy,(m4crombuf)
        ld e,3
        call m4cred
        pop bc
        pop de
        pop hl
        ld a,(m4crspbuf)
        inc a
;        call nz,m4crcv
        ld a,(m4ctrxcmd+3)
        call m4csta0
        pop iy
        pop ix
        ret

;### M4CTTX -> M4CPC TCP send data to connection
;### Input      A=socket number (0-3), HL=memory address, E=memory bank, BC=length
;### Output     BC=number of sent bytes, HL=number of remaining bytes, ZF=1 -> all bytes have been sent
;### Destroyed  AF,DE,IYH
m4cttxcmd   db 5:dw M4C_CMDSEND:db 0:dw 0   ;socket, length

m4cttx  push ix
        push iy
        push bc
        push hl
        push de
        push bc
        call m4csct
        ld (m4cttxcmd+3),a
        call m4csta0
        cp 2                    ;still sending -> don't send more data until sending has been finished
        jr z,m4cttx2
        pop bc
        ld hl,M4C_BUFSIZE
        or a
        sbc hl,bc
        jr nc,m4cttx1
        ld bc,M4C_BUFSIZE       ;don't send more than buffer-size
m4cttx1 ld (m4cttxcmd+4),bc
        push bc
        ld hl,m4cttxcmd
        call m4ccmd0            ;start command
        pop bc
        pop de
        pop hl
        push bc
        call m4csnd             ;send data
        call m4ccmd1            ;close command
        ld iy,(m4crombuf)
        ld e,1
        call m4cred             ;check error status
        ei
        pop bc
        pop hl
        ld a,(m4crspbuf)
        or a
        jr nz,m4cttx3           ;error -> nothing has been sent
        or a
        sbc hl,bc               ;hl=target-actual=remaining
        pop iy
        pop ix
        ret
m4cttx2 pop bc
        pop bc
        pop bc
        pop hl
m4cttx3 xor a
        ld c,a
        ld b,a
        inc a
        pop iy
        pop ix
        ret

;### M4CTDC -> M4CPC TCP disconnect connection
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
m4ctdccmd   db 3:dw M4C_CMDCLOSE:db 0

m4ctdc  call m4csct
        ret c
        ld (m4ctdccmd+3),a
        ld (hl),-1
        ld hl,m4ctdccmd
        call m4ccmd
        ei
        ret

;### M4CTSK -> M4CPC TCP skip received data from connection
;### Input      A=socket number (0-3), BC=length (<= last received length)
;### Output     BC=remaining bytes
;### Destroyed  AF,DE,HL,IYH
m4ctsk  ;...
        ret

;### M4CTFL -> M4CPC TCP flush outgoing data to connection
;### Input      A=socket number (0-3)
;### Destroyed  -
m4ctfl  ret                             ;not supported by the M4CPC


;==============================================================================
;### M4CPC UDP ROUTINES #######################################################
;==============================================================================

;### M4CUOP -> M4CPC UDP open
;### Input      A=socket number (0-3), IX=structure (2byte local port)
;### Output     CF=0 ok, CF=1 error (A=error code)
;### Destroyed  AF,BC,DE,HL,IY
m4cuop  scf
        ld a,neterrfnc
        ret

;### M4CUCL -> M4CPC UDP close
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
m4cucl  equ m4ctcl

;### M4CUST -> M4CPC UDP test status
;### Input      A=socket number (0-3)
;### Output     A=status (0=in process, 3=idle, +128=data received [BC=received bytes, IX,IY=remote IP, DE=remote port])
;### Destroyed  F,BC,DE,HL
m4cust  ;...
        ret

;### M4CURX -> M4CPC UDP receive data
;### Input      A=socket number (0-3), HL=remote address, E=remote bank, BC=full data length
;### Destroyed  AF,BC,DE,HL,IYH
m4curx  ;...
        ret

;### M4CUTX -> M4CPC UDP send data
;### Input      A=socket number (0-3), HL=local address, E=local bank, BC=length, IX=structure (4byte remote IP, 2byte remote port])
;### Output     CF=1 send buffer full
;### Destroyed  AF,DE,HL,IYH
m4cutx  ;...
        ret

;### M4CUSK -> M4CPC UDP skip received data
;### Input      A=socket number (0-3), BC=full data length
;### Destroyed  AF,BC,DE,HL,IYH
m4cusk  ;...
        ret


;==============================================================================
;### M4CPC SPECIFIC ROUTINES ##################################################
;==============================================================================

;### M4CPOL -> Poll stuff (Screenmode)
m4cpolc db 1
m4cpol  ld hl,m4cpolc
        dec (hl)
        ret nz
        ld (hl),10
;        ld hl,jmp_scrget
;        rst #28
ld e,1
        ld a,#84
        add e
        ld (m4cromena),a
        add 8
        ld (m4cromdis),a
        ret

;### M4CSIG -> Get signal strength
;### Output     E=signal strength (0=no signal, 1-5=strength)
;### Destroyed  F,BC,DE,HL,IX,IY
m4csigcmd   db 2:dw M4C_CMDRSSI

m4csig  ld hl,m4csigcmd
        call m4ccmd
        ld iy,(m4crombuf)
        ld e,1
        call m4cred
        ei
        ld a,(m4crspbuf)
        cp #1f
        ld e,0
        ret z
        inc e
        cp #a8
        ret c
        inc e
        cp #b0
        ret c
        inc e
        cp #b8
        ret c
        inc e
        cp #c0
        ret c
        inc e
        ret

;### M4CDNS -> DNS start request
;### Input      (pck_buffer+3)=domain name string (0-terminated)
;### Output     CF=0 -> ok, DNS lookup in progress
;###            CF=1 -> error (A=error code)
;### Destroyed  AF,BC,DE,HL
m4cdns  ld hl,pck_buffer+3
        xor a
        ld c,2
m4cdns1 inc c
        cp (hl)
        inc hl
        jr nz,m4cdns1
        ld hl,M4C_CMDHOSTIP
        ld (pck_buffer+1),hl
        ld hl,pck_buffer
        ld (hl),c
        call m4ccmd
        ld iy,(m4crombuf)
        ld e,1
        call m4cred
        ei
        ld a,(m4crspbuf)
        or a
        inc a
        ret nz
        ld a,neterrdto
        scf
        ret

;### M4CDNR -> DNS check for resolve
;### Input      M4CDNS has been called before
;### Output     CF=0 -> IP received, IX,IY=IP
;###            CF=1 -> A=status (0=still in progress, >0=error)
;### Destroyed  AF,BC,DE,HL,IX,IY
m4cdnr  ld e,8
        ld iy,(m4cromsoc)
        call m4cred
        ei
        ld de,(m4crspbuf+6)
        db #dd:ld l,d
        db #dd:ld h,e
        ld de,(m4crspbuf+4)
        db #fd:ld l,d
        db #fd:ld h,e
        ld a,(m4crspbuf+0)
        or a
        ret z
        cp 5
        scf
        ld a,0
        ret z
        ld a,neterrdto
        ret


;==============================================================================
;### M4CPC SUBROUTINES ########################################################
;==============================================================================

;### M4CCMD -> send command to the M4
;### Input      HL=command bytes, (HL)=length-1
;### Output     DI
;### Destroyed  AF,BC,HL
m4ccmd  call m4ccmd0
m4ccmd1 ld bc,M4C_PORTACK
        out (c),c
        ret
m4ccmd0 ld bc,M4C_PORTDATA
        ld a,(hl)
        inc a
        di
m4ccmd2 inc b
        outi
        dec a
        jr nz,m4ccmd2
        ret

;### M4CADR -> calculates banking code and new address
;### Input      HL=address (#0000-#ffff), E=bank
;### Output     HL=address (#4000-#7fff), A=banking code
;### Destroyed  F,D
m4cadr  ld a,h
        rlca
        rlca
        and #3
        add #c4
        ld d,a
        ld a,e
        dec a
        add a
        add a
        add a
        add d
        res 7,h
        set 6,h
        ret

;### M4CSCK -> get new socket from M4
;### Input      A=daemon socket (0-3)
;### Output     CF=0 ok (m4cscktrn updated), A=M4 socket
;###            CF=1 error (A=error code)
;### Destroyed  F,BC,HL,IX,IY
m4csckcmd   db 5:dw M4C_CMDSOCKET:db 0,0,6

m4csck  ld (m4csck1+2),a
        ld a,(m4cromnum)
        or a
        scf
        ld a,neterrnhw
        ret z
        ld hl,m4csckcmd
        call m4ccmd
        ld iy,(m4crombuf)
        ld e,1
        call m4cred
        ei
        ld a,(m4crspbuf)
        ld iy,m4cscktrn
m4csck1 ld (iy+0),a
        cp -1
        ccf
        ret nc
        ld a,neterrsfr
        ret

;### M4CSCT -> translate daemon socket to M4 socket
;### Input      A=daemon socket (0-3)
;### Output     CF=0 -> A,(HL)=M4 socket
;###            CF=1 -> error (A=error code)
;### Destroyed  F,BC
m4csct  ld c,a
        ld b,0
        ld hl,m4cscktrn
        add hl,bc
        ld a,(hl)
        cp -1
        ccf
        ret nc
        ld a,neterrsex
        ret

;### M4CSTA -> get socket status
;### Input      A=daemon socket (0-3)
;### Output     CF=0 -> (m4crspbuf)=socket data, A=socket status [0=ok, 1=connect in progress, 2=send in progress, ...], BC=received size
;###            CF=1 -> error
;### Destroyed  F,DE,HL,IX,IY
m4csta  call m4csct
        ret c
m4csta0 add a:add a:add a:add a
        ld c,a
        ld b,0
        ld iy,(m4cromsoc)
        add iy,bc
        ld e,16
        call m4cred
        ei
        ld a,(m4crspbuf+0)
        ld bc,(m4crspbuf+2)
        ret


;==============================================================================
;### M4CPC HIGHLEVEL INTERFACE EQUS ###########################################
;==============================================================================

lowini      equ m4cini

lowtop      equ m4ctop
lowtcl      equ m4ctcl
lowtst      equ m4ctst
lowtrx      equ m4ctrx
lowttx      equ m4cttx
lowtdc      equ m4ctdc
lowtsk      equ m4ctsk
lowtfl      equ m4ctfl

lowuop      equ m4cuop
lowucl      equ m4cucl
lowust      equ m4cust
lowurx      equ m4curx
lowutx      equ m4cutx
lowusk      equ m4cusk
