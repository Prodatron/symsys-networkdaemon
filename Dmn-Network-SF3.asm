;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@                   SYMBIFACE 3 - MIDLEVEL DRIVER ROUTINES                   @
;@                                                                            @
;@             (c) 2019-2019 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo



;--- SF3 CONTROL ROUTINES ---------------------------------------------------
;### SF3INI -> init the SF3 and setup network configuration

;--- SF3 TCP ROUTINES -------------------------------------------------------
;### SF3TOP -> SF3 TCP open connection
;### SF3TCL -> SF3 TCP close connection
;### SF3TST -> SF3 TCP test connection status
;### SF3TRX -> SF3 TCP receive data from connection
;### SF3TTX -> SF3 TCP send data to connection
;### SF3TDC -> SF3 TCP disconnect connection
;### SF3TSK -> SF3 TCP skip received data from connection
;### SF3TFL -> SF3 TCP flush outgoing data to connection

;--- SF3 UDP ROUTINES -------------------------------------------------------
;### SF3UOP -> SF3 UDP open socket
;### SF3UCL -> SF3 UDP close socket
;### SF3UST -> SF3 UDP test status
;### SF3URX -> SF3 UDP receive data
;### SF3UTX -> SF3 UDP send data
;### SF3USK -> SF3 UDP skip received data

;--- SF3 SPECIFIC ROUTINES --------------------------------------------------
;### SF3POL -> Poll stuff (Screenmode)
;### SF3SIG -> Get signal strength
;### SF3DNS -> DNS start request
;### SF3DNR -> DNS check for resolve

;--- SF3 SUBROUTINES --------------------------------------------------------
;### SF3CMD -> send command to the M4
;### SF3ADR -> calculates banking code and new address
;### SF3SCK -> get new socket from M4
;### SF3SCT -> translate daemon socket to M4 socket
;### SF3STA -> get socket status
;### SF3SND -> send data from application memory to the M4

;--- SF3 HIGHLEVEL INTERFACE EQUS -------------------------------------------


low_vermaj      equ 0           ;version 0.1
low_vermin      equ 1

low_sockmax     equ 4

sf3scktrn       ds low_sockmax  ;socket translation table

sf3_sta_delay   equ 2           ;delay for RX/TX status
sf3_sta_tx      db 0            ;TX "led" status
sf3_sta_rx      db 0            ;RX "led" status

sf3exist        db 0            ;0=no SF3, 1=SF3 wifi existing


;==============================================================================
;### SF3 CONSTANTS ############################################################
;==============================================================================

SF3_PORT_CMD        equ #fd41
SF3_PORT_DATA       equ #fd42
SF3_PORT_SOCKET     equ #fd45
SF3_PORT_WIFI       equ #fd49
SF3_PORT_WIFIRSP    equ #fd4e

SF3_BUFSIZE         equ 1400

SF3_CMD_MAIN        equ 90
SF3_CMD_SOCKET      equ 13
SF3_CMD_OPEN_ACT    equ 5
SF3_CMD_CLOSE       equ 10
SF3_CMD_SEND        equ 8
SF3_CMD_RECVSIZE    equ 18


;==============================================================================
;### SF3 CONTROL ROUTINES #####################################################
;==============================================================================

;### SF3INI -> init the SF3 and setup network configuration
;### Input      A=type (5=search, init values -> CF=1 no hardware
;###                    6=get complete config
;###                    7=set complete config
;###                   )
;### Destroyed  AF,BC,DE,HL,IX,IY
sf3ini  xor a
        inc a
        ld (sf3exist),a
        ret


;==============================================================================
;### SF3 TCP ROUTINES #######################################################
;==============================================================================

;### SF3TOP -> SF3 TCP open connection
;### Input      A=socket number (0-3), E=mode (0=client, 1=server), IX=structure (2byte local port [, 4byte remote IP, 2byte remote port])
;### Output     CF=0 ok, CF=1 error (A=error code)
;### Destroyed  AF,BC,DE,HL,IY
sf3topcmd   db 7, 5, 0,0,0,0, 0,0, 0    ;IP (msb), port (msb), socket

sf3top  push de
        push ix
        call sf3sck
        pop ix
        pop de
        ret c
        dec e
        jr z,sf3top1
        ld (sf3topcmd+2+6),a    ;** client mode
        ld l,(ix+5)
        ld h,(ix+4)
        ld (sf3topcmd+2+0),hl
        ld l,(ix+3)
        ld h,(ix+2)
        ld (sf3topcmd+2+2),hl
        ld l,(ix+6)
        ld h,(ix+7)
        ld (sf3topcmd+2+4),hl
        call sf3wir
        ld hl,sf3topcmd
        call sf3cmd
        jp sf3wir
sf3top1 ld a,neterrfnc          ;** server mode (not supported)
        scf
        ret

;### SF3TCL -> SF3 TCP close connection
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
sf3tcl  equ sf3tdc

;### SF3TST -> SF3 TCP test connection status
;### Input      A=socket number (0-3)
;### Output     A=status (0=in process, 2=established, 3=close_wait, 4=close, +128=data received)
;###            BC=received bytes, IX,IY=remote IP, DE=remote port
;### Destroyed  F,HL
sf3tst  call sf3sta         ;get socket status (A=socket status [0=ok, 1=connect in progress, 2=send in progress, 3=close, >3=error], BC=received size)
        ld e,a
        ld a,4
        ret c               ;no socket -> closed (=4)
        ld a,e
        ld e,0
        cp 4
        jr z,sf3tst1        ;4=incoming connection in process (=0)
        cp 1
        jr z,sf3tst1        ;1=outgoing connection in process (=0)
        ld e,2
        jr c,sf3tst1        ;0=connection established (=2)
        cp 3
        jr c,sf3tst1        ;2=sending, connection established (=2)
        ld e,4              ;3=connection closed, >3=error (both results in a 4=close status)
sf3tst1 ld a,c
        or b
        jr z,sf3tst2
        ld a,128
sf3tst2 add e
        ld de,(sf3rspbuf+6)
        db #dd:ld l,d
        db #dd:ld h,e
        ld de,(sf3rspbuf+4)
        db #fd:ld l,d
        db #fd:ld h,e
        ld de,(sf3rspbuf+8)
        ret

;### SF3TRX -> SF3 TCP receive data from connection
;### Input      A=socket number (0-3), HL=memory address, E=memory bank, BC=length (<= last received length)
;### Output     BC=remaining bytes
;### Destroyed  AF,DE,HL,IYH
sf3trxcmd   db 5:dw SF3_CMDRECV:db 0:dw 0

sf3trx  push ix
        push iy
        ld (sf3trxcmd+4),bc
        push hl
        push de
        push bc
        call sf3sct
        ld (sf3trxcmd+3),a
        ld hl,sf3trxcmd
        call sf3cmd
        ld iy,(sf3rombuf)
        ld e,3
        call sf3red
        pop bc
        pop de
        pop hl
        ld a,(sf3rspbuf)
        inc a
        call nz,sf3rcv
        ld a,(sf3trxcmd+3)
        call sf3sta0
        pop iy
        pop ix
        ret

;### SF3TTX -> SF3 TCP send data to connection
;### Input      A=socket number (0-3), HL=memory address, E=memory bank, BC=length
;### Output     BC=number of sent bytes, HL=number of remaining bytes, ZF=1 -> all bytes have been sent
;### Destroyed  AF,DE,IYH
sf3ttxcmd   db 5:dw SF3_CMDSEND:db 0:dw 0   ;socket, length

sf3ttx  push ix
        push iy
        push bc
        push hl
        push de
        push bc
        call sf3sct
        ld (sf3ttxcmd+3),a
        call sf3sta0
        cp 2                    ;still sending -> don't send more data until sending has been finished
        jr z,sf3ttx2
        pop bc
        ld hl,SF3_BUFSIZE
        or a
        sbc hl,bc
        jr nc,sf3ttx1
        ld bc,SF3_BUFSIZE       ;don't send more than buffer-size
sf3ttx1 ld (sf3ttxcmd+4),bc
        push bc
        ld hl,sf3ttxcmd
        call sf3cmd0            ;start command
        pop bc
        pop de
        pop hl
        push bc
        call sf3snd             ;send data
        call sf3cmd1            ;close command
        ld iy,(sf3rombuf)
        ld e,1
        call sf3red             ;check error status
        ei
        pop bc
        pop hl
        ld a,(sf3rspbuf)
        or a
        jr nz,sf3ttx3           ;error -> nothing has been sent
        or a
        sbc hl,bc               ;hl=target-actual=remaining
        pop iy
        pop ix
        ret
sf3ttx2 pop bc
        pop bc
        pop bc
        pop hl
sf3ttx3 xor a
        ld c,a
        ld b,a
        inc a
        pop iy
        pop ix
        ret

;### SF3TDC -> SF3 TCP disconnect connection
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
sf3tdccmd   db 3:dw SF3_CMDCLOSE:db 0

sf3tdc  call sf3sct
        ret c
        ld (sf3tdccmd+3),a
        ld (hl),-1
        ld hl,sf3tdccmd
        call sf3cmd
        ei
        ret

;### SF3TSK -> SF3 TCP skip received data from connection
;### Input      A=socket number (0-3), BC=length (<= last received length)
;### Output     BC=remaining bytes
;### Destroyed  AF,DE,HL,IYH
sf3tsk  ;...
        ret

;### SF3TFL -> SF3 TCP flush outgoing data to connection
;### Input      A=socket number (0-3)
;### Destroyed  -
sf3tfl  ret                             ;not supported by the SF3


;==============================================================================
;### SF3 UDP ROUTINES #######################################################
;==============================================================================

;### SF3UOP -> SF3 UDP open socket
;### Input      A=socket number (0-3), IX=structure (2byte local port)
;### Output     CF=0 ok, CF=1 error (A=error code)
;### Destroyed  AF,BC,DE,HL,IY
sf3uop  scf
        ld a,neterrfnc
        ret

;### SF3UCL -> SF3 UDP close socket
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
sf3ucl  equ sf3tcl

;### SF3UST -> SF3 UDP test status
;### Input      A=socket number (0-3)
;### Output     A=status (0=in process, 3=idle, +128=data received [BC=received bytes, IX,IY=remote IP, DE=remote port])
;### Destroyed  F,BC,DE,HL
sf3ust  ;...
        ret

;### SF3URX -> SF3 UDP receive data
;### Input      A=socket number (0-3), HL=remote address, E=remote bank, BC=full data length
;### Destroyed  AF,BC,DE,HL,IYH
sf3urx  ;...
        ret

;### SF3UTX -> SF3 UDP send data
;### Input      A=socket number (0-3), HL=local address, E=local bank, BC=length, IX=structure (4byte remote IP, 2byte remote port])
;### Output     CF=1 send buffer full
;### Destroyed  AF,DE,HL,IYH
sf3utx  ;...
        ret

;### SF3USK -> SF3 UDP skip received data
;### Input      A=socket number (0-3), BC=full data length
;### Destroyed  AF,BC,DE,HL,IYH
sf3usk  ;...
        ret


;==============================================================================
;### SF3 SPECIFIC ROUTINES ##################################################
;==============================================================================

;### SF3POL -> Poll stuff (Screenmode)
sf3polc db 1
sf3pol  ld hl,sf3polc
        dec (hl)
        ret nz
        ld (hl),10
        ld hl,jmp_scrget
        rst #28
        ld a,#84
        add e
        ld (sf3romena),a
        add 8
        ld (sf3romdis),a
        ret

;### SF3SIG -> Get signal strength
;### Output     E=signal strength (0=no signal, 1-5=strength)
;###            D=status (0=idle, 1=connecting, 2=wrong password, 3=no accesspoint found, 4=connection failed, 5=connected and got IP, 6=unknown error)
;### Destroyed  F,BC,D,HL,IX,IY
sf3sigcmd   db 2:dw SF3_CMDRSSI

sf3sig  ld hl,sf3sigcmd
        call sf3cmd
        ld iy,(sf3rombuf)
        ld e,2
        call sf3red
        ei
        ld de,(sf3rspbuf)
        ld a,6
        cp d
        jr nc,sf3sig1
        ld d,a
sf3sig1 ld a,e
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

;### SF3DNS -> DNS start request
;### Input      (pck_buffer+3)=domain name string (0-terminated)
;### Output     CF=0 -> ok, DNS lookup in progress
;###            CF=1 -> error (A=error code)
;### Destroyed  AF,BC,DE,HL
sf3dns  ld hl,(net_status)
        inc l
        ld a,l
        or h
        scf
        ld a,neterrnip
        ret nz
        ld hl,pck_buffer+3
        xor a
        ld c,2
sf3dns1 inc c
        cp (hl)
        inc hl
        jr nz,sf3dns1
        ld hl,SF3_CMDHOSTIP
        ld (pck_buffer+1),hl
        ld hl,pck_buffer
        ld (hl),c
        call sf3cmd
        ld iy,(sf3rombuf)
        ld e,1
        call sf3red
        ei
        ld a,(sf3rspbuf)
        or a
        inc a
        ret nz
        ld a,neterrdto
        scf
        ret

;### SF3DNR -> DNS check for resolve
;### Input      SF3DNS has been called before
;### Output     CF=0 -> IP received, IX,IY=IP
;###            CF=1 -> A=status (0=still in progress, >0=error)
;### Destroyed  AF,BC,DE,HL,IX,IY
sf3dnr  ld e,8
        ld iy,(sf3romsoc)
        call sf3red
        ei
        ld de,(sf3rspbuf+6)
        db #dd:ld l,d
        db #dd:ld h,e
        ld de,(sf3rspbuf+4)
        db #fd:ld l,d
        db #fd:ld h,e
        ld a,(sf3rspbuf+0)
        or a
        ret z
        cp 5
        scf
        ld a,0
        ret z
        ld a,neterrdto
        ret


;==============================================================================
;### SF3 SUBROUTINES ########################################################
;==============================================================================

;### SF3RDY -> wait for SF3 ready
;### Output     DI,BC=#FD41
;### Veraendert AF
sf3rdy  ld bc,#fd41
        di
        in a,(c)
        cp 1
        ret c
        ei
        rst #30
        jr sf3rdy

;### SF3RDY -> wait for SF3 response
;### Output     BC=#FD41
;### Veraendert AF
sf3rsp  ld bc,#fd41
        in a,(c)
        dec a
        jr z,sf3rsp
        ret

;### SF3WIR -> wait for WIFI response
;### Output     CF=0 ok, CF=1 error (A=error code)
;### Veraendert AF,BC
sf3wir  ld bc,SF3_PORT_WIFIRSP
        in a,(c)
        sub 1
        jr nz,sf3wir1
        rst #30         ;##!!## ???
        jr sf3wir
sf3wir1 ccf
        ret nc
        dec a
        jr z,sf3wir2
        ld c,a
        dec c
        ld a,neterrcon
        ret z
        dec c
        ld a,neterrsex
        ret z
        ld a,neterruhw
        ret
sf3wir2 call sf3rdy
        xor a
        out (c),a
        inc c
        ld a,4
        out (c),a
        dec c
        ld a,72
        out (c),a
        call sf3rsp
        inc c
        in a,(c)
        ei
        ;...            ##!!## analyse error code
        ld a,neterrwif
        scf
        ret

;### SF3CMD -> send Wifi command to the SF3 (without Wifi response)
;### Input      HL=number of wifi bytes,sub command,wifi bytes,...
;### Destroyed  AF,BC,HL
sf3cmd  call sf3rdy
        ld a,(hl)
        inc hl
        inc b:outi
        ld c,SF3_PORT_WIFI
sf3cmd1 inc b:outi
        dec a
        jr nz,sf3cmd1
        ld c,SF3_PORT_CMD
        ld a,SF3_CMD_MAIN
        out (c),a
        call sf3rsp
        ei
        ret

;### SF3ADR -> calculates banking code and new address
;### Input      HL=address (#0000-#ffff), E=bank
;### Output     HL=address (#4000-#7fff), A=banking code
;### Destroyed  F,D
sf3adr  ld a,h
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

;### SF3SCK -> get new socket from SF3
;### Input      A=daemon socket (0-3)
;### Output     CF=0 ok (sf3scktrn updated), A=SF3 socket
;###            CF=1 error (A=error code)
;### Destroyed  F,BC,HL,IX,IY
sf3sckcmd   db 1, 7,13

sf3sck  ld (sf3sck1+2),a
        ld a,(sf3exist)
        or a
        scf
        ld a,neterrnhw
        ret z
        ld hl,(net_status)
        inc l
        ld a,l
        or h
        scf
        ld a,neterrnip
        ret nz
        ld hl,sf3sckcmd
        call sf3cmd
        ld c,SF3_PORT_WIFI
        in a,(c)
        ld iy,sf3scktrn
sf3sck1 ld (iy+0),a
        cp -1
        ccf
        ret nc
        ld a,neterrsfr
        ret

;### SF3SCT -> translate daemon socket to M4 socket
;### Input      A=daemon socket (0-3)
;### Output     CF=0 -> A,(HL)=M4 socket
;###            CF=1 -> error (A=error code)
;### Destroyed  F,BC
sf3sct  ld c,a
        ld b,0
        ld hl,sf3scktrn
        add hl,bc
        ld a,(hl)
        cp -1
        ccf
        ret nc
        ld a,neterrsex
        ret

;### SF3STA -> get socket status
;### Input      A=daemon socket (0-3)
;### Output     CF=0 -> (sf3rspbuf)=socket data, A=socket status [0=ok, 1=connect in progress, 2=send in progress, ...], BC=received size
;###            CF=1 -> error
;### Destroyed  F,DE,HL,IX,IY
sf3sta  call sf3sct
        ret c
sf3sta0 add a:add a:add a:add a
        ld c,a
        ld b,0
        ld iy,(sf3romsoc)
        add iy,bc
        ld e,16
        call sf3red
        ei
        ld bc,(sf3rspbuf+2)
        ld a,b
        cp SF3_BUFSIZE/256
        jr c,sf3sta1
        ld bc,SF3_BUFSIZE
sf3sta1 ld a,(sf3rspbuf+0)
        or a
        ret

;### SF3SND -> send data from application memory to the M4
;### Input      HL=address, E=bank, BC=length (1-4095)
;### Output     DI
;### Destroyed  AF,BC,DE,HL,IX,IY
sf3snd  ld a,sf3_sta_delay
        ld (sf3_sta_tx),a
        ld a,e
        add a:add a:add a:add a
        or b
        db #fd:ld l,c
        db #fd:ld h,a               ;iy=bank, length
        ex de,hl                    ;de=address
        ld ix,SF3_PORTDATA          ;ix=port
        ld hl,jmp_iomout
        rst #28
        ret


;==============================================================================
;### SF3 HIGHLEVEL INTERFACE EQUS ###########################################
;==============================================================================

lowini      equ sf3ini

lowtop      equ sf3top
lowtcl      equ sf3tcl
lowtst      equ sf3tst
lowtrx      equ sf3trx
lowttx      equ sf3ttx
lowtdc      equ sf3tdc
lowtsk      equ sf3tsk
lowtfl      equ sf3tfl

lowuop      equ sf3uop
lowucl      equ sf3ucl
lowust      equ sf3ust
lowurx      equ sf3urx
lowutx      equ sf3utx
lowusk      equ sf3usk
