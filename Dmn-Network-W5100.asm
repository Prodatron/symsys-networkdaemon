;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@                       W5100 MIDLEVEL DRIVER ROUTINES                       @
;@                                                                            @
;@             (c) 2015-2015 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- W5100 CONTROL ROUTINES ---------------------------------------------------
;### W51INI -> init the W5100 and setup network configuration

;--- W5100 TCP ROUTINES -------------------------------------------------------
;### W51TOP -> W5100 TCP open connection
;### W51TCL -> W5100 TCP close connection
;### W51TST -> W5100 TCP test connection status
;### W51TRX -> W5100 TCP receive data from connection
;### W51TTX -> W5100 TCP send data to connection
;### W51TDC -> W5100 TCP disconnect connection
;### W51TSK -> W5100 TCP skip received data from connection
;### W51TFL -> W5100 TCP flush outgoing data to connection

;--- W5100 UDP ROUTINES -------------------------------------------------------
;### W51UOP -> W5100 UDP open socket
;### W51UCL -> W5100 UDP close socket
;### W51UST -> W5100 UDP test status
;### W51URX -> W5100 UDP receive data
;### W51UTX -> W5100 UDP send data
;### W51USK -> W5100 UDP skip received data

;--- W5100 SUBROUTINES --------------------------------------------------------
;### W51BTR -> transfers data to/from a W5100 socket buffer
;### W51BLN -> calculates section length for data transfer

;--- W5100 HIGHLEVEL INTERFACE EQUS -------------------------------------------


low_sockmax     equ 4

;==============================================================================
;### W5100 REGISTERS AND CONSTANTS ############################################
;==============================================================================

;W5100 common registers (#00xx)
w51_com_mr      equ #00 ;[1] Mode
w51_com_gar     equ #01 ;[4] Gateway Address
w51_com_subr    equ #05 ;[4] Subnet Mask Address
w51_com_shar    equ #09 ;[6] Local Hardware (MAC) Address
w51_com_sipr    equ #0f ;[4] Local IP Address
w51_com_ir      equ #15 ;[1] Interrupt
w51_com_imr     equ #16 ;[1] Interrupt Mask
w51_com_rtr     equ #17 ;[2] Retry Time
w51_com_rcr     equ #19 ;[1] Retry Count
w51_com_rmsr    equ #1a ;[1] RX Memory Size
w51_com_tmsr    equ #1b ;[1] TX Memory Size
w51_com_patr    equ #1c ;[2] Authentication Type in PPPoE
w51_com_ptimer  equ #28 ;[1] PPP LCP Request Timer
w51_com_pmagic  equ #29 ;[1] PPP LCP Magic number
w51_com_uipr    equ #2a ;[4] Unreachable IP Address
w51_com_uport   equ #2e ;[2] Unreachable Port

;W5100 socket specific registers (#0sxx, s=4-7)
w51_soc_mr      equ #00 ;[1] Socket n Mode
w51_soc_cr      equ #01 ;[1] Socket n Command
w51_soc_ir      equ #02 ;[1] Socket n Interrupt
w51_soc_sr      equ #03 ;[1] Socket n Status
w51_soc_port    equ #04 ;[2] Socket n Local Port
w51_soc_dhar    equ #06 ;[6] Socket n Remote Hardware Address
w51_soc_dipr    equ #0C ;[4] Socket n Remote IP Address
w51_soc_dport   equ #10 ;[2] Socket n Remote Port
w51_soc_mssr    equ #12 ;[2] Socket n Maximum Segment Size
w51_soc_proto   equ #14 ;[1] Socket n Protocol in IP Raw mode
w51_soc_tos     equ #15 ;[1] Socket n IP TOS
w51_soc_ttl     equ #16 ;[1] Socket n IP TTL
w51_soc_txfsr   equ #20 ;[2] Socket n TX Free Size
w51_soc_txrd    equ #22 ;[2] Socket n TX Read Pointer
w51_soc_txwr    equ #24 ;[2] Socket n TX Write Pointer
w51_soc_rxrsr   equ #26 ;[2] Socket n RX Received Size
w51_soc_rxrd    equ #28 ;[2] Socket n RX Read Pointer

;W5100 constants
w51_ini_imr     equ #00     ;disable all interrupts
w51_ini_rtr     equ #07d0   ;default for Retry Time
w51_ini_trc     equ #08     ;default for Retry Count
w51_ini_bufsiz  equ #5555   ;2048bytes RX and TX buffer size for each of the four sockets

;protocol values
w51_prot_none       equ #00
w51_prot_tcp        equ #01
w51_prot_udp        equ #02
w51_prot_ipraw      equ #03

;command values
w51_cmd_open        equ #01
w51_cmd_listen      equ #02
w51_cmd_connect     equ #04
w51_cmd_discon      equ #08
w51_cmd_close       equ #10
w51_cmd_send        equ #20
w51_cmd_send_mac    equ #21
w51_cmd_send_keep   equ #22
w51_cmd_recv        equ #40

;status register values
w51_sta_closed      equ #00
w51_sta_init        equ #13
w51_sta_listen      equ #14
w51_sta_established equ #17
w51_sta_close_wait  equ #1c
w51_sta_udp         equ #22
w51_sta_ipraw       equ #32
w51_sta_macraw      equ #42
w51_sta_pppoe       equ #5f

w51_sta_synsent     equ #15
w51_sta_synrecv     equ #16
w51_sta_fin_wait    equ #18
w51_sta_closing     equ #1a
w51_sta_time_wait   equ #1b
w51_sta_last_ack    equ #1d
w51_sta_arp1        equ #11
w51_sta_arp2        equ #21
w51_sta_arp3        equ #31

w51_sta_arp         equ #01
w51_sta_fin_wait2   equ #19


;==============================================================================
;### W5100 CONTROL ROUTINES ###################################################
;==============================================================================

;### W51INI -> init the W5100 and setup network configuration
;### Input      A=type (0=init, 1=set MAC address, 2=set IP address)
;###            HL=data (1 -> 6byte MAC address;
;###                     2 -> 4byte IP, 4byte Subnet mask, 4byte Gateway)
;### Destroyed  AF,BC,DE,HL,IYH
w51ini  db #fd:ld h,0
        cp 1
        jr z,w51ini3
        jr nc,w51ini2
        ld e,w51_ini_mr             ;*** INIT
        call w51wbr:db w51_com_mr       ;init mode
w51ini1 call w51rbr:db w51_com_mr
        bit 7,e
        jr nz,w51ini1
        ld e,w51_ini_imr
        call w51wbr:db w51_com_imr      ;init interrupt mask
        ld de,w51_ini_rtr
        call w51wwr:db w51_com_rtr      ;init retry time
        ld e,w51_ini_trc
        call w51wbr:db w51_com_rcr      ;init retry counter
        ld de,w51_ini_bufsiz
        call w51wwr:db w51_com_rmsr     ;init RX and TX buffer size
        ret
w51ini2 push hl                     ;*** IP
        ex de,hl
        ld c,4
        call w51wdr:db w51_com_sipr     ;set local IP
        pop hl
        ld bc,4
        add hl,bc
        push hl
        ex de,hl
        call w51wdr:db w51_com_subr     ;set subnet mask
        pop hl
        ld bc,4
        add hl,bc
        ex de,hl
        call w51wdr:db w51_com_gar      ;set gateway
        ret
w51ini3 ex de,hl                    ;*** MAC
        ld c,6
        call w51wdr:db w51_com_shar     ;set local hardware (mac) address
        ret


;==============================================================================
;### W5100 TCP ROUTINES #######################################################
;==============================================================================

;### W51TOP -> W5100 TCP open connection
;### Input      A=socket number (0-3), E=mode (0=client, 1=server), IX=structure (2byte local port [, 4byte remote IP, 2byte remote port])
;### Output     CF=0 ok, CF=1 error (A=error code)
;### Destroyed  AF,BC,DE,HL,IY
w51top  add 4
        db #fd:ld h,a
        db #fd:ld l,e
        ld a,(net_status)
        inc a
        scf
        ld a,neterrnip
        ret nz
w51top1 ld e,w51_prot_tcp               ;*** tcp/port/open
        call w51wbr:db w51_soc_mr       ;set TCP mode
        ld e,(ix+0)
        ld d,(ix+1)
        call w51wwr:db w51_soc_port     ;set local port
        ld e,w51_cmd_open
        call w51wbr:db w51_soc_cr       ;set OPEN command
        call w51rbr:db w51_soc_sr       ;check status
        ld a,e
        cp w51_sta_init
        jr z,w51top3
w51top2 ld e,w51_cmd_close              ;try again, if not INIT ##!!## timeout
        call w51wbr:db w51_soc_cr
        jr w51top1
w51top3 db #fd:dec l
        jr nz,w51top4
        ld e,w51_cmd_listen             ;*** SERVER mode
        call w51wbr:db w51_soc_cr       ;set LISTEN command
        call w51rbr:db w51_soc_sr       ;check status
        ld a,e
        cp w51_sta_listen
        ret z
        db #fd:inc l
        jr w51top2
w51top4 push ix:pop de                  ;*** CLIENT mode
        inc de
        inc de
        ld c,4
        call w51wdr:db w51_soc_dipr     ;set remote IP
        ld e,(ix+6)
        ld d,(ix+7)
        call w51wwr:db w51_soc_dport    ;set remote port
        ld e,w51_cmd_connect
        call w51wbr:db w51_soc_cr       ;set CONNECT command
        or a
        ret

;### W51TCL -> W5100 TCP close connection
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
w51tcl  add 4
        db #fd:ld h,a
        ld e,w51_cmd_close
        call w51wbr:db w51_soc_cr       ;send CLOSE command
        ret

;### W51TST -> W5100 TCP test connection status
;### Input      A=socket number (0-3)
;### Output     A=status (0=in process, 2=established, 3=close_wait, 4=close, +128=data received)
;###            BC=received bytes, IX,IY=remote IP, DE=remote port
;### Destroyed  F,HL
w51tst  add 4
        db #fd:ld h,a
        call w51rbr:db w51_soc_sr
        ld a,e
        cp w51_sta_established
        ld l,2
        jr z,w51tst1
        cp w51_sta_close_wait
        ld l,3
        jr z,w51tst1
        cp w51_sta_closed
        ld l,4
        jr z,w51tst1
        ld l,0
w51tst1 push hl
        call w51rwr:db w51_soc_dipr+0
        db #dd:ld l,e
        db #dd:ld h,d
        call w51rwr:db w51_soc_dipr+2
        push de
        call w51rwr:db w51_soc_dport
        push de
        call w51rwr:db w51_soc_rxrsr
        ld c,e
        ld b,d                          ;bc=length
        pop de                          ;de=remote port
        pop iy                          ;ix,iy=remote IP
        pop hl
        ld a,c
        or b
        ld a,l
        ret z
        add 128
        ret

;### W51TRX -> W5100 TCP receive data from connection
;### Input      A=socket number (0-3), HL=memory address, E=memory bank, BC=length (<= last received length)
;### Output     BC=remaining bytes
;### Destroyed  AF,DE,HL,IYH
w51trx  add 4
        db #fd:ld h,a
        ld a,e
        ld (w51btrbnk),a
        ld (w51btrlen),bc
        push hl
        ld hl,w51rdb                    ;set READ methode
        ld (w51btr2+1),hl
        push bc
        call w51rwr:db w51_soc_rxrd     ;get read position
        pop bc
        pop hl                          ;HL=memory address
        call w51btr                     ;transfer data
        call w51rwr:db w51_soc_rxrd     ;get old read position
        ld hl,(w51btrlen)
        call w51trx1
        call w51rwr:db w51_soc_rxrsr
        ld c,e
        ld b,d                          ;bc=length
        ret
w51trx1 add hl,de
        ex de,hl
        call w51wwr:db w51_soc_rxrd     ;set new read position
        ld e,w51_cmd_recv
        call w51wbr:db w51_soc_cr       ;send RECEIVED command
        ret

;### W51TTX -> W5100 TCP send data to connection
;### Input      A=socket number (0-3), HL=memory address, E=memory bank, BC=length
;### Output     BC=number of sent bytes, HL=number of remaining bytes, ZF=1 -> all bytes have been sent
;### Destroyed  AF,DE,HL,IYH
w51ttxlen   dw 0    ;sent length
w51ttx  add 4
        db #fd:ld h,a
        ld a,e
        ld (w51btrbnk),a
        ld (w51btrlen),bc
        push hl
        ld hl,w51wdb                    ;set WRITE methode
        ld (w51btr2+1),hl
        push bc
        call w51rwr:db w51_soc_txfsr    ;get FREE size
        pop bc
        ld l,e
        ld h,d
        or a
        sbc hl,bc                       ;check, if free<to send
        jr nc,w51ttx1
        ld c,e                          ;BC=min(free,to send)
        ld b,d
w51ttx1 ld (w51ttxlen),bc
        push bc
        call w51rwr:db w51_soc_txwr     ;get write position
        pop bc
        pop hl                          ;HL=memory address
        call w51btr                     ;transfer data
        call w51rwr:db w51_soc_txwr     ;get old write position
        ld hl,(w51ttxlen)
        push hl
        ld a,l
        or h
        jr z,w51ttx2                    ;skip, if no space for sending
        add hl,de
        ex de,hl
        call w51wwr:db w51_soc_txwr     ;set new write position
        ld e,w51_cmd_send
        call w51wbr:db w51_soc_cr       ;send SEND command
w51ttx2 pop bc                          ;BC=sent bytes
        ld hl,(w51btrlen)
        or a
        sbc hl,bc                       ;ZF=1, if all bytes sent
        ret

;### W51TDC -> W5100 TCP disconnect connection
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
w51tdc  add 4
        db #fd:ld h,a
        ld e,w51_cmd_discon
        call w51wbr:db w51_soc_cr       ;send DISCONNECT command
        ret

;### W51TSK -> W5100 TCP skip received data from connection
;### Input      A=socket number (0-3), BC=length (<= last received length)
;### Output     BC=remaining bytes
;### Destroyed  AF,DE,HL,IYH
w51tsk  add 4
        db #fd:ld h,a
        push bc
        call w51rwr:db w51_soc_rxrd     ;get old read position
        pop hl
        call w51trx1                    ;update read position, send RECEIVED command
        call w51rwr:db w51_soc_rxrsr
        ld c,e
        ld b,d                          ;bc=remaining length
        ret

;### W51TFL -> W5100 TCP flush outgoing data to connection
;### Input      A=socket number (0-3)
;### Destroyed  -
w51tfl  ret                             ;not supported by the W5100


;==============================================================================
;### W5100 UDP ROUTINES #######################################################
;==============================================================================

;### W51UOP -> W5100 UDP open socket
;### Input      A=socket number (0-3), IX=structure (2byte local port)
;### Output     CF=0 ok, CF=1 error (A=error code)
;### Destroyed  AF,BC,DE,HL,IY
w51uop0 add 4
        db #fd:ld h,a
        ld a,(net_status)
        or a
        scf
        ld a,neterrnhw
        ret z
        jr w51uop1
w51uop  add 4
        db #fd:ld h,a
        ld a,(net_status)
        inc a
        scf
        ld a,neterrnip
        ret nz
w51uop1 ld e,w51_prot_udp               ;*** udp/port/open
        call w51wbr:db w51_soc_mr       ;set UDP mode
        ld e,(ix+0)
        ld d,(ix+1)
        call w51wwr:db w51_soc_port     ;set local port
        ld e,w51_cmd_open
        call w51wbr:db w51_soc_cr       ;set OPEN command
        call w51rbr:db w51_soc_sr       ;check status
        ld a,e
        cp w51_sta_udp
        ret z
        ld e,w51_cmd_close              ;try again, if not UDP ##!!## timeout
        call w51wbr:db w51_soc_cr
        jr w51uop1

;### W51UCL -> W5100 UDP close socket
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
w51ucl  equ w51tcl

;### W51UST -> W5100 UDP test status
;### Input      A=socket number (0-3)
;### Output     A=status (0=in process, 3=idle, +128=data received [BC=received bytes, IX,IY=remote IP, DE=remote port])
;### Destroyed  F,BC,DE,HL
w51ustb ds 8
w51ust  add 4
        db #fd:ld h,a
        call w51rwr:db w51_soc_rxrsr
        ld a,e
        or d
        jr nz,w51ust1
        call w51rbr:db w51_soc_cr
        ld a,e
        or a
        ld a,3
        ret z
        xor a
        ret
w51ust1 ld a,(App_BnkNum)               ;incoming packet
        ld (w51btrbnk),a
        ld hl,w51rdb                    ;set READ methode
        ld (w51btr2+1),hl
        call w51rwr:db w51_soc_rxrd     ;get read position
        ld bc,8
        ld hl,w51ustb
        call w51btr                     ;read UDP packet header
        ld ix,(w51ustb+0)
        ld iy,(w51ustb+2)       ;ix,iy=remote IP
        ld hl,(w51ustb+4)
        ld e,h:ld d,l           ;de=remote port
        ld hl,(w51ustb+6)
        ld c,h:ld b,l           ;bc=length
        ld a,128+3
        ret

;### W51URX -> W5100 UDP receive data
;### Input      A=socket number (0-3), HL=remote address, E=remote bank, BC=full data length
;### Destroyed  AF,BC,DE,HL,IYH
w51urx  add 4
        db #fd:ld h,a
        ld a,e
        ld (w51btrbnk),a
        ld (w51btrlen),bc
        push hl
        ld hl,w51rdb                    ;set READ methode
        ld (w51btr2+1),hl
        push bc
        call w51rwr:db w51_soc_rxrd     ;get read position
        ex de,hl
        ld bc,8
        add hl,bc
        ex de,hl
        pop bc
        pop hl                          ;HL=memory address
        call w51btr                     ;transfer data
        call w51rwr:db w51_soc_rxrd     ;get old read position
        ld hl,(w51btrlen)
        add hl,de
        ld de,8
        jp w51trx1                      ;update read position, send RECEIVED command

;### W51UTX -> W5100 UDP send data
;### Input      A=socket number (0-3), HL=local address, E=local bank, BC=length, IX=structure (4byte remote IP, 2byte remote port])
;### Output     CF=1 send buffer full
;### Destroyed  AF,DE,HL,IYH
w51utx  add 4
        db #fd:ld h,a
        ld a,e
        ld (w51btrbnk),a
        push hl
        ld hl,w51wdb                    ;set WRITE methode
        ld (w51btr2+1),hl
        push bc
        call w51rwr:db w51_soc_txfsr    ;get FREE size
        pop bc
        ex de,hl
        or a
        sbc hl,bc                       ;check, if free<to send
        pop hl
        ret c                           ;not enough buffer space -> return
        push hl
        push ix:pop de
        push bc
        ld c,4
        call w51wdr:db w51_soc_dipr     ;set remote IP
        ld e,(ix+4)
        ld d,(ix+5)
        call w51wwr:db w51_soc_dport    ;set remote port
        call w51rwr:db w51_soc_txwr     ;get write position
        pop bc
        pop hl                          ;HL=memory address
        push bc
        call w51btr                     ;transfer data
        call w51rwr:db w51_soc_txwr     ;get old write position
        pop hl
        add hl,de
        ex de,hl
        call w51wwr:db w51_soc_txwr     ;set new write position
        ld e,w51_cmd_send
        call w51wbr:db w51_soc_cr       ;send SEND command
        or a
        ret

;### W51USK -> W5100 UDP skip received data
;### Input      A=socket number (0-3), BC=full data length
;### Destroyed  AF,BC,DE,HL,IYH
w51usk  add 4
        db #fd:ld h,a
        push bc
        call w51rwr:db w51_soc_rxrd     ;get old read position
        pop hl
        add hl,de
        ld de,8
        jp w51trx1                      ;update read position, send RECEIVED command


;==============================================================================
;### W5100 SUBROUTINES ########################################################
;==============================================================================

;### W51BTR -> transfers data to/from a W5100 socket buffer
;### Input      HL=memory address, (w51btrbnk)=memory bank, DE=buffer address, BC=length (<= available size in buffer), IYH=socket+4, (w51btr2+1)=w51rdb/w51wdb
;### Destroyed  AF,BC,DE,HL
w51btrbnk   db 0    ;memory bank
w51btrlen   dw 0    ;total length

w51btr  ld a,d
        and #07
        ld d,a              ;DE=buffer offset (position AND #7ff)
w51btr1 ld a,c
        or b
        ret z
        push bc
        call w51bln         ;BC=section length (min[max bufdif,max memdif] until next 2K/16k boundary)
        push hl
        push de
        push bc
        db #fd:ld a,h
        sub 4
        add a:add a:add a   ;A=slot*8
        add d
        ld d,a              ;DE=total buffer offset
        ld a,(w51btrbnk)
w51btr2 call 0              ;TRANSFER section
        pop bc
        pop hl
        add hl,bc
        res 3,h
        ex de,hl            ;DE=new buffer offset (0-2047)
        pop hl
        add hl,bc           ;HL=new memory address
        ex (sp),hl
        sbc hl,bc
        ld c,l
        ld b,h              ;BC=remaining length
        pop hl
        jr w51btr1

;### W51BLN -> calculates section length for data transfer
;### Input      DE=buffer offset (0-2047), HL=memory address (0-65535), BC=length (1-2048)
;### Output     BC=section length (until DE hits 2K boundary or HL hits 16K boundary)
;### Destroyed  AF
w51bln  push hl
        push de
        res 7,h
        res 6,h         ;reduce memory address to 16K area
        add hl,bc       ;add length to memory address
        bit 6,h         ;check if still in same 16K area
        res 6,h
        call nz,w51bln3 ;no -> substract overflow from current length
        pop hl
        push hl
        add hl,bc       ;add length to buffer offset
        bit 3,h         ;check if still in 2K area
        res 3,h
        call nz,w51bln3 ;no -> substract overflow from current length
w51bln2 pop de
        pop hl
        ret
w51bln3 ld a,l:ld l,c:ld c,a    ;BC=BC-HL
        ld a,h:ld h,b:ld b,a
        sbc hl,bc
        ld c,l
        ld b,h
        ret


;==============================================================================
;### W5100 HIGHLEVEL INTERFACE EQUS ###########################################
;==============================================================================

lowini      equ w51ini

lowtop      equ w51top
lowtcl      equ w51tcl
lowtst      equ w51tst
lowtrx      equ w51trx
lowttx      equ w51ttx
lowtdc      equ w51tdc
lowtsk      equ w51tsk
lowtfl      equ w51tfl

lowuop      equ w51uop
lowuop0     equ w51uop0
lowucl      equ w51ucl
lowust      equ w51ust
lowurx      equ w51urx
lowutx      equ w51utx
lowusk      equ w51usk
