;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@                         LOCALHOST DRIVER ROUTINES                          @
;@                                                                            @
;@             (c) 2015-2015 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- LOCALHOST TCP ROUTINES ---------------------------------------------------
;### LHSTOP -> Localhost TCP open connection
;### LHSTCL -> Localhost TCP close connection
;### LHSTST -> Localhost TCP test connection status
;### LHSTRX -> Localhost TCP receive data from connection
;### LHSTTX -> Localhost TCP send data to connection
;### LHSTDC -> Localhost TCP disconnect connection

;--- LOCALHOST UDP ROUTINES ---------------------------------------------------
;### LHSUOP -> Localhost UDP open
;### LHSUCL -> Localhost UDP close
;### LHSUST -> Localhost UDP test status
;### LHSURX -> Localhost UDP receive data
;### LHSUTX -> Localhost UDP send data

;--- LOCALHOST SUBROUTINES ----------------------------------------------------
;### LHSSCK -> get socket datarecord

;--- LOCALHOST HIGHLEVEL INTERFACE EQUS ---------------------------------------


low_sockmax     equ 16
low_vermaj      equ 0   ;version 0.1
low_vermin      equ 1

lhsstanon   equ 0   ;unused
lhsstatli   equ 1   ;tcp listening
lhsstates   equ 2   ;tcp established
lhsstatcw   equ 3   ;tcp close waiting
lhsstatcl   equ 4   ;tcp close
lhsstaudp   equ 5   ;udp


lhsdatsta   equ 0   ;status (0=unused, 1=TCP listening, 2=TCP established, 3=TCP close waiting, 4=TCP close, 5=UDP)
lhsdatsck   equ 1   ;connected socket
lhsdatprt   equ 2   ;port
lhsdatins   equ 4   ;input start (0-1023) [UDP=remote port]
lhsdatinl   equ 6   ;input length (0-1024)
lhsdatlen   equ 8   ;struct length

lhsdatmem   ds lhsdatlen*low_sockmax


;==============================================================================
;### LOCALHOST TCP ROUTINES ###################################################
;==============================================================================

;### LHSTOP -> Localhost TCP open connection
;### Input      A=socket number (0-3), E=mode (0=client, 1=server), IX=structure (2byte local port [, 4byte remote IP, 2byte remote port])
;### Output     CF=0 ok, CF=1 error (A=error code)
;### Destroyed  AF,BC,DE,HL,IY
lhstop  call lhssck
        ld a,(ix+0)                 ;store local port
        ld (iy+lhsdatprt+0),a
        ld a,(ix+1)
        ld (iy+lhsdatprt+1),a
        ld (iy+lhsdatinl+0),0       ;clear receive buffer
        ld (iy+lhsdatinl+1),0
        ld a,e
        or a
        jr z,lhstop1
        ld (iy+lhsdatsta),lhsstatli ;** server -> switch to listening status
        ret
lhstop1 ld (iy+lhsdatsta),lhsstatcl ;** client
        ld c,(ix+6)                 ;search for remote port on listening sockets
        ld b,(ix+7)
        ld hl,lhsdatmem
        ld e,low_sockmax
lhstop2 push de
        ld a,(hl)
        inc hl
        inc hl
        dec a
        jr nz,lhstop3
        ld e,(hl)
        inc hl
        ld d,(hl)
        dec hl
        ex de,hl
        or a
        sbc hl,bc
        ex de,hl
        jr z,lhstop4
lhstop3 ld de,6
        add hl,de
        pop de
        dec e
        jr nz,lhstop2
        or a
        ret
lhstop4 ld a,(lhssckn)              ;socket found, establish connection on both sides
        dec hl
        ld (hl),a
        dec hl
        ld (hl),lhsstates
        pop de
        ld a,low_sockmax
        sub e
        ld (iy+lhsdatsck),a
        ld (iy+lhsdatsta),lhsstates
        or a
        ret

;### LHSTCL -> Localhost TCP close connection
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IY
lhstcl  call lhssck
        ld a,(iy+lhsdatsta)
        ld (iy+lhsdatsta),lhsstatcl
        cp 2
        ret nz
        ld a,(iy+lhsdatsck)         ;close remote as well
        call lhssck
        ld (iy+lhsdatsta),lhsstatcl
        ret

;### LHSTST -> Localhost TCP test connection status
;### Input      A=socket number (0-3)
;### Output     A=status (0=in process, 2=established, 3=close_wait, 4=close, +128=data received)
;###            BC=received bytes, IX,IY=remote IP, DE=remote port
;### Destroyed  F,HL
lhstst  call lhssck
        push iy
        ld a,(iy+lhsdatsck)
        call lhssck
        ld e,(iy+lhsdatprt+0)       ;get remote port
        ld d,(iy+lhsdatprt+1)
        pop iy
        ld c,(iy+lhsdatinl+0)       ;get received bytes
        ld b,(iy+lhsdatinl+1)
        ld a,b
        or c
        jr z,lhstst1
        ld a,128
lhstst1 add (iy+lhsdatsta)          ;status
        ld ix,127
        ld iy,1*256                 ;ix,iy=127.0.0.1
        ret

;### LHSTRX -> Localhost TCP receive data from connection
;### Input      A=socket number (0-3), HL=memory address, E=memory bank, BC=length (<= last received length)
;### Output     BC=remaining bytes
;### Destroyed  AF,DE,HL,IY
lhstrna dw 0    ;transfer address
lhstrnb db 0    ;transfer bank
lhstrnl dw 0    ;transfer length

lhstrx  ld (lhstrna),hl
        ld (lhstrnl),bc
        call lhssck
        ld (lhstrx4+1),bc
        ld a,e
        add a:add a:add a:add a
        ld hl,App_BnkNum
        add (hl)
        ld (lhstrnb),a
lhstrx1 ld e,(iy+lhsdatins+0)
        ld d,(iy+lhsdatins+1)
        call lhstrx0
lhstrx4 ld hl,0
        add hl,de
        push de
        ld de,(lhstrna)
        ld a,(lhstrnb)
        push iy
        push bc
        push de
        rst #20:dw jmp_bnkcop
        pop hl
        pop bc
        add hl,bc
        ld (lhstrna),hl
        pop iy
        pop hl
        add hl,bc
        res 2,h
        ld (iy+lhsdatins+0),l
        ld (iy+lhsdatins+1),h
        ld l,(iy+lhsdatinl+0)
        ld h,(iy+lhsdatinl+1)
        sbc hl,bc
        ld (iy+lhsdatinl+0),l
        ld (iy+lhsdatinl+1),h
        push hl
        ld hl,(lhstrnl)
        or a
        sbc hl,bc
        ld (lhstrnl),hl
        pop bc
        jr nz,lhstrx1
        ret
lhstrx0 ld bc,(lhstrnl)
lhstrx2 ld hl,1024      ;BC=min(BC,1024-DE)
        or a
        sbc hl,de
lhstrx3 or a            ;BC=min(HL,BC)
        sbc hl,bc
        ret nc
        add hl,bc
        ld c,l
        ld b,h
        ret

;### LHSTTX -> Localhost TCP send data to connection
;### Input      A=socket number (0-3), HL=memory address, E=memory bank, BC=length
;### Output     BC=number of sent bytes, HL=number of remaining bytes, ZF=1 -> all bytes have been sent
;### Destroyed  AF,DE,HL,IY
lhsttx  ld (lhstrna),hl
        push bc
        push bc
        call lhssck
        ld a,(App_BnkNum)
        add a:add a:add a:add a
        add e
        ld (lhstrnb),a
        ld a,(iy+lhsdatsck)
        call lhssck                 ;IY=receiver socket data
        ld (lhsttx4+1),bc
        pop bc
        ld e,(iy+lhsdatinl+0)       ;DE=current received buffer length
        ld d,(iy+lhsdatinl+1)
        call lhstrx2                ;BC=min(free space, length)
        ld (lhstrnl),bc
        ld a,c
        or b
        jr z,lhsttx2
        push bc
        ld l,(iy+lhsdatins+0)
        ld h,(iy+lhsdatins+1)
        ld c,(iy+lhsdatinl+0)
        ld b,(iy+lhsdatinl+1)
        add hl,bc
        res 2,h
        ex de,hl                    ;DE=write position in buffer
lhsttx1 push de
        call lhstrx0                ;BC=min(data to send, length until buffer end)
lhsttx4 ld hl,low_bufadr
        add hl,de
        ex de,hl                    ;DE=destination in buffer
        ld hl,(lhstrna)
        ld a,(lhstrnb)
        push iy
        push bc
        push hl
        rst #20:dw jmp_bnkcop
        pop hl
        pop bc
        add hl,bc
        ld (lhstrna),hl
        pop iy
        pop hl
        add hl,bc
        res 2,h
        ex de,hl
        ld l,(iy+lhsdatinl+0)
        ld h,(iy+lhsdatinl+1)
        add hl,bc
        ld (iy+lhsdatinl+0),l
        ld (iy+lhsdatinl+1),h
        ld hl,(lhstrnl)
        or a
        sbc hl,bc
        ld (lhstrnl),hl
        jr nz,lhsttx1
        pop bc
lhsttx2 pop hl
        or a
        sbc hl,bc
        ret

;### LHSTDC -> Localhost TCP disconnect connection
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
lhstdc  call lhssck
        ld a,(iy+lhsdatsta)
        cp 2
        ret nz
        ld a,(iy+lhsdatsck)         ;set close waiting to remote
        call lhssck
        ld (iy+lhsdatsta),lhsstatcw
        ret

;### LHSTSK -> Localhost TCP skip received data from connection
;### Input      A=socket number (0-3), BC=length (<= last received length)
;### Output     BC=remaining bytes
;### Destroyed  F,HL,IY
lhstsk  push bc
        call lhssck
        pop bc
        ld l,(iy+lhsdatins+0)
        ld h,(iy+lhsdatins+1)
        add hl,bc
        res 2,h
        ld (iy+lhsdatins+0),l
        ld (iy+lhsdatins+1),h
        ld l,(iy+lhsdatinl+0)
        ld h,(iy+lhsdatinl+1)
        or a
        sbc hl,bc
        ld (iy+lhsdatinl+0),l
        ld (iy+lhsdatinl+1),h
        ld c,l
        ld b,h
        ret

;### LHSTFL -> Localhost TCP flush outgoing data to connection
;### Input      A=socket number (0-3)
;### Destroyed  -
lhstfl  ret                             ;useless for localhost


;==============================================================================
;### LOCALHOST UDP ROUTINES ###################################################
;==============================================================================

;### LHSUOP -> Localhost UDP open
;### Input      A=socket number (0-3), IX=structure (2byte local port)
;### Output     CF=0 ok, CF=1 error (A=error code)
;### Destroyed  AF,BC,DE,HL,IY
lhsuop  call lhssck
        ld a,(ix+0)                 ;store local port
        ld (iy+lhsdatprt+0),a
        ld a,(ix+1)
        ld (iy+lhsdatprt+1),a
        ld (iy+lhsdatinl+0),0       ;clear receive buffer
        ld (iy+lhsdatinl+1),0
        ld (iy+lhsdatsta),lhsstaudp
        or a
        ret

;### LHSUCL -> Localhost UDP close
;### Input      A=socket number (0-3)
;### Destroyed  AF,BC,DE,HL,IYH
lhsucl  call lhssck
        ld (iy+lhsdatsta),lhsstanon
        ret

;### LHSUST -> Localhost UDP test status
;### Input      A=socket number (0-3)
;### Output     A=status (0=in process, 3=idle, +128=data received [BC=received bytes, IX,IY=remote IP, DE=remote port])
;### Destroyed  F,BC,DE,HL
lhsust  call lhssck
        ld c,(iy+lhsdatinl+0)
        ld b,(iy+lhsdatinl+1)
        ld a,c
        or b
        ld a,3
        ret z
        ld e,(iy+lhsdatins+0)
        ld d,(iy+lhsdatins+1)
        ld ix,127
        ld iy,1*256                 ;ix,iy=127.0.0.1
        ld a,3+128
        ret

;### LHSURX -> Localhost UDP receive data
;### Input      A=socket number (0-3), HL=remote address, E=remote bank, BC=full data length
;### Destroyed  AF,BC,DE,HL,IYH
lhsurx  call lhssck
        push bc
        ld c,(iy+lhsdatinl+0)
        ld b,(iy+lhsdatinl+1)
        ld (iy+lhsdatinl+0),0
        ld (iy+lhsdatinl+1),0
        ld a,e
        add a:add a:add a:add a
        ex de,hl
        ld hl,App_BnkNum
        add (hl)
        pop hl
        rst #20:dw jmp_bnkcop
        ret

;### LHSUTX -> Localhost UDP send data
;### Input      A=socket number (0-3), HL=local address, E=local bank, BC=length, IX=structure (4byte remote IP, 2byte remote port])
;### Output     CF=1 send buffer full
;### Destroyed  AF,DE,HL,IYH
lhsutx  ld (lhsutx5+1),hl
        ld (lhsutx4+1),bc
        call lhssck
        ld a,e
        ld (lhsutx6+1),a
        ld c,(ix+4)                 ;search for remote port on UDP sockets
        ld b,(ix+5)
        ld hl,lhsdatmem
        ld e,low_sockmax
lhsutx1 push de
        ld a,(hl)
        inc hl
        inc hl
        cp 5
        jr nz,lhsutx2
        ld e,(hl)
        inc hl
        ld d,(hl)
        dec hl
        ex de,hl
        or a
        sbc hl,bc
        ex de,hl
        jr z,lhsutx3
lhsutx2 ld de,6
        add hl,de
        pop de
        dec e
        jr nz,lhsutx1
        ret
lhsutx3 ld de,4                     ;test, if there is still a package in the receive-buffer
        add hl,de
        pop de
        ld a,(hl)
        inc hl
        or (hl)
        scf
        ret nz
lhsutx4 ld bc,0                     ;store length for the receiver
        ld (hl),b
        dec hl
        ld (hl),c
        dec hl
        ld a,(iy+lhsdatprt+1)       ;store remote port for the receiver
        ld (hl),a
        dec hl
        ld a,(iy+lhsdatprt+0)
        ld (hl),a
        ld a,low_sockmax
        sub e
        push bc
        call lhssck                 ;get receive buffer address
        ld e,c
        ld d,b
        pop bc
lhsutx5 ld hl,0
        ld a,(App_BnkNum)
        add a:add a:add a:add a
lhsutx6 add 0
        rst #20:dw jmp_bnkcop       ;copy package to receiver
        or a
        ret

;### LHSUSK -> Localhost UDP skip received data
;### Input      A=socket number (0-3), BC=full data length
;### Destroyed  AF,BC,DE,HL,IYH
lhsusk  call lhssck
        ld (iy+lhsdatinl+0),0
        ld (iy+lhsdatinl+1),0
        ret


;==============================================================================
;### LOCALHOST SUBROUTINES ####################################################
;==============================================================================

;### LHSSCK -> get socket datarecord
;### Input      A=socket number (0-3)
;### Output     IY=socket datarecord, BC=buffer address
;### Destroyed  F
lhssckn db 0
lhssck  ld (lhssckn),a
        ld b,a
        add a:add a:add a
        ld c,a
        ld a,b
        ld b,0
        ld iy,lhsdatmem
        add iy,bc
        push hl
        ld l,0
        ld h,a
        sla h:sla h
        ld bc,low_bufadr
        add hl,bc
        ld c,l
        ld b,h
        pop hl
        ret


;==============================================================================
;### LOCALHOST HIGHLEVEL INTERFACE EQUS #######################################
;==============================================================================

lowtop      equ lhstop
lowtcl      equ lhstcl
lowtst      equ lhstst
lowtrx      equ lhstrx
lowttx      equ lhsttx
lowtdc      equ lhstdc
lowtsk      equ lhstsk
lowtfl      equ lhstfl

lowuop      equ lhsuop
lowucl      equ lhsucl
lowust      equ lhsust
lowurx      equ lhsurx
lowutx      equ lhsutx
;lowusk      equ lhsusk
