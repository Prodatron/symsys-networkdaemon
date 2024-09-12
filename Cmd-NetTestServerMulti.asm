;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                           SymbOS network daemon                            @
;@                               N E T T E S T                                @
;@                                                                            @
;@               (c) 2018 by Prodatron / SymbiosiS (Jörn Mika)                @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;### PRGPRZ -> Programm-Prozess
tmpbuf  ds 256

prgprz  call SyShell_PARALL     ;angehangene Parameter und Shell-Prozess, -Höhe und -Breite holen
        call SyShell_PARSHL
        jp c,prgend

        ld hl,txttit            ;title text
        ld e,0
        call SyShell_STROUT
        jp c,prgend

        call SyNet_NETINI           ;init network API
        ld hl,txterrdmn
        jp c,prgend0
        ld a,(SyNet_PrcID)
        call clcdez
        ld (txtdmnfnd1),hl
        ld hl,txtdmnfnd
        ld e,0
        call SyShell_STROUT

prgprz0 call netlis                 ;open first passive connection

prgprz1 rst #30                     ;wait for a network event
        call SyNet_NETEVT
        jr c,prgprz1
        ld (netsck),a
        bit 7,l
        jr z,prgprz4

        push hl                             ;*** DATA RECEIVED
        push bc
        call clcdez
        ld (txtstarcv1),hl
        ld hl,txtstarcv:ld e,0:call SyShell_STROUT0
        pop bc
prgprz3 inc b
        dec b
        jr z,prgprz7
        ld bc,255
prgprz7 ld de,(App_BnkNum)          ;receive data
        ld hl,tmpbuf
        ld a,(netsck)
        push bc
        call SyNet_TCPRCV
        pop de
        jp c,prgprz5
        jr z,prgprz6
        push hl
        ld hl,tmpbuf
        add hl,bc
        ld (hl),0
        ld hl,tmpbuf   :ld e,0:call SyShell_STROUT0
        pop bc
        ld a,c
        or b
        jr nz,prgprz3
prgprz6 ld hl,txtstalfd:ld e,0:call SyShell_STROUT0
        pop hl

prgprz4 ld a,l
        and 127
        cp 2
        jr c,prgprz1
        jr nz,prgprz2

        ld a,(netsck)                       ;*** NEW CONNECTION ESTABLISHED
        call clcdez
        ld (txtstaest1),hl
        ld hl,txtstaest:ld e,0:call SyShell_STROUT0

        ld a,(netanz)
        call clcdez
        ld (nettstdat1),hl
        ld hl,txtstabeg:call SyShell_STROUT0
        ld hl,nettstdat             ;send testdata
        ld bc,nettstdat0-nettstdat
        ld de,(App_BnkNum)
        ld a,(netsck)
        call netsnd
        jr c,prgprz5
        ld hl,txtstasnd:call SyShell_STROUT0
        jp prgprz0

prgprz2 ld a,(netsck)                       ;*** CONNECTION CLOSED
        call clcdez
        ld (txtstacrm1),hl
        ld hl,txtstacrm:ld e,0:call SyShell_STROUT0
        ld hl,netanz
        dec (hl)
        ld hl,txtstaclo
        jp z,prgend0
        jp prgprz1

prgprz5 add "A"                     ;error
        ld hl,txterrx
        ld (hl),a
        call SyShell_STROUT0
        jp prgprz1

;### PRGEND -> Programm beenden
prgend0 ld e,0
        call SyShell_STROUT
prgend  call SyShell_EXIT       ;tell Shell, that process will quit
        ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend1 rst #30
        jr prgend1


;==============================================================================
;### SUB-ROUTINEN #############################################################
;==============================================================================

;### NETLIS -> open new passive connection
;### Output     NETANZ, NETSCK updated
;### Destroyed  ??
netlis  ld a,1
        ld hl,23
        call SyNet_TCPOPN
        ld hl,txterrcon
        jp c,prgend0
        ld hl,netanz
        inc (hl)
        ld hl,txtstaopn:ld e,0:call SyShell_STROUT0
        ret

;### NETSND -> sends data to a TCP connection
;### Input      A=handle, HL=address, E=bank, BC=length
;### Output     CF=0 ok, CF=1 connection closed (BC=remaining length)
;### Destroyed  ??
netsnd  push de
        push hl
        call SyNet_TCPSND   ;BC=bytes sent, HL=bytes remaining
        ex de,hl
        pop hl
        add hl,bc
        ld c,e
        ld b,d
        pop de
        ret z
        rst #30
        jr netsnd

;### CLCDEZ -> Rechnet Byte in zwei Dezimalziffern um
;### Eingabe    A=Wert
;### Ausgabe    L=10er-Ascii-Ziffer, H=1er-Ascii-Ziffer
;### Veraendert AF
clcdez  ld l,"0"
clcdez1 sub 10
        jr c,clcdez2
        inc l
        jr clcdez1
clcdez2 add "0"+10
        ld h,a
        ret


;==============================================================================
;### DATEN-TEIL ###############################################################
;==============================================================================

txttit  db 13,10
        db "NetTestMultiServer 1.0 (c)oded 2018 by Prodatron",13,10,0
txtdmnfnd   db "Network daemon found as process "
txtdmnfnd1  db "##",13,10,0

txterrdmn   db "Network daemon not running!",13,10,0
txterrcon   db "Error: Can't open connection",13,10,13,10,0

txtstaopn   db "Listening at port 23 ",13,10,0
txtstaest   db "New connection at socket "
txtstaest1  db "## established",13,10,0
txtstabeg   db "Start sending...",13,10,0
txtstasnd   db "Data sent",13,10,0
txtstarcv   db "Data at socket "
txtstarcv1  db "## received: ",0
txtstacrm   db "Connection at socket "
txtstacrm1  db "## closed by remote host",13,10,0
txtstaclo   db "All connection closed",13,10,0
txtstalfd   db "#",13,10,0

txterrx     db "  Error",13,10,0

nettstdat   db "Welcome, Client "
nettstdat1  db "##!",13,10
nettstdat0

netanz  db 0    ;number of used sockets
netsck  db 0    ;temp.socket number

App_BegData

;### nothing (more) here
db 0

;==============================================================================
;### TRANSFER-TEIL ############################################################
;==============================================================================

App_BegTrns
;### PRGPRZS -> stack for application process
        ds 128
prgstk  ds 6*2
        dw prgprz
App_PrcID db 0
App_MsgBuf ds 14
