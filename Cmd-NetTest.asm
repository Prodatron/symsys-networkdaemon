;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                           SymbOS network daemon                            @
;@                               N E T T E S T                                @
;@                                                                            @
;@               (c) 2015 by Prodatron / SymbiosiS (Jörn Mika)                @
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

        ld a,1                      ;open connection
        ld hl,80
        call SyNet_TCPOPN
        ld (netsck),a
        ld hl,txterrcon
        jp c,prgend0
        ld hl,txtstaopn:call SyShell_STROUT0

prgprz1 rst #30                     ;wait until established
        call SyNet_NETEVT
        jr c,prgprz1
        ld a,l
        and 127
        cp 2
        jr c,prgprz1
        ld a,10
        jr nz,prgprz5
        ld hl,txtstaest:call SyShell_STROUT0

prgprz2 rst #30                     ;check for data received or close
        call SyNet_NETEVT
        jr c,prgprz2
        ld a,l
        and 127
        cp 3
        jr nc,prgprz4
        bit 7,l
        jr z,prgprz2

if 1
prgprz3 ld de,(App_BnkNum)          ;receive line
        ld hl,tmpbuf
        ld a,(netsck)
        call SyNet_TCPRLN
        jr c,prgprz5
        jr z,prgprz2
        ld a,d
        or a
        jr z,prgprz6
        push bc
        ld c,d
        ld b,0
        ld hl,tmpbuf
        add hl,bc
        ld (hl),0
        ld hl,txtstarcv:call SyShell_STROUT0
        ld hl,tmpbuf   :call SyShell_STROUT0
        ld hl,txtstalfd:call SyShell_STROUT0
        pop bc
        jr prgprz3
else
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
        jr c,prgprz5
        jr z,prgprz6
        push hl
        ld hl,tmpbuf
        add hl,bc
        ld (hl),0
        ld hl,txtstarcv:call SyShell_STROUT0
        ld hl,tmpbuf   :call SyShell_STROUT0
        ld hl,txtstalfd:call SyShell_STROUT0
        pop bc
        ld a,c
        or b
        jr nz,prgprz3
endif

prgprz6 ld hl,txtstabeg:call SyShell_STROUT0
        ld hl,nettstdat             ;send testdata
        ld bc,nettstdat0-nettstdat
        ld de,(App_BnkNum)
        ld a,(netsck)
        call netsnd
        jr c,prgprz5
        ld hl,txtstasnd:call SyShell_STROUT0
        jr prgprz4

prgprz5 add "A"
        ld hl,txterrx
        ld (hl),a
        call SyShell_STROUT0
        jr prgprz4
prgprz4 ld a,(netsck)               ;send disconnect
        call SyNet_TCPDIS
        ld a,(netsck)               ;close connection
        call SyNet_TCPCLO
        ld hl,txtstaclo:call SyShell_STROUT0
        jr prgend

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
        db "NetTest 1.0 (c)oded 2015 by Prodatron",13,10,0
txtdmnfnd   db "Network daemon found as process "
txtdmnfnd1  db "##",13,10,0

txterrdmn   db "Network daemon not running!",13,10,0
txterrcon   db "Error: Can't open connection",13,10,0

txtstaopn   db "Listening at port 80 ",13,10,0
txtstaest   db "Connection established",13,10,0
txtstabeg   db "Start sending...",13,10,0
txtstasnd   db "Data sent",13,10,0
txtstarcv   db "Data received: ",0
txtstacrm   db "Closed by remote host",13,10,0
txtstaclo   db 13,10,"Connection closed",13,10,0
txtstalfd   db 13,10,0

txterrx     db "  Error",13,10,0



nettstdat

db "HTTP/1.1 200 OK",13,10
db "Server: SymPache/0.1",13,10
db "Connection: close",13,10
db "Accept-Ranges: bytes",13,10
db "Content-Length: 733",13,10
db "Vary: Accept-Encoding",13,10
db "Content-Type: text/html; charset=ISO-8859-15",13,10
db "",13,10
db "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>",13,10
db "<html>",13,10
db "<head>",13,10
db "",13,10
db "  <title>MSX.fi</title>",13,10
db "</head>",13,10
db "",13,10
db "",13,10
db "<body alink='#ee2222' bgcolor='#000000' link='#ff3333' vlink='#ee2222'>",13,10
db "",13,10
db "",13,10
db "",13,10
db "<br>",13,10
db "<br>",13,10
db "<br>",13,10
db "",13,10
db "<center>",13,10
db "<br>",13,10
db "<br>",13,10
db "<br>",13,10
db "",13,10
db "<font color='#ff3333' size='6'>",13,10
db "The home of:<br>",13,10
db "<br>",13,10
db "",13,10
db "~ <a href='http://www.damage.fi/stt/'>STT</a> ~<br>",13,10
db "",13,10
db "~ <a href='http://msx.fi/nyyrikki'>NYYRIKKI</a> ~<br>",13,10
db "",13,10
db "~ Ahti ~<br>",13,10
db "<br>",13,10
db "~ <a href='http://msx.fi/party'>MSX Info Update party</a> ~<br>",13,10
db "",13,10
db "<br>",13,10
db "",13,10
db "<font size='3'>",13,10
db "... and MSX computer system in Finland<br>",13,10
db "",13,10
db "<br>",13,10
db "<a href='http://msx.partys.at.endofinternet.org'></a>",13,10
db "</font></font>",13,10
db "</center>",13,10
db "",13,10
db "",13,10
db "<br>",13,10
db "<br>",13,10
db "<br>",13,10
db "<br>",13,10
db "<br>",13,10
db "<br>",13,10
db "",13,10
db "",13,10
db "",13,10
db "",13,10
db "</body>",13,10
db "</html>",13,10

nettstdat0

netsck      db 0

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
