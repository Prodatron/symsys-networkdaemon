;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@                     W5100 CPC LOWLEVEL DRIVER ROUTINES                     @
;@                                                                            @
;@             (c) 2015-2015 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- W5100 PLATTFORM SPECIFIC SUBROUTINES (CPC) -------------------------------
;### W51ADR -> W5100 Set read/write address for indirect access

;--- W5100 PLATTFORM SPECIFIC INTERFACE ROUTINES (CPC) ------------------------
;### W51WBR -> W5100 Write Byte to   register memory
;### W51WWR -> W5100 Write Word to   register memory
;### W51WDR -> W5100 Write Data to   register memory
;### W51RBR -> W5100 Read  Byte from register memory
;### W51RWR -> W5100 Read  Word from register memory
;### W51RDR -> W5100 Read  Data from register memory
;   ### W51WDB -> W5100 Write Data to   buffer   memory
;   ### W51RDB -> W5100 Read  Data from buffer   memory


;==============================================================================
;### W5100 PLATTFORM SPECIFIC SUBROUTINES (CPC) ###############################
;==============================================================================

w51_port    equ #fbdc   ;w5100 base port
w51_idm_ar0 equ 1       ;w5100 indirect mode address register (MSB)
w51_idm_ar1 equ 2       ;w5100 indirect mode address register (LSB)
w51_idm_dr  equ 3       ;w5100 indirect mode data register

;### W51ADR -> W5100 Set read/write address for indirect access
;### Input      (SP)=offset (0-255), IYH=area (0=common, 4-7=socket)
;### Output     BC=data port
;### Destroyed  AF,HL
w51adr  ld bc,w51_port+w51_idm_ar0
        db #fd:ld a,h
        out (c),a
        inc c
        pop hl
        ld a,(hl)
        inc hl
        out (c),a
        inc c
        jp (hl)


;==============================================================================
;### W5100 PLATTFORM SPECIFIC INTERFACE ROUTINES (CPC) ########################
;==============================================================================

;### W51WBR -> W5100 Write Byte to register memory
;### Input      (SP)=offset (0-255), E=byte, IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,HL
w51wbr  call w51adr
        out (c),e
        ret

;### W51WWR -> W5100 Write Word to register memory
;### Input      (SP)=offset (0-255), DE=word, IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,HL
w51wbr  call w51adr
        out (c),d
        out (c),e
        ret

;### W51WDR -> W5100 Write Data to register memory
;### Input      (SP)=offset (0-255), DE=data address, C=length, IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,DE,HL
w51wdr  ld a,c
        ld (w51wdr+1),a
        call w51adr
        ex de,hl
w51wdr1 ld a,0
w51wdr2 ini
        inc b
        dec a
        jr nz,w51wdr2
        ret

;### W51RBR -> W5100 Read Byte from register memory
;### Input      (SP)=offset (0-255), IYH=area (0=common, 4-7=socket)
;### Output     E=byte
;### Destroyed  AF,BC,HL
w51rbr  call w51adr
        in e,(c)
        ret

;### W51RBR -> W5100 Read Word from register memory
;### Input      (SP)=offset (0-255), IYH=area (0=common, 4-7=socket)
;### Output     DE=word
;### Destroyed  AF,BC,HL
w51rbr  call w51adr
        in d,(c)
        in e,(c)
        ret

;### W51RDR -> W5100 Read Data from register memory
;### Input      (SP)=offset (0-255), DE=data address, C=length, IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,DE,HL
w51rdr  ld a,c
        ld (w51rdr+1),a
        call w51adr
        ex de,hl
w51rdr1 ld a,0
w51rdr2 inc b
        outi
        dec a
        jr nz,w51rdr2
        ret
