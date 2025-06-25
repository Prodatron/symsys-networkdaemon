;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@               W5100 CPC (Net4CPC) LOWLEVEL DRIVER ROUTINES                 @
;@                                                                            @
;@             (c) 2015-2015 by Prodatron / SymbiosiS (Jörn Mika)             @
;@             (c) 2019 by d_kef (Dimitris Kefalas)                           @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- W5100 PLATTFORM SPECIFIC SUBROUTINES (CPC) -------------------------------
;### W51SET -> Setup Net4CPC W5100S hardware

;--- W5100 PLATTFORM SPECIFIC INTERFACE ROUTINES (CPC) ------------------------
;### W51WBR -> W5100 Write Byte to   register memory
;### W51WWR -> W5100 Write Word to   register memory
;### W51WDR -> W5100 Write Data to   register memory
;### W51RBR -> W5100 Read  Byte from register memory
;### W51RWR -> W5100 Read  Word from register memory
;### W51RDR -> W5100 Read  Data from register memory
;### W51WDB -> W5100 Write Data to   buffer   memory
;### W51RDB -> W5100 Read  Data from buffer   memory


;==============================================================================
;### W5100 PLATTFORM SPECIFIC SUBROUTINES (CPC) ###############################
;==============================================================================

low_vermaj      equ 1   ;version 1.0
low_vermin      equ 0

;has to be placed at #c000-#ffff (transfer area)

;W5100 constants
w51_ini_mr      equ #80     ;CPC -> reset W5100

w51_mr_port     equ #fd20
w51_adH_port    equ w51_mr_port+1
w51_adL_port    equ w51_mr_port+2
w51_dat_port    equ w51_mr_port+3

w51rommac       db #de, #ad, #be, #ef, #00, #ff ;default MAC address

;### W51SET -> Setup Net4CPC W5100S hardware (CPC)
;### Input      
;### Output     CF=0 ok
;###            CF=1 W5100S not found
w51set  ld bc,w51_mr_port
        ;ld a,w51_ini_mr
        ;out (c),a
        in a,(c)
        cp 3		;W5100S reads 3 after reset
        ret z
        scf
	ret

;==============================================================================
;### W5100 PLATTFORM SPECIFIC INTERFACE ROUTINES (CPC) ########################
;==============================================================================

;### W51WBR -> W5100 Write Byte to register memory
;### Input      (SP)=offset (0-255), E=byte, IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,HL
w51wbr  xor a
        ld bc,w51_adH_port
        db #fd:add h
        out (c),a
        inc c
        pop hl
        ld a,(hl)
        inc hl
        push hl
        out (c),a
        inc c
        out (c),e
        ret

;### W51WWR -> W5100 Write Word to register memory
;### Input      (SP)=offset (0-255), DE=word, IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,HL
w51wwr  xor a
        ld bc,w51_adH_port
        db #fd:add h
        out (c),a
        inc c
        pop hl
        ld a,(hl)
        inc hl
        push hl
        out (c),a
        inc c
        out (c),d
        out (c),e
        ret

;### W51WDR -> W5100 Write Data to register memory
;### Input      (SP)=offset (0-255), DE=data address, C=length (<=16), IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,DE,HL
w51wdr  ld a,48
        sub c
        sub c
        sub c
        ld (w51wdr0+1),a
        xor a
        ld bc,w51_adH_port
        db #fd:add h
        out (c),a
        inc c
        pop hl
        ld a,(hl)
        inc hl
        push hl
        out (c),a
        inc c
        ex de,hl
w51wdr0 jr w51wdr1
w51wdr1 inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi
        inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi:inc b:outi
        ret

;### W51RBR -> W5100 Read Byte from register memory
;### Input      (SP)=offset (0-255), IYH=area (0=common, 4-7=socket)
;### Output     E=byte
;### Destroyed  AF,BC,HL
w51rbr  xor a
        ld bc,w51_adH_port
        db #fd:add h
        out (c),a
        inc c
        pop hl
        ld a,(hl)
        inc hl
        push hl
        out (c),a
        inc c
        in e,(c)
        ret

;### W51RWR -> W5100 Read Word from register memory
;### Input      (SP)=offset (0-255), IYH=area (0=common, 4-7=socket)
;### Output     DE=word
;### Destroyed  AF,BC,HL
w51rwr  xor a
        ld bc,w51_adH_port
        db #fd:add h
        out (c),a
        inc c
        pop hl
        ld a,(hl)
        inc hl
        push hl
        out (c),a
        inc c
        in d,(c)
        in e,(c)
        ret

;### W51RDR -> W5100 Read Data from register memory
;### Input      (SP)=offset (0-255), DE=data address, C=length (<=16), IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,DE,HL
w51rdr  ld a,48
        sub c
        sub c
        sub c
        ld (w51rdr0+1),a
        xor a
        ld bc,w51_adH_port
        db #fd:add h
        out (c),a
        inc c
        pop hl
        ld a,(hl)
        inc hl
        push hl
        out (c),a
        inc c        
        ex de,hl
w51rdr0 jr w51rdr1
w51rdr1 ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b
        ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b:ini:inc b
        ret

;### W51WDB -> W5100 Write Data to buffer memory (TX)
;### Input      A=source bank (0-15), HL=source address (must stay within 16K boundary), DE=buffer offset (#0000-#1fff), BC=length
;### Destroyed  AF,BC,DE,HL
w51wdb	push ix
        push iy
        set 6,d
        add a:add a:add a:add a
        or b
        db #fd:ld l,c
        db #fd:ld h,a               ;iy=bank, length
        ld bc,w51_adH_port
        out (c),d
        inc c
        out (c),e
        ex de,hl                    ;de=address
        ld ix,w51_dat_port          ;ix=port
        ld hl,jmp_iomout
        rst #28
        pop iy
        pop ix
        ret

;### W51RDB -> W5100 Read Data from buffer memory (RX)
;### Input      A=destination bank (0-15), HL=destination address (must stay within 16K boundary), DE=buffer offset (#0000-#1fff), BC=length
;### Destroyed  AF,BC,DE,HL
w51rdb	push ix
        push iy
        set 6,d
        set 5,d
        add a:add a:add a:add a
        or b
        db #fd:ld l,c
        db #fd:ld h,a               ;iy=bank, length
        ld bc,w51_adH_port
        out (c),d
        inc c
        out (c),e
        ex de,hl                    ;de=address
        ld ix,w51_dat_port          ;ix=port
        ld hl,jmp_iominp
        rst #28
        pop iy
        pop ix
        ret
