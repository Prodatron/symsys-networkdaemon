;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@               W5100 MSX (DENYONET) LOWLEVEL DRIVER ROUTINES                @
;@                                                                            @
;@             (c) 2015-2015 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- W5100 PLATTFORM SPECIFIC SUBROUTINES (MSX) -------------------------------
;### W51SLT -> Setup MSX slot config of the DenYoNet W5100 hardware
;### W51SHT -> Map W5100 transfer buffer memory into CPU ram at #8000 and application buffer at #4000
;### W51SHR -> Map W5100 register memory into CPU ram at #8000
;### W51HIT -> Hides application buffer and W5100 memory from CPU ram
;### W51HIR -> Hides W5100 memory from CPU ram

;--- W5100 PLATTFORM SPECIFIC INTERFACE ROUTINES (MSX) ------------------------
;### W51WBR -> W5100 Write Byte to   register memory
;### W51WWR -> W5100 Write Word to   register memory
;### W51WDR -> W5100 Write Data to   register memory
;### W51RBR -> W5100 Read  Byte from register memory
;### W51RWR -> W5100 Read  Word from register memory
;### W51RDR -> W5100 Read  Data from register memory
;### W51WDB -> W5100 Write Data to   buffer   memory
;### W51RDB -> W5100 Read  Data from buffer   memory


;==============================================================================
;### W5100 PLATTFORM SPECIFIC SUBROUTINES (MSX) ###############################
;==============================================================================

low_vermaj      equ 0   ;version 0.2
low_vermin      equ 2

;has to be placed at #c000-#ffff (transfer area)

;W5100 constants
w51_ini_mr      equ #80     ;MSX -> disable Indirect Bus I/F mode

w51_mem_ofs     equ #80
w51_mem_selreg  equ #00
w51_mem_selbuf  equ #10

w51_mem_port    equ #28     ;bit0-3=select 32K out of 512KB flash eprom for #0000-#7ffff, bit4=select reg(0) or buffer(1) for #8000-#bfff, bit5=select reg(0) or buffer(1) for #c000-#ffff
w51_sta_port    equ #29     ;bit0=TX, bit1=RX, bit2=collision, bit3=full duplex, bit4=100mbit, bit5=link


w51rommac   ds 6
w51romidn   db "DenYoNet"
w51tmpbuf   ds 16

;### W51SLT -> Setup MSX slot config of the DenYoNet W5100 hardware
;### Input      L=pslot (0-3), H=sslot (0-3)
;### Output     CF=0 ok, (w51rommac)=MAC address from ROM
;###            CF=1 ROM not found
;### Destroyed  AF,BC,L
w51slt  ld a,l
        add a:add a
        ld (w51rom1+1),a
        ld a,(#218)
        and #cf
        ld c,a          ;C=original pslot at 0,1,3
        and #0f
        ld b,a          ;B=original pslot at 0,1
        ld a,l
        rrca:rrca
        ld l,a          ;L=w5100 pslot at 3
        rrca:rrca       ;A=w5100 pslot at 2
        or c            ;A=pslot 0,1,3>original, 2>w5100
        ld (w51shr3+1),a
        ld a,b
        or l            ;A=pslot 0,1>original, 3>w5100, 2>(0)
        ld (w51shr1+1),a
        ld a,h
        add a:add a
        add h
        add a:add a     ;A=w5100 sslot at 1,2
        ld h,a
        ld a,(#219)
        and #c3
        or h            ;A=sslot 0,3>original, 1,2>w5100
        ld (w51shr2+1),a
        call w51rom         ;show rom
        ld hl,#4010         ;get MAC address
        ld de,w51rommac
        ld bc,6
        ldir
        ld de,w51romidn     ;check identifier
        ld bc,8*256+255
w51slt1 ld a,(de)
        inc de
        cpi
        jr nz,w51slt2
        djnz w51slt1
        call w51hir
        or a
        ret
w51slt2 call w51hir
        scf
        ret

;### W51SHT -> Map W5100 buffer memory into CPU ram at #8000 and application buffer at #4000
;### Input      A=application ram bank (0-15), HL=application buffer (#0000-#ffff)
;### Output     HL=mapped application buffer (#4000-#7fff), DI
;### Destroyed  AF
w51sht  push af
        ld a,i
        and #78             ;A=(current bank-1)*8
        rra
        add 5               ;A=current page at #4000
        ld (w51hit+1),a
        ld a,h
        res 7,h             ;set HL to #4000-#7fff
        set 6,h
        ex (sp),hl
        and #c0
        or h                ;A[bit76]=application block, A[bit3210]=application bank
        pop hl
        rlca:rlca           ;A[bit5432]=application bank, A[bit10]=application block -> application page
        di
        out (#fd),a
        ld a,w51_mem_selbuf
        jr w51shr0

;### W51SHR -> Map W5100 register memory into CPU ram at #8000
;### Output     DI
;### Destroyed  A
w51shr  ld a,w51_mem_selreg
        di
w51shr0 out (w51_mem_port),a
w51shr1 ld a,0
        ld (#203),a
w51shr2 ld a,0
        ld (#207),a
w51shr3 ld a,0
        ld (#20c),a
        jp #202

;### W51ROM -> Maps DenYoNet rom to #4000-#7fff
;### Output     DI
;### Destroyed  AF
w51rom  call w51shr
        ld a,(w51shr3+1)
        and #f3
w51rom1 or 0
        out (#a8),a
        ret

;### W51HIT -> Hides application buffer and W5100 memory from CPU ram
;### Output     EI
;### Destroyed  A
w51hit  ld a,0
        out (#fd),a
;### W51HIR -> Hides W5100 memory from CPU ram
;### Output     EI
;### Destroyed  A
w51hir  ld a,(#218)
        out (#a8),a
        ld a,(#219)
        ld (#ffff),a
        ei
        ret


;==============================================================================
;### W5100 PLATTFORM SPECIFIC INTERFACE ROUTINES (MSX) ########################
;==============================================================================

;### W51WBR -> W5100 Write Byte to register memory
;### Input      (SP)=offset (0-255), E=byte, IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,HL
w51wbr  pop hl
        ld a,(hl)
        inc hl
        push hl
        ld l,a
        call w51shr
        ld a,w51_mem_ofs
        db #fd:add h
        ld h,a
        ld (hl),e
        jp w51hir

;### W51WWR -> W5100 Write Word to register memory
;### Input      (SP)=offset (0-255), DE=word, IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,HL
w51wwr  pop hl
        ld a,(hl)
        inc hl
        push hl
        ld l,a
        call w51shr
        ld a,w51_mem_ofs
        db #fd:add h
        ld h,a
        ld (hl),d
        inc hl
        ld (hl),e
        jp w51hir

;### W51WDR -> W5100 Write Data to register memory
;### Input      (SP)=offset (0-255), DE=data address, C=length (<=16), IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,DE,HL
w51wdr  ex de,hl
        ld de,w51tmpbuf
        ld b,0
        push bc
        ldir
        pop bc
        pop hl
        ld e,(hl)
        inc hl
        push hl
        call w51shr
        ld a,w51_mem_ofs
        db #fd:add h
        ld d,a
        ld hl,w51tmpbuf
        ldir
        jp w51hir

;### W51RBR -> W5100 Read Byte from register memory
;### Input      (SP)=offset (0-255), IYH=area (0=common, 4-7=socket)
;### Output     E=byte
;### Destroyed  AF,BC,HL
w51rbr  pop hl
        ld a,(hl)
        inc hl
        push hl
        ld l,a
        call w51shr
        ld a,w51_mem_ofs
        db #fd:add h
        ld h,a
        ld e,(hl)
        jp w51hir

;### W51RWR -> W5100 Read Word from register memory
;### Input      (SP)=offset (0-255), IYH=area (0=common, 4-7=socket)
;### Output     DE=word
;### Destroyed  AF,BC,HL
w51rwr  pop hl
        ld a,(hl)
        inc hl
        push hl
        ld l,a
        call w51shr
        ld a,w51_mem_ofs
        db #fd:add h
        ld h,a
        ld d,(hl)
        inc hl
        ld e,(hl)
        jp w51hir

;### W51RDR -> W5100 Read Data from register memory
;### Input      (SP)=offset (0-255), DE=data address, C=length (<=16), IYH=area (0=common, 4-7=socket)
;### Destroyed  AF,BC,DE,HL
w51rdr  pop hl
        ld a,(hl)
        inc hl
        push hl
        ld l,a
        call w51shr
        ld a,w51_mem_ofs
        db #fd:add h
        ld h,a
        ld b,0
        push de
        push bc
        ld de,w51tmpbuf
        ldir
        pop bc
        pop de
        ld hl,w51tmpbuf
        ldir
        jp w51hir

;### W51WDB -> W5100 Write Data to buffer memory (TX)
;### Input      A=source bank (0-15), HL=source address (must stay within 16K boundary), DE=buffer offset (#0000-#1fff), BC=length
;### Destroyed  AF,BC,DE,HL
w51wdb  call w51sht
        set 7,d
        jr w51rdb1

;### W51RDB -> W5100 Read Data from buffer memory (RX)
;### Input      A=destination bank (0-15), HL=destination address (must stay within 16K boundary), DE=buffer offset (#0000-#1fff), BC=length
;### Destroyed  AF,BC,DE,HL
w51rdb  call w51sht
        set 7,d
        set 5,d
        ex de,hl
w51rdb1 xor a
        sub c
        and 15
        add a
        ld (w51rdb2+1),a
w51rdb2 jr nz,w51rdb2
w51rdb3 ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        jp pe,w51rdb3
        jp w51hit
