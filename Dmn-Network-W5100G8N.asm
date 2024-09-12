;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@               S y m b O S   -   N e t w o r k - D a e m o n                @
;@                W5100 MSX (GR8NET) LOWLEVEL DRIVER ROUTINES                 @
;@                                                                            @
;@             (c) 2015-2017 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- W5100 PLATTFORM SPECIFIC SUBROUTINES (MSX) -------------------------------
;### W51SLT -> Setup MSX slot config of the GR8NET W5100 hardware
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

;GR8NET constants and variables
w51_ini_mr      equ #80     ;MSX -> disable Indirect Bus I/F mode

w51_mem_ofs     equ #80

g8n_sel_port    equ #5e     ;bit0-3=register, bit4-5=default adapter, bit6-7=selected adapter
g8n_reg_port    equ #5f     ;reg0="G", reg1=adapter slot (RDSLT format), reg2=mapper, reg3=majver, reg4=minver

g8n_mem_selreg  equ #c0     ;select register bank
g8n_mem_seltx   equ #c2     ;select TX buffer
g8n_mem_selrx   equ #c3     ;select RX buffer
g8n_mem_bank    equ #bfe2   ;special control register -> page for bank3

g8n_mac_adr     equ #8001   ;address of MAC address in config page

g8n_sta_delay   equ 2       ;delay for RX/TX status
g8n_sta_tx      db 0        ;TX "led" status
g8n_sta_rx      db 0        ;RX "led" status

w51tmpbuf   ds 16
w51rommac   ds 6

;### W51SLT -> Setup MSX slot config of the GR8NET W5100 hardware
;### Input      L=card index (0-3)
;### Output     CF=0 ok, (w51rommac)=MAC address from ROM
;###            CF=1 no card with this index
;### Destroyed  ?
w51slt  ld a,l
        rrca:rrca
        ld e,a
        in a,(g8n_sel_port)
        and #30
        or e
        ld e,a
        out (g8n_sel_port),a
        in a,(g8n_reg_port)
        cp "G"
        scf
        ret nz

        ld a,e
        add 2
        ld (w51shr0+1),a
        out (g8n_sel_port),a
        in a,(g8n_reg_port)
        and #0F
        or 128
        ld (w51shr4+1),a
        ld a,e
        inc a
        out (g8n_sel_port),a

        in a,(g8n_reg_port)

        ld e,a
        and 3
        ld l,a          ;L=pslot
        ld a,e
        add a:add a
        and #30
        ld h,a          ;H=w5100 sslot at 2

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
        ld a,(#219)
        and #cf
        or h            ;A=sslot 0,1,3>original, 2>w5100
        ld (w51shr2+1),a
        ld c,#ff
        di
        call w51shr0
        ld hl,#8001
        ld de,w51rommac
        ld bc,6
        ldir
        call w51hir
        or a
        ret

;### W51SHT -> Map W5100 buffer memory into CPU ram at #8000 and application buffer at #4000
;### Input      A=application ram bank (0-15), HL=application buffer (#0000-#ffff), C=buffer type (#C2=TX send, #C3=RX receive)
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
        jr w51shr0

;### W51SHR -> Map W5100 register memory into CPU ram at #8000
;### Output     DI
;### Destroyed  A,C
w51shr  ld c,g8n_mem_selreg
        di
w51shr0 ld a,0
        out (g8n_sel_port),a        ;select card and mapper register
w51shr4 ld a,128+0                  ;set mapper type X and make special control register visible in gr8net bank 3 (#a000-#bfff)
        out (g8n_reg_port),a
w51shr1 ld a,0
        ld (#203),a
w51shr2 ld a,0
        ld (#207),a
w51shr3 ld a,0
        ld (#20c),a
        call #202
        ld a,c
        ld (g8n_mem_bank),a         ;show W5100 memory (reg/tx/rx) at #8000
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
        push bc
        call w51shr
        pop bc
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
        ld h,c
        call w51shr
        ld c,h
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
w51wdb  push bc
        ld c,g8n_mem_seltx
        call w51sht
        pop bc
        ld a,g8n_sta_delay
        ld (g8n_sta_tx),a
        set 7,d
        jr w51rdb1

;### W51RDB -> W5100 Read Data from buffer memory (RX)
;### Input      A=destination bank (0-15), HL=destination address (must stay within 16K boundary), DE=buffer offset (#0000-#1fff), BC=length
;### Destroyed  AF,BC,DE,HL
w51rdb  push bc
        ld c,g8n_mem_selrx
        call w51sht
        pop bc
        ld a,g8n_sta_delay
        ld (g8n_sta_rx),a
        set 7,d
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
