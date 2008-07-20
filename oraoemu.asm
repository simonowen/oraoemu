base:          equ  &9000

status:        equ  249             ; Status and extended keyboard port
lmpr:          equ  250             ; Low Memory Page Register
hmpr:          equ  251             ; High Memory Page Register
vmpr:          equ  252             ; Video Memory Page Register
keyboard:      equ  254             ; Keyboard port
border:        equ  254             ; Border port
rom0_off:      equ  %00100000       ; LMPR bit to disable ROM0
vmpr_mode2:    equ  %00100000       ; Mode 2 select for VMPR

low_page:      equ  3               ; LMPR during emulation
screen_page:   equ  5               ; SAM display

bord_stp:      equ  2               ; STP instruction halted CPU (red)
bord_wai:      equ  6               ; WAI instruction waiting for interrupt (yellow)
bord_brk:      equ  4               ; BRK not fully supported (green)

m6502_nmi:     equ  &fffa           ; nmi vector address
m6502_reset:   equ  &fffc           ; reset vector address
m6502_int:     equ  &fffe           ; int vector address

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               org  base
               dump $
               autoexec

start:         jr   start2

viewmode:      defb &00             ; view offset, &ff=scale

start2:        di
               in   a,(lmpr)
               ld   b,a
               in   a,(vmpr)
               ld   c,a

               ld   a,low_page+rom0_off
               out  (lmpr),a
               ld   a,vmpr_mode2+screen_page
               out  (vmpr),a

               ld   (old_stack+1),sp
               ld   sp,stack_top
               push bc              ; save original LMPR+VMPR

               CALL do_emu

               di
               pop  bc              ; restore HMPR
old_stack:     ld   sp,0
               ld   a,b
               out  (lmpr),a
               ld   a,c
               out  (vmpr),a
               im   1
               ei
               ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

do_emu:        call mk_bitrev_tab
               call update_view
               call reorder_decode
               call clear_io_area
               call setup_im2       ; enable IM 2

               ; Extract the register values from the snapshot
               ld   hl,(&0006)      ; PC
               ld   de,(&0000)      ; A+P
               ld   bc,(&0002)      ; X+Y
               ld   a,(&0004)       ; S
               ex   af,af'

               ; Only use snapshot registers if PC is not zero
               ld   a,h
               or   l
               jr   nz,got_regs

               ; Start from reset vector address, and known register contents
               ld   hl,(m6502_reset)
               ld   de,&ffff
               ld   b,d
               ld   c,e
               xor  a
               ex   af,af'

got_regs:      ld   (reg_pc),hl
               ld   a,e
               ld   (reg_a),a
               ld   a,d
               ld   (reg_p),a
               ld   a,c
               ld   (reg_x),a
               ld   a,b
               ld   (reg_y),a
               ex   af,af'
               ld   (reg_s),a

               ; Save the 32-byte snapshot state, and shift up the memory
               ; contents to be in the correct position for running
               ld   hl,&0000
               ld   de,snap_state
               ld   bc,&0020
               ldir
               ld   de,&0000
               ld   bc,&8000-&0020
               ldir

               ; Update the SAM screen with the new Orao screen contents
               call update_screen
emu_ready:
               ei
no_snap:       call execute     ; GO!
               di

               ; Shift the current memory up, and restore the previous state
               ld   hl,&8000-&0021
               ld   de,&8000-&0001
               ld   bc,&8000-&0020
               lddr
               ld   hl,snap_state+&0020-1
               ld   bc,&0020
               lddr

               ; Update the state with the current register values
               ld   a,(reg_a)
               ld   (&0000),a
               ld   a,(reg_p)
               ld   (&0001),a
               ld   a,(reg_x)
               ld   (&0002),a
               ld   a,(reg_y)
               ld   (&0003),a
               ld   a,(reg_s)
               ld   (&0004),a
               ld   hl,(reg_pc)
               ld   (&0006),hl
               ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

update_view:   ld   a,screen_page+rom0_off
               out  (lmpr),a

               call set_sam_attrs
               call mk_line_tab
               call mk_addr_tab

               ld   a,low_page+rom0_off
               out  (lmpr),a
               ret

               ; Fill SAM mode 2 display attributes to make the screen visible
set_sam_attrs: ld   hl,&2000
               ld   bc,&1807        ; 24 blocks of white on black
clear_lp:      ld   (hl),c
               inc  l
               jr   nz,clear_lp
               inc  h
               djnz clear_lp
               ret

mk_line_tab:   ld   hl,line_table
               ld   a,(viewmode)
               cp   &ff
               jr   z,scale
               neg

               ; Set linear layout
linear_lp:     ld   (hl),a
               inc  a
               inc  l
               jr   nz,linear_lp
               ret

               ; Set scaled layout (75%)
scale:         ld   bc,&40c0        ; 64 blocks of 4, &c0 for off-screen
               ld   e,l
scale_lp:      ld   (hl),e
               inc  l
               inc  e
               ld   (hl),e
               inc  l
               inc  e
               ld   (hl),e
               inc  l
               inc  e
               ld   (hl),c          ; hide every 4th line
               inc  l
               djnz scale_lp
               ret

               ; Build the display address lookup table from the line table
mk_addr_tab:   ld   hl,&4000
               ld   de,line_table
               ld   bc,&0000
addr_lp1:      ld   a,&20
               ex   de,hl
               ld   b,(hl)
               ld   c,0
               srl  b
               rr   c
               srl  b
               rr   c
               srl  b
               rr   c
               ex   de,hl
addr_lp2:      ld   (hl),c
               set  5,h
               ld   (hl),b
               res  5,h
               inc  bc
               inc  hl
               dec  a
               jr   nz,addr_lp2
               inc  e
               jr   nz,addr_lp1
               ret

               ; Full SAM screen refresh from Orao display memory
update_screen: ld   hl,&6000
               ld   b,rev_table/256
update_lp:     ld   c,(hl)
               ld   a,screen_page+rom0_off
               out  (lmpr),a
               ld   a,(bc)
               ld   d,(hl)
               res  5,h
               ld   e,(hl)
               set  5,h
               ld   (de),a
               ld   a,low_page+rom0_off
               out  (lmpr),a
               inc  l
               jr   nz,update_lp
               inc  h
               bit  7,h
               jr   z,update_lp
               ret

               ; Create the bit-reverse look-up table
mk_bitrev_tab: ld   hl,rev_table
bitrev1:       ld   a,l
               ld   c,1
bitrev2:       rra
               rl   c
               jr   nc,bitrev2
               ld   (hl),c
               inc  l
               jr   nz,bitrev1
               ret

               ; Clear the Orao I/O area of keys and other devices
clear_io_area: ld   hl,&8000
               ld   bc,&10ff        ; 16 blocks of white on black
clear_lp2:     ld   (hl),c
               inc  l
               jr   nz,clear_lp2
               inc  h
               djnz clear_lp2
               ret

               ; Start by clearing key addresses we can affect
update_keys:   ld   hl,keyports
               ld   b,&14           ; 20 entries
               ld   a,&ff           ; &ff = not pressed
kbclear_lp:    ld   e,(hl)
               inc  l
               ld   d,(hl)
               inc  l
               ld   (de),a
               djnz kbclear_lp

               ; Check for Sym, used for emulation functions
               ld   a,&7f
               in   a,(keyboard)
               bit  1,a
               jr   nz,scan_keymap

               ld   a,&f7
               in   a,(keyboard)
               ld   b,&00
               rra
               jr   nc,got_view
               ld   b,&20
               rra
               jr   nc,got_view
               ld   b,&40
               rra
               jr   nc,got_view
               ld   b,&ff
               rra
               jr   nc,got_view

               ; Scrolling not available in scale mode
               ld   a,(viewmode)
               cp   &ff
               ret  z
               ld   b,a

               ld   a,&ff
               in   a,(keyboard)
               rra
               rra
               jr   c,not_up
               ld   a,b
               sub  8
               ret  c
               ld   b,a
               jr   got_view
not_up:        rra
               ret  c
               ld   a,b
               add  a,8
               ld   b,a
               cp   &41
               ret  nc

got_view:      ld   a,b
               ld   (viewmode),a
               call update_view
               jp   update_screen

               ; Scan the full keymap
scan_keymap:   ld   hl,keymap
               ld   b,&fe           ; keyboard row mask
key_lp1:       ld   c,status        ; SAM extended key port (and status)
               in   a,(c)
               and  %11100000       ; only top 3 bits used for keys
               ld   e,a
               ld   c,keyboard      ; Speccy keyboard port
               in   a,(c)
               and  %00011111       ; only bottom 5 bits used for keys
               or   e               ; merge with extended keys
               inc  a
               jr   z,no_row        ; jump if no keys on row are pressed
               dec  a
               ld   c,8             ; 8 bits to scan in each byte
key_lp2:       ld   e,(hl)
               inc  l
               ld   d,(hl)
               inc  l
               rla                  ; shift next bit into carry
               jr   c,no_press      ; jump if not pressed
               ex   af,af'
               ld   a,d
               and  a
               jr   z,no_press_ex
               ld   a,(hl)
               ld   (de),a
no_press_ex:   ex   af,af'
no_press:      inc  l
               dec  c
               jr   nz,key_lp2      ; complete row
next_row:      inc  b               ; done the last row?
               jr   z,done_matrix
               dec  b
               rlc  b               ; move to next row to scan
               jr   c,key_lp1
               inc  b               ; scan final row &ff
               jr   key_lp1
no_row:        ld   a,l
               add  a,24            ; skip 1 row in keymap
               ld   l,a
               jr   next_row
done_matrix:

;              ld   a,&7f
;              in   a,(keyboard)
;              bit  1,a
;              jr   nz,not_sym

;              ld   a,&fb
;              in   a,(keyboard)
;              rra
;              jr   c,not_q
;              ld   a,&20
;              ld   (&87bf),a
not_q:

not_sym:
               ; Enter somehow appears in zero-page
               ld   a,&bf
               in   a,(keyboard)
               rra
               jr   c,not_enter
               ld   a,&0d
               ld   (&00fc),a
not_enter:
               ; Esc returns to SAM BASIC for loading
               ld   a,&f7
               in   a,(status)
               and  %00100000
               jr   nz,not_esc
               ld   a,&c9           ; RET
               ld   (main_loop),a   ; exit loop at next instruction
not_esc:
               ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Interrupt handling

im2_table:     equ  &ae00
im2_jp:        equ  &afaf

setup_im2:     ld   a,&c3           ; JP
               ld   (im2_jp),a
               ld   hl,im2_handler
               ld   (im2_jp+1),hl

               ld   hl,im2_table
               ld   a,im2_jp/256
im2_fill:      ld   (hl),a
               inc  l
               jr   nz,im2_fill
               inc  h
               ld   (hl),a          ; complete the final entry
               ld   a,im2_table/256
               ld   i,a
               im   2               ; set interrupt mode 2
               ret

im2_handler:   push af
               push bc
               push de
               push hl
               ex   af,af'
               exx
               push af
               push bc
               push de
               push hl
               push ix
               push iy

               in   a,(lmpr)
               push af
               ld   a,low_page+rom0_off
               out  (lmpr),a

               call update_keys

               pop  af
               out  (lmpr),a

               pop  iy
               pop  ix
               pop  hl
               pop  de
               pop  bc
               pop  af
               exx
               ex   af,af'
               pop  hl
               pop  de
               pop  bc
               pop  af
               ei
               reti
end_1:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 65C02 emulation

execute:       call load_state
               ld   a,&1a           ; LD A,(DE)
               ld   (main_loop),a
               call main_loop
               jp   save_state

               ; check for write to normal RAM or screen upwards
write_loop:    ld   a,h
               cp   &60
               jr   nc,screen_or_up

i_nop:
i_undoc_1:
main_loop:     ld   a,(de)          ; fetch opcode
               inc  de              ; PC=PC+1
               ld   l,a
               ld   h,decode_table/256
               ld   a,(hl)          ; handler low
               inc  h
               ld   h,(hl)          ; handler high
               ld   l,a
               jp   (hl)            ; execute!

               ; write to screen or above
screen_or_up:  cp   &80
               jr   nc,io_or_up

               defb &dd
               ld   h,d
               defb &dd
               ld   l,e

               ld   e,(hl)          ; look up byte written
               ld   d,rev_table/256
               ld   a,screen_page+rom0_off
               out  (lmpr),a
               ld   a,(de)          ; look up bit-reversed display byte
               ld   d,(hl)          ; MSB of SAM address
               res  5,h             ; switch tables halves
               ld   e,(hl)          ; LSB of SAM address
               ld   (de),a          ; write SAM display byte
               ld   a,low_page+rom0_off
               out  (lmpr),a

               defb &dd
               ld   d,h
               defb &dd
               ld   e,l

               ld   a,(de)          ; fetch opcode
               inc  de              ; PC=PC+1
               ld   l,a
               ld   h,decode_table/256
               ld   a,(hl)          ; handler low
               inc  h
               ld   h,(hl)          ; handler high
               ld   l,a
               jp   (hl)            ; execute!

               ; write to the I/O area
io_or_up:      cp   &88
               jr   nz,main_loop    ; ignore non-sound writes

               ; Reading from &88xx is unknown, so best leave visible as &ff
               ld   (hl),&ff

               ; Toggle speaker bit
               ld   a,(beeper)
               xor  %00010000
               ld   (beeper),a
               out  (border),a

               ; short-circuit any inner sound loop in X/Y
               ld   a,(de)
               cp   &88             ; DEY
               jr   nz,not_dey
               defb &fd
               ld   l,1
               jp   main_loop
not_dey:       cp   &ca             ; DEX
               jr   nz,main_loop
               defb &fb
               ld   h,1
               jp   main_loop


; 6502 addressing modes, shared by logical and arithmetic
; instructions, but inlined into the load and store.

a_indirect_x:  ld   a,(de)          ; indirect pre-indexed with X
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               jp   (ix)

a_zero_page:   ld   a,(de)          ; zero-page
               inc  de
               ld   h,0
               ld   l,a
               ld   a,(hl)
               jp   (ix)

a_absolute:    ex   de,hl           ; absolute (2-bytes)
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,(hl)
               jp   (ix)

a_indirect_y:  ld   a,(de)          ; indirect post-indexed with Y
               inc  de
               ld   l,a
               ld   h,0
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ld   a,(hl)
               jp   (ix)

a_zero_page_x: ld   a,(de)          ; zero-page indexed with X
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               jp   (ix)

a_zero_page_y: ld   a,(de)          ; zero-page indexed with Y
               inc  de
               defb &fd
               add  a,l             ; add Y (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               jp   (ix)

a_absolute_y:  ex   de,hl           ; absolute indexed with Y
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   a,(hl)
               jp   (ix)

a_absolute_x:  ex   de,hl           ; absolute indexed with X
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   a,(hl)
               jp   (ix)

a_indirect_z:  ld   a,(de)          ; indirect zero-page [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               jp   (ix)

; Instruction implementations

i_undoc_3:     inc  de
i_undoc_2:     inc  de
               jp   main_loop       ; continue

i_clc:         exx                  ; clear carry
               ld   c,0
               exx
               jp   main_loop
i_sec:         exx                  ; set carry
               ld   c,1
               exx
               jp   main_loop
i_cli:         exx                  ; clear interrupt disable
               res  2,d
               exx
               jp   main_loop
i_sei:         exx                  ; set interrupt disable
               set  2,d
               exx
               jp   main_loop
i_clv:         exx                  ; clear overflow
               ld   b,0
               exx
               jp   main_loop
i_cld:         exx                  ; clear decimal mode
               res  3,d
               exx
               xor  a               ; NOP
               ld   (adc_daa),a     ; use binary mode for adc
               ld   (sbc_daa),a     ; use binary mode for sbc
               jp   main_loop
i_sed:         exx
               set  3,d
               exx
               ld   a,&27           ; DAA
               ld   (adc_daa),a     ; use decimal mode for adc
               ld   (sbc_daa),a     ; use decimal mode for sbc
               jp   main_loop

i_bpl:         ld   a,(de)
               inc  de
               ex   af,af'
               ld   l,a             ; copy N
               ex   af,af'
               bit  7,l             ; test N
               jr   z,i_branch      ; branch if plus
               jp   main_loop
i_bmi:         ld   a,(de)
               inc  de
               ex   af,af'
               ld   l,a             ; copy N
               ex   af,af'
               bit  7,l             ; test N
               jr   nz,i_branch     ; branch if minus
               jp   main_loop
i_bvc:         ld   a,(de)          ; V in bit 6
               inc  de              ; V set if non-zero
               exx
               bit  6,b
               exx
               jr   z,i_branch      ; branch if V clear
               jp   main_loop
i_bvs:         ld   a,(de)          ; V in bit 6
               inc  de
               exx
               bit  6,b
               exx
               jr   nz,i_branch     ; branch if V set
               jp   main_loop
i_bcc:         ld   a,(de)          ; C in bit 1
               inc  de
               exx
               bit  0,c
               exx
               jr   z,i_branch      ; branch if C clear
               jp   main_loop
i_bcs:         ld   a,(de)
               inc  de
               exx
               bit  0,c
               exx
               jr   nz,i_branch     ; branch if C set
               jp   main_loop
i_beq:         ld   a,(de)
               inc  de
               inc  c
               dec  c               ; zero?
               jr   z,i_branch      ; branch if zero
               jp   main_loop
i_bne:         ld   a,(de)
               inc  de
               inc  c
               dec  c               ; zero?
               jp   z,main_loop     ; no branch if not zero
i_branch:      ld   l,a             ; offset low
               rla                  ; set carry with sign
               sbc  a,a             ; form high byte for offset
               ld   h,a
               add  hl,de           ; PC=PC+e
               ex   de,hl
               jp   main_loop
i_bra:         ld   a,(de)          ; unconditional branch [65C02]
               inc  de
               jr   i_branch

i_bbr_0:       ld   a,%00000001     ; BBRn [65C02]
               jp   i_bbr
i_bbr_1:       ld   a,%00000010
               jp   i_bbr
i_bbr_2:       ld   a,%00000100
               jp   i_bbr
i_bbr_3:       ld   a,%00001000
               jp   i_bbr
i_bbr_4:       ld   a,%00010000
               jp   i_bbr
i_bbr_5:       ld   a,%00100000
               jp   i_bbr
i_bbr_6:       ld   a,%01000000
               jp   i_bbr
i_bbr_7:       ld   a,%10000000
i_bbr:         ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,0
               ex   de,hl
               and  (hl)
               ld   a,(de)
               inc  de
               jr   z,i_branch
               jp   main_loop

i_bbs_0:       ld   a,%00000001     ; BBSn [65C02]
               jp   i_bbs
i_bbs_1:       ld   a,%00000010
               jp   i_bbs
i_bbs_2:       ld   a,%00000100
               jp   i_bbs
i_bbs_3:       ld   a,%00001000
               jp   i_bbs
i_bbs_4:       ld   a,%00010000
               jp   i_bbs
i_bbs_5:       ld   a,%00100000
               jp   i_bbs
i_bbs_6:       ld   a,%01000000
               jp   i_bbs
i_bbs_7:       ld   a,%10000000
i_bbs:         ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,0
               ex   de,hl
               and  (hl)
               ld   a,(de)
               inc  de
               jr   nz,i_branch
               jp   main_loop

i_jmp_a:       ex   de,hl           ; JMP nn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               jp   main_loop

i_jmp_i:       ex   de,hl           ; JMP (nn)
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   e,(hl)
;              inc  l               ; 6502 bug wraps within page, OR ...
               inc  hl              ; 65C02 spans pages correctly
               ld   d,(hl)
               jp   main_loop

i_jmp_ax:      ex   de,hl           ; JMP (nn,X) [65C02]
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)          ; carry spans page
               ld   d,a
               inc  hl
               ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               jp   main_loop

i_jsr:         ex   de,hl           ; JSR nn
               ld   e,(hl)          ; subroutine low
               inc  hl              ; only 1 inc - we push ret-1
               ld   d,(hl)          ; subroutine high
               ld   a,h             ; PCh
               exx
               ld   (hl),a          ; push ret-1 high byte
               dec  l               ; S--
               exx
               ld   a,l             ; PCl
               exx
               ld   (hl),a          ; push ret-1 low byte
               dec  l               ; S--
               exx
               jp   main_loop

i_brk:         inc  de              ; return to BRK+2
               ld   a,d
               exx
               ld   (hl),a          ; push return MSB
               dec  l               ; S--
               exx
               ld   a,e
               exx
               ld   (hl),a          ; push return LSB
               dec  l               ; S--
               ld   a,d
               or   %00010000       ; B flag
               ld   (hl),a          ; push flags with B set
               dec  l               ; S--
               set  2,d             ; set I flag
               exx
               ld   de,(m6502_int)  ; fetch interrupt handler

               ld   a,(beeper)
               or   bord_brk        ; BRK not supported
               out  (border),a
               jp   main_loop

i_rts:         exx                  ; RTS
               inc  l               ; S++
               ld   a,(hl)          ; PC LSB
               exx
               ld   e,a
               exx
               inc  l               ; S++
               ld   a,(hl)          ; PC MSB
               exx
               ld   d,a
               inc  de              ; PC++ (strange but true)
               jp   main_loop

i_rti:         exx                  ; RTI
               inc  l               ; S++
               ld   a,(hl)          ; pop P
               or   %00110000       ; set T and B flags
               call split_p_exx     ; split P into status+flags (already exx)
               exx
               inc  l               ; S++
               ld   a,(hl)          ; pop return LSB
               exx
               ld   e,a
               exx
               inc  l               ; S++
               ld   a,(hl)          ; pop return MSB
               exx
               ld   d,a
               jp   main_loop

i_php:         call make_p          ; make P from status+flags
               or   %00010000       ; B always pushed as 1
               exx
               ld   (hl),a
               dec  l               ; S--
               exx
               jp   main_loop
i_plp:         exx                  ; PLP
               inc  l               ; S++
               ld   a,(hl)          ; P
               or   %00110000       ; set T and B flags
               exx
               call split_p         ; split P into status+flags
               jp   main_loop
i_pha:         ld   a,b             ; PHA
               exx
               ld   (hl),a
               dec  l               ; S--
               exx
               jp   main_loop
i_pla:         exx                  ; PLA
               inc  l               ; S++
               ld   a,(hl)
               exx
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_phx:         defb &fd             ; PHX [65C02]
               ld   a,h             ; X
               exx
               ld   (hl),a
               dec  l               ; S--
               exx
               jp   main_loop
i_plx:         exx                  ; PLX [65C02]
               inc  l               ; S++
               ld   a,(hl)
               exx
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_phy:         defb &fd             ; PHY [65C02]
               ld   a,l             ; Y
               exx
               ld   (hl),a
               dec  l               ; S--
               exx
               jp   main_loop
i_ply:         exx                  ; PLY [65C02]
               inc  l               ; S++
               ld   a,(hl)
               exx
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_dex:         defb &fd             ; DEX
               dec  h               ; X--
               defb &fd
               ld   a,h             ; X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_dey:         defb &fd             ; DEY
               dec  l               ; Y--
               defb &fd
               ld   a,l             ; Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_inx:         defb &fd             ; INX
               inc  h               ; X++
               defb &fd
               ld   a,h             ; X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_iny:         defb &fd             ; INY
               inc  l               ; Y++
               defb &fd
               ld   a,l             ; Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_txa:         defb &fd             ; TXA
               ld   a,h             ; X
               ld   b,a             ; A=X
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_tya:         defb &fd             ; TYA
               ld   a,l             ; Y
               ld   b,a             ; A=Y
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_tax:         defb &fd             ; TAX
               ld   h,b             ; X=A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   main_loop
i_tay:         defb &fd             ; TAY
               ld   l,b             ; Y=A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   main_loop
i_txs:         defb &fd             ; TXS
               ld   a,h             ; X
               exx
               ld   l,a             ; set S (no flags set)
               exx
               jp   main_loop
i_tsx:         exx                  ; TSX
               ld   a,l             ; fetch S
               exx
               defb &fd
               ld   h,a             ; X=S
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop


; For speed, LDA/LDX/LDY instructions have addressing inlined

i_lda_ix:      ld   a,(de)          ; LDA ($nn,X)
               inc  de
               defb &fd
               add  a,h             ; add X
               ld   l,a             ; (may wrap in zero page)
               ld   h,0
               ld   a,(hl)
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_lda_z:       ld   a,(de)          ; LDA $nn
               inc  de
               ld   h,0
               ld   l,a
               ld   a,(hl)
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_lda_a:       ex   de,hl           ; LDA $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,(hl)
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_lda_iy:      ld   a,(de)          ; LDA ($nn),Y
               inc  de
               ld   l,a
               ld   h,0
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ld   a,(hl)
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_lda_zx:      ld   a,(de)          ; LDA $nn,X
               inc  de
               defb &fd
               add  a,h             ; add X
               ld   l,a             ; (may wrap in zero page)
               ld   h,0
               ld   a,(hl)
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_lda_ay:      ex   de,hl           ; LDA $nnnn,Y
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   a,(hl)
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_lda_ax:      ex   de,hl           ; LDA $nnnn,X
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   a,(hl)
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_lda_i:       ld   a,(de)          ; LDA #$nn
               inc  de
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_lda_iz:      ld   a,(de)          ; LDA ($nn) [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_ldx_z:       ld   a,(de)          ; LDX $nn
               inc  de
               ld   h,0
               ld   l,a
               ld   a,(hl)          ; set NZ
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_ldx_a:       ex   de,hl           ; LDX $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,(hl)
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_ldx_zy:      ld   a,(de)          ; LDX $nn,Y
               inc  de
               defb &fd
               add  a,l             ; add Y
               ld   l,a             ; (may wrap in zero page)
               ld   h,0
               ld   a,(hl)
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_ldx_ay:      ex   de,hl           ; LDX $nnnn,Y
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   a,(hl)
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_ldx_i:       ld   a,(de)          ; LDX #$nn
               inc  de
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_ldy_z:       ld   a,(de)          ; LDY $nn
               inc  de
               ld   h,0
               ld   l,a
               ld   a,(hl)
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_ldy_a:       ex   de,hl           ; LDY $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,(hl)
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_ldy_zx:      ld   a,(de)          ; LDY $nn,X
               inc  de
               defb &fd
               add  a,h             ; add X
               ld   l,a             ; (may wrap in zero page)
               ld   h,0
               ld   a,(hl)
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_ldy_ax:      ex   de,hl           ; LDY $nnnn,X
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   a,(hl)
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_ldy_i:       ld   a,(de)          ; LDY #$nn
               inc  de
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop


; For speed, STA/STX/STY instructions have addressing inlined

i_sta_ix:      ld   a,(de)          ; STA ($xx,X)
               inc  de
               defb &fd
               add  a,h             ; add X
               ld   l,a             ; (may wrap in zero page)
               ld   h,0
               ld   (hl),b
               jp   main_loop
i_sta_z:       ld   a,(de)          ; STA $nn
               inc  de
               ld   h,0
               ld   l,a
               ld   (hl),b
               jp   main_loop
i_sta_iy:      ld   a,(de)
               inc  de
               ld   l,a
               ld   h,0
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               inc  l
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ld   (hl),b
               jp   write_loop
i_sta_zx:      ld   a,(de)
               inc  de
               defb &fd
               add  a,h             ; add X
               ld   l,a             ; (may wrap in zero page)
               ld   h,0
               ld   (hl),b
               jp   main_loop
i_sta_ay:      ex   de,hl
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   (hl),b
               jp   write_loop

i_sta_ax:      ex   de,hl
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   (hl),b
               jp   write_loop
i_sta_a:       ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   (hl),b
               jp   write_loop
i_sta_iz:      ld   a,(de)          ; STA ($nn) [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   (hl),b          ; store A
               jp   write_loop

i_stx_z:       ld   a,(de)
               inc  de
               ld   h,0
               ld   l,a
               defb &fd
               ld   a,h             ; X
               ld   (hl),a
               jp   main_loop
i_stx_zy:      ld   a,(de)
               inc  de
               defb &fd
               add  a,l             ; add Y
               ld   l,a             ; (may wrap in zero page)
               ld   h,0
               defb &fd
               ld   a,h             ; X
               ld   (hl),a
               jp   main_loop
i_stx_a:       ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               defb &fd
               ld   a,h             ; X
               ld   (hl),a
               jp   write_loop

i_sty_z:       ld   a,(de)
               inc  de
               ld   h,0
               ld   l,a
               defb &fd
               ld   a,l             ; Y
               ld   (hl),a
               jp   main_loop
i_sty_zx:      ld   a,(de)
               inc  de
               defb &fd
               add  a,h             ; add X
               ld   l,a             ; (may wrap in zero page)
               ld   h,0
               defb &fd
               ld   a,l             ; Y
               ld   (hl),a
               jp   main_loop
i_sty_a:       ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               defb &fd
               ld   a,l             ; Y
               ld   (hl),a
               jp   write_loop

i_stz_z:       ld   a,(de)          ; STZ $nn [65C02]
               inc  de
               ld   h,0
               ld   l,a
               ld   (hl),h
               jp   main_loop
i_stz_zx:      ld   a,(de)
               inc  de
               defb &fd
               add  a,h             ; add X
               ld   l,a             ; (may wrap in zero page)
               ld   h,0
               ld   (hl),h
               jp   main_loop
i_stz_ax:      ex   de,hl
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   (hl),0
               jp   write_loop
i_stz_a:       ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   (hl),0
               jp   write_loop

i_adc_ix:      ld   ix,i_adc
               jp   a_indirect_x
i_adc_z:       ld   ix,i_adc
               jp   a_zero_page
i_adc_a:       ld   ix,i_adc
               jp   a_absolute
i_adc_zx:      ld   ix,i_adc
               jp   a_zero_page_x
i_adc_ay:      ld   ix,i_adc
               jp   a_absolute_y
i_adc_ax:      ld   ix,i_adc
               jp   a_absolute_x
i_adc_iz:      ld   ix,i_adc        ; [65C02]
               jp   a_indirect_z
i_adc_i:       ld   a,(de)
               inc  de
               jp   i_adc
i_adc_iy:      ld   ix,i_adc
               jp   a_indirect_y
i_adc:         ld   l,a
               exx
               ld   a,c             ; C
               exx
               rra                  ; set up carry
               ld   a,b             ; A
               adc  a,l             ; A+M+C
adc_daa:       nop
               ld   b,a             ; set A
;              jp   set_nvzc
               ; fall through to set_nvzc...

set_nvzc:      ld   c,a             ; set Z
               rla                  ; C in bit 0, no effect on V
               exx
               ld   c,a             ; set C
               jp   pe,set_v
               ld   b,%00000000     ; V clear
               exx
               ld   a,c
               ex   af,af'          ; set N
               jp   main_loop
set_v:         ld   b,%01000000     ; V set
               exx
               ld   a,c
               ex   af,af'          ; set N
               jp   main_loop

i_sbc_ix:      ld   ix,i_sbc
               jp   a_indirect_x
i_sbc_z:       ld   ix,i_sbc
               jp   a_zero_page
i_sbc_a:       ld   ix,i_sbc
               jp   a_absolute
i_sbc_zx:      ld   ix,i_sbc
               jp   a_zero_page_x
i_sbc_ay:      ld   ix,i_sbc
               jp   a_absolute_y
i_sbc_ax:      ld   ix,i_sbc
               jp   a_absolute_x
i_sbc_iz:      ld   ix,i_sbc        ; [65C02]
               jp   a_indirect_z
i_sbc_i:       ld   a,(de)
               inc  de
               jp   i_sbc
i_sbc_iy:      ld   ix,i_sbc
               jp   a_indirect_y
i_sbc:         ld   l,a
               exx
               ld   a,c             ; C
               exx
               rra                  ; set up carry
               ld   a,b             ; A
               ccf                  ; uses inverted carry
               sbc  a,l             ; A-M-(1-C)
sbc_daa:       nop
               ccf                  ; no carry for overflow
               ld   b,a             ; set A
               jp   set_nvzc

i_and_ix:      ld   ix,i_and
               jp   a_indirect_x
i_and_z:       ld   ix,i_and
               jp   a_zero_page
i_and_a:       ld   ix,i_and
               jp   a_absolute
i_and_zx:      ld   ix,i_and
               jp   a_zero_page_x
i_and_ay:      ld   ix,i_and
               jp   a_absolute_y
i_and_ax:      ld   ix,i_and
               jp   a_absolute_x
i_and_iz:      ld   ix,i_and        ; [65C02]
               jp   a_indirect_z
i_and_i:       ld   a,(de)
               inc  de
               jp   i_and
i_and_iy:      ld   ix,i_and
               jp   a_indirect_y
i_and:         and  b               ; A&x
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_eor_ix:      ld   ix,i_eor
               jp   a_indirect_x
i_eor_z:       ld   ix,i_eor
               jp   a_zero_page
i_eor_a:       ld   ix,i_eor
               jp   a_absolute
i_eor_zx:      ld   ix,i_eor
               jp   a_zero_page_x
i_eor_ay:      ld   ix,i_eor
               jp   a_absolute_y
i_eor_ax:      ld   ix,i_eor
               jp   a_absolute_x
i_eor_iz:      ld   ix,i_eor        ; [65C02]
               jp   a_indirect_z
i_eor_i:       ld   a,(de)
               inc  de
               jp   i_eor
i_eor_iy:      ld   ix,i_eor
               jp   a_indirect_y
i_eor:         xor  b               ; A^x
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_ora_ix:      ld   ix,i_ora
               jp   a_indirect_x
i_ora_z:       ld   ix,i_ora
               jp   a_zero_page
i_ora_a:       ld   ix,i_ora
               jp   a_absolute
i_ora_zx:      ld   ix,i_ora
               jp   a_zero_page_x
i_ora_ay:      ld   ix,i_ora
               jp   a_absolute_y
i_ora_ax:      ld   ix,i_ora
               jp   a_absolute_x
i_ora_iz:      ld   ix,i_ora        ; [65C02]
               jp   a_indirect_z
i_ora_i:       ld   a,(de)
               inc  de
               jp   i_ora
i_ora_iy:      ld   ix,i_ora
               jp   a_indirect_y
i_ora:         or   b               ; A|x
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_cmp_ix:      ld   ix,i_cmp
               jp   a_indirect_x
i_cmp_z:       ld   ix,i_cmp
               jp   a_zero_page
i_cmp_a:       ld   ix,i_cmp
               jp   a_absolute
i_cmp_zx:      ld   ix,i_cmp
               jp   a_zero_page_x
i_cmp_ay:      ld   ix,i_cmp
               jp   a_absolute_y
i_cmp_ax:      ld   ix,i_cmp
               jp   a_absolute_x
i_cmp_iz:      ld   ix,i_cmp        ; [65C02]
               jp   a_indirect_z
i_cmp_i:       ld   a,(de)
               inc  de
               jp   i_cmp
i_cmp_iy:      ld   ix,i_cmp
               jp   a_indirect_y
i_cmp:         ld   l,a             ; save operand
               ld   a,b             ; A
               sub  l               ; SUB needed for end result
               ccf
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_cpx_z:       ld   ix,i_cpx
               jp   a_zero_page
i_cpx_a:       ld   ix,i_cpx
               jp   a_absolute
i_cpx_i:       ld   a,(de)
               inc  de
i_cpx:         ld   l,a             ; save operand
               defb &fd
               ld   a,h             ; X
               sub  l               ; SUB needed for end result
               ccf
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_cpy_z:       ld   ix,i_cpy
               jp   a_zero_page
i_cpy_a:       ld   ix,i_cpy
               jp   a_absolute
i_cpy_i:       ld   a,(de)
               inc  de
i_cpy:         ld   l,a             ; save operand
               defb &fd
               ld   a,l             ; Y
               sub  l               ; SUB needed for end result
               ccf
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop


i_dec_z:       ld   ix,i_dec_zp
               jp   a_zero_page
i_dec_zx:      ld   ix,i_dec_zp
               jp   a_zero_page_x
i_dec_a:       ld   ix,i_dec
               jp   a_absolute
i_dec_ax:      ld   ix,i_dec
               jp   a_absolute_x
i_dec:         dec  a
               ld   (hl),a          ; set memory
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   write_loop
i_dec_zp:      dec  a
               ld   (hl),a          ; set zero-page memory
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_dec_ac:      dec  b               ; A--
               ld   a,b
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_inc_z:       ld   ix,i_inc_zp
               jp   a_zero_page
i_inc_zx:      ld   ix,i_inc_zp
               jp   a_zero_page_x
i_inc_a:       ld   ix,i_inc
               jp   a_absolute
i_inc_ax:      ld   ix,i_inc
               jp   a_absolute_x
i_inc:         inc  a
               ld   (hl),a          ; set memory
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   write_loop
i_inc_zp:      inc  a
               ld   (hl),a          ; set zero-page memory
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_inc_ac:      inc  b               ; A++
               ld   a,b
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_asl_z:       ld   ix,i_asl
               jp   a_zero_page
i_asl_zx:      ld   ix,i_asl
               jp   a_zero_page_x
i_asl_a:       ld   ix,i_asl
               jp   a_absolute
i_asl_ax:      ld   ix,i_asl
               jp   a_absolute_x
i_asl_acc:     sla  b               ; A << 1
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   main_loop
i_asl:         add  a,a             ; x << 1
               ld   (hl),a          ; set memory
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   write_loop

i_lsr_z:       ld   ix,i_lsr
               jp   a_zero_page
i_lsr_zx:      ld   ix,i_lsr
               jp   a_zero_page_x
i_lsr_a:       ld   ix,i_lsr
               jp   a_absolute
i_lsr_ax:      ld   ix,i_lsr
               jp   a_absolute_x
i_lsr_acc:     srl  b               ; A >> 1
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   main_loop
i_lsr:         srl  a               ; x >> 1
               ld   (hl),a          ; set memory
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   write_loop

i_rol_z:       ld   ix,i_rol
               jp   a_zero_page
i_rol_zx:      ld   ix,i_rol
               jp   a_zero_page_x
i_rol_a:       ld   ix,i_rol
               jp   a_absolute
i_rol_ax:      ld   ix,i_rol
               jp   a_absolute_x
i_rol_acc:     ld   a,b
               exx
               rr   c               ; set up carry
               rla                  ; A << 1
               rl   c               ; retrieve carry
               exx
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_rol:         exx
               rr   c               ; set up carry
               rla                  ; x << 1
               rl   c               ; retrieve carry
               exx
               ld   (hl),a          ; set memory
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   write_loop

i_ror_z:       ld   ix,i_ror
               jp   a_zero_page
i_ror_zx:      ld   ix,i_ror
               jp   a_zero_page_x
i_ror_a:       ld   ix,i_ror
               jp   a_absolute
i_ror_ax:      ld   ix,i_ror
               jp   a_absolute_x
i_ror_acc:     ld   a,b
               exx
               rr   c               ; set up carry
               rra                  ; A >> 1
               rl   c               ; retrieve carry
               exx
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_ror:         exx
               rr   c               ; set up carry
               rra                  ; x >> 1
               rl   c               ; retrieve carry
               exx
               ld   (hl),a          ; set memory
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   write_loop


i_bit_z:       ld   ix,i_bit
               jp   a_zero_page
i_bit_zx:      ld   ix,i_bit
               jp   a_zero_page_x
i_bit_a:       ld   ix,i_bit
               jp   a_absolute
i_bit_ax:      ld   ix,i_bit
               jp   a_absolute_x
i_bit_i:       ld   a,(de)          ; BIT #$nn
               inc  de
i_bit:         ld   l,a             ; keep memory value
               ex   af,af'          ; N flag set from bit 7
               ld   a,l
               and  %01000000       ; V flag set from bit 6
               exx
               ld   b,a             ; set V
               exx
               ld   a,b             ; A
               and  l               ; perform BIT test
               ld   c,a             ; set Z
               jp   main_loop

i_tsb_z:       ld   ix,i_tsb        ; TSB [65C02]
               jp   a_zero_page
i_tsb_a:       ld   ix,i_tsb
               jp   a_absolute
i_tsb:         ld   c,a             ; keep memory value
               or   b               ; set bits from A
               ld   (hl),a
               ld   a,c
               and  b               ; test bits against A
               ld   c,a             ; set Z
               jp   write_loop

i_trb_z:       ld   ix,i_trb        ; TRB [65C02]
               jp   a_zero_page
i_trb_a:       ld   ix,i_trb
               jp   a_absolute
i_trb:         ld   c,a             ; keep memory value
               ld   a,b             ; A
               cpl                  ; ~A
               and  c               ; reset bits from A
               ld   (hl),a
               ld   a,c
               and  b               ; test bits against A
               ld   c,a             ; set Z
               jp   write_loop

i_smb_0:       ld   a,%00000001     ; SMBn [65C02]
               jp   i_smb
i_smb_1:       ld   a,%00000010
               jp   i_smb
i_smb_2:       ld   a,%00000100
               jp   i_smb
i_smb_3:       ld   a,%00001000
               jp   i_smb
i_smb_4:       ld   a,%00010000
               jp   i_smb
i_smb_5:       ld   a,%00100000
               jp   i_smb
i_smb_6:       ld   a,%01000000
               jp   i_smb
i_smb_7:       ld   a,%10000000
i_smb:         ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,0
               ex   de,hl
               or   (hl)
               ld   (hl),a
               jp   main_loop

i_rmb_0:       ld   a,%11111110     ; RMBn [65C02]
               jp   i_smb
i_rmb_1:       ld   a,%11111101
               jp   i_smb
i_rmb_2:       ld   a,%11111011
               jp   i_smb
i_rmb_3:       ld   a,%11110111
               jp   i_smb
i_rmb_4:       ld   a,%11101111
               jp   i_smb
i_rmb_5:       ld   a,%11011111
               jp   i_smb
i_rmb_6:       ld   a,%10111111
               jp   i_smb
i_rmb_7:       ld   a,%01111111
i_rmb:         ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,0
               ex   de,hl
               and  (hl)
               ld   (hl),a
               jp   main_loop

i_stp:         dec  de              ; STP [65C02]
               ld   a,bord_stp
               out  (border),a
               jp   main_loop

i_wai:         dec  de              ; WAI [65C02]
               ld   a,bord_wai
               out  (border),a
               jp   main_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

make_p:        ex   af,af'
               and  %10000000       ; keep N
               ld   l,a             ; N
               ex   af,af'
               ld   a,c             ; Z
               sub  1               ; set carry if zero
               rla
               rla
               and  %00000010       ; keep 6510 Z bit
               or   l               ; N+Z
               exx
               or   b               ; N+V+Z
               ld   e,a
               ld   a,c
               and  %00000001       ; keep C
               or   e               ; N+V+Z+C
               exx
               ret

split_p:       exx
split_p_exx:   ld   e,a             ; save P
               and  %00111100       ; keep CPU bits
               ld   d,a             ; set status
               ld   a,e
               ex   af,af'          ; set N
               ld   a,e
               and  %01000000       ; keep V
               ld   b,a             ; set V
               ld   a,e
               and  %00000001       ; keep C
               ld   c,a             ; set C
               ld   a,e
               cpl
               and  %00000010       ; Z=0 NZ=2
               exx
               ld   c,a             ; set NZ
               ret

load_state:    ld   a,(reg_a)
               ld   b,a             ; set A
               ld   a,(reg_x)
               defb &fd
               ld   h,a             ; set X
               ld   a,(reg_y)
               defb &fd
               ld   l,a             ; set Y
               exx
               ld   a,(reg_s)
               ld   l,a             ; set S
               ld   h,&01           ; MSB for stack pointer
               exx
               ld   a,(reg_p)
               call split_p         ; set P and flags
               ld   de,(reg_pc)     ; set PC
               ret

save_state:    ld   a,b             ; get A
               ld   (reg_a),a
               defb &fd
               ld   a,h             ; get X
               ld   (reg_x),a
               defb &fd
               ld   a,l             ; get Y
               ld   (reg_y),a
               exx
               ld   a,l             ; get S
               ld   (reg_s),a
               exx
               call make_p          ; get P
               ld   (reg_pc),de
               ret

; Reordering the decode table to group low and high bytes means
; we avoid any 16-bit arithmetic for the decode stage, saving
; 12T on the old method (cool tip from Dave Laundon)
reorder_decode:ld   bc,&8000
               ld   hl,&8100
               ld   de,decode_table
reorder_loop:  ld   a,(de)
               ld   (bc),a          ; low byte
               inc  e
               inc  c
               ld   a,(de)
               ld   (hl),a
               inc  de
               inc  l
               jr   nz,reorder_loop
               ld   h,&82           ; HL=&8200
               ld   b,2             ; BC=512
               lddr
               ld   a,&c9           ; RET
               ld   (reorder_decode),A
               ret


               defs -$\256          ; align to 256-bytes

decode_table:  DEFW i_brk,i_ora_ix,i_undoc_1,i_undoc_2     ; 00
               DEFW i_tsb_z,i_ora_z,i_asl_z,i_rmb_0        ; 04
               DEFW i_php,i_ora_i,i_asl_acc,i_undoc_2      ; 08
               DEFW i_tsb_a,i_ora_a,i_asl_a,i_bbr_0        ; 0C

               DEFW i_bpl,i_ora_iy,i_ora_iz,i_undoc_2      ; 10
               DEFW i_trb_z,i_ora_zx,i_asl_zx,i_rmb_1      ; 14
               DEFW i_clc,i_ora_ay,i_inc_ac,i_undoc_3      ; 18
               DEFW i_trb_a,i_ora_ax,i_asl_ax,i_bbr_1      ; 1C

               DEFW i_jsr,i_and_ix,i_undoc_1,i_undoc_2     ; 20
               DEFW i_bit_z,i_and_z,i_rol_z,i_rmb_2        ; 24
               DEFW i_plp,i_and_i,i_rol_acc,i_undoc_2      ; 28
               DEFW i_bit_a,i_and_a,i_rol_a,i_bbr_2        ; 2C

               DEFW i_bmi,i_and_iy,i_and_iz,i_undoc_2      ; 30
               DEFW i_bit_zx,i_and_zx,i_rol_zx,i_rmb_3     ; 34
               DEFW i_sec,i_and_ay,i_dec_ac,i_undoc_3      ; 38
               DEFW i_bit_ax,i_and_ax,i_rol_ax,i_bbr_3     ; 3C

               DEFW i_rti,i_eor_ix,i_undoc_1,i_undoc_2     ; 40
               DEFW i_undoc_2,i_eor_z,i_lsr_z,i_rmb_4      ; 44
               DEFW i_pha,i_eor_i,i_lsr_acc,i_undoc_2      ; 48
               DEFW i_jmp_a,i_eor_a,i_lsr_a,i_bbr_4        ; 4C

               DEFW i_bvc,i_eor_iy,i_eor_iz,i_undoc_2      ; 50
               DEFW i_undoc_2,i_eor_zx,i_lsr_zx,i_rmb_5    ; 54
               DEFW i_cli,i_eor_ay,i_phy,i_undoc_3         ; 58
               DEFW i_undoc_3,i_eor_ax,i_lsr_ax,i_bbr_5    ; 5C

               DEFW i_rts,i_adc_ix,i_undoc_1,i_undoc_2     ; 60
               DEFW i_stz_z,i_adc_z,i_ror_z,i_rmb_6        ; 64
               DEFW i_pla,i_adc_i,i_ror_acc,i_undoc_2      ; 68
               DEFW i_jmp_i,i_adc_a,i_ror_a,i_bbr_6        ; 6C

               DEFW i_bvs,i_adc_iy,i_adc_iz,i_undoc_2      ; 70
               DEFW i_stz_zx,i_adc_zx,i_ror_zx,i_rmb_7     ; 74
               DEFW i_sei,i_adc_ay,i_ply,i_undoc_3         ; 78
               DEFW i_jmp_ax,i_adc_ax,i_ror_ax,i_bbr_7     ; 7C

               DEFW i_bra,i_sta_ix,i_undoc_2,i_undoc_2     ; 80
               DEFW i_sty_z,i_sta_z,i_stx_z,i_smb_0        ; 84
               DEFW i_dey,i_bit_i,i_txa,i_undoc_2          ; 88
               DEFW i_sty_a,i_sta_a,i_stx_a,i_bbs_0        ; 8C

               DEFW i_bcc,i_sta_iy,i_sta_iz,i_undoc_2      ; 90
               DEFW i_sty_zx,i_sta_zx,i_stx_zy,i_smb_1     ; 94
               DEFW i_tya,i_sta_ay,i_txs,i_undoc_2         ; 98
               DEFW i_stz_a,i_sta_ax,i_stz_ax,i_bbs_1      ; 9C

               DEFW i_ldy_i,i_lda_ix,i_ldx_i,i_undoc_2     ; A0
               DEFW i_ldy_z,i_lda_z,i_ldx_z,i_smb_2        ; A4
               DEFW i_tay,i_lda_i,i_tax,i_undoc_2          ; A8
               DEFW i_ldy_a,i_lda_a,i_ldx_a,i_bbs_2        ; AC

               DEFW i_bcs,i_lda_iy,i_lda_iz,i_undoc_2      ; B0
               DEFW i_ldy_zx,i_lda_zx,i_ldx_zy,i_smb_3     ; B4
               DEFW i_clv,i_lda_ay,i_tsx,i_undoc_3         ; B8
               DEFW i_ldy_ax,i_lda_ax,i_ldx_ay,i_bbs_3     ; BC

               DEFW i_cpy_i,i_cmp_ix,i_undoc_2,i_undoc_2   ; C0
               DEFW i_cpy_z,i_cmp_z,i_dec_z,i_smb_4        ; C4
               DEFW i_iny,i_cmp_i,i_dex,i_wai              ; C8
               DEFW i_cpy_a,i_cmp_a,i_dec_a,i_bbs_4        ; CC

               DEFW i_bne,i_cmp_iy,i_cmp_iz,i_undoc_2      ; D0
               DEFW i_undoc_2,i_cmp_zx,i_dec_zx,i_smb_5    ; D4
               DEFW i_cld,i_cmp_ay,i_phx,i_stp             ; D8
               DEFW i_undoc_3,i_cmp_ax,i_dec_ax,i_bbs_5    ; DC

               DEFW i_cpx_i,i_sbc_ix,i_undoc_2,i_undoc_2   ; E0
               DEFW i_cpx_z,i_sbc_z,i_inc_z,i_smb_6        ; E4
               DEFW i_inx,i_sbc_i,i_nop,i_undoc_2          ; E8
               DEFW i_cpx_a,i_sbc_a,i_inc_a,i_bbs_6        ; EC

               DEFW i_beq,i_sbc_iy,i_sbc_iz,i_undoc_2      ; F0
               DEFW i_undoc_2,i_sbc_zx,i_inc_zx,i_smb_7    ; F4
               DEFW i_sed,i_sbc_ay,i_plx,i_undoc_3         ; F8
               DEFW i_undoc_3,i_sbc_ax,i_inc_ax,i_bbs_7    ; FC

; Mapping from 8x9 SAM matrix to native $20xx I/O address
keymap:
    defb &fa,&87,&80, &fa,&87,&c0, &fa,&87,&e0,  &ff,&86,&10, &7f,&87,&10, &7f,&87,&20, &f6,&87,&c0, &fb,&87,&10   ; F3 F2 F1 V C X Z Shift
    defb &fe,&85,&c0, &fe,&85,&e0, &fa,&87,&10,  &fe,&86,&80, &fe,&86,&e0, &7e,&87,&e0, &7e,&87,&80, &7e,&87,&c0   ; F6 F5 F4 G F D S A
    defb &fe,&85,&80, &fe,&83,&c0, &fe,&83,&80,  &f6,&87,&80, &f6,&87,&e0, &de,&87,&e0, &de,&87,&80, &de,&87,&c0   ; F9 F8 F7 T R E W Q
    defb &fa,&87,&e0, &00,&00,&00, &00,&00,&00,  &f7,&87,&20, &f7,&87,&10, &df,&87,&10, &df,&87,&20, &de,&87,&20   ; Caps Tab Esc 5 4 3 2 1
    defb &fc,&87,&e0, &fe,&83,&10, &ff,&83,&20,  &f6,&87,&10, &ee,&87,&10, &ef,&87,&20, &ef,&87,&10, &ff,&83,&10   ; DEL + - 6 7 8 9 0
    defb &00,&00,&00, &df,&87,&20, &ff,&83,&20,  &7e,&87,&10, &ee,&87,&80, &ee,&87,&c0, &ee,&87,&e0, &fe,&83,&e0   ; F0 " = Y U I O P
    defb &00,&00,&00, &fe,&85,&10, &fe,&83,&10,  &fe,&86,&c0, &be,&87,&c0, &be,&87,&80, &be,&87,&e0, &fd,&87,&00   ; Edit : ; H J K L Return
    defb &fa,&87,&80, &bf,&87,&10, &bf,&87,&20,  &ff,&86,&20, &fe,&86,&10, &be,&87,&10, &00,&00,&00, &fb,&87,&20   ; Inv . , B N M Sym Space
    defb &00,&00,&00, &00,&00,&00, &00,&00,&00,  &fc,&87,&10, &fc,&87,&e0, &fc,&87,&80, &fc,&87,&c0, &fd,&87,&10   ; - - - Right Left Down Up Cntrl

keyports:
    defw &83fe,&83ff, &85fe,&85ff, &86fe,&86ff, &877e,&877f, &87be,&87bf
    defw &87de,&87df, &87ee,&87ef, &87f6,&87f7, &87fa,&87fb, &87fc,&87fd

end:           equ  $
length:        equ  end-start

               defs -$\256          ; align to 256-bytes

rev_table:     DEFS 256
line_table:    DEFS 256
scr_addrs:     DEFS 512

; During running we keep the 65xx registers in Z80 registers
; These are used only to hold the state before/after running
reg_a:         defb 0
reg_p:         defb 0
reg_x:         defb 0
reg_y:         defb 0
reg_s:         defb 0
reg_pc:        defw 0


stack:         defs 64              ; small private stack
stack_top:     equ  $

; Orao snapshots have a 32-bit header for register+state information
snap_state:    defs 32

beeper:        defb &00


; BASIC ROM (&c000-&dfff)
    dump low_page-1,0
MDAT "bas13.rom"

; Monitor ROM (&e000-&ffff)
    dump low_page-1,8192
MDAT "crt13.rom"

; Sample
    dump low_page,0
;MDAT "Snapshots/manic.dmp"
