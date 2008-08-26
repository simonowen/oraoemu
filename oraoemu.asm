; Orao emulator for SAM Coupe, by Simon Owen
;
; Version 1.2 (26/8/2008)
;
; WWW: http://simonowen.com/sam/oraoemu/

base:          equ  &9000

status:        equ  &f9             ; Status and extended keyboard port
lmpr:          equ  &fa             ; Low Memory Page Register
hmpr:          equ  &fb             ; High Memory Page Register
vmpr:          equ  &fc             ; Video Memory Page Register
keyboard:      equ  &fe             ; Keyboard port
border:        equ  &fe             ; Border port
rom0_off:      equ  %00100000       ; LMPR bit to disable ROM0
vmpr_mode2:    equ  %00100000       ; Mode 2 select for VMPR

low_page:      equ  3               ; LMPR during emulation
screen_page:   equ  5               ; SAM display

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

               call do_emu

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
               call load_state
               ei
               call execute     ; GO!
               di
               call save_state

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

setup_im2:     ld   hl,im2_table
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
               push af

               in   a,(lmpr)
               push af
               ld   a,low_page+rom0_off
               out  (lmpr),a

               call update_keys

               pop  af
               out  (lmpr),a

               pop  af
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

execute:       ld   a,&1a           ; LD A,(DE)
               ld   (ix),a
               jp   (ix)


i_undoc_3:     inc  de              ; 3-byte NOP
i_undoc_2:     inc  de              ; 2-byte NOP
i_undoc_1:     jp   (ix)


read_write_loop:
write_loop:    ld   a,h
               cp   &60             ; screen or above?
               jr   nc,write_trap

main_loop:     ld   a,(de)          ; 7/7/15  - fetch opcode
               inc  de              ; 6/7/11  - PC++
               ld   l,a             ; 4/6/6   - LSB is opcode
               ld   h,msb_table/256 ; 7/7/15  - look-up table
               ld   h,(hl)          ; 7/8/16  - opcode MSB
               jp   (hl)            ; 4/5/9   - execute!
                                    ; = 35T (official) / 40T (off-screen) / 72T (on-screen)

write_trap:    cp   &80             ; I/O area or above?
               jr   nc,io_or_up

               ; screen write
               push de
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
               pop  de
               jp   (ix)

               ; write to the I/O area
io_or_up:      cp   &88
               jr   z,sound_write
               jp   (ix)            ; ignore non-sound writes

               ; Reading from &88xx is unknown, so best leave visible as &ff
sound_write:   ld   (hl),&ff

               ; Toggle speaker bit
               ld   a,(beeper)
               xor  %00010000
               ld   (beeper),a
               out  (border),a

               ; short-circuit any inner sound loop in X/Y
               ld   a,(de)
               cp   &88             ; DEY
               jr   nz,not_dey
               ld   iyl,1
               jp   (ix)
not_dey:       cp   &ca             ; DEX
               jr   nz,not_dex
               ld   iyh,1
not_dex:       jp   (ix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

load_state:    call update_screen

               ld   a,(reg_a)
               ld   b,a             ; set A
               ld   a,(reg_x)
               ld   iyh,a           ; set X to IYh
               ld   a,(reg_y)
               ld   iyl,a           ; set Y to IYl
               exx
               ld   a,(reg_s)
               ld   l,a             ; set S
               ld   h,&01           ; MSB for stack pointer
               ld   a,(reg_p)
               ld   c,a             ; keep safe
               and  %00001100       ; keep D and I
               or   %00110000       ; force T and B
               ld   d,a             ; set P
               ld   a,c
               and  %01000000       ; keep V
               ld   e,a             ; set V
               ld   a,c
               rra                  ; carry from C
               ex   af,af'          ; set carry
               ld   a,c
               and  %10000010       ; keep N Z
               xor  %00000010       ; zero for Z
               exx
               ld   c,a             ; set N Z
               ld   de,(reg_pc)     ; set PC
               ld   ix,main_loop    ; decode loop
               ret

save_state:    ld   a,b             ; get A
               ld   (reg_a),a
               ld   a,iyh           ; get X from IYh
               ld   (reg_x),a
               ld   a,iyl           ; get Y from IYl
               ld   (reg_y),a
               ex   af,af'          ; carry
               inc  c
               dec  c               ; set N Z
               push af              ; save flags
               ex   af,af'          ; protect carry
               exx
               pop  bc
               ld   a,c
               and  %10000001       ; keep Z80 N and C
               bit  6,c             ; check Z80 Z
               jr   z,save_nz
               or   %00000010       ; set Z
save_nz:       or   e               ; merge V
               or   d               ; merge T B D I
               ld   (reg_p),a
               ld   a,l             ; get S
               ld   (reg_s),a
               exx
               ld   (reg_pc),de
               ret


; During running we keep the 65xx registers in Z80 registers
; These are used only to hold the state before/after running
reg_a:         defb 0
reg_p:         defb 0
reg_x:         defb 0
reg_y:         defb 0
reg_s:         defb 0
reg_pc:        defw 0

; Orao snapshots have a 32-bit header for register+state information
snap_state:    defs 32

beeper:        defb &00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; IM 2 table must be aligned to 256-byte boundary
               defs -$\256
im2_table:     defs 257

; IM 2 vector must have LSB==MSB
               defs $/256-1
stack_top:                          ; stack in slack space
im2_jp:        jp   im2_handler

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               defs -$\256

msb_table:     defb op_00>>8, op_01>>8, op_02>>8, op_03>>8, op_04>>8, op_05>>8, op_06>>8, op_07>>8
               defb op_08>>8, op_09>>8, op_0a>>8, op_0b>>8, op_0c>>8, op_0d>>8, op_0e>>8, op_0f>>8
               defb op_10>>8, op_11>>8, op_12>>8, op_13>>8, op_14>>8, op_15>>8, op_16>>8, op_17>>8
               defb op_18>>8, op_19>>8, op_1a>>8, op_1b>>8, op_1c>>8, op_1d>>8, op_1e>>8, op_1f>>8
               defb op_20>>8, op_21>>8, op_22>>8, op_23>>8, op_24>>8, op_25>>8, op_26>>8, op_27>>8
               defb op_28>>8, op_29>>8, op_2a>>8, op_2b>>8, op_2c>>8, op_2d>>8, op_2e>>8, op_2f>>8
               defb op_30>>8, op_31>>8, op_32>>8, op_33>>8, op_34>>8, op_35>>8, op_36>>8, op_37>>8
               defb op_38>>8, op_39>>8, op_3a>>8, op_3b>>8, op_3c>>8, op_3d>>8, op_3e>>8, op_3f>>8
               defb op_40>>8, op_41>>8, op_42>>8, op_43>>8, op_44>>8, op_45>>8, op_46>>8, op_47>>8
               defb op_48>>8, op_49>>8, op_4a>>8, op_4b>>8, op_4c>>8, op_4d>>8, op_4e>>8, op_4f>>8
               defb op_50>>8, op_51>>8, op_52>>8, op_53>>8, op_54>>8, op_55>>8, op_56>>8, op_57>>8
               defb op_58>>8, op_59>>8, op_5a>>8, op_5b>>8, op_5c>>8, op_5d>>8, op_5e>>8, op_5f>>8
               defb op_60>>8, op_61>>8, op_62>>8, op_63>>8, op_64>>8, op_65>>8, op_66>>8, op_67>>8
               defb op_68>>8, op_69>>8, op_6a>>8, op_6b>>8, op_6c>>8, op_6d>>8, op_6e>>8, op_6f>>8
               defb op_70>>8, op_71>>8, op_72>>8, op_73>>8, op_74>>8, op_75>>8, op_76>>8, op_77>>8
               defb op_78>>8, op_79>>8, op_7a>>8, op_7b>>8, op_7c>>8, op_7d>>8, op_7e>>8, op_7f>>8
               defb op_80>>8, op_81>>8, op_82>>8, op_83>>8, op_84>>8, op_85>>8, op_86>>8, op_87>>8
               defb op_88>>8, op_89>>8, op_8a>>8, op_8b>>8, op_8c>>8, op_8d>>8, op_8e>>8, op_8f>>8
               defb op_90>>8, op_91>>8, op_92>>8, op_93>>8, op_94>>8, op_95>>8, op_96>>8, op_97>>8
               defb op_98>>8, op_99>>8, op_9a>>8, op_9b>>8, op_9c>>8, op_9d>>8, op_9e>>8, op_9f>>8
               defb op_a0>>8, op_a1>>8, op_a2>>8, op_a3>>8, op_a4>>8, op_a5>>8, op_a6>>8, op_a7>>8
               defb op_a8>>8, op_a9>>8, op_aa>>8, op_ab>>8, op_ac>>8, op_ad>>8, op_ae>>8, op_af>>8
               defb op_b0>>8, op_b1>>8, op_b2>>8, op_b3>>8, op_b4>>8, op_b5>>8, op_b6>>8, op_b7>>8
               defb op_b8>>8, op_b9>>8, op_ba>>8, op_bb>>8, op_bc>>8, op_bd>>8, op_be>>8, op_bf>>8
               defb op_c0>>8, op_c1>>8, op_c2>>8, op_c3>>8, op_c4>>8, op_c5>>8, op_c6>>8, op_c7>>8
               defb op_c8>>8, op_c9>>8, op_ca>>8, op_cb>>8, op_cc>>8, op_cd>>8, op_ce>>8, op_cf>>8
               defb op_d0>>8, op_d1>>8, op_d2>>8, op_d3>>8, op_d4>>8, op_d5>>8, op_d6>>8, op_d7>>8
               defb op_d8>>8, op_d9>>8, op_da>>8, op_db>>8, op_dc>>8, op_dd>>8, op_de>>8, op_df>>8
               defb op_e0>>8, op_e1>>8, op_e2>>8, op_e3>>8, op_e4>>8, op_e5>>8, op_e6>>8, op_e7>>8
               defb op_e8>>8, op_e9>>8, op_ea>>8, op_eb>>8, op_ec>>8, op_ed>>8, op_ee>>8, op_ef>>8
               defb op_f0>>8, op_f1>>8, op_f2>>8, op_f3>>8, op_f4>>8, op_f5>>8, op_f6>>8, op_f7>>8
               defb op_f8>>8, op_f9>>8, op_fa>>8, op_fb>>8, op_fc>>8, op_fd>>8, op_fe>>8, op_ff>>8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               defs -$\256          ; align to 256-bytes

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               defs -$\256          ; align to 256-bytes

rev_table:     defs &100
line_table:    defs &100
scr_addrs:     defs &200

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

end:           equ  $
length:        equ  end-start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Instruction implementations
INC "opimpl.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BASIC ROM (&c000-&dfff)
    dump low_page-1,0
MDAT "bas13.rom"

; Monitor ROM (&e000-&ffff)
    dump low_page-1,8192
MDAT "crt13.rom"

; Sample
    dump low_page,0
;MDAT "Snapshots/manic.dmp"
