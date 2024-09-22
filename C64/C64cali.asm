; 4 Computer Buffs - C64 calibrate program.
; 
; Assembly source format: 64TASS (Turbo Assembler Macro).
;
; Revision history [authors in square brackets]:
;   2024-09-15: First version [DDT]
;
; This code uses User Port line L (CIA2 port B bit 7) to calibrate data reception from the light sensor.
; User needs to adjust potentiometer until a reliable stream of alternating 0s and 1s is shown, and no errors are reported in red.


* = $C000      ; Set start address of the program (standard BASIC start address)

; Target (overwrite) BASIC program at current BASIC program start address

    LDA $2B        ; Start of BASIC program text [LO].
    STA $FC        ; Save next write offset [LO] to scratch.
    LDA $2C        ; Start of BASIC program text [HI].
    STA $FD        ; Save next write offset [HI] to scratch.

; Init data input from user port

    LDA #$00
    STA $DD03      ; Set CIA2 port B as input (receive).
    

; Change background color for better contrast.
    LDA #$00
    STA $D021


; Print "PRESS ANY KEY".
    LDA #<str_press_key
    STA $12
    LDA #>str_press_key
    STA $13
    JSR print_str
    
    ; Print default delay value.
    LDA delay_outer + 1
    JSR print_A_hex


; Wait for keypress

wait_key:
    JSR $FFE4      ; Call Kernal GETIN
    CMP #$00
    BEQ wait_key   ; Wait for key...
    
; Print "READING".
    LDA #<str_reading
    STA $12
    LDA #>str_reading
    STA $13
    JSR print_str

; DISABLE INTERRUPTS HERE
    SEI

; Steal two system zeropage addresses to use as ptr to screen memory. This is ok, kernal won't be offended.
    LDA #$00
    STA $AE
    LDA #$04
    STA $AF

; MAIN LOOP START
nxt_byte:
    JSR read_byte        ; Read next byte from stream (i.e. pointer to next BASIC line LO).
    BNE nxt_byte         ; If a RUN/STOP is not pressed, continue waiting for a 0 (start bit).
; MAIN LOOP END


exit:
    LDA #$0E        ; Change border to standard color.
    STA $D020
    
    ; Move cursor to next line relative to current screen position (ptr in $AE, $AF).
    DEC $AF        ; Find screen offset (i.e. sub $0400).
    DEC $AF
    DEC $AF
    DEC $AF
    LDY #0         ; Y coord.
mod40:
    LDA $AE
    SEC            ; No borrow for sub.
    SBC #40
    STA $AE
    BCS mod40_nxt  ; No underflow, skip to next line.
    ; Underflow.
    LDA $AF
    BEQ mod40_end
    DEC $AF
mod40_nxt:
    INY            ; Inc Y coord.
    JMP mod40
mod40_end:

    ; Set cursor position at (Y, 0)    
    LDX #0        ; Cursor to first char of line.
    STX $D3       ; Current cursor column.
    STY $D6       ; Current cursor row.

    CLI            ; ENABLE INTERRUPTS
    RTS            ; EXIT THIS PROGRAM.


;==========================================================================================================
; DELAY routine
; NOTE: This is invoked with X as the primary loop counter.
;
; Formula to calculate total cycles taken by this routine, with X passed as input and Y defined here:
;
; Note that:
;  - PAL  C64, input video at 50Hz (20    ms): 19656 cycles. <===== This is the original target audience in 1985.
;  - NTSC C64, input video at 50Hz (20    ms): 20455 cycles.
;  - PAL  C64, input video at 60Hz (16.67 ms): 16421 cycles. <===== This is default in this program.
;  - NTSC C64, input video at 60Hz (16.67 ms): 17045 cycles.
;
;           8 +   (X * (2 + Y*5 - 1 + 5)) - 1   + 12
;
; IMPORTANT: The 6510 CPU cannot use all cycles in a frame due to VIC-II memory access priority (bad lines et cetera).


delay:
    LDY #$02          ; [2 cycles] Change border to red. 
    STY $D020         ; [6 cycles]
             
delay_outer:        
    ; WARNING: Do not insert code here. We modify the code for the delay (i.e. LDY value).
    LDY #$67          ; [2 cycles] Counter for a PAL C64 with "the dot" at 60Hz.
                      ;            TODO: Find the right number for all other combinations above.

delay_inner:        
    DEY               ; [2 cycles]
    BNE delay_inner   ; [3 cycles if taken, 2 if not]
    DEX               ; [2 cycles]
    BNE delay_outer   ; [3 cycles if taken, 2 if not]
                    
    LDY #$00          ; [2 cycles] Change border to black.
    STY $D020         ; [4 cycles]
    RTS               ; [6 cycles]



;==========================================================================================================
; Read next calibration byte from stream.
; The calibration bitstream contains alternating 0s and 1s.
; We parse a byte as 10 bits: 0.10101010.1
; We sync on the first 0 as a "start_bit".
; We check that the last 1 is an "end_bit".
; We check that the bits are alternating.
; On any error, we display a red "E" on screen, and try to re-sync to the next start_bit.

read_byte:
    ; Output a " " to separate this byte from the previous.
    LDA #' '
    JSR print_char  ; (trashes A)

    LDA #$09        ; Read 10 bits (start_bit + 9 others).
    STA $FB         ; Scratch[FB] = bits remaining

wait_0:             ; Wait until we read a 0 (start_bit).
    LDA #$07        ; Change border to yellow.
    STA $D020

    ; Check if user has pressed "+" or "-" to adjust the delay.
    LDA $DC05         ; Get Timer A high byte.
    AND #$E0
    CMP $D3           ; Compare to previous value read.
    BEQ no_plus_minus ; Enforce keypress delay.
    STA $D3
    JSR check_plus_minus
    CMP #$00
    BEQ no_plus_minus
    CLC
    ADC delay_outer + 1
    STA delay_outer + 1
    JSR print_A_hex
no_plus_minus:
    
    
    LDA $DD01       ; Read CIA2 data port B (User Port).
    AND #$80        ; Check port B bit 7.
    BEQ found_0     ; We found our start bit!
    
    ; If start bit not found, we can check if a key is pressed to interrupt the operation.
    JSR check_run_stop
    BNE wait_0      ; If a RUN/STOP is not pressed, continue waiting for a 0 (start bit).
    ; If we are here, then RUN/STOP has been pressed.
    ; Since we are in a first-level sub, we use a dirty trick to exit anyway.
    PLA             ; Remove return PC[LO] from stack.
    PLA             ; Remove return PC[HI] from stack. Now we are no more inside a subroutine ;-)
    JMP exit        ; Exit program.

found_0:
; This is where we synchronize sampling, i.e. the middle of the start_bit duration.

    LDX #$0F       ; Delay (15 loops == ~10 ms): Sync sampling the "center" of the start_bit.
    JSR delay      ; ^
    
    ; Now check that we are actually centered on the start bit.
    LDA $DD01      ; Read CIA2 data port B (User Port).
    ASL A          ; Set carry with port B bit 7.
    ; DEBUG: Print read bit on screen.
    JSR print_carry ; Output carry value on screen (trashes A).
    BCC start_bit_ok ; Check this is a zero start bit.
    ; ERROR: Start bit "half" is not zero.
    ; Output an extra "E" to signal the start bit error.
    LDA #$85       ; 'E' | $80 for red
    JSR print_char ; Output carry value on screen (trashes A).
    JMP read_byte  ; Resync on next 0...
    
start_bit_ok:
    DEC $FB        ; Decrement number of bits to read (now 9 more).
    LDX #$1E       ; Delay 20 ms to skip the start bit.
    JSR delay      ; ^

    ; Output a "." to signal the start of the byte.
    LDA #$2E
    JSR print_char ; Output carry value on screen (trashes A).
    
; Read a bit. Use A as the shift register for the byte being read.

    LDA #$00       ; Init byte to $00 (shouldn't be needed; cleaner debugging).

next_bit:
    PHA            ; Save A (shift register).
    LDA $DD01      ; Read CIA2 data port B (User Port).
    ASL A          ; Set carry with port B bit 7.
    
    ; DEBUG: Print read bit on screen.
    JSR print_carry ; Output carry value on screen (trashes A).
    
    PLA            ; Restore A (shift register).
    ROR A          ; Insert port B bit 7 in shift register bit 7.
    ; Now verify that this bit is the opposite of the last one read.
    TAX            ; Save A (shift register) to tmp
    AND #$c0       ; Mask last two read bits.
    BEQ bit_error  ; ERROR: Two adjacent zeros
    TXA            ; Restore A (shift register)
    EOR #$c0       ; Invert last two read bits.
    AND #$c0       ; Mask last two read bits.
    BEQ bit_error  ; ERROR: Two adjacent ones
    TXA            ; Restore A (shift register)
    JMP no_error   ; No error detected.
    
bit_error:
    ; Output an extra "E" to signal the start bit error.
    LDA #$85       ; 'E' | $80 for red
    JSR print_char ; Output carry value on screen (trashes A).
    JMP read_byte  ; Resync on next 0...

no_error:    
    LDX #$1E       ; Delay 20 ms to wait for the next bit.
    JSR delay      ; ^
    DEC $FB        ; Decrease loop counter.
    BNE next_bit   ; Fetch next bit...

; Byte read. Now we should be positioned on the first bit of the stop_sequence (i.e. the weird bit set to 0).
; First thing, save the read byte, so we can perform checks.
    PHA            ; Save read byte (should be 10101010)

    ; Output a "." to signal the end of the byte.
    LDA #'.'
    JSR print_char ; Output carry value on screen (trashes A).
    
; Now check that we are actually centered on a 1 bit (stop_bit).
    LDA $DD01      ; Read CIA2 data port B (User Port).
    ASL A          ; Set carry with port B bit 7.
    ; DEBUG: Print read bit on screen.
    JSR print_carry ; Output carry value on screen (trashes A).
    BCS done_reading_byte

    ; If we are here, then we found a stop bit error (i.e. not set to 1).
    ; Output an extra "E" to signal the stop bit error.
    LDA #$85       ; 'E' | $80 for red
    JSR print_char ; (trashes A).
    

done_reading_byte:
    PLA            ; Restore read byte.
    RTS            ; Done reading this byte.



;==========================================================================================================
; Check if RUN/STOP is pressed.
; Trashes A. If pressed, Z flag is set.
check_run_stop:
    LDA #%01111111  ; Selects column 7.
    STA $DC00       ; Write to CIA#1 Port A (column select).
    LDA $DC01       ; Read from CIA#1 Port B (row read).
    AND #%10000000  ; If bit 7 is reset, then key is pressed.
    RTS


;==========================================================================================================
; Check if "+" or "-" key is pressed.
; Return value is A: $00 = None pressed; $01 = "+" pressed; $FF = "-" pressed.
check_plus_minus:
    LDA #%11011111  ; Selects column 5.
    STA $DC00       ; Write to CIA#1 Port A (column select).
    LDA $DC01       ; Read from CIA#1 Port B (row read).
    AND #%00000001  ; If bit 0 is reset, then "+" key is pressed.
    BNE no_plus
    LDA #$01
    RTS
no_plus:
    LDA $DC01       ; Read from CIA#1 Port B (row read).
    AND #%00001000  ; If bit 3 is reset, then "-" key is pressed.
    BNE none
    LDA #$FF
    RTS
none:
    LDA #$00
    RTS


;==========================================================================================================
; Print carry status ("0" or "1").
; Preserves all registers and flags.
print_carry:
    PHA
    LDA #$30       ; "0"
    BCC was_zero
    LDA #$31       ; "1"
was_zero:   
    JSR print_char
    PLA
    RTS
    


;==========================================================================================================
; Print a character in A on screen.
; Set bit 7 to show it in red color, otherwise we use white.
; Preserves all registers and flags, but not A.
print_char:
    STA   $12       ; Save A (char to print).

    PHP   ; Save SR
    TYA
    PHA   ; Save Y
    
    LDA   $12       ; Restore A (char to print).
    
    LDY #$00
    AND #$7F         ; Remove color flag.
    STA ($AE),Y      ; Print char.

    ; Set color in Color Memory.
    LDA $AF
    CLC
    ADC #$D4      ; Point to Color Memory.
    STA $AF
    LDA $12       ; A = char to print. This will set the N flag if we need to use red color.
    BPL not_red
    AND #$7F
    SEC           ; Flag the use of red.
not_red:
    LDA #$01      ; White.
    BCC no_red
    LDA #$0A      ; .. or red.
no_red:
    STA ($AE),Y

    ; Point back to Screen Memory.
    LDA $AF
    SEC
    SBC #$D4      
    STA $AF

    ; Prepare ptr to next char.
    INC $AE
    BNE no_ovfl
    ; Ovfl.
    INC $AF
no_ovfl:
    ; Check for end of screen
    LDA $AF
    CMP #$07
    BNE end_print_char
    LDA $AE
    CMP #$E8
    BNE end_print_char
    ; At $07E8 we are out of screen. Reset ptr to beginning of screen.
    LDA #$00
    STA $AE
    LDA #$04
    STA $AF
    
end_print_char:

    PLA    ; Restore Y
    TAY    
    PLP    ; Restore SR
    RTS



;==========================================================================================================
; Print A as a hex number on the upper-left corner of the screen in YELLOW color.
; NOTE: Registers are preserved.

print_A_hex:
    
    PHA        ; Save A.
    PHA        ; Save A.
    
    LSR
    LSR
    LSR
    LSR
    CMP #$0A
    BCS pA_alpha_0
    ; Not alpha, i.e. [0..9]
    ADC #$30 + 9
pA_alpha_0:    
    SEC
    SBC #9
    STA $400

nxt_nibble:
    PLA        ; Restore A.
    AND #$0F
    CMP #$0A
    BCS pA_alpha_1
    ; Not alpha, i.e. [0..9]
    ADC #$30 + 9
pA_alpha_1:    
    SEC
    SBC #9
    STA $401
    
    ; Set color.
    LDA #$07   ; Yellow.
    STA $D800
    STA $D801
    
    PLA        ; Restore A.
    RTS


;==========================================================================================================    
; PRINT routine. Print zero-terminated string pointed to by ($12), max 255 chars.
; This routine trashes registers and changes $12.
print_str:
pr_ch:
    LDY #$00           ; Clear index.
    LDA ($12), Y       ; Load the next char from the message.
    BEQ pr_end         ; If character is 0 (end of string), jump to end
    JSR $FFD2          ; Call CHROUT ($FFD2) to print character
    INC $12            ; Increase char*.
    JMP pr_ch          ; Repeat the l oop
pr_end:
    RTS

;==========================================================================================================
* = $c800
; Strings
str_press_key:         .byte $93 ;Clear screen.      
                       .text $0D, "calibrate - version 2024-09-16.ddt", $0D, "any key to start...", $00
                       
str_reading:           .byte $93 ;Clear screen.
                       .text "waiting for stream...", $0D, "run/stop to break.", $00

keypress_dly:          .word $0000 ; Keypress delay counter.
