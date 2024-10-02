; 4 Computer Buffs - C64 receive program.
;
; Assembly source format: 64TASS (Turbo Assembler Macro).
;
; Revision history [authors in square brackets]:
;   1985-03-xx: Original version in "Personal Computer World" magazine.
;   2024-08-07: Fixed to actually write bytes read in memory. [nc513]
;   2024-08-15: Rev-eng comments. [DDT]
;   2024-08-25: Ported by DDT to C64tass assembler. [DDT]
;   2024-08-26: Added consistency checks, visual debugging info, and minimal UI. [DDT]
;   2024-08-27: Fixed async serial timings. [DDT]
;   2024-09-15: Improved visual debugging aids. [DDT]
;   2024-09-16: Support also NTSC C64 and non-PAL video sources. [DDT]
;   2024-10-01: Output debug timing on Use Port pin C. [DDT]
;   2024-10-02: Support autodetect of system frequency and selecting default timing for 50 Hz or 60 Hz source. [DDT]

; This code uses User Port line L (CIA2 port B bit 7) to receive data from the light sensor (hopefully calibrated).
;
; Let's take the first transmitted byte as an example:
;
;   0            11010000         011
;   ^start_bit   ^byte_payload    ^stop_sequence (NOTE: there might be more 1's at the end). Weirdly, the first stop_bit is 0.
;
; Each 0 or 1 bit is encoded in a single PAL field, with a duration of 20 ms (the light sensor hardware must be calibrated for the same bit duration).
; PROBLEM: Interrupts are not disabled, so there could be jitter introduced by the IRQ service routine (60 Hz).
;
; DECODE_BYTE ALGORITHM (described for PAL input timings):
; - Wait for the start_bit (0), then wait 10 ms to center the sampling interval in the middle of the start_bit duration. This is in order to minimize errors due to jitter.
; - Read 9 bits: I.e. the start_code bit and the 8 payload bits. After each read bit, wait 20ms to center to the next bit.
; - At the end of the payload, the decoder is positioned in the middle of the first bit of the stop_sequence, which for obscure reasons is always set to 0 instead of 1 like any respectable stop bit(s).
; - The decoder then waits another 20 ms to skip that zero "stop_bit", so we are in the middle of the second bit of the stop_sequence.
; - If the previous step fails (i.e. we are short on timing), then we'll be still reading the zero "stop_bit" and it will be mistaken as the start of a new byte, hence a corrupt stream.
; - GOTO DECODE_BYTE

* = $C000      ; Set start address of the program (standard BASIC start address)

; Target (overwrite) BASIC program at current BASIC program start address

    LDA $2B        ; Start of BASIC program text [LO].
    STA $FC        ; Save next write offset [LO] to scratch.
    LDA $2C        ; Start of BASIC program text [HI].
    STA $FD        ; Save next write offset [HI] to scratch.

; Init data input from user port

    LDA #$7F
    STA $DD03      ; Set CIA2 port B bit 7 as input (receive).
                   ; All other bits as output. We can use bit 0 to check/debug the delay.
    
; Change background color for better contrast.
    LDA #$00
    STA $D021

; Print "PRESS KEY".
    LDA #<str_press_key
    STA $12
    LDA #>str_press_key
    STA $13
    JSR print_str
    
; Wait for selection of default timing.
wait_key:
    JSR $FFE4      ; Call Kernal GETIN
    CMP #$00
    BEQ wait_key   ; Wait for key...
    CMP #$31       ; Check if "1" is pressed (50 Hz source)
    BEQ src_50
    CMP #$32       ; Check if "2" is pressed (60 Hz source)
    BEQ src_60
    JMP wait_key   ; Wrong choice. Select again...

    ; Auto timing for 50 Hz source.
src_50:
    LDX #$7B       ; NTSC C64 with 50 Hz source.
    LDA $02A6
    BEQ set_src_50 ; Branch if NTSC C64.
    LDX #$78       ; PAL C64 with 50 Hz source.
set_src_50:
    STX delay_outer + 1
    JMP start_reading

    ; Auto timing for 60 Hz source.
src_60:
    LDX #$78       ; NTSC C64 with 60 Hz source.
    LDA $02A6
    BEQ set_src_60 ; Branch if NTSC C64.
    LDX #$66       ; PAL C64 with 60 Hz source.
set_src_60:
    STX delay_outer + 1    


; Print "READING".
start_reading:
    LDA #<str_reading
    STA $12
    LDA #>str_reading
    STA $13
    JSR print_str

print_delay:
    ; Print default delay value.
    LDA delay_outer + 1
    JSR print_A_hex


; DISABLE INTERRUPTS HERE
    SEI

; Steal two system zeropage addresses to use as ptr to screen memory. This is ok, kernal won't be offended.
    LDA #$00
    STA $AE
    LDA #$04
    STA $AF

; Use address $13 to debug:
;   Bit 0 is flipped and output to user port pin C (CIA2 PB0) each time the delay routing is called.
    LDA #$00
    STA $13

; START OF STREAM - NOTE: We expect a C64 BASIC program in binary format

; Read the next BASIC line in binary format
nxt_line:
    JSR read_byte       ; Read next byte from stream (i.e. pointer to next BASIC line LO).
    JSR write_byte      ; Write it into the BASIC program area.
    STA $FE             ; Save it into Scratch[FE] i.e. "previous byte read".
    
    JSR read_byte       ; Read next byte from stream (i.e. pointer to next BASIC line HI).
    JSR write_byte      ; Write it into the BASIC program area.
    CMP #$00            ; Is byte $00 ?
    BNE skip_eop_check  ; If not, skip end of program check.

; This is executed if next_line_ptr HI is zero, so we can check if also next_line_ptr LO is zero (i.e. end of BASIC program).

    LDA $FE             ; If "previous byte read" was also $00, then we found the END of BASIC program.
    BEQ end_of_stream   ; If it was $00, then we found the END OF STREAM.

skip_eop_check:

; Fetch BASIC line number

    JSR read_byte       ; Read next byte from stream.
    JSR write_byte      ; Write it into the BASIC program area.
    JSR read_byte       ; Read next byte from stream.
    JSR write_byte      ; Write it into the BASIC program area.

; Fetch remainder of BASIC line (0 terminated).
nxt_line_byte:
    JSR read_byte       ; Read next byte from stream.
    JSR write_byte      ; Write it into the BASIC program area.
    CMP #$00            ; Is byte $00 ?
    BNE nxt_line_byte   ; Not yet EOL, read another byte.
    JMP nxt_line        ; EOL, fetch next BASIC line...


; END OF STREAM: Write BASIC program end address to system variables
end_of_stream:
    LDA $FC             ; Retrieve next write offset [LO].
    STA $2D             ; End of BASIC program text +1 [LO].
    LDA $FD             ; Retrieve next write offset [HI].
    STA $2E             ; End of BASIC program text +1 [HI].


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
    CLC            ; We want to SET the position.
    LDX #0        ; Cursor to first char of line.
    STX $D3       ; Current cursor column.
    STY $D6       ; Current cursor row.

    CLI            ; ENABLE INTERRUPTS
    RTS            ; EXIT THIS PROGRAM.



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
;           8+19 +   (X * (2 + Y*5 - 1 + 5)) - 1   + 12
;
; IMPORTANT: The 6510 CPU cannot use all cycles in a frame due to VIC-II memory access priority (bad lines et cetera).
; This routine preserves A.


delay:
    ; Change border to red. [8 cycles]
    LDY #$02          ; [2 cycles]
    STY $D020         ; [6 cycles]

    ; Swap debug delay bit and output on User Port pin C. [19 cycles]
    PHA               ; [3 cycles]
    LDA #$01          ; [2 cycles]
    EOR $13           ; [3 cycles]
    STA $13           ; [3 cycles]
    STA $DD01         ; [4 cycles]
    PLA               ; [4 cycles]

                    
delay_outer:        
    ; WARNING: Do not insert code here. We modify the code for the delay (i.e. LDY value).
    ;LDY #$66          ; [2 cycles] Counter for a PAL C64 with "the dot" at 60Hz.
    LDY #$78          ; [2 cycles] Counter for a PAL C64 with "the dot" at 50Hz.
    ;LDY #$78          ; [2 cycles] Counter for a NTSC C64 with "the dot" at 60Hz.
    ;LDY #$7B          ; [2 cycles] Counter for a NTSC C64 with "the dot" at 50Hz.


delay_inner:        
    DEY               ; [2 cycles]
    BNE delay_inner   ; [3 cycles if taken, 2 if not]
    DEX               ; [2 cycles]
    BNE delay_outer   ; [3 cycles if taken, 2 if not]
                    
    LDY #$00          ; [2 cycles] Change border to black.
    STY $D020         ; [4 cycles]
    RTS               ; [6 cycles]

; Read next byte from stream.
; Each byte is sent as:
;    - 1 bit set to 0 (start_bit).
;    - 8 payload bits (LSb first). These are the byte to receive.
;    - 3 or more bits of a weird stop_sequence: A 0 and usually two (but sometimes more) 1's.
; NOTE: Here we read also the start bit which will be shifted out after reading the other 8 payload bits. No problem.
;       The least significant bit is read first (we shift right).
read_byte:

    LDA #$08        ; Read 8 bits of payload.
    STA $FB         ; Scratch[FB] = 8 (loop counter).

wait_0:             ; Wait until we read a 0 (start_bit).
    LDA #$07        ; Change border to yellow.
    STA $D020
    
    JSR handle_userinput
    
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
; This is done to minimize the probability of errors caused by jitter.

    LDX #$0F       ; Delay (15 loops == ~10 ms): Sync sampling the "center" of the start_bit.
    JSR delay      ; ^
    
    ; Now check that we are actually centered on the start bit.
    LDA $DD01      ; Read CIA2 data port B (User Port).
    ASL A          ; Set carry with port B bit 7.
    ; DEBUG: Print read bit on screen.
    JSR print_carry ; Output carry value on screen (trashes A).
    BCC start_bit_ok ; Check this is a zero start bit.
    ; If not zero, then output an extra "E" to signal the start bit error.
    LDA #$85       ; 'E' | $80 for red
    JSR print_char ; Output carry value on screen (trashes A).
    
start_bit_ok:
    LDX #$1E       ; Delay 20 ms to skip the start bit.
    JSR delay      ; ^

    ; Output a "." to signal the start of the byte.
    LDA #$2E
    JSR print_char ; Output carry value on screen (trashes A).
    
; Read a bit. Use A as the shift register for the byte being read.

    LDA #$00       ; Init byte to $00 (shouldn't be needed; cleaner debugging ?).

next_bit:
    PHA            ; Save A (shift register).
    LDA $DD01      ; Read CIA2 data port B (User Port).
    ASL A          ; Set carry with port B bit 7.
    
    ; DEBUG: Print read bit on screen.
    JSR print_carry ; Output carry value on screen (trashes A).
    
    PLA            ; Restore A (shift register).
    ROR A          ; Insert port B bit 7 in shift register bit 7.
    LDX #$1E       ; Delay 20 ms to wait for the next bit.
    JSR delay      ; ^
    DEC $FB        ; Decrease loop counter.
    BNE next_bit   ; Fetch next bit...

; Byte read. Now we should be positioned on the first bit of the stop_sequence (i.e. the weird bit set to 0).
; First thing, save the read byte, so we can perform checks.
    PHA            ; Save read byte.

    ; Output a "." to signal the end of the byte.
    LDA #'.'
    JSR print_char ; Output carry value on screen (trashes A).


; NOTE: We need to skip this 0 bit now, because... well, it's 0 and not 1 as it should be.
;       Note that any other trailing bits set to 1 in the stop_sequence will be skipped automatically while searching for the start_bit of the next byte.

    ; Now check that we are actually centered on the weird 0 bit.
    LDA $DD01      ; Read CIA2 data port B (User Port).
    ASL A          ; Set carry with port B bit 7.
    ; DEBUG: Print read bit on screen.
    JSR print_carry ; Output carry value (the bit we just read) on screen (trashes A, does not modify carry).

    LDX #$1E       ; Delay 20 ms to skip the weird 0 bit of the stop sequence.
    JSR delay      ; ^
    
; Now check that we are actually centered on a 1 bit.
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
    ; Output a " " to separate this byte from the next.
    LDA #' '
    JSR print_char ; (trashes A)
     
    PLA            ; Restore read byte.
    RTS            ; Done reading this byte.


;==========================================================================================================
; Write next byte of BASIC program and inc offset.
write_byte:
    LDY #$00
    STA ($FC),Y
    INC $FC        ; Inc next write offset [LO].
    BNE skip_hi
    INC $FD        ; Inc next write offset [HI].
skip_hi:
    RTS


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
; Handle user input.
handle_userinput:
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
    STA $12       ; Save A (char to print).

    PHP   ; Save SR
    TYA
    PHA   ; Save Y
    
    LDA $12       ; Restore A (char to print).
    
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
                       .text $0D, "decoder - version 2024-10-02.ddt", $0D
                       .text "To start press:", $0D
                       .text "  1: decode 50 hz signal", $0D
                       .text "  2: decode 60 hz signal", $0D
                       .text $00
                       
str_reading:           .byte $93 ;Clear screen.
                       .text $0D, "waiting for stream...", $0D, "run/stop to break.", $00
