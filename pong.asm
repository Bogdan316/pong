.segment "HEADER"
; used for telling the emulator the info about the ROM
    .byte "NES"
    .byte $1a
    .byte $02 ; 2 * 16KB PRG ROM
    .byte $01 ; 1 * 8KB CHR ROM
    .byte %00000000
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00,$00,$00,$00,$00 ; filler bytes

.segment "STARTUP"
PPUCTRL      = $2000 ; used for controlling the PPU
PPUMASK      = $2001 ; used for controlling the rendering of sprites and colors in the PPU
PPUSTATUS    = $2002 ; used for reading the PPU status
PPUADDR      = $2006 ; tells the PPU at which address to write
PPUDATA      = $2007 ; tells the PPU what to write starting at the address given to the PPUADDR
PPUSCROLL    = $2005 ; controls the scrolling
OAMDMA       = $4014 ; tells the PPU from where to read the sprites

CTRLENABLE   = $4016 ; controls the controller latch
CTRLPLAYER1  = $4016 ; left shift register, outputs the status of the controller buttons, 1 for pressed 0 otherwise
CTRLPLAYER2  = $4017 ; left shift register, outputs the status of the controller buttons, 1 for pressed 0 otherwise

APU          = $4017 ; used for addressing the APU
DMC          = $4010 ; used for controlling the IRQ interrupt when playing sounds

; screen boundaries
RIGHTWALL      = $F4 
TOPWALL        = $0A
BOTTOMWALL     = $E0
LEFTWALL       = $04

LPADDLESTART  = $0200 ; the address at which the left paddle sprites start
RPADDLESTART  = $0214 ; the address at which the right paddle sprites start
PADDLESPEED   = $06   ; how many pixels the paddle moves at a time
BALLSPEED     = $03   ; how many pixels the ball moves at a time
BALLY         = $0228 ; the Y coord of the ball
BALLX         = $022B ; the X coord of the ball

PLAYER1SCORE  = $022C ; the start of the sprites used for displaying player one's score
PLAYER2SCORE  = $0264 ; the start of the sprites used for displaying player two's score

.segment "ZEROPAGE"
    buttons:          .res 1 ; reserve 1 byte for storing the buttons
    boundary:         .res 1 ; used to store temporary values
    background:       .res 2 ; reserve 2 bytes to store the background data address
    crt_player:       .res 2 ; reserve 2 bytes to hold the current player register address
    crt_paddle:       .res 2 ; reserve 2 bytes to hold the register address of the paddle that is being moved
    ball_x:           .res 1 ; reserve 1 byte to hold the direction the ball is going on the x axis
    ball_y:           .res 1 ; reserve 1 byte to hold the direction the ball is going on the y axis
    tmp:              .res 1 ; variable used for temporary storage
    player1_score:    .res 1 ; holds the value of player one's score
    player2_score:    .res 1 ; holds the value of player two's score
    crt_player_score: .res 1 ; holds the value of the score that is being updated
    player_score:     .res 2 ; used for looping over the sprites of the score display
    score_addr:       .res 2 ; stores the beginning address of the score's sprites that is being updated
    game_state:       .res 1 ; stores the current state of the game
.segment "CODE"

; ! ||--------------------------------------------------------------------------------||
; ! ||                                LOAD TITLE SCREEN                               ||
; ! ||--------------------------------------------------------------------------------||

title_screen_start:
; it runs at the start and every time the reset button is pressed
; first game state, loads the title screen background and waits for the start button to be pressed
    SEI ; disables all interrupts
    CLD ; disable decimal mode, it is not supported by the NES

    ; Disable sound IRQ
    LDX #$40
    STX APU

    ; initialize the stack register
    LDX #$FF
    TXS

    INX ; #$FF + 1 => #$00

    STX PPUCTRL ; zero out the PPU control register (disables NMI)
    STX PPUMASK ; zero out the PPU rendering register

    ; disable DMC IRQs
    STX DMC
    STX game_state  ; initialize the game state

    ; ensure that everything has been reset
    JSR vblank_wait

clrmem:
    ; clear the 2KB of RAM
    LDA #$00

    STA $0000, X
    STA $0100, X
    STA $0300, X
    STA $0400, X
    STA $0500, X
    STA $0600, X
    STA $0700, X
    ; the 0200 -> 02FF is reserved for sprites
    LDA #$FE ; move all sprites offscreen
    STA $0200, X

    INX ; increment until X becomes 0 again
    BNE clrmem

    ; ensure that the memory has been reset
    JSR vblank_wait

load_palettes:
; loads palette data into the PPU starting at address 3F00
    BIT PPUSTATUS ; reset PPU latch
    LDA #$3F
    STA PPUADDR
    LDA #$00
    STA PPUADDR

    LDX #$00
load_palettes_loop:
; write palette data to PPU
    LDA PaletteData, X
    STA PPUDATA
    INX
    CPX #$20
    BNE load_palettes_loop

    ; load the title screen background into the third nametable
    BIT PPUSTATUS
    LDA #$28
    STA PPUADDR
    LDA #$00
    STA PPUADDR

    LDA #<TitleScreen
    STA background
    LDA #>TitleScreen
    STA background+1
    JSR load_background

    LDA #%10010010   ; enable NMI, sprites and background from Pattern Table 0, use third named table
    STA PPUCTRL

    LDA #%00011110   ; enable sprites
    STA PPUMASK

    LDA #$00 ; tells the PPU that there is only 1 screen
    STA PPUSCROLL
    STA PPUSCROLL

; ! ||--------------------------------------------------------------------------------||
; ! ||                                TITLE SCREEN LOOP                               ||
; ! ||--------------------------------------------------------------------------------||

title_screen:
; wait for the start button to be pressed
    LDA #$16
    STA crt_player ; switch to first player by changing the low byte
    LDA #$40
    STA crt_player+1

    JSR read_controller
    LDA buttons
    AND #%00010000 ; check if the start button is pressed

    BEQ title_screen_done
    INC game_state ; if the start button is pressed, move to the next game state
title_screen_done:

:
    ; stop the code execution from going any further
    JMP :-

; ! ||--------------------------------------------------------------------------------||
; ! ||                              LOAD GAME ASSETS                                  ||
; ! ||--------------------------------------------------------------------------------||
 
load_game_background:
    SEI ; disables all interrupts
    CLD ; disable decimal mode, it is not supported by the NES

    ; Disable sound IRQ
    LDX #$40
    STX APU

    ; initialize the stack register
    LDX #$FF
    TXS

    INX ; #$FF + 1 => #$00

    STX PPUCTRL ; zero out the PPU control register (disables NMI)
    STX PPUMASK ; zero out the PPU rendering register

    ; disable DMC IRQs
    STX DMC

    ; ensure that everything has been reset
    JSR vblank_wait

    ; load the game's background into the first nametable
    BIT PPUSTATUS
    LDA #$20
    STA PPUADDR
    LDA #$00
    STA PPUADDR

    LDA #<BackgroundData
    STA background
    LDA #>BackgroundData
    STA background+1
    JSR load_background

load_sprites:
; loads sprites data in RAM starting at 0200
    LDX #$00
load_sprites_loop:
    LDA Sprites, X
    STA $0200, X
    INX
    CPX #$9C
    BNE load_sprites_loop

    INC game_state

    LDA #$40
    STA crt_player+1 ; load the high byte of the player's controller address
                     ; which is identical for both players

    LDA #$00
    STA ball_y
    STA ball_x

    STA player1_score
    STA player2_score
    
    JSR vblank_wait
    LDA #%10000000   ; enable NMI, sprites and background from Pattern Table 0
    STA PPUCTRL

    LDA #%00011110   ; enable sprites
    STA PPUMASK

    LDA #$00 ; tells the PPU that there is only 1 screen
    STA PPUSCROLL
    STA PPUSCROLL

:
; stop the code execution from going any further
    JMP :-

; ! ||--------------------------------------------------------------------------------||
; ! ||                                    GAME LOOP                                   ||
; ! ||--------------------------------------------------------------------------------||

gameplay_loop:
    LDA #$02
    STA OAMDMA ; tells the PPU to load the sprite data starting from the 0200 RAM address

    LDA #$16
    STA crt_player ; switch to first player by changing the low byte

    JSR read_controller

    ; load into crt_paddle the address of the left paddle
    LDA #<LPADDLESTART
    STA crt_paddle
    LDA #>LPADDLESTART
    STA crt_paddle+1

    JSR move_down
    JSR move_up

    LDA #$17
    STA crt_player ; switch to second player by changing the low byte

    JSR read_controller

    ; load into crt_paddle the address of the right paddle
    LDA #<RPADDLESTART
    STA crt_paddle
    LDA #>RPADDLESTART
    STA crt_paddle+1

    JSR move_down
    JSR move_up

    JSR check_ball_collisions
    JSR check_lpaddle_collision
    JSR check_rpaddle_collision
    JSR move_ball_x
    JSR move_ball_y

    LDA player1_score
    CMP #$0A
    BNE:+
    INC game_state
:

    LDA player2_score
    CMP #$0A
    BNE:+
    INC game_state
:

    LDA #%10000000   ; enable NMI, sprites and background from Pattern Table 0
    STA PPUCTRL

    LDA #%00011110   ; enable sprites
    STA PPUMASK

    LDA #$00 ; tells the PPU that there is only 1 screen
    STA PPUSCROLL
    STA PPUSCROLL
    RTS

; ! ||--------------------------------------------------------------------------------||
; ! ||                                 LOAD END SCREEN                                ||
; ! ||--------------------------------------------------------------------------------||

load_end_screen:
    SEI ; disables all interrupts
    CLD ; disable decimal mode, it is not supported by the NES

    ; Disable sound IRQ
    LDX #$40
    STX APU

    ; Intialize the stack register
    LDX #$FF
    TXS

    INX ; #$FF + 1 => #$00

    STX PPUCTRL ; zero out the PPU control register (disables NMI)
    STX PPUMASK ; zero out the PPU rendering register

    ; disable DMC IRQs
    STX DMC

    ; ensure that everything has been reset
    JSR vblank_wait
    
move_sprites:
    ; the 0200 -> 02FF is reserved for sprites
    LDA #$FE ; move all sprites offscreen
    STA $0200, X

    INX ; increment until X becomes 0 again
    BNE move_sprites

    ; select the winner message based on which player got first
    ; at 10
    LDA player1_score
    CMP #$0A
    BNE:+
    LDX #$00
    LDA #$2C
    STA tmp
:

    LDA player2_score
    CMP #$0A
    BNE:+
    LDX #$2C
    LDA #$58
    STA tmp
:

load_winner_sprites_loop:
    LDA EndScreenSprites, X
    STA $0200, X
    INX
    CPX tmp
    BNE load_winner_sprites_loop
    
    INC game_state

    JSR vblank_wait
    LDA #%10001000   ; enable NMI, sprites from Pattern Table 1 and background from Pattern Table 0
    STA PPUCTRL

    LDA #%00011110   ; enable sprites
    STA PPUMASK

    LDA #$00 ; tells the PPU that there is only 1 screen
    STA PPUSCROLL
    STA PPUSCROLL

:
; stop the code execution from going any further
    JMP :-

; ! ||--------------------------------------------------------------------------------||
; ! ||                                 END SCREEN LOOP                                ||
; ! ||--------------------------------------------------------------------------------||

end_screen_loop:
    LDA #$02
    STA OAMDMA ; tells the PPU to load the sprite data starting from the 0200 RAM address

    LDA #%10001000   ; enable NMI, sprites from Pattern Table 1 and background from Pattern Table 0
    STA PPUCTRL

    LDA #%00011110   ; enable sprites
    STA PPUMASK

    LDA #$00 ; tells the PPU that there is only 1 screen
    STA PPUSCROLL
    STA PPUSCROLL
    RTS

; ! ||--------------------------------------------------------------------------------||
; ! ||                                   SCREEN NMI                                   ||
; ! ||--------------------------------------------------------------------------------||

update_screen:
; updates the info on screen every vblank
    LDA game_state
    BNE:+
    JMP title_screen
:
    CMP #$01
    BNE:+
    JMP load_game_background
:
    CMP #$02
    BNE:+
    JSR gameplay_loop
:
    CMP #$03
    BNE:+
    JMP load_end_screen
:
    CMP #$04
    BNE:+
    JSR end_screen_loop
:
    RTI

; ! ||--------------------------------------------------------------------------------||
; ! ||                              SUBROUTINES FOR INIT                              ||
; ! ||--------------------------------------------------------------------------------||

vblank_wait:
; wait for vblank
    BIT PPUSTATUS ; set the N flag equal to the 7th bit of PPUSTATUS
                  ; if it is set then we are in a vblank
    BPL vblank_wait
    RTS

load_background:
    LDX #$00
    LDY #$00
load_background_loop:
; fills the given nametable with $02C0 with the background data
; to be able to count to that value we need to use both X and Y registers and
; a nested loop
    LDA (background), Y
    STA PPUDATA
    INY
    CPX #$03
    BNE:+
    CPY #$C0
    BEQ done_loading_background
:
    CPY #$00 
    BNE load_background_loop
    INX         ; if Y is 0 then we loaded 256 bytes, increment X
    INC background+1
    JMP load_background_loop
done_loading_background:
    RTS

; ! ||--------------------------------------------------------------------------------||
; ! ||                              PADDLE MOVEMENT LOGIC                             ||
; ! ||--------------------------------------------------------------------------------||

read_controller:
; reads the status of the controller's buttons into the buttons variable
    ; enable reading from controllers
    LDA #$01
    STA CTRLENABLE
    LDA #$00
    STA CTRLENABLE
    
    STA buttons

    LDX #$08
read_controller_loop:
    ; reads one bite for each button in the order:
    ; A B Select Start Up Down Left Right (the order in buttons variable will start from the right)
    ; and inserts that into the buttons variable
    LDY #$00
    LDA (crt_player), Y
    LSR A
    ROL buttons
    DEX
    BNE read_controller_loop
    RTS

move_down:
    LDA buttons
    AND #%00000100
    BEQ move_down_done

    ; check that the paddle position does not exceed the bottom boundary
    LDA #BOTTOMWALL;
    LDY #$10
    CMP (crt_paddle), Y ; get the Y coord of the bottom sprite
    BCC move_down_done ; the carry flag is set if #BOTTOMWALL >= #RPADDLESTART+16

    LDX #$00
    LDY #$00 ; sprite's Y position is in the 3rd byte
move_down_loop:
    LDA (crt_paddle), Y
    CLC
    ADC #PADDLESPEED ; subtracts the paddle speed value from the Y coord 
    STA (crt_paddle), Y

    TYA
    CLC
    ADC #$04 ; each sprites takes 4 bytes, move Y to point to the next sprite's X position
    TAY

    INX
    CPX #$05
    BNE move_down_loop
move_down_done:
    RTS

move_up:
    LDA buttons
    AND #%00001000
    BEQ move_up_done

    ; check that the paddle position does not exceed the top boundary 
    LDY #$00
    LDA (crt_paddle), Y
    LDX #TOPWALL
    STX boundary
    CMP boundary 
    BCC move_up_done ; the carry flag is set if #LPADDLESTART >= #TOPWALL

    LDX #$00
    LDY #$00 ; sprite's Y position is in the 1st byte
move_up_loop:
    LDA (crt_paddle), Y
    SEC
    SBC #PADDLESPEED ; subtracts the paddle speed value from the Y coord 
    STA (crt_paddle), Y
    
    TYA
    CLC
    ADC #$04 ; each sprites takes 4 bytes, move Y to point to the next sprite's Y position
    TAY

    INX
    CPX #$05
    BNE move_up_loop
move_up_done:
    RTS

; ! ||--------------------------------------------------------------------------------||
; ! ||                               BALL MOVEMENT LOGIC                              ||
; ! ||--------------------------------------------------------------------------------||

move_ball_y:
    LDA ball_y
    AND #01
    BEQ :+
    LDA BALLY
    SEC
    SBC #BALLSPEED
    STA BALLY
    RTS
:
    LDA BALLY
    CLC
    ADC #BALLSPEED
    STA BALLY
    RTS

move_ball_x:
    LDA ball_x
    AND #01
    BEQ :+
    LDA BALLX
    CLC
    ADC #BALLSPEED
    STA BALLX
    RTS
:
    LDA BALLX
    SEC
    SBC #BALLSPEED
    STA BALLX
    RTS

; ! ||--------------------------------------------------------------------------------||
; ! ||                              BALL COLLISION LOGIC                              ||
; ! ||--------------------------------------------------------------------------------||

check_ball_collisions:
    LDA BALLY
    CMP #BOTTOMWALL ; if BALLY >= BOTTOMWALL the carry bit is set
    BCC:+           ; check if the carry bit is clear
    LDA ball_y
    EOR #$01
    STA ball_y     ; complement the ball_y variable to change the direction of the y movement
    RTS
:
    STA tmp
    LDA #TOPWALL
    CMP tmp        ; if TOPWALL >= BALLY the carry bit is set
    BCC:+         
    LDA ball_y
    EOR #$01
    STA ball_y
    RTS
:
    LDA BALLX
    CMP #LEFTWALL  ; if BALLX >= LEFTWALL the carry bit is set
    BCS:+          ; check if the carry bit is set
    ; reset ball if it touches the left wall
    LDA #$80
    STA BALLY
    LDA #$7C
    STA BALLX
    ; make the ball move to player 2
    LDA #$00
    STA ball_y
    LDA #$01
    STA ball_x

    ; store the address of the score sprites into score_addr
    LDA #<PLAYER2SCORE
    STA score_addr
    LDA #>PLAYER2SCORE
    STA score_addr+1

    ; store the address of the score sprites into crt_player_score
    INC player2_score ; update player score
    LDA player2_score

    STA crt_player_score
    JSR show_player_score

    RTS
:
    STA tmp
    LDA #RIGHTWALL
    CMP tmp
    BCS:+
    ; reset ball if it touches the right wall
    LDA #$80
    STA BALLY
    LDA #$7C
    STA BALLX
    ; make the ball move to player 1
    LDA #$00
    STA ball_y
    LDA #$00
    STA ball_x

    LDA #<PLAYER1SCORE
    STA score_addr
    LDA #>PLAYER1SCORE
    STA score_addr+1

    INC player1_score
    LDA player1_score
    STA crt_player_score
    JSR show_player_score

    RTS
:
    RTS

check_lpaddle_collision:
    LDX #$00
    LDA ball_x
    AND #$01  ; check that the ball is not leaving the paddle after a collision
    BNE check_lpaddle_collision_done

    ; check if the ball's X position passed the lpaddle X position 
    LDA LPADDLESTART+3
    CLC
    ADC #$09
    CMP BALLX
    BCC check_lpaddle_collision_done

    LDA BALLY
    CLC
    ADC #$04 ; the ball is 5x5 pixels, add 4 to the ball Y coord to 
             ; get one pixel away from the bottom and use the new Y coord
             ; to test for collisions 
    CMP LPADDLESTART
    BCC check_lpaddle_collision_done

    LDA LPADDLESTART+16
    CLC
    ADC #$08   ; add 8 to get the Y coord of the bottom of the paddle
    CMP BALLY
    BCC check_lpaddle_collision_done

    ; if the ball's Y coord is <= the bottom of the paddle and >= than the top
    ; then a collision happens
    LDA ball_x
    EOR #$01       ; complement the ball_x variable to change the direction of the x movement
    STA ball_x
check_lpaddle_collision_done:
    RTS

check_rpaddle_collision:
    LDX #$00
    LDA ball_x
    AND #$01  ; check that the ball is not leaving the paddle after a collision
    BEQ check_rpaddle_collision_done

    ; check if the ball's X position passed the rpaddle X position 
    LDA BALLX
    CLC
    ADC #$05 ; add more pixels to the X position of the ball to account for the sprite's width
    CMP RPADDLESTART+3
    BCC check_rpaddle_collision_done

    LDA BALLY
    CLC
    ADC #$04 ; the ball is 5x5 pixels, add 4 to the ball Y coord to 
             ; get one pixel away from the bottom and use the new Y coord
             ; to test for collisions 
    CMP RPADDLESTART
    BCC check_rpaddle_collision_done

    LDA RPADDLESTART+16
    CLC
    ADC #$08   ; add 8 to get the Y coord of the bottom of the paddle
    CMP BALLY
    BCC check_rpaddle_collision_done

    ; if the ball's Y coord is <= the bottom of the paddle and >= than the top
    ; then a collision happens
    LDA ball_x
    EOR #$01       ; complement the ball_x variable to change the direction of the x movement
    STA ball_x
check_rpaddle_collision_done:
    RTS

; ! ||--------------------------------------------------------------------------------||
; ! ||                               DISPLAY SCORE LOGIC                              ||
; ! ||--------------------------------------------------------------------------------||

show_player_score:
; the score is displayed in a similar way to an 8 segment display 
    CMP #$01
    BNE:+
    JSR show_zero
    JSR show_one
    RTS
:
    CMP #$02
    BNE:+
    JSR show_one
    JSR show_two
    RTS
:
    CMP #$03
    BNE:+
    JSR show_two
    JSR show_three
    RTS
:
    CMP #$04
    BNE:+
    JSR show_three
    JSR show_four
    RTS
:
    CMP #$05
    BNE:+
    JSR show_four
    JSR show_five
    RTS
:
    CMP #$06
    BNE:+
    JSR show_five
    JSR show_six
    RTS
:
    CMP #$07
    BNE:+
    JSR show_six
    JSR show_seven
    RTS
:
    CMP #$07
    BNE:+
    JSR show_six
    JSR show_seven
    RTS
:
    CMP #$08
    BNE:+
    JSR show_seven
    JSR show_eight
    RTS
:
    CMP #$09
    BNE:+
    JSR show_eight
    JSR show_nine
    RTS
:
    RTS

show_one:
    JSR switch_top_bar
    JSR switch_middle_bar
    JSR switch_bottom_bar
    JSR switch_first_left_bar
    JSR switch_second_left_bar
    RTS

show_two:
    JSR switch_first_left_bar
    JSR switch_second_right_bar
    RTS

show_three:
    JSR switch_first_left_bar
    JSR switch_second_left_bar
    RTS

show_four:
    JSR switch_top_bar
    JSR switch_bottom_bar
    JSR switch_second_left_bar
    RTS

show_five:
    JSR switch_first_right_bar
    JSR switch_second_left_bar
    RTS

show_six:
    JSR switch_first_right_bar
    RTS

show_seven:
    JSR switch_middle_bar
    JSR switch_bottom_bar
    JSR switch_first_left_bar
    JSR switch_second_left_bar
    RTS

show_eight:
    RTS

show_nine:
    JSR switch_second_left_bar
    RTS

show_zero:
    JSR switch_middle_bar
    RTS

switch_first_left_bar:
    LDA #$00
    STA tmp
    JSR switch_score_bar
    RTS

switch_second_left_bar:
    LDA #$08 ; tells where the second left bar starts
    STA tmp
    JSR switch_score_bar
    RTS

switch_first_right_bar:
    LDA #$10
    STA tmp
    JSR switch_score_bar
    RTS

switch_second_right_bar:
    LDA #$18
    STA tmp
    JSR switch_score_bar
    RTS

switch_top_bar:
    LDA #$20
    STA tmp
    JSR switch_score_bar
    RTS

switch_middle_bar:
    LDA #$28
    STA tmp
    JSR switch_score_bar
    RTS

switch_bottom_bar:
    LDA #$30
    STA tmp
    JSR switch_score_bar
    RTS

switch_score_bar:
    ; this procedure can be reused because every score bar uses 2 sprites
    ; store the address of where the score bar starts
    ; into the player_score variable
    LDA score_addr ; get the low byte of the first sprite of the score display 
    CLC
    ADC tmp   ; add a multiple of 2*4 bytes to jump over the sprites before
    STA player_score

    LDA score_addr+1 ; get the high byte of the first sprite of the score diplay 
    ADC #$00    ; add 0 + C if the carry was set in the previous addition
    STA player_score+1

    LDY #$02 ; access the third byte of the sprite which stores the theme of the sprite
switch_score_bar_loop:
    ; loop over the two sprites (4 bytes each) that form the score bar and
    ; change their themes
    LDA (player_score), Y
    EOR #$03    ; flip the last two bits of the byte that hold the number of the theme used
                ; switching between the first (00 -> ON) and the last theme (11 -> OFF) 
    STA (player_score), Y

    TYA
    CLC
    ADC #$04    ; add 4 to move 4 bytes to the next sprite
    TAY

    CPY #$0A    ; 2 * 4 bytes for each sprite + 2 for the byte that controls the theme
    BNE switch_score_bar_loop
    RTS  

; ! ||--------------------------------------------------------------------------------||
; ! ||                                  ASSETS DATA                                   ||
; ! ||--------------------------------------------------------------------------------||

PaletteData:
    .byte $0F,$30,$30,$00,  $0F,$05,$26,$16,    $0F,$30,$21,$0F,    $0F,$27,$18,$0C  ;background palette data
    .byte $0F,$00,$10,$30,  $0F,$1A,$30,$27,    $0F,$16,$30,$27,    $0F,$0F,$0F,$0F  ;sprite palette data

Sprites:
    ; left paddle sprites
    .byte $70, $01, $00, $04
    .byte $78, $01, $00, $04
    .byte $80, $01, $00, $04
    .byte $88, $01, $00, $04
    .byte $90, $01, $00, $04
    ; right paddle sprites
    .byte $70, $01, $40, $F3
    .byte $78, $01, $40, $F3
    .byte $80, $01, $40, $F3
    .byte $88, $01, $40, $F3
    .byte $90, $01, $40, $F3
    ; ball sprite
    .byte $80, $02, $00, $7C

    ; player 1 score
    ; first left bar 
    .byte $20, $07, $00, $34
    .byte $24, $04, $00, $38
    ; second left bar 
    .byte $28, $04, $00, $38
    .byte $2C, $04, $00, $38
    ; first right bar
    .byte $1C, $07, $40, $48
    .byte $20, $04, $00, $48
    ; second right bar
    .byte $28, $07, $40, $48
    .byte $30, $07, $40, $48
    ; top bar
    .byte $18, $06, $00, $38
    .byte $18, $06, $00, $40
    ; middle bar
    .byte $24, $06, $03, $3C
    .byte $24, $04, $03, $44
    ; bottom bar
    .byte $30, $06, $40, $38
    .byte $30, $06, $40, $40

    ; player 2 score
    ; first left bar 
    .byte $BA, $07, $00, $AC
    .byte $BE, $04, $00, $B0
    ; second left bar 
    .byte $C2, $04, $00, $B0
    .byte $C6, $04, $00, $B0
    ; first right bar
    .byte $B6, $07, $40, $C0
    .byte $BA, $04, $00, $C0
    ; second right bar
    .byte $C2, $07, $40, $C0
    .byte $CA, $07, $40, $C0
    ; top bar
    .byte $B2, $06, $00, $B0
    .byte $B2, $06, $00, $B8
    ; middle bar
    .byte $BE, $06, $03, $B4
    .byte $BE, $04, $03, $BC
    ; bottom bar
    .byte $CA, $06, $40, $B0
    .byte $CA, $06, $40, $B8

EndScreenSprites:
    .byte $20, $5F, $00, $20 ;P
    .byte $20, $5B, $00, $28 ;L
    .byte $20, $50, $00, $30 ;A
    .byte $20, $68, $00, $38 ;Y
    .byte $20, $54, $00, $40 ;E
    .byte $20, $61, $00, $48 ;R
    ; space
    .byte $20, $6A, $00, $58 ;1
    .byte $2A, $66, $00, $30 ;W
    .byte $2A, $58, $00, $38 ;I
    .byte $2A, $5D, $00, $40 ;N
    .byte $2A, $62, $00, $48 ;S

    .byte $BA, $5F, $00, $9E ;P
    .byte $BA, $5B, $00, $A6 ;L
    .byte $BA, $50, $00, $AE ;A
    .byte $BA, $68, $00, $B6 ;Y
    .byte $BA, $54, $00, $BE ;E
    .byte $BA, $61, $00, $C6 ;R
    ; space
    .byte $BA, $6B, $00, $D6 ;2
    .byte $C5, $66, $00, $AE ;W
    .byte $C5, $58, $00, $B6 ;I
    .byte $C5, $5D, $00, $BE ;N
    .byte $C5, $62, $00, $C6 ;S
    

BackgroundData:
    .incbin "game_background.bin"

TitleScreen:
    .incbin "start_screen_background.bin"

.segment "VECTORS"
    .word update_screen
    .word title_screen_start    ; runs at the beginning and everytime the reset button is pressed
    .word 0

.segment "CHARS"
    .incbin "pong.chr"
