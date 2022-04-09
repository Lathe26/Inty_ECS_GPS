' Inty ECS GPS
'
' Purpose: displays GPS data using the ECS serial port
'
' Keypad menu:
'   1. Change protocol and its settings (ex: Garmin TextOut and baud rate).  Use disc and Enter/Clear.
'   2. Change timezone (default is UTC).  Use disc and Enter/Clear.
'   3. Toggle between metric and imperial.
'   8. Clear displayed error messages
'   9. Toggle whether to dump debug info and stop processing
'
' Metadata for .ROM files
ASM CFGVAR "name" = "Inty ECS GPS"
ASM CFGVAR "short_name" = "Inty ECS GPS"
ASM CFGVAR "author" = "Chris Dreher"
ASM CFGVAR "release_date" = "2019-02-17"
ASM CFGVAR "license" = "no license"
ASM CFGVAR "music_by" = "no one"
ASM CFGVAR "ecs_compat" = 3
ASM CFGVAR "voice_compat" = 2
ASM CFGVAR "tv_compat" = 2
ASM CFGVAR "intv2_compat" = 2
ASM CFGVAR "kc_compat" = 2
ASM MEMATTR $C000, $CFFF, "+RW"

INCLUDE "contrib\constants.bas"
    
OPTION EXPLICIT ON

' Compiler options
CONST UART_SIMULATION = 0
DIM enable_dump_debug_info
DIM stop_all_processing
   
' Given a controller value masked by BUTTON_MASK, returns whether there is a keypad press
DEF FN is_masked_keypad( mask_controller ) = \
    ( ( ( mask_controller ) = $80 ) + ( ( mask_controller ) = $40 ) + ( ( mask_controller ) = $20 ) )
    
' Prints a 2 digit hex value at the current screen location with the specified color
DEF FN print_hex2( hex_value, color_val ) = PRINT HEX_CHARS( ( ( hex_value ) / 16 ) AND $0F ) + ( color_val ), HEX_CHARS( ( ( hex_value ) AND $0F ) ) + ( color_val )

' Status register flags (ex: result from ADD_WITH_STATUS)
CONST STATUS_REG_CARRY      = $1010
CONST STATUS_REG_OVERFLOW   = $2020
CONST STATUS_REG_ZERO       = $4040
CONST STATUS_REG_SIGN       = $8080
    
' Scratch and garbage variables
DIM a
DIM b
DIM c
DIM #a
DIM #garbage
#garbage = #garbage + 1     ' Silence compiler warning

' Controller debouncing variables
CONST DEBOUNCE_MIN = 1      ' Higher values filter out more flakey signals but add latency for the user
CONST DEBOUNCE_STOP = $FF
DIM cont_debounce
DIM cont_current
DIM cont_previous
DIM cont_upper
DIM disc_pressed
DIM side_button_pressed

' Other controller variables
CONST KEYPAD_NOT_NUMERIC = 10
DIM keypad_digit
keypad_digit = KEYPAD_NOT_NUMERIC
CONST DISC_NOT_A_DIRECTION = 16

' Variables for scrolling through value at slow, medium, and fast speeds
CONST SCROLL_COUNTDOWN_MAX = 4
DIM scroll_countdown
DIM #dec_delta

' The ASCII vs GROM offset (i.e. ASCII index = GROM index + 32)
CONST ASCII_OFFSET = 32

' Variables for the address and its value
DIM #current_value  ' The current value stored at #address (as read by memory_interrupt_routine)
DIM #new_value      ' The new value the user enters

' Which entry mode is the code in (ex: the user is entering a decimal value)
CONST ENTRY_MODE_NORMAL     = 0
CONST ENTRY_MODE_PROTOCOL   = 1
CONST ENTRY_MODE_TIMEZONE   = 2
DIM entry_mode
entry_mode = ENTRY_MODE_NORMAL

' Variables for controlling the cursor
CONST ENTRY_CURSOR_HEX_MAX = 3
CONST ENTRY_CURSOR_DEC_MAX = 4
CONST ENTRY_CURSOR_COUNTDOWN_MAX    = 16
CONST ENTRY_CURSOR_COUNTDOWN_BLINK  = 4
DIM entry_cursor_position                  ' Positions start at the TOP (i.e. bits 15-12 are pos 0, bits 3-0 are pos 3)
DIM entry_cursor_countdown

' Variables for handling the user entering hex numbers.
CONST ENTRY_HEX_PRESS_COUNTDOWN_MAX      = 40
CONST ENTRY_HEX_PRESS_COUNTDOWN_DISABLED = $FFFF
DIM entry_hex_press_countdown
entry_hex_press_countdown = ENTRY_HEX_PRESS_COUNTDOWN_DISABLED

' Variables for managing the display of messages
CONST MESSAGE_TIMER_MAX = 180
CONST MESSAGE_TIMER_NOW = 1
CONST MESSAGE_LIST_MAX = 3
CONST MESSAGE_VALUE_DEFAULT = 0
CONST MESSAGE_VALUE_STOP = $FFFF    ' Put this in #message_list and the previous message never times out
CONST MESSAGE_TRUNCATE_DEFAULT = 20
DIM #message_timer
DIM message_index
DIM #message_current
DIM #message_buffer( BACKGROUND_COLUMNS )
DIM #message_list( MESSAGE_LIST_MAX )
DIM message_list_truncate( MESSAGE_LIST_MAX )  ' What character to truncate the string copy (default is 20)

'Variables for the ECS UART
CONST UART_STATUS_REG           = $00E0
CONST UART_STATUS_RX_HAS_DATA   = $01
CONST UART_STATUS_TX_EMPTY      = $02
CONST UART_STATUS_NOT_CTS       = $08
CONST UART_STATUS_FRAMING_ERROR = $10
CONST UART_STATUS_PARITY_ERROR  = $40
CONST UART_STATUS_UNKNOWN       = $A4
CONST UART_CONFIG_1_REG         = $00E0
CONST UART_CONFIG_1_WORD_7E2    = $00
CONST UART_CONFIG_1_WORD_7O2    = $04
CONST UART_CONFIG_1_WORD_7E1    = $08
CONST UART_CONFIG_1_WORD_7O1    = $0C
CONST UART_CONFIG_1_WORD_8N2    = $10
CONST UART_CONFIG_1_WORD_8N1    = $14
CONST UART_CONFIG_1_WORD_8E2    = $18
CONST UART_CONFIG_1_WORD_8O2    = $1C
CONST UART_CONFIG_1_BREAK       = $20
CONST UART_CONFIG_2_REG         = $00E2
CONST UART_CONFIG_2_BAUD_150    = $00
CONST UART_CONFIG_2_BAUD_300    = $01
CONST UART_CONFIG_2_BAUD_600    = $02
CONST UART_CONFIG_2_BAUD_1200   = $03
CONST UART_CONFIG_2_BAUD_MASK   = $03
CONST UART_CONFIG_2_PORT_AUX    = $00
CONST UART_CONFIG_2_PORT_TAPE   = $08
CONST UART_CONFIG_2_REMOTE_ON   = $10
CONST UART_CONFIG_2_MODE_RX     = $00
CONST UART_CONFIG_2_MODE_OFF    = $04
CONST UART_CONFIG_2_MODE_TX_CTS = $20
CONST UART_CONFIG_2_MODE_TX     = $24
CONST UART_DATA_R_REG           = $00E1
CONST UART_DATA_W_REG           = $00E1
DIM uart_config_2
DIM uart_status
uart_config_2   = UART_CONFIG_2_BAUD_1200 + UART_CONFIG_2_MODE_RX + UART_CONFIG_2_PORT_AUX

'Variables for receive buffer.  When read=write, the buffer is empty.  When write+1=read, the buffer is full.
CONST RECV_BUFFER_MAX   = 8
DIM recv_buffer( RECV_BUFFER_MAX )
DIM recv_buffer_r_index
DIM recv_buffer_w_index
DEF FN is_recv_buffer_empty             = ( ( recv_buffer_w_index % RECV_BUFFER_MAX ) = ( recv_buffer_r_index % RECV_BUFFER_MAX ) )
DEF FN is_recv_buffer_full              = ( ( ( recv_buffer_w_index + 1 ) % RECV_BUFFER_MAX ) = ( recv_buffer_r_index % RECV_BUFFER_MAX ) )
DEF FN recv_buffer_read                 = recv_buffer( recv_buffer_r_index ) : recv_buffer_r_index = ( ( recv_buffer_r_index + 1 ) % RECV_BUFFER_MAX )
DEF FN recv_buffer_write( write_data )  = recv_buffer( recv_buffer_w_index ) = ( write_data ) : recv_buffer_w_index = ( ( recv_buffer_w_index + 1 ) % RECV_BUFFER_MAX )
DEF FN conv_ascii( value, dest )        = IF value = $5E THEN dest = ( $102 * 8 ) ELSE IF value = $5F THEN dest = ( $103 * 8 ) ELSE IF value <= $1F THEN dest = ( ( value + $120 ) * 8 ) ELSE  IF value >= $F5 THEN dest = ( ( value - $F5 + $115 ) * 8 ) ELSE dest = ( ( ( value - ASCII_OFFSET ) AND $FF ) * 8 )

CONST RECV_BUFFER_ERROR_FULL = 1
DIM recv_buffer_error

CONST PACKET_BUFFER_MAX = 60
DIM packet_buffer( PACKET_BUFFER_MAX )
DIM packet_buffer_size
DIM packet_done
                                          
' Constants that control where various screen elements are placed.
CONST ROW_MESSAGE           = 0
CONST ROW_TIME              = 1
CONST COL_TIME              = 0
CONST ROW_TIMEZONE          = 2
CONST COL_TIMEZONE_LBL      = 0
CONST COL_TIMEZONE_NUM_VAL  = 9
CONST COL_TIMEZONE_NAME_VAL = 14
CONST ROW_LATITUDE          = 3
CONST COL_LATITUDE_LBL      = 0
CONST COL_LATITUDE_VAL      = 4
CONST ROW_LONGITUDE         = 4
CONST COL_LONGITUDE_LBL     = 0
CONST COL_LONGITUDE_VAL     = 4
CONST ROW_ALTITUDE          = 5
CONST COL_ALTITUDE_LBL      = 0
CONST COL_ALTITUDE_VAL      = 4
CONST COL_ALTITUDE_UNIT     = 11
CONST ROW_VELOCITY_LBL      = 6
CONST COL_VELOCITY_LBL      = 0
CONST ROW_VELOCITY_NS       = ROW_VELOCITY_LBL
CONST COL_VELOCITY_NS_VAL   = 4
CONST COL_VELOCITY_NS_UNIT  = 11
CONST ROW_VELOCITY_EW       = ROW_VELOCITY_LBL+1
CONST COL_VELOCITY_EW_VAL   = COL_VELOCITY_NS_VAL
CONST COL_VELOCITY_EW_UNIT  = COL_VELOCITY_NS_UNIT
CONST ROW_VELOCITY_UD       = ROW_VELOCITY_LBL+2
CONST COL_VELOCITY_UD_VAL   = COL_VELOCITY_NS_VAL
CONST COL_VELOCITY_UD_UNIT  = COL_VELOCITY_NS_UNIT
CONST ROW_STATUS            = 9
CONST COL_STATUS_LBL        = 0
CONST COL_STATUS_VAL        = 7
CONST ROW_TYPE              = 10
CONST COL_TYPE_LBL          = 0
CONST COL_TYPE_VAL          = 5
CONST ROW_CNFG              = 11
CONST COL_CNFG_LBL          = 0
CONST COL_CNFG_VAL          = 5
CONST COL_CNFG_UNIT         = 9
CONST ROW_POS_LBL           = 6
CONST COL_POS_LBL           = 17
CONST ROW_ERR_LBL           = 7
CONST COL_ERR_LBL           = 17
CONST ROW_ERR               = 8
CONST COL_ERR_VAL           = 16
CONST COL_ERR_UNIT          = 19
CONST ROW_UART_STATUS       = ROW_ERR_LBL
CONST COL_UART_STATUS       = COL_ERR_LBL-2

' Constants for the types of colors of the UI
CONST COLOR_LABELS      = CS_TAN
CONST COLOR_DATA        = CS_WHITE
CONST COLOR_MESSAGE     = CS_RED
CONST COLOR_NEW_VALUE   = CS_RED

' Make sure to disable the interrupt service routine until all other variables are initialized
DIM enable_interrupt_routine
DIM #uart_sim_value
DIM uart_sim_value
DIM uart_sim_wait
DIM uart_sim_prior_frame
RESTORE UART_SIM_DATA
enable_interrupt_routine = 0
CLS
MODE SCREEN_COLOR_STACK, STACK_BLACK, STACK_BLACK, STACK_BLACK, STACK_BLACK
WAIT

DIM #prior_frame
#prior_frame = FRAME

GOSUB reset_messages    ' Disable displaying any messages on startup

GOSUB start_entry_mode_normal

' Set up GRAM with the 1/2 and 3/4 cards
DEFINE 0, 4, CARD_1_2
WAIT
DEFINE 32, 16, CARD_00
WAIT
DEFINE 48, 16, CARD_10
WAIT
DEFINE 21, 11, CARD_F5
WAIT

'Initializes the UART
IF UART_SIMULATION = 0 THEN
    POKE UART_CONFIG_1_REG, UART_CONFIG_1_WORD_8N1
    POKE UART_CONFIG_2_REG, uart_config_2
END IF

' Install the interrupt service routine
ON FRAME GOSUB interrupt_routine

' Enable the ISR
enable_interrupt_routine = 1

GOSUB read_uart

' Main loop, mostly this handles debouncing the controllers and dispatching
DO WHILE 1
    IF stop_all_processing = 1 THEN
        WHILE CONT = 0
        WEND
        GOSUB clear_error_messages
        GOSUB flush_data
        WAIT
    END IF

    ' Debounce the controller
    cont_current = CONT
    cont_upper = cont_current AND BUTTON_MASK
    IF cont_current = cont_previous THEN
        IF cont_debounce <> DEBOUNCE_STOP THEN cont_debounce = cont_debounce - 1
    ELSE
        cont_debounce = DEBOUNCE_MIN
        cont_previous = cont_current
    END IF
    IF ( cont_debounce = 0 ) THEN
        ' Execute the debouncing logic
        IF is_masked_keypad( cont_upper ) THEN
        
            disc_pressed = 0

            ON entry_mode GOSUB handle_mode_normal_keypad, handle_mode_protocol_keypad, handle_mode_timezone_keypad
            
        ELSEIF ( cont_upper = BUTTON_1 ) + \
               ( cont_upper = BUTTON_2 ) + \
               ( cont_upper = BUTTON_3 ) THEN
            ' Side button pressed... was this a new press?
            disc_pressed = ( cont_previous AND NOT BUTTON_MASK ) <> 0
            side_button_pressed = cont_upper
        ELSE
            ' No buttons pressed, just check the disc
            disc_pressed = ( cont_previous AND NOT BUTTON_MASK ) <> 0
        END IF
    END IF
    
    GOSUB read_uart
    
    ' Handle disc presses
    IF disc_pressed THEN
        ON entry_mode GOSUB , handle_mode_protocol_disc, handle_mode_timezone_disc
    END IF
    
    GOSUB read_uart
    
    ' Handle blinking the cursor, if there is one on the screen
    entry_cursor_countdown = entry_cursor_countdown - 1
    IF entry_cursor_countdown > ENTRY_CURSOR_COUNTDOWN_MAX THEN
        entry_cursor_countdown = ENTRY_CURSOR_COUNTDOWN_MAX
        ON entry_mode GOSUB , update_new_value_protocol, update_new_value_timezone
    ELSEIF entry_cursor_countdown = ENTRY_CURSOR_COUNTDOWN_BLINK THEN
        ON entry_mode GOSUB , update_new_value_protocol, update_new_value_timezone
    END IF

    GOSUB read_uart
    
    ' Handle timing out a hex keypress, if there is one
    ON entry_mode GOSUB handle_timer_normal, handle_timer_protocol, handle_timer_timezone
    
    ' Check if any error occurred with the recv_buffer
    IF recv_buffer_error <> 0 THEN
        IF recv_buffer_error = RECV_BUFFER_ERROR_FULL THEN
            GOSUB dump_debug_info
            GOSUB reset_messages
            #message_list(0) = VARPTR RECV_BUFFER_FULL_MESSAGE(0)
            #message_list(1) = MESSAGE_VALUE_STOP
        END IF
        recv_buffer_error = 0
    END IF
    
    ' Check and print the UART status register if not zero
    IF uart_status <> 0 THEN
        PRINT AT SCREENPOS( COL_UART_STATUS, ROW_UART_STATUS )
        print_hex2( uart_status, COLOR_MESSAGE )
    ELSE
        PRINT AT SCREENPOS( COL_UART_STATUS, ROW_UART_STATUS ), "  "
    END IF
    
    GOSUB update_message
    WHILE #prior_frame = FRAME
        GOSUB read_uart
    WEND
    #prior_frame = FRAME

    ' Preform any processing here that requires a frame to execute
    ' Ex: MOB may have previously moved via #mobshadow but COL1-7 registers are not 
    ' updated until the frame executes
    IF disc_pressed THEN
        IF disc_pressed = 2 THEN disc_pressed = 0
    END IF
LOOP    

' The interrupt service routine implementation
interrupt_routine: PROCEDURE
    IF enable_interrupt_routine = 0 THEN RETURN
    'GOSUB read_uart
END

' Reads the UART into the recv_buffer
read_uart: PROCEDURE
    IF stop_all_processing = 1 THEN RETURN

    IF UART_SIMULATION THEN
        IF uart_sim_wait = 1 THEN
            IF ( FRAME % RECV_BUFFER_MAX ) < uart_sim_prior_frame THEN
                uart_sim_wait = 0
            END IF
        END IF
        uart_sim_prior_frame = ( FRAME % RECV_BUFFER_MAX )
        IF uart_sim_wait = 0 THEN
            WHILE 1
                IF is_recv_buffer_full THEN
                    uart_sim_wait = 1
                    EXIT WHILE
                END IF
                READ #uart_sim_value
                IF #uart_sim_value = $FFFF THEN
                    RESTORE UART_SIM_DATA
                    READ #uart_sim_value
                END IF
                recv_buffer_write( ( #uart_sim_value + 32 ) AND $FF )                
            WEND
        END IF
    ELSE
        WHILE 1
            IF is_recv_buffer_full THEN
                'TODO: should this be turned back on?  Maybe not since the buffer might be full a few times in a row before it get emptied
                'recv_buffer_error = RECV_BUFFER_ERROR_FULL
                EXIT WHILE
            END IF
            uart_status = PEEK( UART_STATUS_REG )
            IF ( uart_status AND UART_STATUS_RX_HAS_DATA ) = 0 THEN
                ' Exit and save any receive errors flags
                uart_status = uart_status AND NOT ( UART_STATUS_NOT_CTS + UART_STATUS_TX_EMPTY )
                EXIT WHILE
            END IF
            uart_status = 0
            recv_buffer_write( PEEK( UART_DATA_R_REG ) )
        WEND
    END IF
END

' Copies N blocks (each block is 8 words) from source address to destination address
' To use, call it as CALL MEMCOPY8( 5, source_address, destination_address )
ASM MEMCOPY8:           PROC
ASM                     BEGIN           ; Really it's PSHR R5
ASM                     MOVR    R1, R4  ; Copy source_address to auto-incrementing register R4
ASM                     MOVR    R2, R5  ; Copy destination_address to auto-incrementing register R5
ASM _MEMCOPY8_LOOP:
ASM                     REPEAT  8       ; Start 8X copy block block
ASM                     MVI@    R4, R3  ; Read value from source, incr the read address
ASM                     MVO@    R3, R5  ; Write value to dest, incr the write address
ASM                     ENDR            ; End 8X copy block block
ASM                     DECR    R0
ASM                     BNE     _MEMCOPY8_LOOP      ; Copy the next block of 8 if we're not done
ASM                     RETURN          ; Really it's PULR R7
ASM                     ENDP

' Copies a block of N words from source address to destination address
' To use, call it as CALL MEMCOPY( source_address, destination_address, N )
ASM MEMCOPY:            PROC
ASM                     BEGIN           ; Really it's PSHR R5
ASM                     MOVR    R0, R4  ; Copy source_address to auto-incrementing register R4
ASM                     MOVR    R1, R5  ; Copy destination_address to auto-incrementing register R5
ASM                     CMPI    #0, R2  ; Compare to zero and 
ASM _MEMCOPY_LOOP:
ASM                     BEQ     _MEMCOPY_END        ; Exit if we're done
ASM                     MVI@    R4, R3  ; Read value from source, incr the read address
ASM                     MVO@    R3, R5  ; Write value to dest, incr the write address
ASM                     DECR    R2      ; Decrement how many words are left to copy
ASM                     J       _MEMCOPY_LOOP       ; Jump to where the end-checking is done
ASM _MEMCOPY_END:
ASM                     RETURN          ; Really it's PULR R7
ASM                     ENDP

' Add Value1 to Value2 and store into DestAddress and the returns the status word.
' To use, call it as #status = USR ADD_WITH_STATUS( #value1, #value2, VARPTR #dest )
' Return is the status register, mask with STATUS_REG_* flags
ASM ADD_WITH_STATUS:    PROC
ASM                     ADDR    R0, R1  ; R1 = R0 + R1, update the status register
ASM                     GSWD    R0      ; R0 = status register
ASM                     MVO@    R1, R2  ; Store result into address in R2
ASM                     JR      R5      ; Done, return from the call
ASM                     ENDP

' Start the normal user entry mode
start_entry_mode_normal: PROCEDURE
    entry_mode = ENTRY_MODE_NORMAL
    
    ' Place the labels on the screen
    PRINT AT SCREENPOS( COL_TIMEZONE_LBL, ROW_TIMEZONE )        COLOR COLOR_LABELS, "Timezone"
    PRINT AT SCREENPOS( COL_LATITUDE_LBL, ROW_LATITUDE )        COLOR COLOR_LABELS, "Lat"
    PRINT AT SCREENPOS( COL_LONGITUDE_LBL, ROW_LONGITUDE )      COLOR COLOR_LABELS, "Lon"
    PRINT AT SCREENPOS( COL_ALTITUDE_LBL, ROW_ALTITUDE )        COLOR COLOR_LABELS, "Alt"
    PRINT AT SCREENPOS( COL_ALTITUDE_UNIT, ROW_ALTITUDE )       COLOR COLOR_LABELS, "m"
    PRINT AT SCREENPOS( COL_VELOCITY_LBL, ROW_VELOCITY_LBL )    COLOR COLOR_LABELS, "Vel"
    PRINT AT SCREENPOS( COL_VELOCITY_NS_UNIT, ROW_VELOCITY_NS ) COLOR COLOR_LABELS, "m/s"
    PRINT AT SCREENPOS( COL_VELOCITY_EW_UNIT, ROW_VELOCITY_EW ) COLOR COLOR_LABELS, "m/s"
    PRINT AT SCREENPOS( COL_VELOCITY_UD_UNIT, ROW_VELOCITY_UD ) COLOR COLOR_LABELS, "m/s"
    PRINT AT SCREENPOS( COL_STATUS_LBL, ROW_STATUS )            COLOR COLOR_LABELS, "Status"
    PRINT AT SCREENPOS( COL_TYPE_LBL, ROW_TYPE )                COLOR COLOR_LABELS, "Type"
    PRINT AT SCREENPOS( COL_CNFG_LBL, ROW_CNFG )                COLOR COLOR_LABELS, "Cnfg"
    PRINT AT SCREENPOS( COL_CNFG_UNIT, ROW_CNFG )               COLOR COLOR_LABELS, "bps 8N1"
    PRINT AT SCREENPOS( COL_POS_LBL, ROW_POS_LBL )              COLOR COLOR_LABELS, "Pos"
    PRINT AT SCREENPOS( COL_ERR_LBL, ROW_ERR_LBL )              COLOR COLOR_LABELS, "Err"
    PRINT AT SCREENPOS( COL_ERR_UNIT, ROW_ERR )                 COLOR COLOR_LABELS, "m"

    PRINT AT SCREENPOS( COL_TIME +  0, ROW_TIME )                    COLOR COLOR_DATA, "20"
    PRINT AT SCREENPOS( COL_TIME +  4, ROW_TIME )                    COLOR COLOR_DATA, "-"
    PRINT AT SCREENPOS( COL_TIME +  7, ROW_TIME )                    COLOR COLOR_DATA, "-"
    PRINT AT SCREENPOS( COL_TIME + 13, ROW_TIME )                    COLOR COLOR_DATA, ":"
    PRINT AT SCREENPOS( COL_TIME + 16, ROW_TIME )                    COLOR COLOR_DATA, ":"
    PRINT AT SCREENPOS( COL_TIMEZONE_NUM_VAL, ROW_TIMEZONE )         COLOR COLOR_DATA, "+0"
    PRINT AT SCREENPOS( COL_TIMEZONE_NAME_VAL, ROW_TIMEZONE )        COLOR COLOR_DATA, "UTC"
    PRINT AT SCREENPOS( COL_LATITUDE_VAL +  5, ROW_LATITUDE )        COLOR COLOR_DATA, "*"
    PRINT AT SCREENPOS( COL_LATITUDE_VAL +  9, ROW_LATITUDE )        COLOR COLOR_DATA, "."
    PRINT AT SCREENPOS( COL_LATITUDE_VAL + 13, ROW_LATITUDE )        COLOR COLOR_DATA, "'"
    PRINT AT SCREENPOS( COL_LONGITUDE_VAL +  5, ROW_LONGITUDE )      COLOR COLOR_DATA, "*"
    PRINT AT SCREENPOS( COL_LONGITUDE_VAL +  9, ROW_LONGITUDE )      COLOR COLOR_DATA, "."
    PRINT AT SCREENPOS( COL_LONGITUDE_VAL + 13, ROW_LONGITUDE )      COLOR COLOR_DATA, "'"
    PRINT AT SCREENPOS( COL_VELOCITY_NS_VAL + 5, ROW_VELOCITY_NS )   COLOR COLOR_DATA, "."
    PRINT AT SCREENPOS( COL_VELOCITY_EW_VAL + 5, ROW_VELOCITY_EW )   COLOR COLOR_DATA, "."
    PRINT AT SCREENPOS( COL_VELOCITY_UD_VAL + 4, ROW_VELOCITY_UD )   COLOR COLOR_DATA, "."
    PRINT AT SCREENPOS( COL_STATUS_VAL + 1, ROW_STATUS )             COLOR COLOR_DATA, "="
    PRINT AT SCREENPOS( COL_TYPE_VAL , ROW_TYPE )                    COLOR COLOR_DATA, "Garmin TextOut"
    PRINT AT SCREENPOS( COL_CNFG_VAL , ROW_CNFG )                    COLOR COLOR_DATA, "1200"

    GOSUB reset_messages
END

' Handle the keypad while in normal entry mode
handle_mode_normal_keypad: PROCEDURE
    IF cont_previous = KEYPAD_0 THEN
    ELSEIF cont_previous = KEYPAD_1 THEN
        'GOSUB start_entry_mode_protocol
    ELSEIF cont_previous = KEYPAD_2 THEN
        'GOSUB start_entry_mode_timezone
    ELSEIF cont_previous = KEYPAD_3 THEN
        'GOSUB toggle_metric_imperial
    ELSEIF cont_previous = KEYPAD_4 THEN
    ELSEIF cont_previous = KEYPAD_5 THEN
    ELSEIF cont_previous = KEYPAD_6 THEN
    ELSEIF cont_previous = KEYPAD_7 THEN
    ELSEIF cont_previous = KEYPAD_8 THEN
        GOSUB clear_error_messages
    ELSEIF cont_previous = KEYPAD_9 THEN
        GOSUB toggle_dump_debug_info
    ELSEIF cont_previous = KEYPAD_CLEAR THEN
    ELSEIF cont_previous = KEYPAD_ENTER THEN
    END IF
END

' Handle a timer tick in normal mode
handle_timer_normal: PROCEDURE
    GOSUB get_packet
    IF packet_done = 1 THEN
        GOSUB process_packet
        packet_buffer_size = 0
        packet_done = 0
    END IF
END

' Start the user_selecting_protocol entry mode
start_entry_mode_protocol: PROCEDURE
    'entry_mode = ENTRY_MODE_PROTOCOL
    'GOSUB reset_messages
    '#message_list(0) = VARPTR ENTRY_MODE_PROTOCOL_MESSAGE(0)
    '#new_value = #current_value
    'entry_cursor_countdown = ENTRY_CURSOR_COUNTDOWN_MAX
    'entry_cursor_position = 0
    'GOSUB update_new_value_protocol
END

' Handle the keypad while in user_selecting_protocol entry mode
handle_mode_protocol_keypad: PROCEDURE
    'GOSUB get_keypad_digit
    'IF keypad_digit <> KEYPAD_NOT_NUMERIC THEN
    'ELSEIF cont_previous = KEYPAD_CLEAR THEN
    'ELSEIF cont_previous = KEYPAD_ENTER THEN
    'END IF
END

' Handle the disc while in user_selecting_protocol entry mode
handle_mode_protocol_disc: PROCEDURE
    'entry_cursor_countdown = ENTRY_CURSOR_COUNTDOWN_MAX     ' Turn off the blinking for a moment
    'entry_cursor_position  = ENTRY_CURSOR_DEC_MAX
    'scroll_countdown = scroll_countdown - 1
    'IF scroll_countdown > SCROLL_COUNTDOWN_MAX THEN scroll_countdown = SCROLL_COUNTDOWN_MAX
    'a = CONT_TO_DIR( cont_previous AND NOT BUTTON_MASK )
    'IF ( a <> DISC_NOT_A_DIRECTION ) AND ( scroll_countdown = 0 ) THEN
    '    #dec_delta = DIR_TO_DEC_DELTA( a )
    '    IF ABS( #dec_delta ) >= 100 THEN
    '        #a = #new_value + #dec_delta
    '        #new_value = ( #a / 100 * 100 ) + ( #new_value % 100 )
    '    ELSE
    '        #new_value = #new_value + #dec_delta
    '    END IF
    '    GOSUB update_new_value_protocol
    'END IF
END

' Update the screen with a new value for user_selecting_protocol entry mode
update_new_value_protocol: PROCEDURE
END

' Handle a timer tick while in user_selecting_protocol entry mode
handle_timer_protocol: PROCEDURE
    'IF entry_hex_press_countdown <> ENTRY_HEX_PRESS_COUNTDOWN_DISABLED THEN
    '    entry_hex_press_countdown = entry_hex_press_countdown - 1
    '    IF ( entry_hex_press_countdown = 0 ) AND entry_cursor_position < ENTRY_CURSOR_HEX_MAX THEN
    '        entry_cursor_position = entry_cursor_position + 1
    '        GOSUB update_new_value_protocol
    '    END IF
    'END IF
END

' Start the timezone user entry mode
start_entry_mode_timezone: PROCEDURE
    'entry_mode = ENTRY_MODE_TIMEZONE
    'GOSUB reset_messages
    '#message_list(0) = VARPTR ENTRY_MODE_TIMEZONE_MESSAGE(0)
    '#new_value = #current_value
    'entry_cursor_countdown = ENTRY_CURSOR_COUNTDOWN_MAX
    'entry_cursor_position = 0
    'GOSUB update_new_value_timezone
END

' Handle the keypad while in timezone user mode
handle_mode_timezone_keypad: PROCEDURE
    'GOSUB get_keypad_digit
    'IF keypad_digit <> KEYPAD_NOT_NUMERIC THEN
    'ELSEIF cont_previous = KEYPAD_CLEAR THEN
    'ELSEIF cont_previous = KEYPAD_ENTER THEN
    'END IF
END

' Handle the disc while in timezone user mode
handle_mode_timezone_disc: PROCEDURE
    'entry_cursor_countdown = ENTRY_CURSOR_COUNTDOWN_MAX     ' Turn off the blinking for a moment
    'entry_cursor_position  = ENTRY_CURSOR_DEC_MAX
    'scroll_countdown = scroll_countdown - 1
    'IF scroll_countdown > SCROLL_COUNTDOWN_MAX THEN scroll_countdown = SCROLL_COUNTDOWN_MAX
    'a = CONT_TO_DIR( cont_previous AND NOT BUTTON_MASK )
    'IF ( a <> DISC_NOT_A_DIRECTION ) AND ( scroll_countdown = 0 ) THEN
    '    #dec_delta = DIR_TO_DEC_DELTA( a )
    '    IF ABS( #dec_delta ) >= 100 THEN
    '        #a = #new_value + #dec_delta
    '        #new_value = ( #a / 100 * 100 ) + ( #new_value % 100 )
    '    ELSE
    '        #new_value = #new_value + #dec_delta
    '    END IF
    '    GOSUB update_new_value_timezone
    'END IF
END

' Update the screen with a new value for timezone user mode
update_new_value_timezone: PROCEDURE
END

' Handle a timer tick while in timezone entry mode 
handle_timer_timezone: PROCEDURE
    'IF entry_hex_press_countdown <> ENTRY_HEX_PRESS_COUNTDOWN_DISABLED THEN
    '    entry_hex_press_countdown = entry_hex_press_countdown - 1
    '    IF ( entry_hex_press_countdown = 0 ) AND entry_cursor_position < ENTRY_CURSOR_HEX_MAX THEN
    '        entry_cursor_position = entry_cursor_position + 1
    '        GOSUB update_new_value_protocol
    '    END IF
    'END IF
END

' Toggle through the metric vs imperial modes
toggle_metric_imperial: PROCEDURE
END

' Toggle whether to stop processing and dump debug info on an error
toggle_dump_debug_info: PROCEDURE
    GOSUB reset_messages
    IF enable_dump_debug_info THEN
        enable_dump_debug_info = 0
        #message_list(0) = VARPTR DUMP_DEBUG_OFF_MESSAGE(0)
    ELSE
        enable_dump_debug_info = 1
        #message_list(0) = VARPTR DUMP_DEBUG_ON_MESSAGE(0)
    END IF
END

' Reset the message display to its default
reset_messages: PROCEDURE
    #message_timer  = MESSAGE_TIMER_NOW
    message_index   = 0
    FOR a = 0 TO MESSAGE_LIST_MAX-1
        #message_list(a)         = MESSAGE_VALUE_DEFAULT
        message_list_truncate(a) = MESSAGE_TRUNCATE_DEFAULT
    NEXT a
END

' Check to see if it is time to update the message being displayed (and then do so)
update_message: PROCEDURE
    IF #message_timer < 0 THEN RETURN
    
    #message_timer = #message_timer - 1
    IF #message_timer = 0 THEN
        #a = COLOR_MESSAGE
        
        ' Figure out what the next message should be
        #message_current = MESSAGE_VALUE_DEFAULT
        IF message_index < MESSAGE_LIST_MAX THEN
            #message_current = #message_list( message_index )
            b                = message_list_truncate( message_index )
        END IF
        message_index = message_index + 1
 
        ' Either display the default string (the bit labels) or just stop processing new messages
        IF #message_current = MESSAGE_VALUE_DEFAULT THEN
            message_index = MESSAGE_LIST_MAX
            #message_timer = 0  ' This disables all processing by this sub-routine
            #a = COLOR_LABELS
            #message_current = VARPTR DEFAULT_MESSAGE(0)
        ELSEIF #message_current = MESSAGE_VALUE_STOP THEN
            message_index = MESSAGE_LIST_MAX
            #message_timer = 0  ' This disables all processing by this sub-routine
            RETURN
        ELSE
            ' Start the message's countdown timer
            #message_timer = MESSAGE_TIMER_MAX
        END IF
     
        ' Display the next message
        CALL MEMCOPY( #message_current, VARPTR #message_buffer(0), b )
        FOR a = 0 TO b
            #message_buffer(a) = #message_buffer(a) * 8 + #a
        NEXT a
        CALL MEMCOPY( VARPTR #message_buffer(0), VARPTR #backtab(ROW_MESSAGE * BACKGROUND_COLUMNS), b )
    END IF
END

' Based on cont_previous, set keypad_digit to a numerical value
get_keypad_digit: PROCEDURE
    IF cont_previous = KEYPAD_0 THEN
        keypad_digit = 0
    ELSEIF cont_previous = KEYPAD_1 THEN
        keypad_digit = 1
    ELSEIF cont_previous = KEYPAD_2 THEN
        keypad_digit = 2
    ELSEIF cont_previous = KEYPAD_3 THEN
        keypad_digit = 3
    ELSEIF cont_previous = KEYPAD_4 THEN
        keypad_digit = 4
    ELSEIF cont_previous = KEYPAD_5 THEN
        keypad_digit = 5
    ELSEIF cont_previous = KEYPAD_6 THEN
        keypad_digit = 6
    ELSEIF cont_previous = KEYPAD_7 THEN
        keypad_digit = 7
    ELSEIF cont_previous = KEYPAD_8 THEN
        keypad_digit = 8
    ELSEIF cont_previous = KEYPAD_9 THEN
        keypad_digit = 9
    ELSE
        keypad_digit = KEYPAD_NOT_NUMERIC
    END IF
END

' Read from recv_buffer and write into the packet_buffer, set packet_done if appropriate
get_packet: PROCEDURE
    WHILE NOT is_recv_buffer_empty
        IF packet_buffer_size >= PACKET_BUFFER_MAX THEN
            GOSUB dump_debug_info
            GOSUB reset_messages
            #message_list(0) = VARPTR PACKET_BUFFER_OVERFLOW_MESSAGE(0)
            #message_list(1) = MESSAGE_VALUE_STOP
            packet_buffer_size = 0
            EXIT WHILE
        END IF
        packet_buffer( packet_buffer_size ) = recv_buffer_read
        ' Currently, only Garmin's TextOut protocol is supported so just hard-code test for a newline being found
        IF packet_buffer( packet_buffer_size ) = $0A THEN
            packet_done = 1
            packet_buffer_size = packet_buffer_size + 1
            EXIT WHILE
        END IF
        packet_buffer_size = packet_buffer_size + 1
    WEND
END

CONST TEXTOUT_PACKET_SIZE    = 57
CONST TEXTOUT_STATUS_INDEX   = 30

' Process the packet that is in the packet_buffer
process_packet: PROCEDURE
    ' Currently, only Garmin's TextOut protocol is supported so just hard-code process it here.
    IF packet_buffer_size <> TEXTOUT_PACKET_SIZE THEN
        GOSUB dump_debug_info
        GOSUB reset_messages
        #message_list(0) = VARPTR PACKET_INVALID_SIZE_MESSAGE(0)
        #message_list(1) = MESSAGE_VALUE_STOP
        RETURN
    END IF
    IF ( packet_buffer( 0 ) <> $40 ) + ( packet_buffer( TEXTOUT_PACKET_SIZE-2 ) <> $0D ) + ( packet_buffer( TEXTOUT_PACKET_SIZE-1 ) <> $0A ) THEN
        GOSUB dump_debug_info
        GOSUB reset_messages
        #message_list(0) = VARPTR PACKET_INVALID_MESSAGE(0)
        #message_list(1) = MESSAGE_VALUE_STOP
        RETURN
    END IF
    FOR a = 1 TO TEXTOUT_PACKET_SIZE-3
        conv_ascii( packet_buffer( a ), #a )
        #backtab( TEXT_OUT_TO_SCREENPOS( a ) )   = #a + COLOR_DATA
    NEXT a
    a = packet_buffer( TEXTOUT_STATUS_INDEX )
    IF a = $67 THEN     ' ASCII g
        PRINT AT SCREENPOS( COL_STATUS_VAL + 2, ROW_STATUS ) COLOR COLOR_DATA,    " 2D GPS    "
    ELSEIF a = $47 THEN ' ASCII G
        PRINT AT SCREENPOS( COL_STATUS_VAL + 2, ROW_STATUS ) COLOR COLOR_DATA,    " 3D GPS    "
    ELSEIF a = $64 THEN ' ASCII d
        PRINT AT SCREENPOS( COL_STATUS_VAL + 2, ROW_STATUS ) COLOR COLOR_DATA,    "2D diff GPS"
    ELSEIF a = $44 THEN ' ASCII D
        PRINT AT SCREENPOS( COL_STATUS_VAL + 2, ROW_STATUS ) COLOR COLOR_DATA,    "3D diff GPS"
    ELSEIF a = $53 THEN ' ASCII S
        PRINT AT SCREENPOS( COL_STATUS_VAL + 2, ROW_STATUS ) COLOR COLOR_DATA,    " Simulation"
    ELSEIF a = $5F THEN ' ASCII _
        PRINT AT SCREENPOS( COL_STATUS_VAL + 2, ROW_STATUS ) COLOR COLOR_DATA,    " no fix    "
    ELSE
        PRINT AT SCREENPOS( COL_STATUS_VAL + 2, ROW_STATUS ) COLOR COLOR_MESSAGE, "??unknown??"
    END IF
END

' Clears the display of any displayed errors
clear_error_messages: PROCEDURE
    CLS
    GOSUB start_entry_mode_normal
END

' Flushes any in-progress data
flush_data: PROCEDURE
    stop_all_processing = 0
    uart_status = 0
    recv_buffer_error = 0
    recv_buffer_r_index = 0
    recv_buffer_w_index = 0
    packet_buffer_size = 0
    packet_done = 0
END

' Dumps the packet buffer on the screen, overwritting normal portions
dump_debug_info: PROCEDURE
    IF enable_dump_debug_info THEN
        PRINT AT SCREENPOS( 0, 7 ) COLOR COLOR_MESSAGE, <.2>recv_buffer_r_index, <.2>recv_buffer_w_index," "
        FOR a = 0 TO RECV_BUFFER_MAX-1
            conv_ascii( packet_buffer( a ), #a )
            #backtab( POS(0) + a ) = #a + COLOR_MESSAGE
        NEXT a
        PRINT AT SCREENPOS( 0, 8 ) COLOR COLOR_MESSAGE, <.3>packet_buffer_size
        IF packet_buffer_size >= PACKET_BUFFER_MAX THEN packet_buffer_size = PACKET_BUFFER_MAX
        FOR a = 0 TO packet_buffer_size-1
            conv_ascii( packet_buffer( a ), #a )
            #backtab( SCREENPOS( 0, 9 ) + a ) = #a + COLOR_MESSAGE
        NEXT a
        IF packet_buffer_size <= PACKET_BUFFER_MAX-1 THEN
            FOR a = packet_buffer_size TO PACKET_BUFFER_MAX-1
                conv_ascii( packet_buffer( a ), #a )
                #backtab( SCREENPOS( 0, 9 ) + a ) = #a + CS_BLUE
            NEXT a
        END IF
        stop_all_processing = 1
    END IF
END
  
' Converts raw controller disc info into a direction index (like a clock-face)
' N is 0, NNE is 1, NE is 2, etc.
CONT_TO_DIR:
    DATA    DISC_NOT_A_DIRECTION
    DATA    8   ' S
    DATA    4   ' E
    DATA    7   ' SSE
    DATA    0   ' N 
    DATA    DISC_NOT_A_DIRECTION
    DATA    3   ' ENE
    DATA    DISC_NOT_A_DIRECTION
    DATA    12  ' W
    DATA    11  ' WSW
    DATA    DISC_NOT_A_DIRECTION
    DATA    DISC_NOT_A_DIRECTION
    DATA    15  ' NNW
    DATA    DISC_NOT_A_DIRECTION
    DATA    DISC_NOT_A_DIRECTION
    DATA    DISC_NOT_A_DIRECTION
    DATA    DISC_NOT_A_DIRECTION
    DATA    9   ' SSW
    DATA    5   ' ESE
    DATA    6   ' SE
    DATA    1   ' NNE
    DATA    DISC_NOT_A_DIRECTION
    DATA    2   ' NE
    DATA    DISC_NOT_A_DIRECTION
    DATA    13  ' WNW
    DATA    10  ' SW
    DATA    DISC_NOT_A_DIRECTION
    DATA    DISC_NOT_A_DIRECTION
    DATA    14  ' NW
    DATA    DISC_NOT_A_DIRECTION
    DATA    DISC_NOT_A_DIRECTION
    DATA    DISC_NOT_A_DIRECTION
    
' Converts a direction index into how much to change a decimal
DIR_TO_DEC_DELTA:
    DATA    1
    DATA    1
    DATA    0
    DATA    100
    DATA    100
    DATA    100
    DATA    0
    DATA    -1
    DATA    -1
    DATA    -1
    DATA    0
    DATA    -100
    DATA    -100
    DATA    -100
    DATA    0
    DATA    1
    
' Index table for masking the specified bit
BIT_MASK:
    DATA $0001, $0002, $0004, $0008
    DATA $0010, $0020, $0040, $0080
    DATA $0100, $0200, $0400, $0800
    DATA $1000, $2000, $4000, $8000

' Characters used for converting to hexadecimal
HEX_CHARS:
    DATA $10*8  ' "0"
    DATA $11*8
    DATA $12*8
    DATA $13*8
    DATA $14*8
    DATA $15*8
    DATA $16*8
    DATA $17*8
    DATA $18*8
    DATA $19*8  ' "9"
    DATA $21*8  ' "A"
    DATA $22*8
    DATA $23*8
    DATA $24*8
    DATA $25*8
    DATA $26*8  ' "F"
    
' Message table.  All of these MUST be 20 characters long
DEFAULT_MESSAGE:                DATA "    Inty ECS GPS    "
ENTRY_MODE_PROTOCOL_MESSAGE:    DATA "  Change Protocol   "
ENTRY_MODE_TIMEZONE_MESSAGE:    DATA "  Change Timezone   "
PROTOCOL_CHANGED_MESSAGE:       DATA "  Protocol Changed  "
TIMEZONE_CHANGED_MESSAGE:       DATA "  Timezone Changed  "
CANCELLED_MESSAGE:              DATA "     Cancelled      "
PACKET_BUFFER_OVERFLOW_MESSAGE: DATA "Packet Buf Overflow "
PACKET_INVALID_SIZE_MESSAGE:    DATA "  Bad Packet Size   "
PACKET_INVALID_MESSAGE:         DATA "     Bad Packet     "
RECV_BUFFER_FULL_MESSAGE:       DATA "  Recv Buffer Full  "
DUMP_DEBUG_ON_MESSAGE:          DATA " Dump Debug Info ON "
DUMP_DEBUG_OFF_MESSAGE:         DATA " Dump Debug Info OFF"

' GRAM cards
CARD_1_2:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X   X  "
    BITMAP  "XXX X   "
    BITMAP  "   X XX "
    BITMAP  "  X    X"
    BITMAP  "      X "
    BITMAP  "     XXX"
'CARD_3_4:
    BITMAP  "XXX     "
    BITMAP  " XX     "
    BITMAP  "  X  X  "
    BITMAP  "XXX X   "
    BITMAP  "   X X X"
    BITMAP  "  X  X X"
    BITMAP  "     XXX"
    BITMAP  "       X"
'POWER_CHAR:
    BITMAP  "  X     "
    BITMAP  " X X    "
    BITMAP  "X   X   "
    BITMAP  "        "
    BITMAP  "        "
    BITMAP  "        "
    BITMAP  "        "
    BITMAP  "        "
'UNDERSCORE_CHAR:
    BITMAP  "        "
    BITMAP  "        "
    BITMAP  "        "
    BITMAP  "        "
    BITMAP  "        "
    BITMAP  "        "
    BITMAP  "XXXXXXX "
    BITMAP  "        "


CARD_00:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XXX "
    BITMAP  "X X X X "
    BITMAP  "XXX X X "
    BITMAP  "    X X "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_01:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X  X  "
    BITMAP  "X X XX  "
    BITMAP  "XXX  X  "
    BITMAP  "     X  "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_02:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X  X  "
    BITMAP  "X X X X "
    BITMAP  "XXX   X "
    BITMAP  "     X  "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_03:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XXX "
    BITMAP  "X X   X "
    BITMAP  "XXX  XX "
    BITMAP  "      X "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_04:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X X X "
    BITMAP  "X X X X "
    BITMAP  "XXX XXX "
    BITMAP  "      X "
    BITMAP  "      X "
    BITMAP  "        "
'CARD_05:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XXX "
    BITMAP  "X X X   "
    BITMAP  "XXX XX  "
    BITMAP  "      X "
    BITMAP  "    XX  "
    BITMAP  "        "
'CARD_06:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X  XX "
    BITMAP  "X X X   "
    BITMAP  "XXX XXX "
    BITMAP  "    X X "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_07:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XXX "
    BITMAP  "X X   X "
    BITMAP  "XXX  X  "
    BITMAP  "     X  "
    BITMAP  "     X  "
    BITMAP  "        "
'CARD_08:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XXX "
    BITMAP  "X X X X "
    BITMAP  "XXX XXX "
    BITMAP  "    X X "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_09:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XXX "
    BITMAP  "X X X X "
    BITMAP  "XXX XXX "
    BITMAP  "      X "
    BITMAP  "      X "
    BITMAP  "        "
'CARD_0A:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X  X  "
    BITMAP  "X X X X "
    BITMAP  "XXX XXX "
    BITMAP  "    X X "
    BITMAP  "    X X "
    BITMAP  "        "
'CARD_0B:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XX  "
    BITMAP  "X X X X "
    BITMAP  "XXX XX  "
    BITMAP  "    X X "
    BITMAP  "    XX  "
    BITMAP  "        "
'CARD_0C:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XXX "
    BITMAP  "X X X   "
    BITMAP  "XXX X   "
    BITMAP  "    X   "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_0D:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XX  "
    BITMAP  "X X X X "
    BITMAP  "XXX X X "
    BITMAP  "    X X "
    BITMAP  "    XX  "
    BITMAP  "        "
'CARD_0E:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XXX "
    BITMAP  "X X X   "
    BITMAP  "XXX XX  "
    BITMAP  "    X   "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_0F:
    BITMAP  "XXX     "
    BITMAP  "X X     "
    BITMAP  "X X XXX "
    BITMAP  "X X X   "
    BITMAP  "XXX XX  "
    BITMAP  "    X   "
    BITMAP  "    X   "
    BITMAP  "        "
                 
CARD_10: 
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XXX "
    BITMAP  " X  X X "
    BITMAP  "XXX X X "
    BITMAP  "    X X "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_11:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X   X  "
    BITMAP  " X  XX  "
    BITMAP  "XXX  X  "
    BITMAP  "     X  "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_12:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X   X  "
    BITMAP  " X  X X "
    BITMAP  "XXX   X "
    BITMAP  "     X  "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_13:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XXX "
    BITMAP  " X    X "
    BITMAP  "XXX  XX "
    BITMAP  "      X "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_14:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  X X "
    BITMAP  " X  X X "
    BITMAP  "XXX XXX "
    BITMAP  "      X "
    BITMAP  "      X "
    BITMAP  "        "
'CARD_15:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XXX "
    BITMAP  " X  X   "
    BITMAP  "XXX XX  "
    BITMAP  "      X "
    BITMAP  "    XX  "
    BITMAP  "        "
'CARD_16:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X   XX "
    BITMAP  " X  X   "
    BITMAP  "XXX XXX "
    BITMAP  "    X X "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_17:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XXX "
    BITMAP  " X    X "
    BITMAP  "XXX  X  "
    BITMAP  "     X  "
    BITMAP  "     X  "
    BITMAP  "        "
'CARD_18:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XXX "
    BITMAP  " X  X X "
    BITMAP  "XXX XXX "
    BITMAP  "    X X "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_19:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XXX "
    BITMAP  " X  X X "
    BITMAP  "XXX XXX "
    BITMAP  "      X "
    BITMAP  "      X "
    BITMAP  "        "
'CARD_1A:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X   X  "
    BITMAP  " X  X X "
    BITMAP  "XXX XXX "
    BITMAP  "    X X "
    BITMAP  "    X X "
    BITMAP  "        "
'CARD_1B:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XX  "
    BITMAP  " X  X X "
    BITMAP  "XXX XX  "
    BITMAP  "    X X "
    BITMAP  "    XX  "
    BITMAP  "        "
'CARD_1C:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XXX "
    BITMAP  " X  X   "
    BITMAP  "XXX X   "
    BITMAP  "    X   "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_1D:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XX  "
    BITMAP  " X  X X "
    BITMAP  "XXX X X "
    BITMAP  "    X X "
    BITMAP  "    XX  "
    BITMAP  "        "
'CARD_1E:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XXX "
    BITMAP  " X  X   "
    BITMAP  "XXX XX  "
    BITMAP  "    X   "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_1F:
    BITMAP  " X      "
    BITMAP  "XX      "
    BITMAP  " X  XXX "
    BITMAP  " X  X   "
    BITMAP  "XXX XX  "
    BITMAP  "    X   "
    BITMAP  "    X   "
    BITMAP  "        "
    
CARD_F5:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX  XXX "
    BITMAP  "X   X   "
    BITMAP  "X   XX  "
    BITMAP  "      X "
    BITMAP  "    XX  "
    BITMAP  "        "
'CARD_F6:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX   XX "
    BITMAP  "X   X   "
    BITMAP  "X   XXX "
    BITMAP  "    X X "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_F7:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX  XXX "
    BITMAP  "X     X "
    BITMAP  "X    X  "
    BITMAP  "     X  "
    BITMAP  "     X  "
    BITMAP  "        "
'CARD_F8:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX  XXX "
    BITMAP  "X   X X "
    BITMAP  "X   XXX "
    BITMAP  "    X X "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_F9:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX  XXX "
    BITMAP  "X   X X "
    BITMAP  "X   XXX "
    BITMAP  "      X "
    BITMAP  "      X "
    BITMAP  "        "
'CARD_FA:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX   X  "
    BITMAP  "X   X X "
    BITMAP  "X   XXX "
    BITMAP  "    X X "
    BITMAP  "    X X "
    BITMAP  "        "
'CARD_FB:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX  XX  "
    BITMAP  "X   X X "
    BITMAP  "X   XX  "
    BITMAP  "    X X "
    BITMAP  "    XX  "
    BITMAP  "        "
'CARD_FC:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX  XXX "
    BITMAP  "X   X   "
    BITMAP  "X   X   "
    BITMAP  "    X   "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_FD:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX  XX  "
    BITMAP  "X   X X "
    BITMAP  "X   X X "
    BITMAP  "    X X "
    BITMAP  "    XX  "
    BITMAP  "        "
'CARD_FE:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX  XXX "
    BITMAP  "X   X   "
    BITMAP  "X   XX  "
    BITMAP  "    X   "
    BITMAP  "    XXX "
    BITMAP  "        "
'CARD_FF:
    BITMAP  "XXX     "
    BITMAP  "X       "
    BITMAP  "XX  XXX "
    BITMAP  "X   X   "
    BITMAP  "X   XX  "
    BITMAP  "    X   "
    BITMAP  "    X   "
    BITMAP  "        "
    
' Simulated Garmin TextOut UART data
UART_SIM_DATA:   ' IntyBASIC stores strings as Inty Chars, not as ASCII.  The 237 and 234 represent carriage return and linefeed
    DATA "@020604192139__________________________________________\237\234"    ' Test no signal and time
    DATA "@020604192140__________________________________________\237\234"
    DATA "@020604192141__________________________________________\237\234"
    DATA "@020604192241__________________________________________\237\234"
    DATA "@020604192341__________________________________________\237\234"
    DATA "@020604192441__________________________________________\237\234"
    DATA "@020604202441__________________________________________\237\234"
    DATA "@020604212441__________________________________________\237\234"
    DATA "@020604222441__________________________________________\237\234"
    DATA "@020604232441__________________________________________\237\234"
    DATA "@020605002441__________________________________________\237\234"
    DATA "@020606002441__________________________________________\237\234"
    DATA "@020607002441__________________________________________\237\234"
    DATA "@020707002441__________________________________________\237\234"
    DATA "@020807002441__________________________________________\237\234"
    DATA "@020907002441__________________________________________\237\234"
    DATA "@030907002441__________________________________________\237\234"
    DATA "@040907002441__________________________________________\237\234"
    DATA "@050907002441__________________________________________\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000D0001\237\234"    ' Test position
    DATA "@020604192514N4839544W12208417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514S4939434W12208417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514S5034534W12208417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514S5149534W12208417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12308477G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534E12408717G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534E12507417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534E12678417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00055E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00595E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005-05095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005-50095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000D0001\237\234"    ' Test position error
    DATA "@020604192514N4739534W12208417G060+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G700+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000D0001\237\234"    ' Test velocities
    DATA "@020604192514N4739534W12208417G005+00095E0000S0001D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000S0020D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0300D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N4000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095W0002N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0040N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095W0600N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095W8000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000D0020\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000U0300\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000U4000\237\234"
    DATA "@020604192514N4739534W12208417d005+00095E0000N0000D0001\237\234"    ' Test Status
    DATA "@020604192514N4739534W12208417D005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417g005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417G005+00095E0000N0000D0001\237\234"
    DATA "@020604192514N4739534W12208417S005+00095E0000N0000D0001\237\234"
    DATA $FFFF  ' Terminator
    
' Indexed by the index into the packet_buffer, the value is the position on the screen (only for Garmin's TextOut format)
TEXT_OUT_TO_SCREENPOS:  
    DATA    0
    DATA    SCREENPOS( COL_TIME +  2, ROW_TIME )
    DATA    SCREENPOS( COL_TIME +  3, ROW_TIME )
    DATA    SCREENPOS( COL_TIME +  5, ROW_TIME )
    DATA    SCREENPOS( COL_TIME +  6, ROW_TIME )
    DATA    SCREENPOS( COL_TIME +  8, ROW_TIME )
    DATA    SCREENPOS( COL_TIME +  9, ROW_TIME )
    DATA    SCREENPOS( COL_TIME + 11, ROW_TIME )
    DATA    SCREENPOS( COL_TIME + 12, ROW_TIME )
    DATA    SCREENPOS( COL_TIME + 14, ROW_TIME )
    DATA    SCREENPOS( COL_TIME + 15, ROW_TIME )
    DATA    SCREENPOS( COL_TIME + 17, ROW_TIME )
    DATA    SCREENPOS( COL_TIME + 18, ROW_TIME )
    DATA    SCREENPOS( COL_LATITUDE_VAL +  0, ROW_LATITUDE )
    DATA    SCREENPOS( COL_LATITUDE_VAL +  3, ROW_LATITUDE )
    DATA    SCREENPOS( COL_LATITUDE_VAL +  4, ROW_LATITUDE )
    DATA    SCREENPOS( COL_LATITUDE_VAL +  7, ROW_LATITUDE )
    DATA    SCREENPOS( COL_LATITUDE_VAL +  8, ROW_LATITUDE )
    DATA    SCREENPOS( COL_LATITUDE_VAL + 10, ROW_LATITUDE )
    DATA    SCREENPOS( COL_LATITUDE_VAL + 11, ROW_LATITUDE )
    DATA    SCREENPOS( COL_LATITUDE_VAL + 12, ROW_LATITUDE )
    DATA    SCREENPOS( COL_LONGITUDE_VAL +  0, ROW_LONGITUDE )
    DATA    SCREENPOS( COL_LONGITUDE_VAL +  2, ROW_LONGITUDE )
    DATA    SCREENPOS( COL_LONGITUDE_VAL +  3, ROW_LONGITUDE )
    DATA    SCREENPOS( COL_LONGITUDE_VAL +  4, ROW_LONGITUDE )
    DATA    SCREENPOS( COL_LONGITUDE_VAL +  7, ROW_LONGITUDE )
    DATA    SCREENPOS( COL_LONGITUDE_VAL +  8, ROW_LONGITUDE )
    DATA    SCREENPOS( COL_LONGITUDE_VAL + 10, ROW_LONGITUDE )
    DATA    SCREENPOS( COL_LONGITUDE_VAL + 11, ROW_LONGITUDE )
    DATA    SCREENPOS( COL_LONGITUDE_VAL + 12, ROW_LONGITUDE )
    DATA    SCREENPOS( COL_STATUS_VAL, ROW_STATUS )
    DATA    SCREENPOS( COL_ERR_VAL + 0, ROW_ERR )
    DATA    SCREENPOS( COL_ERR_VAL + 1, ROW_ERR )
    DATA    SCREENPOS( COL_ERR_VAL + 2, ROW_ERR )
    DATA    SCREENPOS( COL_ALTITUDE_VAL + 0, ROW_ALTITUDE )
    DATA    SCREENPOS( COL_ALTITUDE_VAL + 2, ROW_ALTITUDE )
    DATA    SCREENPOS( COL_ALTITUDE_VAL + 3, ROW_ALTITUDE )
    DATA    SCREENPOS( COL_ALTITUDE_VAL + 4, ROW_ALTITUDE )
    DATA    SCREENPOS( COL_ALTITUDE_VAL + 5, ROW_ALTITUDE )
    DATA    SCREENPOS( COL_ALTITUDE_VAL + 6, ROW_ALTITUDE )
    DATA    SCREENPOS( COL_VELOCITY_EW_VAL + 0, ROW_VELOCITY_EW )
    DATA    SCREENPOS( COL_VELOCITY_EW_VAL + 2, ROW_VELOCITY_EW )
    DATA    SCREENPOS( COL_VELOCITY_EW_VAL + 3, ROW_VELOCITY_EW )
    DATA    SCREENPOS( COL_VELOCITY_EW_VAL + 4, ROW_VELOCITY_EW )
    DATA    SCREENPOS( COL_VELOCITY_EW_VAL + 6, ROW_VELOCITY_EW )
    DATA    SCREENPOS( COL_VELOCITY_NS_VAL + 0, ROW_VELOCITY_NS )
    DATA    SCREENPOS( COL_VELOCITY_NS_VAL + 2, ROW_VELOCITY_NS )
    DATA    SCREENPOS( COL_VELOCITY_NS_VAL + 3, ROW_VELOCITY_NS )
    DATA    SCREENPOS( COL_VELOCITY_NS_VAL + 4, ROW_VELOCITY_NS )
    DATA    SCREENPOS( COL_VELOCITY_NS_VAL + 6, ROW_VELOCITY_NS )
    DATA    SCREENPOS( COL_VELOCITY_UD_VAL + 0, ROW_VELOCITY_UD )
    DATA    SCREENPOS( COL_VELOCITY_UD_VAL + 2, ROW_VELOCITY_UD )
    DATA    SCREENPOS( COL_VELOCITY_UD_VAL + 3, ROW_VELOCITY_UD )
    DATA    SCREENPOS( COL_VELOCITY_UD_VAL + 5, ROW_VELOCITY_UD )
    DATA    SCREENPOS( COL_VELOCITY_UD_VAL + 6, ROW_VELOCITY_UD )
    DATA    0
    DATA    0
    