# Overview
**_Inty ECS GPS_** turns a Mattel Electronics's Intellivision gaming console into a display for GPS data.

The required hardware for this set is:
- Intellivision gaming console
- ECS (the Entertainment Computer System for the Intellivision)
- GPS device that outputs Garmin TextOut serial data, set to 1200 baud, 8N1
- A compatible TV display
- An Intellivision cartridge that contains the Inty_ECS_GPS program
- Cable that adapts ECS AUX to serial (Tx/Rx version)

A video demonstrating Inty ECS GPS is available at [https://www.youtube.com/watch?v=_8zZaOlPb58](https://www.youtube.com/watch?v=_8zZaOlPb58)

Example screenshot:

![screenshot of Inty ECS GPS executing](/shot0002.gif)

# Cable that adapts ECS AUX to serial (Tx/Rx version)
The wiring from the ECS's AUX plug (uses a 3.5mm audio plug) to a standard DE-9 serial connector is:
```
ECS     ECS     DE-9
Signal  Plug    pin
------  ------  ----
Tx      Tip     2 (optional, not used by Inty ECS GPS)
Rx      Ring    3
GND     Sleeve  5
```

# Author's Personal Note
This project grew out of my studying of the ECS's UART registers, which were poorly understood.  As far as is known, this program demonstrates the first time in decades that anyone has read data from the AUX port into the ECS (writing out the AUX port previously known from the ECS's printer code in ROM).  The eventual plan to present more formal register documentation later.  For eager/curious developers, the source code here contains preliminary definitions of the registers and bit-fields.  Since further research is still being done, expect some of the bit-field names to change in the future.  For example, one unnamed bit field has recently been observed to change values but is still not fully understood at this time.

# Code and Build instructions
Inty_ECS_GPS.bas is the source code for this program.  This program was written as a quick demo program.  Thus:
- The source code quality is **_prototype grade_** in that it is _not_ professionally constructed nor fully commented.
- Some features mentioned or stubbed out in the source code were _not_ implemented in the end.  These include supporting other GPS protocols, supporting timezones, supporting imperial units, etc.
- It was written using both IntyBASIC (a modern _compiled_, non-interpreted BASIC for the Intellivision) and in CP1610 assembly code for some optimizations.

This program can be compiled in one of two modes:
- Operates with real GPS hardware that outputs Garmin TextOut serial data
- Operates in test mode that uses cycles through a list of canned Garmin TextOut data.  No GPS hardware necessary.
For hardware mode, edit the \*.bas file and set `CONST UART_SIMULATION = 0`.  For test mode, set `CONST UART_SIMULATION = 1`

The 2 main tools are necessary to build are the ***IntyBASIC*** tools (tested with version 1.4.0) at the ***jzIntv*** tools (tested with version zintv-20181225-win32).  Newer versions are expected to be compatible, but not confirmed.
- IntyBASIC can be downloaded from [https://nanochess.org/intybasic.html](https://nanochess.org/intybasic.html).
- jzIntv can be downloaded from [http://spatula-city.org/~im14u2c/intv/](http://spatula-city.org/~im14u2c/intv/).

To build Inty_ECS_GPS.rom (and \*.bin and \*.cfg), use the following Bash script.  Edit the paths at the top to match your local tool installation path and versions.
```
#!/bin/bash
FILENAME=Inty_ECS_GPS
BASIC_PATH='../../Tools and Docs/intybasic_compiler_v1.4.0'
SDK_PATH='../../Tools and Docs/jzintv-20181225-win32/bin'

"$BASIC_PATH/IntyBASIC.exe" --cc3 --title "Inty ECS GPS" $FILENAME.bas $FILENAME.asm "$BASIC_PATH"
if [ $? -ne 0 ]; then exit $?; fi

mv $FILENAME.asm $FILENAME-org.asm
sed 's/ORG $8040, $8040/ORG $C040, $C040/g' $FILENAME-org.asm > $FILENAME.asm

"$SDK_PATH/as1600.exe" --cc3 -l $FILENAME.lst -s $FILENAME.sym -j $FILENAME.smap -m -o $FILENAME $FILENAME.asm
if [ $? -ne 0 ]; then exit $?; fi

"$BASIC_PATH/IntySmap.exe" $FILENAME.smap 
if [ $? -ne 0 ]; then exit $?; fi

# Add -d to enable debugging
"$SDK_PATH/jzintv.exe" $1 $2 $3 $4 $5 -z3 $FILENAME --src-map=$FILENAME.smap --sym-file=$FILENAME.sym
```
