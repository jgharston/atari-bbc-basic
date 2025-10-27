# BBC BASIC for the Atari XL/XE

BASIC dialect is described in the
[BBC Microcomputer User Guid](http://regregex.bbcmicro.net/BPlusUserGuide-1.07.pdf).

[Creative Assembler](https://acorn.huininga.nl/pub/docs/manuals/Acornsoft/Creative%20Assembler%20-%20How%20To%20Write%20Arcade%20Games.pdf).


### Differences

* The BREAK key interrupts like ESCape on the BBC. RESET key does coldstart, but you can recover your listing with OLD.

* SOUND works like Atari BASIC, i.e. SOUND voice, pitch, distortion, volume. voice+8 is right pokey if a stereo upgrade is installed

* MODE works like GRAPHICS 0-15. +16 without text window

* POINT, PLOT, DRAW, MOVE work with the Atari coordinate system(!)

* POINT works like LOCATE, i.e. A = POINT(42,42)

* PLOT action, x, y support action 4 (MOVE), 5 (DRAW), and 69 (PLOT POINT)

* MOVE x,y shortcut for PLOT 4,x,y  (Atari BASIC POSITION)

* DRAW x,y shortcut for PLOT 5,x,y  (Atari BASIC DRAWTO)

* COLOR selects the drawing color 0-4

* GCOL acts like SETCOLOR, but takes two arguments. The color number 0-4, and what value to set it to (&00-&ff)

* ADVAL(255) to check if there's a pending keypress, all other values return 0

* OPENUP, EXT# and PTR# do not work because there's no byte accurate way
to do fseek/ftell with DOS 2.5.

* ENVELOPE does nothing

* MOS vectors are in page &2f instead of &ff so you can call them from assembly if you want.

```
OSFIND = &2FCE 
OSBPUT = &2FD4 
OSBGET = &2FD7 
OSARGS = &2FDA 
OSFILE = &2FDD 
OSRDCH = &2FE0 
OSASCI = &2FE3 
OSNEWL = &2FE7 
OSWRCH = &2FEE 
OSWORD = &2FF1 
OSBYTE = &2FF4 
OSCLI  = &2FF7 
```

### Atari characters

You can print all the Atari control characters, *except* CTRL-M, which is
the internal end-of-line character. It was not possible to change this to
the Atari equivalent 155 (&9b) because that would clash with tknCOS, and
internally BBC BASIC sometimes scans a tokenized line and stops when it
encounters the EOL character (&0D). This would fail if EOL and tknCOS are
the same. If you really need the 'overscore' character, you can either poke &4D directly into the screen memory, or bypass OSWRCH and write to CIO channel #0 directly.

Sometimes you need to type the ~ (tilde) if you want to print a hexadecimal
value. You can type them by pressing ESC and then BACKSPACE.

PRINT ~42

