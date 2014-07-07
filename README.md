eForth for the j1 simulator
========

 Memory model
 
 .--------------. 0
 |     COLD     | 
 '--------------' 40
 |   CODE DICT  |    
 '              '
 |  ..........  | 
 '              ' 
 |   NAME DICT  | 
 '--------------' 3ef0
 |     USER     | 
 '--------------' 3f00
 |     TIB      | 
 '--------------' 3fb8 

 The Forth dictionary

 Dictionary header

 .------------,------------,--------------------------------------.
 |            |            |                             ?cells   |
 |    cell    |    cell    |     byte    |   ?bytes    ?alignment |
 .------------.------------.-------------|-----------|------------.
 |  code ptr  |    last    |  3 control  |   2^5 bytes max length |
 |            |   nfa ptr  |   5 count                            |
 '____________'____________'______________________________________'
      cfa          lfa                     nfa


 Dictionary linkage

    1st                2nd                              nth
 .--------.          .-------.                        .--------.
 | null   |      .---| lfa   |       .................| lfa    |
 '--------'--.   |   '-------'--.    |                '--------'--.
 | nfa       |<--'   | nfa      |<---'                | nfa       |
 '-----------'       '----------'                     '-----------'

 Threading

 Subroutine Threading

 Example :

 : SQUARE DUP * ;
                           .--------------.
                  .--------|     JMP      | CFA
                  |        '--------------'
                  .        |     LFA      |     (name dictionary)
                  |        '---.----------'
                  .        | 6 |  SQUARE  | NFA
                  |        '---.----------'
                  |     .----------.----------.
     SQUARE'S PFA '---->| CALL DUP |  CALL *  | (code dictionary) -------- (exit square)
                        '-----.----'----.-----'                      ^
         .----------------.   |    ^     |   .----------------.      |
  (exec) |    DUP'S PFA   |<--'    '     '-->|    *'S  PFA    | -----' (return to square's pfa)
         '----------------'        |         '----------------'
                |                  |               (exec)
                |                  |
                '------------------'
              (return to square's pfa)

 Runtime variable PFA
        .--------.-----------------------.
   PFA  |  NOOP  | CALL DOVAR | CONTENTS | (code dictionary) --- (exit variable)
        '--------'------------'----------'
            |
            '---> This is a Behavioural pointer for (DOES>)

 User variable PFA
        .---------.
   PFA  | ADDRESS | (code dictionary) ----- (exit variable)
        '---------'


Compile simulator on windows with MinGW

gcc j1.c -o j1.exe

Compile j1eForth using an existing gforth installation or any ANS compatible FORTH to genearte the j1.bin

gforth j1.4th

Run the simulator

j1.exe

or

j1.exe [argument] , where [argument] is any forth file to be compiled by the simulator into the dictionary.

enjoy ..

Words to test in the simulator [ see , ' , compile , [compile] , ?branch , branch , call, .. and many more ]

Have fun , modify and pass on , ( I would like to see it running on the actual j1 FPGA if possible )



