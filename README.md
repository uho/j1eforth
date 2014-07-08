eForth for the j1 Simulator
-------------

### Memory model


       0h .--------------. .-------.
          |     cold     | |  80h  |<--- cold boot and default values
      80h '--------------' '-------'---> code dictionary, grows downwards
          |   code dict  | |       |
          '       |      ' |       |
          |  .... + .... | | 3df8h | <--- max dictionary size
          '       |      ' |       | 
          |   name dict  | |       |
    3e78h '--------------' '-------'---> name dictionary, grows upwards
    3e80h .--------------. .-------.
          |     user     | |  80h  |<--- user area size
    3f00h '--------------' '-------'
          |     tib      | | 100h  |<--- tib size
    4000h '--------------' '-------'
    


### Forth dictionary

 

     Dictionary header:
     
    .------------,------------,--------------------------------------.
    |            |            |                             ?cells   |
    |    cell    |    cell    |     byte    |   ?bytes    ?alignment |
    .------------.------------.-------------|-----------|------------.
    |  code ptr  |    prev    |  3 control  |   2^5 bytes max length |
    |            |   nfa ptr  |   5 count                            |
    '____________'____________'______________________________________'
          cfa          lfa                     nfa


 
#### Dictionary linkage

        1st                 2nd                              nth
     .--------.          .-------.                        .-------.
     | null   |      .---| lfa   |       .................| lfa   |
     '--------'--.   |   '-------'--.    |                '-------'--.
     | nfa       |<--'   | nfa      |<---'                | nfa      |
     '-----------'       '----------'                     '----------'
   

#### Threading

    Subroutine Threading
    
    : SQUARE DUP * ;
                           .--------------.
                  .--------|     JMP      | CFA
                  |        '--------------'
                  .        |     LFA      |     (name dictionary)
                  |        '---.----------'
                  .        | 6 |  SQUARE  | NFA
                  |        '---.----------'
                  |     .----------.----------.
     SQUARE'S PFA '---->| CALL DUP |  CALL *  | (code dictionary) -------- (exit) --
                        '-----.----'----.-----'                      ^
           .--------------.   |    ^     |   .----------------.      |
    (exec) |   DUP'S PFA  |<--'    '     '-->|    *'S  PFA    | -----' (exit)
           '--------------'        |         '----------------'
                |                  |               (exec)
                |                  |
                '------------------'
                      (exit)

#### Runtime variable PFA
 
 
         .--------.-----------------------.
    PFA  |  NOOP  | CALL DOVAR | CONTENTS | (code dictionary) --- (exit)
         '--------'------------'----------'
            |
            '---> This is a reserved cell for Behavioural pointer to (DOES>)

#### User variable PFA
 
        .---------.
    PFA | ADDRESS | (code dictionary) ----- (exit)
        '---------'
### Building and running the j1 Simulator
#### Compiling using gcc Mingw (Windows)

    gcc j1.c -o j1.exe

#### Creating flash image j1.bin

    gforth j1.4th
#### Running the Simulator

    j1.exe [optional argument]
    
    The argument to the simulator is an optional forth file that can be used to extend the dictionary and is 
    passed to the simulator as the first argument during startup
    
    Words to test in the simulator : 
    
    [ see , ' , compile , [compile] , ?branch , branch , call, .. and many more ]
    
    Have fun , modify and pass on , ( I would like to see it running on the actual j1 FPGA if possible )



