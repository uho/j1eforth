\ example dictionary extension words

hex

: recurse last @ name> call, ; immediate
: [char] char [compile] literal ; immediate
: ['] ' [compile] literal ; immediate
: does> compile dodoes ; immediate
: constant create , does> @ ;

: >body ( ca -- pa ) 4 + ;
: to ( n -- ) ' >body ! ;
: +to ( n -- ) ' >body swap over @ + swap ! ;

variable z
700 z !

9 to z

z @ .
