( 
  Example dictionary extension words borrowed from https://github.com/hcchengithub/eforth-x86-64bits
  You can now successfully compile words into the dictionary. Control flow works does> is now working 
  properly.
) cr

: // a parse 2drop ; immediate
' // '\ !

hex

: compo $40 ;

: compile-only ( -- ) compo last @ @ or last @ ! ;
: char ( <char> -- char ) ( -- c ) bl word 1+ c@ ;
: [char] ( <char> -- char ) char [compile] literal ; immediate
: ['] ( <name> -- ca ) ' [compile] literal ; immediate
: variable ( -- ; <string> ) create 0 , ;
: does> ( -- ) compile dodoes 0 , ; immediate
: recurse last @ name> call, ; immediate
: constant create , does> @  ;

: >body ( ca -- pa ) 4 + ;
: to ( n -- ) ' >body ! ;
: +to ( n -- ) ' >body swap over @ + swap ! ;

8 constant #vocs
create vocs #vocs 1+ cells allot

create forth-wordlist
       0 ,
	   0 ,
	   0 ,

create current
	   forth-wordlist , forth-wordlist ,

: get-current ( -- wid ) current @ ;
: set-current ( wid -- ) current ! ;
: definitions ( -- ) context @ set-current ;

: >wid ( wid -- ) cell+ ;
: .wid ( wid -- )
  space dup 2 cells + @ ?dup if
   .id drop exit then 0 u.r ;
: !wid ( wid -- ) 2 cells + last @ swap ! ;
: wordlist ( -- wid ) align here 0 , dup current cell+ dup @ , ! 0 , ;

: order@ ( a -- u*wid u ) dup @ dup if >r cell+ recurse r> swap 1+ exit then nip ;
: get-order ( -- u*wid u ) vocs order@ ;
: set-order ( u*wid n -- ) ( 16.6.1.2197 )
  dup -1 = if
   drop forth-wordlist 1 then
  [ #vocs ]
  literal over u< abort" Over size of #vocs"
  vocs swap
  begin
   dup
  while
   >r swap over ! cell+ r>
   1-
  repeat
  swap !
  vocs @ context ! ;
: order ( -- )
  cr ." search:" get-order
  begin
   ?dup
  while
   swap .wid 1-
  repeat
  cr ." define:" get-current .wid ;
: only ( -- ) -1 set-order ;
: also ( -- ) get-order over swap 1+ set-order ;
: previous ( -- ) get-order swap drop 1- set-order ;
: >voc ( wid 'name' -- )
  create
     dup , !wid
  does>
   @ >r get-order swap drop r>
   swap set-order ;
: vocabulary ( 'name' -- ) wordlist >voc ;
: name?(vocs)
  >r get-order
  begin
   ?dup
  while
   swap r@ swap find ?dup if
    >r >r 1- for aft drop then next r> r> r> drop exit
   else
    drop
   then 1-
  repeat r> 0 ;

forth-wordlist >voc forth

