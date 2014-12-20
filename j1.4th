\   eForth 1.0 for j1 Simulator by Edward A., July 2014 
\   Much of the code is derived from the following sources:
\      j1 Cross-compiler by James Bowman August 2010
\     8086 eForth 1.0 by Bill Muench and C. H. Ting, 1990

vocabulary meta.1
vocabulary macro.1
vocabulary target.1

: in-macro.1 only macro.1 also definitions also meta.1 ;
: in-target.1 only target.1 also definitions also macro.1 also meta.1 ;
: in-meta.1 only forth also meta.1 also definitions ;

hex

only forth also meta.1 also definitions

create tflash 1000 cells here over erase allot

variable tdp

: there tdp @ ;
: tc! tflash + c! ;
: tc@ tflash + c@ ;
: t! over ff and over tc! swap 8 rshift swap 1+ tc! ;
: t@ dup tc@ swap 1+ tc@ 8 lshift or ;
: talign there 1 and tdp +! ;
: tc, there tc! 1 tdp +! ;
: t, there t! 2 tdp +! ;
: $lit [char] " word count dup tc, 0 ?do count tc, loop drop talign ;
: tallot tdp +! ;
: org tdp ! ;

: t    0000 ;
: n    0100 ;
: t+n  0200 ;
: t&n  0300 ;
: t|n  0400 ;
: t^n  0500 ;
: ~t   0600 ;
: n==t 0700 ;
: n<t  0800 ;
: n>>t 0900 ;
: t-1  0a00 ;
: rt   0b00 ;
: [t]  0c00 ;
: n<<t 0d00 ;
: dsp  0e00 ;
: nu<t 0f00 ;

: t->n 0080 or ;
: t->r 0040 or ;
: n->[t] 0020 or ;
: d-1  0003 or ;
: d+1  0001 or ;
: r-1  000c or ;
: r-2  0008 or ;
: r+1  0004 or ;

: alu  6000 or t, ;

: ret   t 1000 or r-1 alu ;
: bran   2/ 0000 or t, ;
: ?bran  2/ 2000 or t, ;
: call 2/ 4000 or t, ;

: lit
   dup 8000 and if
    ffff xor recurse
     ~t alu
   else
    8000 or t,
   then ;

0001 constant =ver
0003 constant =ext
0040 constant =comp
0080 constant =imed
7f1f constant =mask
0002 constant =cell
0010 constant =base
0008 constant =bksp
000a constant =lf
000d constant =cr
4000 constant =call

4000 constant =em
0000 constant =cold

40 2 * constant =us

=em 100 - constant =tib
=tib =us - constant =up
=up 8 - constant =name
=cold =us + constant =code

variable tlast
variable tnp
variable tcode
variable tuser
variable tlen
variable tcp

: thead
  talign
   there tcode !
   bl word count dup 1f and =cell / tlen !
    tnp @ tlen @ 3 + =cell * - tnp !
  tnp @ org
    tcode @ t, tlast @ t,
    there tlast !
	dup tc, 0 ?do
     count tc, loop drop talign
  tcode @ org ;
: twords
   cr tlast @
   begin
      dup tflash + count 1f and type space =cell - t@
   ?dup 0= until ;
: immediate tlast @ tflash + dup c@ =imed or swap c! ;
: compile-only tlast @ tflash + dup c@ =comp or swap c! ;
: code
  >in @ thead >in !
   in-target.1 create
   talign tnp @ , does> @ t@ talign call ;
: $user
  >in @ thead >in !
   in-target.1 create
     talign tuser @ dup , lit ret =cell tuser +! in-meta.1 does> @ lit ;

      0 tlast !
  =name tnp !
  =code tcode !
    =up tuser !
      0 tlen !
      0 tcp !

: save-target
  bl word count w/o create-file throw >r
   tflash =em r@ write-file throw r> close-file bye ;

: hex# ( u -- addr len )  0 <# base @ >r hex =lf hold # # # # r> base ! #> ;

: save-hex ( <name> -- )
  bl word count  w/o create-file throw
  =em 0 
  DO 
     I t@  over >r hex# r> write-file throw
  2 +LOOP
  close-file throw ;

only forth also macro.1 also definitions also meta.1

: (begin) there ;
: (until) ?bran ;

: (if)     there 0 ?bran ;
: (skip)   there 0 bran ;
: (then)   (begin) 1 rshift over t@ or swap t! ;
: (else)   (skip) swap (then) ;
: (while)  (if) swap ;
: (repeat) bran (then) ;
: (again)  bran ;
: (aft)    drop (skip) (begin) swap ;

: drop n d-1 alu ;
: 1- t-1 alu ;
: >r n t->r r+1 d-1 alu ;
: r> rt t->n r-1 d+1 alu ;
: r@ rt t->n d+1 alu ;

: exit ret ;

: (for)    >r (begin) ;
: (next)   r@ (while) r> 1- >r (repeat) r> drop ;

: _' ' ;
: _@ w@ ;
: >_body >body ;
: ( [char] ) parse 2drop ;
: _2/ 1 rshift ;
: _s .s ;

=cold org

0 t,

there constant =uzero 
   =base t,
   0 t,
   0 t,
   0 t,
   =tib t,
   0 t,
   0 t,
   0 t,
   0 t,
   0 t,
   0 t,
   0 t,
   0 t,
there constant =ulast 
=ulast =uzero - constant =udiff

=code org

code @ ( a -- w ) [t] alu ret
code ! ( w a -- )
   t n->[t] d-1 alu
   n d-1 alu ret
code dsp0 dsp t->n d+1 alu ret
code noop t alu ret
code + t+n d-1 alu ret
code xor t^n d-1 alu ret
code and t&n d-1 alu ret
code or t|n d-1 alu ret
code invert ~t alu ret
code = n==t d-1 alu ret
code < n<t d-1 alu ret
code u< nu<t d-1 alu ret
code swap n t->n alu ret
code dup t t->n d+1 alu ret
code drop n d-1 alu ret
code over n t->n d+1 alu ret
code nip t d-1 alu ret
code lshift n<<t d-1 alu ret
code rshift n>>t d-1 alu ret
code 1- t-1 alu ret
code 2r> rt t->n r-1 d+1 alu
    rt t->n r-1 d+1 alu
    n t->n alu ret
code 2>r n t->n alu
    n t->r r+1 d-1 alu
    n t->r r+1 d-1 alu ret
code 2r@ rt t->n r-1 d+1 alu
    rt t->n r-1 d+1 alu
    n t->n d+1 alu
    n t->n d+1 alu
    n t->r r+1 d-1 alu
    n t->r r+1 d-1 alu
    n t->n alu ret
code unloop
    t r-1 alu
    t r-1 alu ret
code dup@ [t] t->n d+1 alu ret
code dup>r t t->r r+1 alu ret
code 2dupxor t^n t->n d+1 alu ret
code 2dup= n==t t->n d+1 alu ret
code !nip t n->[t] d-1 alu ret
code 2dup! t n->[t] alu ret
code up1 t d+1 alu ret
code down1 t d-1 alu ret
code copy n alu ret
code >r n t->r r+1 d-1 alu ret
code r> rt t->n r-1 d+1 alu ret
code r@ rt t->n d+1 alu ret
code <> = invert ret
code 0< 0 lit < ret
code 0= 0 lit = ret
code > swap < ret
code 0> 0 lit swap < ret
code >= < invert ret
code tuck swap over ret
code -rot swap >r swap r> ret
code 2/ 1 lit rshift ret
code 2* 1 lit lshift ret
code 1+ 1 lit + ret
code sp@ dsp0 ff lit and ret
code rp@ dsp0 8 lit rshift ret
code rp! drop ret
code sp! drop ret
code execute ( ca -- ) >r ret
code c@ ( b -- c )
  dup @ swap 1 lit and (if)
   8 lit rshift (else) ff lit and (then) ret
code c! ( c b -- )
  swap ff lit and dup 8 lit lshift or swap
   tuck dup @ swap 1 lit and 0 lit = ff lit xor
   >r over xor r> and xor swap ! ret
code um+ ( w w -- w cy )
  over over + >r
   r@ 0 lit >= >r
    over over and
	 0< r> or >r
   or 0< r> and invert 1+
  r> swap ret
code dovar ( -- a ) r> ret compile-only
code up ( -- a ) dovar =up t, ret
code douser ( -- a ) r> @ up @ + ret compile-only
$user base
$user tmp
$user >in
$user #tib
   =cell tuser +!
$user 'eval
$user 'abort
$user hld
$user handler
$user context
$user cp
$user np
$user last
code ?dup ( w -- w w | 0 ) dup (if) dup (then) ret
code rot ( w1 w2 w3 -- w2 w3 w1 ) >r swap r> swap ret
code 2drop ( w w -- ) drop drop ret
code 2dup ( w1 w2 -- w1 w2 w1 w2 ) over over ret
code negate ( n -- -n ) invert 1+ ret
code dnegate ( d -- -d )
  invert >r invert 1 lit um+ r> + ret
code - ( n1 n2 -- n1-n2 ) negate + ret
code abs ( n -- n ) dup 0< (if) negate (then) ret
code max ( n n -- n ) 2dup > (if) drop (else) nip (then) ret
code min ( n n -- n ) 2dup < (if) drop (else) nip (then) ret
code within ( u ul uh -- t ) over - >r - r> u< ret
code um/mod ( udl udh u -- ur uq )
   2dup u< (if)
    negate f lit
    (for) >r dup um+ >r >r dup um+ r> + dup
     r> r@ swap >r um+ r> or (if)
      >r drop 1+ r>
     (else)
      drop
     (then) r>
    (next) drop swap exit
   (then) drop 2drop -1 lit dup ret
code m/mod ( d n -- r q )
   dup 0< dup >r (if)
    negate >r dnegate r>
   (then) >r dup 0< (if)
    r@ +
   (then) r> um/mod r> (if)
    swap negate swap (then) ret
code /mod ( n n -- r q ) over 0< swap m/mod ret
code mod ( n n -- r ) /mod drop ret
code / ( n n -- q ) /mod nip ret
code um* ( u u -- ud )
   0 lit swap f lit
   (for) dup um+ >r >r dup um+ r> + r> (if)
    >r over um+ r> + (then)
   (next) rot drop ret
code * ( n n -- n ) um* drop ret
code m* ( n n -- d )
   2dup xor 0< >r abs swap abs um* r> (if)
    dnegate (then) ret
code */mod ( n1 n2 n3 -- r q ) >r m* r> m/mod ret
code */ ( n1 n2 n3 -- q ) */mod nip ret
code cell+ ( a -- a ) =cell lit + ret
code cell- ( a -- a ) =cell lit - ret
code cells ( n -- n ) 1 lit lshift ret
code aligned ( b -- a )
   dup 0 lit =cell lit um/mod drop dup (if)
    =cell lit swap - (then) + ret
code bl ( -- 32 ) 20 lit ret
code >char ( c -- c )
   7f lit and dup 7f lit bl within (if)
    drop 5f lit
   (then) ret
( code pick ( ... +n -- ... w ) ( 1+ cells sp@ + @ ret   \ no adressing into J1 stack ! )
code +! ( n a -- ) tuck @ + swap ! ret
code 2! ( d a -- ) swap over ! cell+ ! ret
code 2@ ( a -- d ) dup cell+ @ swap @ ret
code count ( b -- b +n ) dup 1+ swap c@ ret
code here ( -- a ) cp @ ret
code pad ( -- a ) here 50 lit + ret
code tib ( -- a ) #tib cell+ @ ret
code @execute ( a -- ) @ ?dup (if) execute (then) ret
code cmove ( b1 b2 u -- ) (for) (aft) >r count r@ c! r> 1+ (then) (next) 2drop ret
code fill ( b u c -- )
   swap (for) swap (aft) 2dup c! 1+ (then) (next) 2drop ret
code erase 0 lit fill ret
code -trailing ( b u -- b u )
   (for) (aft) dup r@ + c@ bl xor
    (if) r> 1+ exit (then) (then)
   (next) 0 lit ret
code pack$ ( b u a -- a ) aligned dup >r 2dup c! 1+ 2dup + 0 lit swap ! swap cmove r> ret
code digit ( u -- c ) 9 lit over < 7 lit and + 30 lit + ret
code extract ( n base -- n c ) 0 lit swap um/mod swap digit ret
code <# ( -- ) pad hld ! ret
code hold ( c -- ) hld @ 1- dup hld ! c! ret
code # ( u -- u ) base @ extract hold ret
code #s ( u -- 0 ) (begin) # dup (while) (repeat) ret
code sign ( n -- ) 0< (if) 2d lit hold (then) ret
code #> ( w -- b u ) drop hld @ pad over - ret
code str ( n -- b u ) dup >r abs <# #s r> sign #> ret
code hex ( -- ) 10 lit base ! ret
code decimal ( -- ) a lit base ! ret
code digit? ( c base -- u t )
   >r
   30 lit - 9 lit  over < (if)
    dup 20 lit > (if)
	 20 lit  -
	(then)
	7 lit - dup a lit  < or
   (then) dup r> u< ret
code number? ( a -- n T | a F )
   base @ >r 0 lit over count
   over c@ 24 lit = (if)
    hex swap 1+ swap 1- (then)
   over c@ 2d lit = >r
   swap r@ - swap r@ + ?dup (if)
    1-
    (for) dup >r c@ base @ digit?
      (while) swap base @ * + r> 1+
    (next) r@ nip (if)
	  negate (then) swap
     (else) r> r> 2drop 2drop 0 lit
      (then) dup
   (then) r> 2drop r> base ! ret
code ?key ( -- T | F )
   f001 lit @ 1 lit and 0= invert ret  ( status/f001: { 0 0 0 0 0 0 TxBusy RXAv } )
code key ( -- c )
   (begin)
     ?key
	(until) f000 lit @ ret
code emit ( c -- ) (begin) f001 lit @ 2 lit and 0= (until) f000 lit ! ret
code nuf? ( -- t ) ?key dup (if) 2drop key =cr lit = (then)  ret
code space ( -- ) bl emit ret
code spaces ( +n -- ) 0 lit max (for) (aft) space (then) (next) ret
code type ( b u -- ) (for) (aft) count emit (then) (next) drop ret
code cr ( -- ) =cr lit emit =lf lit emit ret
code do$ ( -- a )
   r> r@ r> count + aligned >r swap >r ret compile-only
code $"| ( -- a ) do$ ret compile-only
code ."| ( -- ) do$ count type ret compile-only
code .r ( n +n -- ) >r str r> over - spaces type ret
code u.r ( u +n -- ) >r <# #s #> r> over - spaces type ret
code u. ( u -- ) <# #s #> space type ret
code . ( w -- ) base @ a lit xor (if) u. exit (then) str space type ret
code ? ( a -- ) @ . ret
code (parse) ( b u c -- b u delta ; <string> )
  tmp ! over >r dup (if)
    1- tmp @ bl = (if)
     (for)
	  count tmp @ swap - 0< invert r@ 0> and
	 (while) (next) r> drop 0 lit dup exit
	 (then) 1- r>
    (then) over swap
     (for)
	  count tmp @ swap - tmp @ bl = (if)
	   0< (then)
	 (while) (next) dup >r (else) r> drop dup >r 1-
     (then) over - r> r> - exit
   (then) over r> - ret
code parse ( c -- b u ; <string> )
   >r
   tib >in @ +
   #tib @ >in @ - r>
   (parse)
   >in +! ret
code .( ( -- ) 29 lit parse type ret immediate
code ( ( -- ) 29 lit parse 2drop ret immediate
code \ ( -- ) #tib @ >in ! ret immediate
code char ( -- c ) bl parse drop c@ ret
code token ( -- a ; <string> )
   bl parse 1f lit min np @ over - cell-
   dup 1 lit and - pack$ ret
code word ( c -- a ; <string> ) parse here pack$ ret
code name> ( na -- ca ) cell- cell- @ ret
code same? ( a a u -- a a f \ -0+ )
   1-
   (for) (aft) over r@ + c@
     over r@ + c@ - ?dup
   (if) r> drop exit (then) (then)
   (next) 0 lit ret
code find ( a va -- ca na | a F )
   swap
   dup c@ tmp !
   dup @ >r
   cell+ swap
   (begin) @ dup
   (if) dup @ =mask lit and r@ xor
     (if) cell+ -1 lit (else) cell+ tmp @ same? (then)
    (else) r> drop swap cell- swap exit
   (then)
   (while) 2 lit cells -
   (repeat) r> drop nip cell- dup name> swap ret
code name? ( a -- ca na | a F ) context find ret
code ^h ( bot eot cur -- bot eot cur )
   >r over r@ < dup (if)
    =bksp lit dup emit space
	emit (then) r> + ret
code tap ( bot eot cur c -- bot eot cur )
   dup emit over c! 1+ ret
code ktap ( bot eot cur c -- bot eot cur )
   dup =cr lit xor (if)
    =bksp lit xor (if)
     bl tap
    (else) ^h (then) exit
   (then) drop nip dup ret
code accept ( b u -- b u )
   over + over
   (begin)
    2dup xor
   (while)
      key dup bl - 7f lit u< (if) tap (else) ktap (then)
   (repeat) drop over - ret
code query ( -- ) tib 50 lit accept #tib ! drop 0 lit >in ! ret
code abort" (if) do$
code abort1 space count type 3f lit emit cr 'abort @execute
code abort2 (then) do$ drop ret
code forget ( -- )
   token name? ?dup (if)
    1- dup cp !
     @ dup context ! last !
     drop exit
   (then) abort1 ret
code $interpret ( a -- )
   name? ?dup (if)
    @ =comp lit and
     abort" $lit  compile-only" execute exit
   (then)
   number? (if)
     exit (then) abort1 ret
code [ ( -- ) _' $interpret >_body _@ t@ lit 'eval ! ret immediate
code .ok ( -- )
   _' $interpret >_body _@ t@ lit 'eval @ = (if)
    ."| $lit  ok" cr
   (then) ret
code eval ( -- )
   (begin)
    token dup c@
   (while)
    'eval @execute
   (repeat) drop .ok ret
code $eval ( a u -- )
   >in @ >r
	#tib @ >r
	 tib >r
       _' >in >_body _@ lit dup
	   0 lit swap !
       cell+ 2dup !
       cell+ swap drop swap over !
       >r eval r>
	 r> over ! cell-
	r> over ! cell-
   r> swap ! ret compile-only
code preset ( -- ) =tib lit #tib cell+ ! ret
code quit ( -- )
   [ (begin)
	 query eval
   (again) ret
code abort preset .ok quit ret
code ' ( -- ca ) token name? (if) exit (then) abort1 ret
code allot ( n -- ) cp +! ret
code , ( w -- ) here dup cell+ cp ! ! ret 
code call, ( ca -- ) 1 lit rshift 4000 lit or , ret compile-only
code ?branch ( ca -- ) 1 lit rshift 2000 lit or , ret compile-only
code branch ( ca -- ) 1 lit rshift 0000 lit or , ret compile-only
code [compile] ( -- ; <string> ) ' call, ret immediate
code compile ( -- ) r> dup @ , cell+ >r ret compile-only
code literal ( w -- ) 8000 lit or , ret immediate
code $," ( -- ) 22 lit parse here aligned pack$ c@ 1+ allot ret
code for ( -- a ) compile >r here ret compile-only immediate
code begin ( -- a ) here ret compile-only immediate
code until ( a -- ) ?branch ret compile-only immediate
code again ( a -- ) branch ret compile-only immediate
code if ( -- A ) here 0 lit ?branch ret compile-only immediate
code ahead ( -- A )  ret compile-only immediate
code then ( A -- ) here 1 lit rshift over @ or swap ! ret compile-only immediate
code repeat ( A a -- ) branch then ret compile-only immediate
code skip here 0 lit branch ret compile-only immediate
code aft ( a -- a A ) compile drop skip begin swap ret compile-only immediate
code else ( A -- A ) skip swap then ret compile-only immediate
code while ( a -- A a ) if swap ret compile-only immediate
code next ( a -- )
   compile r@ 
   while
    compile r> compile 1- compile >r
   repeat compile r> compile drop ret compile-only immediate
code $" ( -- ; <string> ) compile $"| $," ret immediate
code ." ( -- ; <string> ) compile ."| $," ret immediate
code ?unique ( a -- a )
   dup name? (if) ."| $lit  reDef " over count type (then) drop ret
code $,n ( na -- )
   dup c@ (if)
    ?unique
    dup last !
    here aligned swap
    cell-
    context @
    over !
    cell- dup np ! ! exit
   (then) drop $"| $lit 'name" abort1
   ret
code $compile ( a -- )
   name? ?dup (if)
    c@ =imed lit and (if)
	 execute
	(else)
	 call,
	(then) exit
   (then)
   number? (if)
     literal exit (then) abort1 ret
code overt ( -- ) last @ context ! ret
code ; ( -- ) compile exit [ overt ret compile-only immediate
code ] ( -- ) _' $compile >_body _@ t@ lit 'eval ! ret
code : ( -- ; <string> ) token $,n ] ret
code immediate ( -- ) =imed lit last @ c@ or last @ c! ret
code compile-only ( -- ) =comp lit last @ c@ or last @ c! ret
code user ( u -- ; <string> ) token $,n overt compile douser , ret
code create ( -- ; <string> ) token $,n overt compile noop compile dovar ret
code dodoes last @ name> r> 1 lit rshift over cell+ ! dup cell+ cell+ 8000 lit or swap ! ret
code variable ( -- ; <string> ) create 0 lit , ret
code _type ( b u -- ) (for) (aft) count >char emit (then) (next) drop ret
code dm+ ( a u -- a )
  over 4 lit u.r space
  (for) (aft) count 3 lit u.r (then) (next) ret
code dump ( a u -- )
  base @ >r hex 10 lit /
  (for) cr 10 lit 2dup dm+ -rot
   2 lit spaces _type
  (next) drop r> base ! ret
code .s ( ... -- ... )
  sp@ (begin) sp@ 1 lit - (while) swap >r (repeat)
  (begin) dup (while) r> dup . swap 1 lit - (repeat) drop 
  ."| $lit  <tos" ret
code >name ( ca -- na | F )
  context
  (begin)
   @ dup 
  (while)
   2dup name> xor
   (while)
   cell-
  (repeat) nip exit (then) 2drop 0 lit ret
code .id ( na -- )
  ?dup (if)
   count 1f lit and type exit (then)
    cr ."| $lit {noName}" ret
code see ( -- ; <string> )
  ' cr
  (begin)
    dup @ ?dup 700c lit xor
  (while)
    4000 lit xor 1 lit lshift
	>name ?dup (if)
     space .id
	(else)
	  dup @ u. 
	(then)
	cell+
  (repeat) drop ret 
code words ( -- )
  cr context
  (begin)
   @ ?dup
  (while)
   dup .id space cell-
  (repeat) ret
code ver ( -- n ) =ver lit 100 lit * =ext lit + ret
code hi ( -- )
  cr ."| $lit eForth j1 v"
	base @ hex
	ver <# # # 2e lit hold # #>
	type base ! cr ret
code 'boot ( -- a ) dovar hi ret
code cold ( -- )
  =uzero lit =up lit =udiff lit cmove
  preset
  'boot execute
  overt
  4000 lit 8000 lit $eval
  quit 
  _' cold >_body _@ t@ _2/ t, ret

_' $interpret >_body _@ t@ c t!
_' abort >_body _@ t@ e t!
_' cold >_body _@ t@ _2/ =cold t!

there     16 t!
tnp   _@  18 t!
tlast _@  1a t!

save-hex j1.hex
save-target j1.bin
