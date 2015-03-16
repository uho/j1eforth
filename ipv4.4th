( 
	I feel that the Kernel is at it's best for now and that I can proceed
	to do some other things. Note that version 1 is just to make the whole
	thing work, later on I might look at optimisation where I might have to move 
	some stuff around so that memory utilization and execution speed efficiency is 
	achieved.So far the Kernel works without needing tweaks.
	
	Work in progress: Implementing simple ipv4 for the j1eforth model
	
	7 project targets:
	
	  1. Add multi-tasking support to the Kernel
	  2. Modify j1 sim to use pcap interface for network tx and rx - 0%
	  3. ARP  - 0%
	  4. IP   - 0%
	  5. ICMP - 0%
	  6. UDP  - 0%
	  7. TCP  - 0%
	  
	Hopefully I will get time to do all this and also document the design of
	the j1eforth Kernel for those who are starting out with forth and also those
	who wish to tinker with the Kernel for fun.
)

hex

forth-wordlist >voc forth

1 cells ( init offset )
  cell- dup user follower ( address of next task's status )
  cell- dup user status   ( pass or wake )
  cell- dup user tos      ( top of stack )
  cell- dup user tid      ( back link tid )
  cell- dup user tf       ( throw frame )
  cell- dup user u1       ( free )
drop ( cleanup )

: 's ( tid a -- a ) ( index another task's local variable )
  follower  cell+ - swap @ + ;
: _pass ( -- ) ( hilevel absolute branch )
  r> @ >r ; compile-only
' _pass constant pass
: _wake ;
' _wake constant wake
: pause ;
: stop ( -- ) ( sleep current task )
  pass status ! pause ; compile-only
: get ( semaphore -- )
  pause ( remember your manners )
  dup @ status xor ( owner ? )
  if begin dup @ while pause repeat ( no, wait for release )
    status swap ! ( lock ) exit
  then drop ;
: release ( semaphore -- )
  dup @ status xor if drop exit then  0 swap ! ( unlock ) ;
: sleep ( tid -- ) ( sleep another task )
  pass swap status 's ! ;
: awake ( tid -- ) ( wake another task )
  wake swap status 's ! ;
: activate ( tid -- )
  dup 2@        ( tid sp rp )
  r> over !     ( save entry at rp )
  over !        ( save rp at sp )
  over tos 's ! ( save sp in tos )
  awake ; compile-only
: build ( tid -- )
  dup sleep                     ( sleep new task )
  follower @ over follower 's ! ( link new task )
  dup status 's follower !      ( link old task )
  dup tid 's ! ;
: char+ ;
: chan ( "name" -- ) ( -- a )
  variable [ 1 cells ] literal allot ;
: c> ( a -- n ) ( get data from a channel )
  >r ( save channel )
  begin r@ r@ @ xor ( channel empty ? )
  while pause ( yes, wait )
  repeat r@ cell+ @  0 r> ! ( get data, flag empty ) ;
: >c ( n a -- ) ( put data to a channel )
  >r ( save channel )
  begin r@ @ ( channel full ? )
  while pause ( yes, do not overwrite )
  repeat r@ r> 2! ( put data, flag full ) ;
: buff ( n "name" -- ) ( -- a ) ( only powers of 2 )
  create 0 , ( in and out index )
  dup 1- , allot ( mask and circular buffer ) ;
: b> ( a -- n ) ( get data from the buffer )
  >r ( save buffer )
  r@ char+ c@ ( out index )
  begin dup r@ c@ = ( buffer empty ? )
  while pause ( yes, wait )
  repeat dup r@ cell+ cell+ + @ ( get data )
  swap cell+ r@ cell+ c@ and
  r> char+ c! ( update out index ) ;
: >b ( n a -- ) ( put data to the buffer )
  >r ( save buffer )
  r@ c@ ( in index )
  dup cell+ r@ cell+ c@ and ( next in index )
  begin dup r@ char+ c@ = ( buffer full ? )
  while pause ( yes, wait )
  repeat r@ c! ( update in index )
  r> cell+ cell+ + ! ( put data ) ;

vocabulary ipv4.1

only forth also ipv4.1 definitions

variable active-struct

: field
   create over , +
  does>
   @ active-struct @ + ;

( ethernet header )

0
  6 field eth_dest      ( 48 bit source address )
  6 field eth_src       ( 48 bit destination address )
  2 field eth_type      ( 16 bit type )
constant eth_header%

0 constant eth_proto_ip
6 constant eth_proto_arp

: eth_set_broadcast_dest ;
: eth_set_my_mac_src ;
: eth_set_proto ;

( ip header )

0
  1 field ip_ver_ihl    (  4 bit version and 4 bit header length )
  1 field ip_tos        (  8 bit type of service )
  2 field ip_len        ( 16 bit length )
  2 field ip_id         ( 16 bit identification )
  2 field ip_frags      (  3 bit flags 13 bit fragment offset )
  1 field ip_ttl        (  8 bit time to live )
  1 field ip_proto      (  8 bit protocol number )
  2 field ip_checksum   ( 16 bit checksum )
  4 field ip_source     ( 32 bit source address )
  4 field ip_dest       ( 32 bit destination address )
constant ip_header%

 1 constant ip_proto_icmp
 6 constant ip_proto_tcp
17 constant ip_proto_udp

: ip_process ;
: ip_calc_checksum ;
: ip_create_packet ;
: ip_send ;
: ip_broadcast ;

( udp header )

0
  2 field udp_source    ( 16 bit source port )
  2 field udp_dest      ( 16 bit destination port )
  2 field udp_len       ( 16 bit length )
  2 field udp_checksum  ( 16 bit checksum )
constant udp_header%

( tcp header )

0
  2 field tcp_source    ( 16 bit source port )
  2 field tcp_dest      ( 16 bit destination port )
  4 field tcp_seq       ( 32 bit sequence number )
  4 field tcp_ack       ( 32 bit acknowledgement )
  1 field tcp_offset    (  8 bit offset )
  2 field tcp_flags     ( 16 bit flags )
  1 field tcp_window    (  8 bit window size )
  2 field tcp_checksum  ( 16 bit checksum )
  2 field tcp_urgent    ( 16 bit urgent pointer )
constant tcp_header%

( arp packet )

0
  2 field arp_hw        ( 16 bit hardware type )
  2 field arp_proto     ( 16 bit protocol )
  1 field arp_hwlen     (  8 bit hardware address length )
  1 field arp_protolen  (  8 bit protocol address length )
  2 field arp_op        ( 16 bit option )
  6 field arp_shw       ( 48 bit sender hardware address )
  4 field arp_sp        ( 32 bit sender protocol address )
  6 field arp_thw       ( 48 bit target hardware address )
  4 field arp_tp        ( 32 bit target protocol address )
constant arp_packet%

( arp cache )

0
  6 field ac_hw         ( 48 bit hardware address )  
  4 field ac_ip         ( 32 bit protocol address )
constant arp_cache%

create arp_cache arp_cache% 8 * here over allot swap erase


: arp_lookup ;
: arp_add ;
: arp_del ;

( icmp packet )

0
  1 field icmp_type     (  8 bits type )
  1 field icmp_code     (  8 bits code )
  2 field icmp_checksum ( 16 bits checksum )
  1 field icmp_data     (  8 bits data )
constant icmp_packet%

ip_header% .

arp_cache cell+ cell+ cell+ @ .

only forth also definitions
