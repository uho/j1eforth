(
	I feel that the Kernel is at it's best for now and that I can proceed
	to do some other things. Note that version 1 is just to make the whole
	thing work, later on I might look at optimisation where I might have to move
	some stuff around so that memory utilization and execution speed efficiency is
	achieved.So far the Kernel works without needing tweaks.

	Work in progress: Implementing simple ipv4 for the j1eforth model

	7 project targets:

	  1. Add multi-tasking support to the Kernel - 0%
	  2. Modify j1 sim to use pcap interface for network tx and rx - 0%
	  3. ARP  - 0%
	  4. ICMP - 0%
	  5. IP   - 0%
	  6. UDP  - 0%
	  7. TCP  - 0%

	Hopefully I will get time to do all this and also document the design of
	the j1eforth Kernel for those who are starting out with forth and also those
	who wish to tinker with the Kernel for fun.
)

hex

forth-wordlist >voc forth

vocabulary ipv4.1
only forth also ipv4.1

ipv4.1 definitions

variable active_struct

: field
   create over , +
  does>
   @ active_struct @ + ;

( ethernet frame )

0
  6 field eth_dest      ( 48 bit source address )
  6 field eth_src       ( 48 bit destination address )
  2 field eth_type      ( 16 bit type )
constant eth_frame%

( arp message )

0
  2 field arp_hw        ( 16 bit hw type )
  2 field arp_proto     ( 16 bit protocol )
  1 field arp_hlen      (  8 bit hw address length )
  1 field arp_plen      (  8 bit protocol address length )
  2 field arp_op        ( 16 bit operation )
  6 field arp_shw       ( 48 bit sender hw address )
  4 field arp_sp        ( 32 bit sender ipv4 address )
  6 field arp_thw       ( 48 bit target hw address )
  4 field arp_tp        ( 32 bit target ipv4 address )
constant arp_message%

( arp cache )

0
  4 field ac_ip         ( 32 bit protocol address )
  6 field ac_hw         ( 48 bit hw address )
constant arp_cache%

( ipv4 datagram header )

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

( icmp header )

0
  1 field icmp_type     (  8 bits type )
  1 field icmp_code     (  8 bits code )
  2 field icmp_checksum ( 16 bits checksum )
constant icmp_header%

( udp datagram )

0
  2 field udp_source    ( 16 bit source port )
  2 field udp_dest      ( 16 bit destination port )
  2 field udp_len       ( 16 bit length )
  2 field udp_checksum  ( 16 bit checksum )
constant udp_datagram%

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

4000 constant eth_rx_buf

: htons ( n -- n )
  dup ff and 8 lshift swap ff00 and 8 rshift or ;
 
create ip_addr c0a8 htons , 0bfe htons ,
create ip_netmask ffff , ff00 htons ,
create hw_addr 00bd htons , 3b33 htons , 057f htons ,

 800 htons constant eth_ip_type
 806 htons constant eth_arp_type
8035 htons constant eth_rarp_type

1 htons constant arp_request_type
2 htons constant arp_reply_type

 1 constant protocol_icmp
 6 constant protocol_tcp
17 constant protocol_udp

0 constant icmp_echo_reply
8 constant icmp_echo

create arp_cache arp_cache% 8 * here over allot swap erase

variable arp_cache_count 0 arp_cache_count !

: eth_rx f008 @ ;
: eth_tx f008 ! ;

: arp_update
  eth_rx_buf active_struct !
  eth_frame% active_struct +!
  arp_sp arp_shw
  arp_cache active_struct !
  ac_hw 6 cmove
  ac_ip 4 cmove ;
: checksum ( addr count -- checksum)
  0 swap 
  2 /mod swap >r 1- for
   over @ + over @ over u> if
    1+ then swap cell+ swap
  next swap r> if c@ + else drop then
  dup 10 rshift swap ffff and +
  dup 10 rshift +
  ffff xor ;
: ip_in ( -- )
  eth_rx_buf active_struct !
  eth_type @ eth_ip_type = if
    eth_frame% active_struct +!
	ip_proto c@ protocol_icmp = if
	 ip_source ip_dest 4 cmove
	 ip_addr ip_source 4 cmove
	 0 ip_checksum !
	 active_struct @ ip_header% checksum 
	 ip_checksum !
	 ip_len @ htons
	 ip_header% active_struct +!
	 icmp_echo_reply icmp_type c!
	 0 icmp_checksum !
	 ip_header% - active_struct @ swap checksum
	 icmp_checksum !
	 eth_tx
    then
  then ;
: arp_in ( -- )
  eth_rx_buf active_struct !
  eth_type @ eth_arp_type = if
	eth_frame% active_struct +!
	arp_op @ arp_request_type = if
     100            arp_hw !
     eth_ip_type    arp_proto !
     6              arp_hlen c!
     4              arp_plen c!
     arp_reply_type arp_op !
	 arp_shw        arp_thw 6 cmove
     hw_addr        arp_shw 6 cmove
	 arp_sp         arp_tp  4 cmove
     ip_addr        arp_sp  4 cmove
	 arp_thw
	 eth_rx_buf active_struct !
	 eth_dest 6 cmove
     hw_addr eth_src 6 cmove
     eth_arp_type eth_type !
	 eth_tx
	else
	 arp_update
	then
  then ;

: pcap_poll eth_rx_buf 5dc erase eth_rx ;

: round 
	pcap_poll if
	 arp_in
	 ip_in
	then ;
: main
  begin
	round
  again
;

( main )

forth definitions
ipv4.1 definitions
