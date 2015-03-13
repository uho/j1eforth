( 
	I feel that the kernel is at it's best for now and that I can proceed
	to do some other things. Note that version 1 is just to make the whole
	thing work, later onI might look at optimisation where I might have to move 
	some stuff around so that memory utilization and execution speed efficiency is 
	achieved.So far Kernel works without needing tweaks.
	
	Work in progress: Implementing simple ipv4 for the j1eforth model
	
	6 project targets:
	
	  0. Modify j1 sim to use pcap interface for network tx and rx - 0%
	  1. ARP  - 0%
	  2. IP   - 0%
	  3. ICMP - 0%
	  4. UDP  - 0%
	  5. TCP  - 0%
	  
	Hopefully I will get time to do all this and also document the design of
	the j1eforth Kernel for those who are starting out with forth and also those
	who wish to tinker with the kernel for fun.
)

hex

forth-wordlist >voc forth

vocabulary ipv4.1

only forth also ipv4.1 definitions

variable active-struct

: field
   create over , +
  does>
   @ active-struct @ + ;

( ethernet header )

0
  6 field eth_dest      ( 48 bits source address )
  6 field eth_src       ( 48 bits destination address )
  2 field eth_type      ( 16 bit type )
constant eth_header%

0 constant eth_proto_ip
6 constant eth_proto_arp

: eth_set_broadcast_dest ;
: eth_set_my_mac_src ;
: eth_set_proto ;

( ip header )

0
  1 field ip_ver_ihl    (  4 bits version field and 4 bits header length )
  1 field ip_tos        (  8 bits type of service )
  2 field ip_len        ( 16 bits length )
  2 field ip_id         ( 16 bits identification )
  2 field ip_frags      (  3 bits flags 13 bits fragment offset )
  1 field ip_ttl        (  8 bits time to live )
  1 field ip_proto      (  8 bits protocol number )
  2 field ip_checksum   ( 16 bits checksum )
  4 field ip_source     ( 32 bits source address )
  4 field ip_dest       ( 32 bits destination address )
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
  2 field udp_source    ( 16 bits source port )
  2 field udp_dest      ( 16 bits destination port )
  2 field udp_len       ( 16 bits length )
  2 field udp_checksum  ( 16 bits checksum )
constant udp_header%

( tcp header )

0
  2 field tcp_source    ( 16 bits source port )
  2 field tcp_dest      ( 16 bits destination port )
  4 field tcp_seq       ( 32 bits length )
  4 field tcp_ack       ( 32 bits checksum )
  1 field tcp_offset    (  8 bits checksum )
  2 field tcp_flags     ( 16 bits checksum )
  1 field tcp_window    (  8 bits checksum )
  2 field tcp_checksum  ( 16 bits checksum )
  2 field tcp_urgent    ( 16 bits checksum )
  2 field tcp_data      ( 16 bits checksum )
constant tcp_header%

( arp packet )

0
  2 field arp_hw        ( 16 bits set to 1 for ethernet )
  2 field arp_proto     ( 16 bits set to ip_type )
  1 field arp_hwlen     (  8 bits set to 6 for ethernet )
  1 field arp_protolen  (  8 bits set to 4 for ip )
  2 field arp_op        ( 16 bits 1 for request, 2 for reply)
  6 field arp_shw       ( 48 bits sender hw addr )
  4 field arp_sp        ( 32 bits sender proto addr )
  6 field arp_thw       ( 48 bits target hw addr )
  4 field arp_tp        ( 32 bits target proto addr )
constant arp_packet%

( arp packet )

0
  6 field ac_mac        ( 48 bits set to ip_type )  
  4 field ac_ip         ( 32 bits set to 1 for ethernet )
constant arp_cache%

create arp_cache arp_cache% 8 * allot

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

only forth
