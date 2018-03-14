# DHCP Study

## Client-server model dhcp

When a computer joins a network, the dhcp client software sends a DHCP broadcast in a connectionless manner (using UDP).

## DHCP Operations
### Discovery
DHCP client sends a `DHCPDISCOVER` message on the network using the **broadcast** address. It can also request it last known IP address. If the client remains connected to the same network, the server may grant the request, otherwise, if the server is set up as **authoritative**, then it would deny the request. A non-authoritative server would simply ignore the request causing the client to timeout and ask for a new IP address.

### Offer
When the server receives a `DHCPDISCOVER` message from a client, the DHCP server reserves an IP address for the client and makes a **lease offer** by sending a `DHCPOFFER` back to the client. This message contains the client's MAC address, the IP address that the server offers, the subnet mask, lease duration and the IP address of the DHCP server making the offer.

The DHCP server determines the configuration based on the client's hardware address as specified in the CHADDR field.

### Request
The client then replies with a `DHCPREQUEST` message **via broadcast**, requesting the offered address. Based on required *server identification* option in the request and broadcast messaging, servers are informed whose offer the client has accepted. When other DHCP servers receive this message, they withdraw any offer that they have made to the client and return the offered IP address to the pool of available addresses.

### Acknowledgement
The acknowledgement phase involves sending a `DHCPACK` packet to the client. This packet includes the lease duration and any other configuration information that the client might have requested.

The protocol expects the DHCP client to configure its network interface with the negotiated parameters.

After the client obtains an IP address, it should probe the newly received address to prevent address conflicts caused by overlapping address pools of DHCP servers.

### Release
The client sends a request to the DHCP server to release the DHCP information and the client deactivates its IP address.

# ARP Study
ARP is a protocol used to map IP network addresses to hardware addresses used by a data link protocol. It is used when *IPv4 is used over Ethernet*.

An Ethernet network uses two hardware addresses which identifies the source and destination of each frame sent. These hardware address is also called the Medium Access Control (MAC) address. Each NIC is allocated a unique 6 byte link address stored in PROM. MAC address is a link layer address and is dependent on the interface card used.

IP operates at the network layer and is not concerned with the MAC address of individual nodes. ARP is therefore used to translate between the two types of addresses. The arp client and server processes operate on all computers using IP over Ethernet.

## ARP Operations
### ARP Request
The arp request message essentially sends "Who is `X.X.X.X` tell `Y.Y.Y.Y`" (where `X.X.X.X` and `Y.Y.Y.Y` are IP addresses) via broadcast.

### ARP Reply
The target system forms an arp response "`X.X.X.X` is `hh:hh:hh:hh:hh:hh`", where `hh:hh:hh:hh:hh:hh` is the Ethernet source address of the computer with the IP address of `X.X.X.X`. This packet is unicasted to `Y.Y.Y.Y`. Since the original request also included the MAC of `Y.Y.Y.Y`, this is already known, and doesn't require another arp message to find this out.

[Source (with diagrams)](http://www.erg.abdn.ac.uk/users/gorry/course/inet-pages/arp.html)

## RARP
RARP (Reverse ARP) finds the IP address given the hardware address.

# LLC Study
Logical Link Control (LLC) protocol is an alternative of way of sending IPv4 datagrams over Ethernet. LLC provides multiplexing mechanisms that makes it possible for several network protocols (e.g. IP, IPX, Decnet and Appletalk) to coexist within a multipoint network and to be transported over the same network medium.

When the LLC protocol is used, the MAC layer SDU (the payload data) is **further** encapsulated with two additional headers: LLC protocol header and a Sub Network Access Protocol (SNAP) layer.

[More on LLC headers](http://www.erg.abdn.ac.uk/users/gorry/course/lan-pages/llc.html)
