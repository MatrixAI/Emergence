# Terminologies
- Failover is switching to a redundant or standby computer server, system, hardware component or network upon the failure or abnormal termination of the previously active application, server, system ,hardware component, or network.
- BIRD Routing daemon: BIRD establish multiple routing tables, and uses BGP, RIP and OSPF routing protocols, as well as statically defined routes.

# Logical Interface
Logical Interface (LIF) is a software entity consisting of an IP address that is associated with a number of attributes such as a role, a home port, a firewall policy, a home node, a routing group and a list of ports for failover purposes.

- Data LIFs use a Vserver and communicate with clients, configured through data specific ports.


# Configuring Linux Network Connection
Read `interfaces(5)` for more information.


`/etc/network/interfaces` contains the network interface configuration for `ifup(8)` and `ifdown(8)`

- `auto <interface>`: Identify the physical interfaces to be brought up when ifup is run with the -a option (used by system boot scripts)
- `allow-auto <interface>`: Same as auto
- `allow-hotplug <interface>`: Start the interface when a "hotplug" event is detected.
- `iface <logical_interface> <addr_fam> <method>`:


# LXC Container Networking
## Empty
Creates a container with loopback only.

## Veth
Use the peer network device (a pair of fake Ethernet devices that act as a pipe) with one side assigned to the container and the other side attached to a bridge specified by `lxc.network.link` in config

Simplified process:

1. A pair of `veth` devices is created on the host. Future containers networking devices is then configured via DHCP server (actually dnsmasq daemon) which is listening on the IP assigned to the LXC network bridge `lxcbr0`. The bridge's IP will serve as the container's default gateway as well as its nameserver.
1. "Slave" part of the pair is then moved to the container, renamed to `eth0` and finally configured in the container's network namespace.
1. Once the container's `init` process is started, it brings up particular netowrk device interface in the container and we can start networking.

## Macvlan
Take a single network interface and create multiple virtual network interfaces with different MAC addresses assigned to them.
