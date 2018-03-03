# Terminologies
- Failover is switching to a redundant or standby computer server, system, hardware component or network upon the failure or abnormal termination of the previously active application, server, system ,hardware component, or network.
- BIRD Routing daemon: BIRD establish multiple routing tables, and uses BGP, RIP and OSPF routing protocols, as well as statically defined routes.

# Logical Interface
Logical Interface (LIF) is a software entity consisting of an IP address that is associated with a number of attributes such as a role, a home port, a firewall policy, a home node, a routing group and a list of ports for failover purposes.

# VLAN
Any broadcast domain that is partitioned and isolated in a computer network at the data link layer. They work by applying tags to network packets and handling these tags in  networking systems - creating the appearance and functionality of network traffic that is physically on a single network but acts as if it is split between separate networks.

# Logical Switches
A logical switch creates logical broadcast domains or segments to which an application (or VM) can be logically wired. it is mapped to an unique VXLAN, which encapsulates the virtual machine traffic and carries it over the physical IP network.

# Linux Bridging
A bridge is a way to connect 2 ethernet segments together in a protocol independent way. Packets are forwarded based on Ethernet address, rather than IP address (like a router). Since forwarding is done at layer 2, all protocols can go transparently through a bridge.

A bridge can be created between a logical switch and a VLAN, which enables you to migrate virtual workloads to physical devices with no impact on IP addresses.

## How bridge works (Not backed by experiment)

A bridge mainly consists of 4 major components:

1. Set of network ports (or interfaces): used to forward traffic between end switches to other hosts in the network.
1. A control plane: used to run spanning tree protocol (STP) that calculates minimum spanning tree, preventing loops from crashing the netwrok.
1. A forwarding plane: used to process input frames from the ports, forward them to the network port by making a forwarding decision based on MAC learning database
1. MAC learning database: Used to track of the host locations in the LAN. (NAT?)

There are 3 main configuration subsystems (in C) to do bridges:

1. `ioctl`: create/destroy bridges and add/remove interfaces to/from bridge.
1. `sysfs`: Management of bridge and port specific parameters.
1. `netlink`: Asynchronous queue based communication that uses `AF_NETLINK` address family, can also be used to interact with bridge.

Details: [View Source](https://goyalankit.com/blog/linux-bridge)

## Bridging and Firewalling
A Linux bridge is more powerful than hardware bridge because it can also filter and shape traffic. The combination of bridging and firewalling is done with the companion project `ebtables`.

## Spanning Tree Protocol (STP)
A network protocol that ensures a loop-free topology for any bridged Ethernet local area network. Works at Data Link Layer.

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

1. A pair of `veth` devices is created on the host. Future containers networking devices is then configured via DHCP server (actually dnsmasq daemon) which is listening on the IP assigned to the LXC network bridge `lxcbr0`. The bridge's IP will serve as the container's default gateway as well as its name server.
1. "Slave" part of the pair is then moved to the container, renamed to `eth0` and finally configured in the container's network namespace.
1. Once the container's `init` process is started, it brings up particular network device interface in the container and we can start networking.

Experiment:

```bash
# First we create a bridge called "bridge0" that will be used
# to join namespace(s) and the host together
sudo brctl create bridge0

# We give the bridge a subnet 10.0.3.1/24 and let it control the network
# under this subnet
sudo ip addr add 10.0.3.1/24 dev bridge0
sudo ip link set bridge0 up

# Now we can create a network namespace called "mynamespace"
# The namespace file will be stored at /var/run/netns
sudo ip netns add mynamespace

# `sudo ip netns list` shows the namespace

# We can now go into the namespace
sudo ip netns exec mynamespace bash

# now create a veth pair where one end is called "veth0"
# and the other is "eth0".
ip link add veth0 type veth peer name eth0

# None of our interfaces have addresses yet, so let's give them some addresses
ip addr add 10.0.3.78/24 dev eth0
ip link set eth0 up

# Now if we run `ifconfig` we can see eth0 up and running at 10.0.3.78
# Notice here that the address we give to eth0 has to be a subnet of
# the bridge that we wish to join it up to at host, which in this case
# is 10.0.3.1/24.

# We have to put the other end of the veth pair to the host netns
ip link set veth0 netns 1

# We exit to the host ns and get veth0 up
exit
sudo ip link set veth0 up

# We now have to bridge veth0 to our bridge mentioned earlier
sudo brctl addif lxcbr0 veth0

# Now if we ping 10.0.3.78, we should get some packets back!
ping 10.0.3.78

# Done. The container is connected to the host!
```

## Macvlan
Take a single network interface and create multiple virtual network interfaces with different MAC addresses assigned to them.

Note that this is different to Linux VLANs which are capable to use single network interface and map it to multiple virtual networks provided "one-to-many" mapping - one network interface, many network VLANs in one trunk. MAC VLANs maps multiple network interfaces (i.e. with different MAC addresses) to one network interface. We can obviously combine both if we want to.

MAC VLAN allows each configured "slave" device be in one of three modes:

- `private` The device never talks with any other device on the `upper_dev` (master device). I.e. the slaves can't communicate with each other.
- `VEPA` Virtal Ethernet Port Aggregator is a MAC VLAN mode that aggregates virtual machine packets on a *server* before the resulting single stream is transmitted to the switch.
- `bridge` Provides the behavior of a simple bridge between different macvlan interfaces on the same port.


Experiment (DMZ!):

```bash
# Create two net namespaces and connect them with macvlan bridge mode

sudo ip netns add cont1
sudo ip netns add cont2

# Move the new interface mv1/mv2 to the new
sudo ip link add mv1 link eth0 type macvlan mode bridge
sudo ip link add mv2 link eth0 type macvlan mode bridge

# Move the new interface mv1/mv2 to the new namespace
sudo ip link set mv1 netns cont1
sudo ip link set mv2 netns cont2

# Set ip addresses
sudo ip netns exec cont1 ip addr add 10.0.3.15/24 dev mv1
sudo ip netns exec cont2 ip addr add 10.0.3.16/24 dev mv2

# Bring the two interfaces up
sudo ip netns exec ns1 ip link set dev mv1 up
sudo ip netns exec ns2 ip link set dev mv2 up

# Now we have 2 net namespaces running that cannot be pinged from the
# host, but can ping each other from within themselves!
```

# IP Masquerade
IP Masquerade (IPMASQ or MASQ) alllows one or more computers in a netowrk without assigned IP addresses to communicate with the internet using the Linux server's assigned IP address. The IPMASQ server acts as a gateway, and the other devices are invisible behind it, so to other machines on the internet the outgoing traffic appears to be comming from the IPMASQ server and not the internal PCs.

## Source
[Exploring LXC Networking](http://containerops.org/2013/11/19/lxc-networking/)

[Linux Networking](http://networkstatic.net/configuring-macvlan-ipvlan-linux-networking/)
