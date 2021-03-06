# Notes: Container Architecture and Networking
tags: docker

## Terminologies
- **Bind mounting** Copying a directory tree onto another destination. Altering
of the files under one of the trees will reflect directly onto the other.
- **Microservice architecture** is a method of developing software applications as a suite of independently deployable, small, modular services which each service runs a unique process and communicates through a well-defined, lightweight mechanism to serve a business goal.
- **Linux Bridge** A device that separate two or more network segments within one logical network (e.g. a single IP subnet). The job of the bridge is to examine the destination of the data packets one at a time, and decide whether or not to pass the packets to the other side of the Ethernet segment. The result is a faster, quieter network with less collisions.
The bridging code decides whether to bridge data or drop it by looking at the MAC-address unique to each NIC.
- **FreeBSD Jail** essentially a container in FreeBSD systems.
- **Overlay network** is a computer network that is built on top of another network.

## Docker Swarm Architecture
*Docker engine client* calls *Swarm Manager*'s API, which takes in the commands and create *service objects*. The *orchestrator* creates tasks for that satisfy the requirements of the service objects, then the *allocator* allocates IP addresses to tasks. The *dispatcher* then assigns tasks to worker nodes, and the *schedular* instructs a worker node to run a task.

Each worker node has a container, it connects to dispatcher to check for assigned tasks, and the *executor* will execute tasks assigned to the worker node.

### Pending services
A service may remain to be `pending` if no node currently in the swarm can run its tasks.

### Replicated and global services
There are two types of service deployments, *replicated* and *global*. Replicated service involves in specifying a number of identical tasks a client wish to run, a global service is a service that runs one task on every node. (Useful for monitoring agents, anti-virus scanner etc.)

## Micro-services
- Cloud Native applications
  - Loosely coupled distributed application.
  - Datastore: Each micro-service typically has its own datastore.
  - Packaging: Each micro-service is typically packaged in a "container" image

### Challenges
- Service discovery
- Operational overhead
- Distributed system
- Service dependencies
  - Service fan-out
  - dependency services running "hot"
- Traffic/load each service can handle
- Service Health/Fault tolerance
- Auto-scale

## Container Networking
Requirements:
  - IP address
  - IP address management (IPAM) and network device creation
  - External connectivity via Host (NAT or route advertisement).

### Docker: The Container Network Model (CNM) Interfacing
- Docker image is saved and loaded from `backup.tar`, which could be used later on to create a new docker image.
- Docker container runs and commit its state into the image.

- **Sandbox** contains the configuration of a Container's network stack. This includes management of the container's *interface*, *routing table* and *DNS settings*. An implementation of a sandbox could be a *Linux network namespace*, a *FreeBSD jail* or other similar concept.
- **Endpoint** An endpoint joins a sandbox to a network. An implementation of an endpoint could be a *veth pair*, an *Open vSwitch Internal port* or similar.
- **Network** is a group of endpoints that are able to communicate with each other directly. An implementation of a network could be *VXLAN Segment*, a *linux bridge*, *VLAN*, etc.
- Intention: Implement and use any kind of network technology to *connect* and *discover* containers.
- Partitioning, isolation, and traffic segmentation are achieved by dividing network addresses
- CNM does not specify one preferred methodology for any network overlay scheme.

Within a swarm node, containers communicates with the linux bridge `docker0` through veth pairs. The linux bridge then then pass on the information to the desired destination, whether to another container, or to the host network via *Linux Kernel Routing*.

### Docker Swarm && libnetwork - Built-in Overlay model

- The swarm master writes available global overlay networks into *Distributed Key-value store node(s) (kvs)*
- The swarm nodes write endpoints seen with all their details into the kvs.
- The swarm nodes create the network seen in kvs as new `lx bridges`. Each container has two interfaces:
  - `eth0` (plugs into the overlay)
  - `eth1` (plugs into a local bridge for NAT internet / uplink access).
- Overlay networks are implemented with fixed/static MAC to VTEP mappings


## Linux Switching
- Frontend to manage linux bridge is `brctl`. Newer tool is the OpenvSwitch, with the main frontend `ovs-vsctl`.

### TUN/TAP
TUN/TAP Provides packet reception and transmission for user space programs. It receives packets from user space program and writes them to user space program.

In other words, TUN/TAP driver build a virtual network interface on your host. The interface functions like any other interface, when traffic is sent to the interface, the traffic is sent to your user space program rather than the real network.

There are 2 driver modes for TUN/TAP:

1. TUN(tunnel) device operate at layer 3, meaning the data (packets) you will receive from the file descriptor will be IP based. Data written back to the device must also be in the form of an IP packet.
1. TAP (Network tap) operates much like TUN however instead of only being able to write and receive at layer 3, to/from the file descriptor, it can also do with raw ethernet packets. You will typically see tap deices used by KVM/QEMU virtualisation, where a TAP device is assigned to a virtual guest interface during creation.

### Veth Pairs
Veth devices are built as pairs of connected virtual ethernet interfaces, and what goes in one end will come out the other.

Veth pairs are ideal for connecting different virtual networking components together, such as Linux Bridges, OVS bridges and LXC containers.

[Diagrams showing each device at kernel/user space](https://www.fir3net.com/images/articles/virtual-devices-all-4.png)

### Comparison
|Tap Interface|Veth Pair|
|:--:|:--:|
|Cannot be used to attach network namespaces to linux bridge or the openvswitch.|Used to connect two network namespaces|

## Sources
- [What are microservices](https://smartbear.com/learn/api-design/what-are-microservices/)
- [Linux Switching](http://www.opencloudblog.com/?p=66)
- [Containers and Microservices](https://www.youtube.com/watch?v=SQ-ZF5oHm6w)
- [Container Interface and Networking Connectivity Video](https://www.youtube.com/watch?v=I_0j1Ri9glg&t=600s)
- [TUN, TAP and Veth pairs Explained](https://www.fir3net.com/Networking/Terms-and-Concepts/virtual-networking-devices-tun-tap-and-veth-pairs-explained.html)
