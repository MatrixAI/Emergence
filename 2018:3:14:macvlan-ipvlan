# Macvlan
## Overview
- Allows you to host multiple logical interfaces on a single interface.
- These logical interfaces are differentiated by MAC address.
- Can host macvlan interfaces on physical interfaces or VLAN interfaces
- Can place macvlan interfaces in a separate network namespace, if desired.
- Can use with KVM guest domains (macvtap)

## Modes

### Private
sub-interfaces on the same parent interface cannot communicate with each other.
All frames from sub-interfaces are forwarded out through the parent interface. Even if physical switch reflects the frame sourced from one sub-interface and destined to another sub-interface, frame gets dropped.

### Macvlan VEPA
All frames from sub-interfaces are forwarded out through parent interface. VEPA mode requires an IEEE

[Source (Contains useful diagrams)](https://hicu.be/bridge-vs-macvlan)

# Source
[Sreenivas Makam's Blog: Macvlan and IPvlan](https://sreeninet.wordpress.com/2016/05/29/macvlan-and-ipvlan/)
[How VLAN bridges discover MAC addresses](https://learningnetwork.cisco.com/thread/119212)
