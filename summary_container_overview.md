# Container Overview

Containers are lightweight since all instances are sharing the same Linux kernel.
They start instantly and use less RAM. Images are constructed from filesystem layers
and share common files. This minimises disk usage and image downloads are much faster.

Different container images (with different distros) just means that different
userland software are installed to simulate a different distribution environment.

## Docker containers

Docker container uses a client-server architecture. Client communicates with the
docker daemon via REST API over unix sockets or network interface, which the
daemon does the heavy lifting of building, running and distributing docker
containers.


## Sources
[Mount and propagation](https://www.kernel.org/doc/Documentation/filesystems/sharedsubtree.txt)

## Needs more research
- Kubernetes
