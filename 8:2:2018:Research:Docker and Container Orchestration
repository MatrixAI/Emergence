# Notes: Docker
tags: docker, containers, kubernetes

## Questions
### Docker
1. What is docker?
  - Docker is a company which provides a container platform to address applications on the cloud.
1. What is a container?
  - Container is a lightweight, stand-alone, executable package of a piece of software which includes everything needed to run it.
1. Why are docker containers light weight?
  - Docker containers running on a single machine share the machine's OS kernel. They start instantly and use less compute and RAM. Images are constructed from filesystem layers and share common files. This minimises disk usage and image downloads are much faster.
1. How does docker achieves this?
  - Docker Engine is a client-server appplication with these major components:
    - A server (docker daemon) which is a type of long-running program called a daemon process (the dockerd command)
    - A REST API which specifies interfaces taht programs can use to talk to the daemon and instruct it what to do.
    - A command line interface client.
1. What is REST API?
  - Representational State Transfer-compliant web services allow requesting systems to access and manipulate textual representations of web resources using a uniform and predefined set of stateless operations.
1. What is pseudo-tty?
  - Pseudoterminals (pseudo-ttys) are used by users and applications to gain access to a shell. A pseudo-TTY is a pair of character special files, a master file and a corresponding slave file. It is any device that acts like a terminal.
1. What is character special file?
  - Character special files (character devices) behave like pipes. Writing or reading to them is an immediate action. The name comes from the fact that each character is handled individually. (e.g. /dev/urandom)
1. Block special files?
  - Block special files (aka block devices) behave like ordinary files. The name comes from the fact that the corresponding hardware typically reads and writes a whole block at a time (a sector on a hard disk).
1. Docker Architecture
  - Docker uses a client-server architecture. Client communicates with the docker daemon via REST API over Unix sockets or network interface, which the daemon does the heavy lifting of building, running, and distributing docker containers.
1. What are docker registries
  - A docker registry stores docker images.
1. Docker objects
  - Images: Read-only template with instructions for creating a docker container. To build your own image, create a Dockerfile with a simple syntax for defining the steps needed to create the image and run it.
  - Container: A runnable instance of an image. Can be created, started, stopped, moved or deleted using Docker API.
1. What is Linux namespaces?
  - Namespaces are a feature of the Linux kernel that isolates and virtualises system resources of a collection of processes. Docker uses namespaces to provide the isolated workspace called *container*. When user runs a container, Docker creates a set of *namespaces* for that container.
    - `pid`: Process isolation
    - `net`: Managing network interfaces.
    - `ipc`: Managing IPC resoruces.
    - `mnt`: Managing filesystem mount points.
    - `uts`: Isolating kernel and version identifiers.
1. Control groups
  - Docker uses control groups `cgroups` to limit an application to a specific set of resources. This allows the Docker Engine to share available hardware resources to containers and optionally enforce limits and constraints.
1. Union file system (UnionFS)
  - A file system that operates by creating layers, Docker Engine uses UnionFS to provide the building blocks for containers. Docker Engine can use multiple UnionFS varians, invluding AUFS, btrfs, bfs, and DeviceMapper.
1. Union mounting
  - A way of combining multiple directories into one that appears to contain their combined contents.
  - Docker offers three different ways to mount data into a countainer from docker host: volumes, bind mounts, or tmpfs volumes.
1. What is mounting by volume?
  - Volumes are stored in a part of the host system which is managed by Docker (`/var/lib/docker/volumes` on Linux). Non-Docker processes should not modify this part of the filesystem.
  - Bind mounts
  - Data points to a filesystem in host
1. What is mounting by tmpfs?
  - Data points to a memory in host.
1. How does Docker achieve container orchestration
  - Managers: Distribute tasks across the cluster, with one manager orchestrating the worker nodes that make up the swarm.
  - Workers: Run docker containers assigned to them by a manager.
  - Services: An interface to a particular set of docker containers running across the swarm.
  - Tasks: The individual docker containers running the image, plus commands, needed by a particular service.
  - Key-value store: etcd. Consul or Zookeeper storing the swarm's state and providing service discoverability.

1. How does Kubernetes achieve container orchestration
  - Master: by default, a single master handles API calls, assigns workloads and maintains configuration state.
  - Minions: The servers that run workloads and anything else taht's not on the master.
  - Pods: Units of compute power, made-up of one or a handful of containers deployed on the same host, that together perform a task, have a single IP address and flat networking within the pods.
  - Services: front end and load balancer for pods, providing a floating IP for access to the pods that power the service, meaning that changes can happen at the pod-level while maintaining a stable interface.
  - Replication controllers: responsible for maintaining X replicas of the required pods.
  - Labels: key-value tags (e.g. 'Name') that you and the system use to identify pods, replication controllers and services.
