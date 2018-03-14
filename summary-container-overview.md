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

### Docker SWARM container orchestration
- Managers: Distribute tasks across the cluster, with one manager orchestrating the worker nodes that make up the swarm.
- Workers: Run docker containers assigned to them by a manager.
- Services: An interface to a particular set of docker containers running across the swarm.
- Tasks: The individual docker containers running the image, plus commands, needed by a particular service.
- Key-value store: [etcd](https://github.com/coreos/etcd),[Consul](https://www.consul.io/) or [Zookeeper](https://zookeeper.apache.org/) storing the swarm's state and providing service discoverability.

## Kubernetes

### Kubernetes container orchestration
- Master: by default, a single master handles API calls, assigns workloads and maintains configuration state.
- Minions: The servers that run workloads and anything else that's not on the master.
- Pods: Units of compute power, made-up of one or a handful of containers deployed on the same host, that together perform a task, have a single IP address and flat networking within the pods.
- Services: front end and load balancer for pods, providing a floating IP for access to the pods that power the service, meaning that changes can happen at the pod-level while maintaining a stable interface.
- Replication controllers: responsible for maintaining X replicas of the required pods.
- Labels: key-value tags (e.g. 'Name') that you and the system use to identify pods, replication controllers and services.

## Needs more research
- Kubernetes
