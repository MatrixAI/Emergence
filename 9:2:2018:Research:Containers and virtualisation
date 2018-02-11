# Notes: Container and Virtualisation
tags: container, virtualisation

## Terminologies
- **Bare Metal** The hardware.
- **Virtualisation** A technology that allows a device to run multiple OSes at the same time.
- **Hypervisor** A piece of software that is responsible of running a guest OS,
partition and share the CPU, manage memory and I/O devices in the OSes
- **Type-1 Hypervisor** a.k.a. bare metal hypervisor. Runs directly on top of the hardware. (SIMMON, Xen)
- **Type-2 Hypervisor** a.k.a. hosted hypervisor. Runs on top of the host OS. (VMWare, VirtualBox)
- **bhyve** A type-2 hypervisor developed by FreeBSD.
- **Full Virtualisation** Virtualisation with unmodified guest OS.
- **Paravirtualization** Virtualisation with modified guest OS.
- **VMM** Virtual Machine Manger, essentially a hypervisor.
- **Linux Namespace** An Unix feature that allows isolation of processes and resources.
  - 6 categories: PID, Hostnames, IPC, filesystems, mount, network
  - `unshare` command
- **cgroup**: An Unix feature that allows restriction/constraints on resources.
  - `cgroup` command
- **MMU** Memory location unit, stores pointers to physical memory in pages.
- **TLB** Translation Lookaside Buffer, part of MMU, caches information to allow faster access to memory locations.


## Full Virtualisation
Full virtualisation is a virtualisation technique where the guest OSes are ran unmodified.
In VMWare's implementation, the guest OS (which sits in ring 1) has its own kernel that
sends unmodified binary instructions to VMM (which sits in ring 0, and acts as a hardware emulator).
Since the guest OS still runs in user mode.
*trap* - which is similar to an exception - are created when the guest OS tries to execute kernel mode instructions, the hypervisor uses a
binary translation technique which matches the restricted binary instructions, and turn them
into safe instructions that it can execute in kernel mode, which it is allowed to do since
it sits at ring 0. The result will be calculated and sent back to guest OS
from VMM.

## Paravirtualisation
Paravirtualisation is another virtualisation technique which involves in modifying
the guest OS. The guest OS's kernel is rewritten to replace non-virtualizable instructions
with *hypercalls* which calls the hypervisor's API. Hypervisor provides hypercall
interfaces for critical kernel operations like memory management, interrupt handling
and time keeping.

## Comparison
||Full virtualisation|Paravirtualisation|
|--:|:--|:--|
|Virtualisation overhead|Higher|Lower|
|Performace|Depend on workload|Depend on workload|
|Compatibility|Better compatibility|Poor compatibility|
|portability|Better|Poor|
|Maintenance|Easier|Harder (Requires deep kernel modification)|
|Development|Harder: binary translation|Easier: opensource support (Xen)|

## Memory Management
Virtual machines have their own MMU and a partition of a physical memory that
they have full access to. However, it cannot directly access actual machine memory.
The VMM is responsible for mapping guest physical memory to the actual machine memory.
It uses *shadow page tables* to map the guest machine, which is a representation of the
host page tables that the guest thinks it is using (This depends on virtual machines
implementation).

## KVM (Kernel-based Virtual Machine)
KVM is a virtualization infrastructure for the Linux on x86 hardware
containing virtualization extensions. (Needs more study)

## Linux Container
Different to a virutal machine, linux containers utilises namespaces and cgroups
to create an isolated environment under your linux host, the underlying resources
and kernel are provided by the host, and the system softwares and services are
provided by the image. The Image is a slim version fo the target distribution, which
is manipulated to reduce size and make it runnable in a container.

## Sources
[How linux containers work](
https://github.com/snap-ci/snap-ci-blog-content/blob/master/posts/2016-10-25-how-linux-containers-work.md)

[VMWare - Full Virtualisation, Paravirtualization and Hardware Assist](
https://www.vmware.com/content/dam/digitalmarketing/vmware/en/pdf/techpaper/VMware_paravirtualization.pdf)

[What even is a container (with unix commands)](https://jvns.ca/blog/2016/10/10/what-even-is-a-container/)

[Shadow page tables](https://stackoverflow.com/questions/9832140/what-exactly-do-shadow-page-tables-for-vmms-do)
