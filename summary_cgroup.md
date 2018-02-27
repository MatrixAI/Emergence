# Cgroup

## General Usage

Cgroup has to be mounted and created before use.

Example of how to use the `cpuset` subsystem:

```
mount -t tmpfs cgroup_root /sys/fs/cgroup
mkdir /sys/fs/cgroup/cpuset
mount -t cgroup cpuset -o cpuset /sys/fs/cgroup/cpuset/
# Create the new cgroup by doing mkdir's and write's (or echo's) in the /sys/fs/cgroup/cpuset VFS
# Start a task that will be the "founding father" of the new job
# Attach that task to the new cgroup by writing its PID to /sys/fs/cgroup/cpuset task file
# fork exec or clone the job tasks from this founding father task.
```

We can have *named cgroup* by using the `-name` flag when mounting a cgroup hierarchy.

```
mount -t cgroup -o none,name=xxx none /some/mount/point
```

When a hierarchy is created, it is associated with a fied set of cgroup subsystems. The set can be changed if the hierarchy has no subgroups below the root, so for most practical purposes, it is fixed. Each subsystem can be attached to at most one hierarchy, but it is possible to have 12 different hierarchies, one for each subsystem, or a single hierarchy with all 12 subsystems attached, or any other combinations in between.

To combine multiple cgroup subsystems, do:

```
mount -t cgroup -o subsys1,subsys2 none /some/mount/point
```

To combine all cgroup subsystems, do:

```
mount -t cgroup -o all cgroup /sys/fs/cgroup
```

### General cgroup files

`notify_on_release` flag allows a hook (command specified by the contents of the `release_agent` file in the hierarchy's root directory) to be run when the last task in the cgroup leaves.

`clone_children` flag only affects the cpuset controller. If it is enabled (1), a new cpuset cgroup will copy its configuration from the parent during initialisation.


## Memory

### Control Files
|Filename|Summary|
|:--|:--|
|tasks				 |attach a task(thread) and show list of threads|
|cgroup.procs			 |show list of processes|
|cgroup.event_control		 |an interface for event_fd()|
|memory.usage_in_bytes		 |show current usage for memory|
|memory.memsw.usage_in_bytes	 |show current usage for memory+Swap|
|memory.limit_in_bytes		 |set/show limit of memory usage|
|memory.memsw.limit_in_bytes	 |set/show limit of memory+Swap usage|
|memory.failcnt			 |show the number of memory usage hits limits|
|memory.memsw.failcnt		 |show the number of memory+Swap hits limits|
|memory.max_usage_in_bytes	 |show max memory usage recorded|
|memory.memsw.max_usage_in_bytes |show max memory+Swap usage recorded|
|memory.soft_limit_in_bytes	 |set/show soft limit of memory usage|
|memory.stat			 |show various statistics|
|memory.use_hierarchy		 |set/show hierarchical account enabled|
|memory.force_empty		 |trigger forced move charge to parent|
|memory.pressure_level		 |set memory pressure notifications|
|memory.swappiness		 |set/show swappiness parameter of vmscan (See sysctl's vm.swappiness)|
|memory.move_charge_at_immigrate |set/show controls of moving charges|
|memory.oom_control		 |set/show oom controls.|
|memory.numa_stat		 |show the number of memory usage per numa node|
|memory.kmem.limit_in_bytes      |set/show hard limit for kernel memory|
|memory.kmem.usage_in_bytes      |show current kernel memory allocation|
|memory.kmem.failcnt             |show the number of kernel memory usage hits limits|
|memory.kmem.max_usage_in_bytes  |show max kernel memory usage recorded|
|memory.kmem.tcp.limit_in_bytes  |set/show hard limit for tcp buf memory|
|memory.kmem.tcp.usage_in_bytes  |show current tcp buf memory allocation|
|memory.kmem.tcp.failcnt            |show the number of tcp buf memory usage hits limits|
|memory.kmem.tcp.max_usage_in_bytes |show max tcp buf memory usage recorded|

|Terminology|Definition|
|----------:|:---------|
|total-vm|Entire virtual memory that a process uses (memory allocated but not necessarily used)|
|RSS|Resident set size. Part of the virtual memory that a process uses that is mapped into the RAM (allocated and used).|
|anon-rss|Part of the RSS that is allocated in real memory blocks|
|file-rss|Part of the RSS that is mapped into devices and files|
|Memory overcommitment|Assignment of more memory to virtual computing devices than the physical machine actually has|
|Page fault|Exception raised when running a program accesses a memory page that is not currently mapped by the MMU|
|Slab allocation|Preallocate memory chunks suitable to fit certain data objects (caches) and instantly satisfy the request for an memory chunk.|

### Memory Accounting

The memory controller `mem_cgroup` for each cgroup has a counter called `res_counter`. The `res_counter` tracks the current memory usage and limit of the group of processes associated with the controller.

Each page has a pointer to `page_cgroup`, which in turn knows the cgroup it belongs to. When a page accounting is triggerred, it sets up the necessary data structures and check if the cgroup that is being charged is over its limit. If it is, then **reclaim** (See later paragraph) is invoked on the cgroup. If everything goes well, a page meta-data-structure called `page_cgroup` is updated. `page_cgroup` has its own LRU on cgroup.

```c
struct page_cgroup {
  unsigned long flags;
  struct mem_cgroup *mem_cgroup;
}
```

** Note: `page_cgroup` structure is allocated at boot/memory-hotplug time.

#### Accounting details

All `anon-rss` and `Page Cache` are accounted. `anon-rss` pages are accounted at *page_fault* unless they've already been accounted for earlier. `file-rss` will be accounted for as `Page Cache` when it's inserted into inode (radix-tree). While it's mapped into the page tables of processes, duplicate accounting is carefully avoided.

An RSS page is unaccounted when it's fully unmapped. A PageCache is unaccounted when it's removed from radix-tree. A rss page that is unmapped but exists as swap cache is still accounted until it is fully freed. A swapped-in page is not accounted until it's mapped (a swapped-in page may contain pages for other tasks than a task causing page fault, hence we avoid accounting at swap-in I/O).

#### Shared Page Accounting

Shared pages are accounted on the basis of the first touch approach. First cgroup to touch a page is accounted for the page. In practice this is usually okay, but it is potentially unfair since all cgroups that share this page aren't charged equally.


#### Reclaim
Each cgroup maintains a per cgroup LRU which has the same structure as global VM. When a cgroup goes over its limit, we first try to reclaim memory from the cgroup so as to make space for the new pages that the cgroup has touched. If the reclaim is unsuccessful, an OOM routine is invoked to select and kill the bulkiest task in the cgroup.

** Note: reclaim does not work for the root cgroup.


### Kernel Memory Extension

- Kernel memory cannot be swapped out.
- It won't be accounted at all until a limit on a group is set.
- The limit cannot be set if the cgroup have children, or if there are already tasks in the cgroup.
- Kernel memory wil be accounted once it is limited, the limitation can be removed by writing `-1` to `memory.kmem.limit_in_bytes`, but the accounting will still happen.
- Kernel memory limits are not imposed for the root cgroup.
- Currently no soft limit is implemented for kernel memory (according to an 2012 article).

#### Current Kernel Memory resources accounted
- Stack pages: every process consumes some stack pages. By accounting into kernel memory, we prevent new processes from created when the kernel memory usage is too high.
- Slab pages: pages allocated by the SLAB or SLUB allocator are tracked.
- Sockets memory pressure: Some sockets protocols have memory


### Source
[Cgroup memory accounting](https://lwn.net/Articles/529927/)
[Linux memory Terminologies](https://www.win.tue.nl/~aeb/linux/lk/lk-9.html)
[How Linux processes uses memory](https://ewirch.github.io/2013/11/linux-process-memory-layout.html)
