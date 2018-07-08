# Page fault
Page fault may sound serious, but it sometimes is just an event that is triggered by the system during a normal usecase. It essentially just tells the system to fetch some memory that a program needs, but isn't mapped (that may be deallocated because the system *once thought* the process doesn't need it). This can happen with program code, for part of the program code that isn't executed over a long time, the system may choose to deallocate the code block, and load it later when the program needs it. which will then be triggered by a page fault.

## Minor page faults
Minor page faults can occur when the code is actually in the memory, but isn't allocated to a specific process. For example, when a code block is shared between multiple processes (e.g. running the same executable twice). The system might let the processes share the same code block (since the binary is read-only). This would trigger a **minor page fault**, which updates the page list without needing to access the disk.

## Copy on Write
When a resource (pages) is allocated but unused, the pages would be marked as **copy on write**. This also happens to when multiple processes share the same resource. It is upon write that the resources will be copied.

# Swapping
Swapping-out is the process of writing pages out to disk to free memory.
Swapping-in is the opposite (happens at page fault).

# Page Migration
Moving of the physical location of pages between nodes in a numa system while the process is running. This means the virtual addresses that the process sees do not change. However, the system rearranges the physical location of those pages.

[Detailed description of page migration](https://www.kernel.org/doc/Documentation/vm/page_migration)


## Sources
[Page Fault and Swap explained](http://blog.scoutapp.com/articles/2015/04/10/understanding-page-faults-and-memory-swap-in-outs-when-should-you-worry)
