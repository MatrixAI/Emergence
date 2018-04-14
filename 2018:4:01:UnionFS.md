# AUFS
AUFS layers multiple directories on a single Linux host and presents them as a single directory. These unification process is referred to as a *union mount*.

AUFS uses the copy-on-write strategy to maximise storage efficiency and minimise overhead.

In docker, the information about images and container layers is stored in subdirectories of `/var/lib/docker/aufs`
- `diff/` the contents of each layer, each stored in a separate subdirectory.
- `layers/` metadata about how many image layers are stacked. This directory contains one file for each image or container layer on the Docker host. Each file contains the IDs of all the layers below it in the stack (its parents)
- `mnt/` Mount points, one per image or container layer, which are used to assemble and mount the unified system for a container. For images, which are read-only, these directories are always empty.

## Reading files
- If the file does not exist in the container layer, the storage driver search for the file in the image layers, starting with the layer just below the container layer, and read from the layer where it is found.
- The file only exists in the container layer, it is read from there.
- The file exists in both: File is read from the container layer, files in the container layer obscure files with the same name in the image layer.

## Modifying files or directories
- If the file does not exist in the `upperdir`, the `aufs` driver performs a `copy_up` to copy the file from the image laeyr to the writable layer. The contaienr then writes the changes to the new copy. However, AUFS works at the file level rather than the block level. This means all `copy_up` copies the entire file. This can have a noticable impact on the performance.
- Deletion of files and directories:
  - If a file is deleted within a container, a whiteout file is created in the container layer.
  - If a directory is deleted within a container, a opaque file is created in the container layer.
- Renaming directories: Calling `rename(2)` for a directory is not fully supported on AUFS, it returns `EXDEV` (cross device link not permitted).

## Pros and Cons
- AUS is less performant than `overlay2`, but it allows efficient sharing of a container image between multiple running containers, enable fast container start times and minimal use of disk space.
- The underlying mechanics of how AUFS shares files between image layers and containers uses the page cache very efficiently.
- AUFS write performance is significantly slower.

# BTRFS
BTRFS uses a subdirectory containing one directory per image or container layer, with the unified filesystem built from a layer plus all its parent layers. Subvolumes are natively copy-on-write and have space allocated to them on-demand from an underlying storage pool. They can also be nested and snapshotted.

Only the base layer is stored as a true subvolume. All other layers are stored as snapshots. On disk, snapshots look and feel just like subvolumes, but in reality they are much smaller as they are managed in block-level. For maximum efficiency, when a container needs more space, it is allocated in *chunks* of roughly 1GB in size.

Docker's btrfs storage driver stores every image layer and container in its own btrfs subvolume or snapshot. The base layer of an image is stored as a subvolume whereas a child image layers and containers are stored as snapshots.

## Reading files
- metadata in the snapshot points to the actual data blocks in the storage pool. Therefore, read performed against a snapshot are essentially the same as reads performed against a subvolume.

## Writing files
- Writing new files: invokes an allocate-on-demand operation to allocate new data block to the container's snapshot. Operates at native Btrfs speeds.
- Modifying existing files: Copy-on-write operation, the data is read from the layer where it currently exists, and only the modified blocks are written into the container's writable layer. This incurs very little overhead.
- Deleting files or directories: btrfs masks the existance of the file in the lower layer. If a container creates a file and then deletes it, this operation is performed in the Btrfs filesystem itslef and the space is reclaimed.

## Pros and Cons
- Page cache sharing is not supported, host container copies the files into the host's memory.
- Small writes
- Sequential writes: reduces the performace by up to 50%
- Fragmentation: many small random writes can compound this issue due to copy-on-writes.


# OverlayFS
OverlayFS allows one, usually read-write directory tree to be overlaid to another read-only directory tree. All modifications go to the upper, writable layer.

An overlayFS combines two directory trees - "upper" and "lower". The lower directory tree can be any filesystem recognized by Linux and it does not have to be writtable. The upper directory tree however, is usually writable and if it is, it must support the creation of trusted extended attributes, and must provide valid `d_type` in their `readdir` responses, so NFS is not suitable. If the upper directory tree is not writtable, then it can be any filesystem type.

Overlaying involves directories. If a given name appears in both `upper` and `lower` filesystems and refers to a non-directory in either, then the lower object is hidden - the name refers only to the upper object.

Where both upper and lower objects are directories, a merged directory is formed.

At mount time, the two directories given as mount options `lowerdir` and `upperdir` are combined into a merged directory.

```
mount -t overlay overlay -olowerdir=/lower,upperdir=/upper,\
      workdir=/work /merged
```

The `workdir` needs to be an empty directory on the same filesystem as `upperdir`

When a lookup is requested in such a merged directory, the lookup is performed in each directory and the combined result is cached in the dentry belonging to the overlay filesystem. If both actual lookups find directories, both are stored and a merged directory is created, otherwise only one is stored: the upper if it exists, else the lower.

Only the lists of names from directories are merged. Other content such as metadata and extended attributes are reported for the upper directory only. These attributes of the lower directory are hidden.

## Whiteouts and opaque directories
performing `rm` and `rmdir` results in whiteouts and opaque directories in the upper directory. When a whiteout is found, any matching name in the lower level is ignored, the whiteout itself is also ignored.

A whiteout is created as a character device with 0/0 device number.
A directory is made opaque by setting the xattr `trusted.overlay.opaque` to `y`. Where the upper filesystem contains an opaque directory, any directory in the lower filesystem with the same name is ignored.

## readdir
When a process performs `readdir` on merged directories, the upper and lower directories are each read and the name lists merged in the obvious way (upper first, lower entries are not re-added). This merged namelist is cached in the `struct file` and so remains as long as the file is kept open. This means changes to the merged directory do not appear while a directory is being read.

## Renaming directories
When renaming a directory on the lower layer or merged:
- return `EXDEV` error: returned by rename(2) when trying to move a file or directory across filesystem boundaries.
- If the `redirect_dir` feature is enabled, then the directory will be copied up (but not the contents). Then the `trusted.overlay.redirect` extended attribute is set to the path of the original location from the root of the overlay. Finally the directory is moved to the new location.

## Non-directories
Objects that are not directories are presented either from the upper or lower filesystem as appropriate, when a write access is requested, the object is *copied up*. Once the copy up is done, the overlayFS provides direct access to the newly created file in the upper layer. Future operations on the file are barely noticed by the overlay filesystem.

## Multiple lower layers
Multiple lower layers can now be given using the colon `:` as a separator character between directory names:
```
mount -t overlay overlay -olowerdir/lower1:/lower2:/lower3 /merged
```
Omitting `upperdir` and `workdir` means the overlay is read-only.

## Sharing and Copying layers
Lower layers may be shared among several overlay mounts and that is indeed a common practice. However sharing a upper layer may fail with various errors and may be result in undefined behavior.

## Changes to the underlying filesystem
Offline changes, when the overlay is not mounted, are allowed to either the upper or lower trees.

Changes to the underlying filesystems while part of a mounted overlay filesystem are no allowed. If the underlying filesystem is chagned, the behavior of the overlay is undefined.


## Source
[Linux Kernel OverlayFS Documentation](https://www.kernel.org/doc/Documentation/filesystems/overlayfs.txt)
