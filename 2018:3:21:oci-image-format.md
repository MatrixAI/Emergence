# OCI Image Format
## Manifest
At a high level the image manifest should contain metadata about the contents, dependencies of the image.
This includes the


### Filesystem changeset
A container filesystem consists of layers of and layers of changes.

There are 3 change types:
1. Addition
1. Modification
1. Removals

File types include:
- regular files
- directories
- sockets
- sumbolic links
- block devices
- character devices
- FIFOs

File Attributes:
- `mtime` - modification time
- `uid`
- `gid`
- `mode`
- `xattrs`
- Symlink refernece (`linkname` + synbolic link type)
- Hardlink reference (`linkname`)

#### Creation
We start we an initial root filesystem, a.k.a base image or parent layer, it is compressed into a plain tar archive.

For the next layer, we create a new directory and initialize it iwith a copy or snapshot of the prior root filesystem. This can be done by:

```bash
# cp(1)
cp -a <base image> <new directory>

#rsync(1)
rsync -aHAX <base image> <new directory>

# tar(1)
mkdir <new directory> &&
  tar --acls --xattrs -C <base image> -c . |
  tar -C <new directory> --cacls --xattrs -x
  # Include --selinux where supported
```

*Note: A copy-on-wrte or union filesystem can efficiently make directory snapshots.*

After this, the directories are compared, looking for files that have been `added`, `modified`, or `removed`.

Example changeset:

```
Added:    /etc/my-app.d/
Modified: /bin/my-app-tools
Deleted:  /etc/my-app-config
```

The way that the changes are represented is via a *tar archive* which only contains this changeset. Added and modified files and directories in their entirety, and deleted files or directories are marked with a **whiteout file**.

A white out file is:
- An empty file consists of the prefix `.wh.` plus the basename of the path to be deleted.
- Once a whiteout is applied, the whiteout itself MUST also be hidden.
- Whiteout files MUST only apply to resources in lower/parent layers.
- Files that are present in the same layer as the whiteout file can only be hidden by whiteout files in subsequent layers.

An opaque whiteout:
- A file with the name `.wh..wh..opq` indicating that all siblings are hidden in the lower layer.

Implementations SHOULD generate layers using *explicit whiteout files*, but MUST accept both.s

#### Applying Changesets
Special consideration for whiteout files are needed, in the absence of any whiteout files in a layer changeset, the archive is extracted like a regular tar archive

For **changeset over existing files**, if the existing paths and the entry are both directories, then the attributes of the existing path MUST be replaced by those of the changset.s

- Removing the file path `unlink` on linux systems
- Recreating the file path, based on the contents and attributes of the changeset entry.
