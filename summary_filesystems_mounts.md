# Filesystems and Mounts

## UnionFS

**UnionFS** is a file system that operates by creating layers.
Docker engine uses UnionFS to provide building blocks for containers, it can use multiple UnionFS variants,
including AUFS, btrFS, bfs and DeviceMapper.

**Union mounting** is a way of combining multiple directories into one that appears
to contain their combined contents.
Docker offers three different ways to mount data into a container from docker host:
volumes, bind mounts or tmpfs volumes.


## Mount

### Mount by volume
Volumes are stored as part of the host system that is *managed by Docker* `/var/lib/docker/volumes`.
Non-docker processes should not modify this part of the filesystem.

Mounting by volume is the preferred machanism for persisting data generated and used by Docker containers, it has several advantages over bind mounts:

- Volumes are easier to back up or migrate than bind mounts.
- It can be managed using Docker CLI or Docker API
- Volumes can be more safely shared among multiple containers.
- Volume drivers allow you to store volumes on remote hosts or cloud providers, to encrypt the contents of volumes, or to add other functionality.
- A new volume's contents can be pre-populated by a container.

### Mount by TMPFS
- Used for non-persistent data storage.
- Only stores data in the host machine's memory (or swap, if memory is low). When the container stops, the `tmpfs` mount is removed. If a container is committed, the `tmpfs` mount is not saved.

### Bind mount
Copying a directory tree onto another destination. Altering of the files under one of the trees will reflect directly onto the other.

### Shared Mount
Shared mount creates mirrors of the mount so that mounts and unmounts within any mirrors propagates to the other. It can be replicated to as many mount points and all the replicas continue to be exactly the same.

Example:

```bash
# Let's say /mnt has a mount that is shared
mount --make-shared /mnt

# Now we replicates the mount at /mnt to the mountpoint /tmp
# The contents should remain identical
mount --bind /mnt /tmp

ls /mnt
# a b c

ls /tmp
# a b c

# Now if we mount a device at /tmp/a
mount /dev/sd0 /tmp/a

ls /tmp/a
# t1 t2 t3

ls /mnt/a
# t1 t2 t3

# Note that the mount has propagated to the mount at /mnt as well.
```
### Slave Mount
Slave mount receives propagation from its master but not vice versa.
All slave mounts have a master mount which is a shared.

Example:

```bash
mount --make-shared /mnt
mount --bind /mnt /tmp

# Now let's make /tmp a slave of /mnt
mount --make-slave /tmp

mount /dev/sd0 /mnt/a

ls /mnt/a
# t1 t2 t3

ls /tmp/a
# t1 t2 t3

mount /dev/sd1 /tmp/b

ls /tmp/b
# s1 s2 s3

ls /mnt/b
```

### Private mount
No propagation ability. This is the default type.

### Unbindable mount
A private mount that cannot be cloned through a bind operation

```bash
# Let's say we have a mount at /mnt and we make it unbindable
mount --make-unbindable /mnt

mount --bind /mnt /tmp
# mount: wrong fs type, bad option, bad superblock on /mnt ...
```

Note: `mount --bind foo foo` can be used to make the directory `foo` a mount point.

## Sources
[Mount and propagation](https://www.kernel.org/doc/Documentation/filesystems/sharedsubtree.txt)
