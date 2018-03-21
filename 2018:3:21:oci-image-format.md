# OCI Image Format

## Creation
The initial root filesystem is the base layer. For example
```
rootfs-c9d-v1/
```
is an example of an initial directory structure

Files and directories are then created
```
rootfs-c9d-v1/
  etc/
    my-app-config
  bin/
    my-app-binary
    my-app-tools
```

The `rootfs-c9d-v1` directory is then created as a plain tar archive with relative path to `rootfs-c9d-v1`. To create a new directory and initialize it with a copy or snapshot of previuos root filesystem, we can use:
- `cp(1)`: `cp -a rootfs-c9d-v1/ rootfs-c9d-v1.s1`
- `rsync(1)`: `rsync -aHAX rootfs-c9d-v1/ rootfs-c9d-v1.s1`
- `tar(1)`: `mkdir rootfs-c9d-v1.s1 && tar --acls --xattrs -C rootfs-c9d-v1/ -c . | tar -C rootfs-c9d-v1.s1/ --acls --xattrs -x` (including ``--selinux` where supported).
