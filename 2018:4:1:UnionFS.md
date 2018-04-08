# AUFS
Short for Advanced

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
