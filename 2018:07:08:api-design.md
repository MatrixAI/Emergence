# hs-libcontainer API


The hs-libcontainer API should include everything that Docker can achieve, but at the same time be flexible enough so our orchestrator is able to manipulate the container in many ways.

Docker essentially shells out to runc, that means that runc's interface is enough to be able to achieve everything that Docker needs to deal with containers. Maybe having an interface as such would be ideal, this means that all low level container related tasks are dealt with in Go. I should investigate the top level design first before I dive straight into serialisation.

## Runc's interface

### RUNC Create

```
NAME:
   runc create - create a container

USAGE:
   runc create [command options] <container-id>

Where "<container-id>" is your name for the instance of the container that you
are starting. The name you provide for the container instance must be unique on
your host.

DESCRIPTION:
   The create command creates an instance of a container for a bundle. The bundle
is a directory with a specification file named "config.json" and a root
filesystem.

The specification file includes an args parameter. The args parameter is used
to specify command(s) that get run when the container is started. To change the
command(s) that get executed on start, edit the args parameter of the spec. See
"runc spec --help" for more explanation.

OPTIONS:
   --bundle value, -b value  path to the root of the bundle directory, defaults to the current directory
   --console-socket value    path to an AF_UNIX socket which will receive a file descriptor referencing the master end of the console's pseudoterminal
   --pid-file value          specify the file to write the process id to
   --no-pivot                do not use pivot root to jail process inside rootfs.  This should be used whenever the rootfs is on top of a ramdisk
   --no-new-keyring          do not create a new session keyring for the container.  This will cause the container to inherit the calling processes session key
   --preserve-fds value      Pass N additional file descriptors to the container (stdio + $LISTEN_FDS + N in total) (default: 0)
```

What needs to be serialised:
	- Bundle config content, and maybe even the filesystem tarball even.
	- preserve-fds

```



## OCI Runtime Specification

## Serialisation needs



