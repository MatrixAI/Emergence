## Namespace Switching 

A demo project for simple namespace switching with Linux namespace API.

### What it does

1. `fork` and `unshare` new process with new uts namespace. Using uts namespace as it is the easiest to show changes
1. bind `mount` the uts namespace file in `/proc/<pid>/ns/uts` to `/tmp/ns/`
1. child processes each sets a different hostname in the uts namespace
1. child processes randomly join a different uts namespace with `setns` and print hostname (5 times)
1. `umount` the namespace files in `/tmp/ns`

### Build

```sh
$ gcc main.c -lpthread
```

### Run

```sh
# Run with 10 child processes
$ sudo ./a.out -c 10
```

### Notes
Hangs if ran without root permission.   
Possibly because child process sends SIGINT to parent and terminates, but parent is waiting on pthread barrier which automatically resumes after signal handling.