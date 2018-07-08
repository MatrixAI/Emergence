# File descriptors

By default, `stdin`, `stdout` and `stderr` are the file descriptors that are kept open for applications by runtime.

The runtime MAY pass additional file descriptors to the application to support features such as **socket activation**. Some of the fds MAY be redirected to `/dev/null` even though they are open.

Socket activation is an important feature of systemd. Read [this article](http://0pointer.de/blog/projects/socket-activated-containers.html) for details. But in summary:

- Systemd sets up listening sockets (IP or otherwise) on behalf of your services (without these running yet).
- Services are activated when the first connection comes in.
- This is invisible on he client side whether the service it is interested in is running or not. Service IP socket stays connectable. No fail gaurenteed.
- It saves resources as services are only running when necessary.

For socket activated OS containers, the host's systemd instance will listen on a number of ports on behalf of a container. For example one for SSH, one for web and one for database. As soon as the first connection comes in, it will spawm the cotnainer this is intended for, and pass to it all three sockets.
