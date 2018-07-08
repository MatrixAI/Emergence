# File descriptors

By default, `stdin`, `stdout` and `stderr` are the file descriptors that are kept open for applications by runtime.

The runtime MAY pass additional file descriptors to the application to support features such as **socket activation**. Some of the fds MAY be redirected to `/dev/null` even though they are open.

