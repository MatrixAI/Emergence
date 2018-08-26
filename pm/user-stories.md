# Artifact Expression User Stories

## Writing Artifact Expression

As an operator, I must be able to configure platform-specific username or uid for the artifact to run the application with. This might be in the format of: `user`, `uid`, `user:group`, `uid:gid`, `uid:group`. `user:gid`. If the group or gid is not given, the default groups and supplementary groups for the given `user/uid` in `/etc/passwd` from the container should be supplied.

As an operator, I must specify a set of ports to expose from a container running the artifact. The ports MAY be represented in the format of: `port/tcp`, `port/udp`, `port` with the default protocol being `tcp` if not specified. These values MUST be merged with any others supplied in the creation of the container.

As an operator, I must be able to supply a list of environment variables in the format `VARNAME=VARVALUE` when creating the artifact. These values must be merged with any others supplied in the creation of the container.

As an operator, I must be able to supply a list of arguments to execute when the container starts (the entrypoint). These values act as defaults and MAY be replaced by an entry point when creating a container.

As an operator, I must be able to supply default arguments to the entrypoint (e.g. CMD Dockerfile instruction), which will be executed when the container starts if the entrypoint is not specified.

As an operator, I must be able to supply a current working directory of the entrypoint in the container. This value SHOULD act as a default and MAY be replaced by a working directory specified when creating the container.

As an operator, I must be able to supply labels for the container using the [annotation rules](https://github.com/opencontainers/image-spec/blob/master/annotations.md#rules).

As an operator, I must be able to specify a system call signal that will be sent to the container to exit. The signal can be a signal name such as `SIGKILL` or `SIGRTMIN+3`.

As an operator, I should not specify the persistent storage mount points (similar to Docker Volumes) in the artifact expression, instead this information should be pesent in the state expression.

As an operator, I must be able to build an artifact from another artifact fetched from a remote or local registry transparently via a content address to the artifact spec. The content address of an artifact must be unique and deterministic (any changes to the content should generate another pseudorandom content address), the content address MUST be supplied through a trusted source.

As an operator, I must be able to specify imperative instructions to add layers to an artifact. These imperative instructions should be translated into deterministic output (i.e. an OCI image) before the artifact is fetched and used by another operator.

As an operator, I want matrix artifacts to comply with the OCI image standards so
we can substitute any OCI compliant runtimes as we wish.

As an operator, I want to be able to delete references to created artifact specs using imperative commands such as:
```
A = Artifact {...}
del A
```
This command should delete the references to the top layer, manifests, and configuration. However the underlying layers may still be utilised by other artifacts. Hence they should remain there until the garbage collector collects them.


## Sharing Artifact expressions

As an operator, I want to be able to push an artifact to a shared registry so all other
operators in the network will be able to pull the artifact via a content address. This may be done through sharing content addresses of artifact specs in real time.

As an operator, I want to have copies of the actual artifact (layer tar archive, manifest, configuration, etc.) when I want to test the artifact locally.

As an operator, I want the artifact layers to be shared across multiple
artifacts (for example, a NixOS artifact shares some base layers with an Alpine artifact) to reduce storage space.

# Automaton Deployment
