Artifacts needs to be able to be represented with a content address, which allows it to be referred to by the Architect language as an unique artifact.

However, the deployment of a contaniner isn't deterministic across all nodes: persistent storage needs to be mounted at the container's runtime, other properties usch as exposed ports, more resource mounts (mounting GPU devices to automatons to support utilisation on GPUs with nodes that has GPUs) cannot be specified as part of the image. As images are immutable and those properties may change depending on where the orchestrator wants to deploy the container.

Artifact specifies an "Image", not a container, but an image is not a simple filesystem too, it contains entry point information and virtually everything that exists in a container's configuration ([Images configuration can be translated into container configuraiton](https://github.com/opencontainers/image-spec/blob/master/conversion.md)). 

From here on I will refer to image config as the configuraiton for the OCI image, and runtime config as the configuration of the container.

After looking at the [image config spec](https://github.com/opencontainers/image-spec/blob/master/config.md), I noticed that most properties are labelled as OPTIONAL. This means that those attributes can be skipped and supplied at the creation of containers. These properties includes `User`, `ExposedPorts`, `Env`, `Volumes` and more. That works well with the way that we want it to work, which is to supply information where possible and have the ability to leave some information to a later stage.

However, this approach also means that as an operator, if I refer to an artifact A with a content address @(A), and it works the first time, it does not mean that another artifact B with a content address identical to @(A) will work the same way, especially when the most critical attribute `ENTRYPOINT` is also an optional property,

# Staged Deployment process
## Stage 1: Artifact to OCI Image
1. Operator writes Artifact expressions (which may include Dockerfile/Nix sources).
2. Artifact expressions gets translated into OCI image manifest, a set of filesystem layers, and configuration. The image manifest is hashed and the digest will be the image ID.
3. The image is shared among a distribution of orchestrators, where it will be referenced by image ID by operators.

## Stage 2: OCI Image to Runtime spec
1. When an image is ready to be deployed:
	1. The orchestrator will decide which node to deploy this automaton on.
	2. The assigned node will pull the image from the closest peer, or a central registry.
	3. The Image config is translated to OCI Runtime config.
	4. The filesystem layers are extracted to form the rootfs of the container
2. The orchestrator will populate the runtime spec with more information w.r.t. the deploying node.
3. Container will be deployed on the node.

