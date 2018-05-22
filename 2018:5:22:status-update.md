# Architect Language

A few things that I've been working on since I started working on Architect specification.
1. Automatons's Artifact spec
2. Addressing of the automatons, how can we construct a deterministic content address.
3. How will the automatons be found/referenced.
4. Deployment of automatons, CI/CD system

# Artifact spec
Artifact spec is basically container image specification and runtime specification.
Right now the plan is to utilise OCI's image/runtime specification. I've got a pretty solid understanding on the image specification now, but still need some work on runtime specification.

There are a few ways to achieve this however:
1. Using fixed-output-derivation to

# Automaton content addresses
The Architect for an automaton can be separated into various categories, I mainly deal with the artifact spec but there are a lot of others that we need to understand how they can be content addressed.

Nix and Docker are both capable of producing deterministic results based on their input, they both have their own kinds of obscure way of calculating hashes, and the documentations are hard to find.

Docker:
  - Dockerfile needs to be traslated into *Image manifest*.
  - *Image manfiest* can be used to calculate the content hashes.
  - Probably doesn't deal with fixed output derivation.

Nix:
  - Nix expressions needs to be evaluated into store derivations.
  - Outpath is different to derivation paths
  - Pros: This method is more flexible compared to Dockerfiles
  - Cons: Requires Nix to be installed.

Needs more investigation:
  - OCI Runtime spec
  - dockerTools source code
  - Haskell binding:
    - hnix - Nix reimplementation in Haskell

Some ideas:
  - Architect spec can be divided into various parts, and be addressed at component level.
  - Both Nix and Docker needs to be parsed into other forms in order to be fully deterministic (store derivation for Nix, and image manifest for docker). This means that to deploy and replicate these automaton specifications, those files needs to be copied, rather than the nix expressions and Dockerfiles.

# How will the automatons be found and referenced
There will be an Architect daemon in every node listing to valid Architect specifications.

Needs investigation
- global addressing vs local addressing
  - To what extend should automaton addresses be local, and global?

# Deployment of Automatons
- The orchestrator
  - Investigate existing orchestrators, Docker SWARM, Kubernetes
  - CI/CD system, DevOps


# Usability
This is something that I question myself a lot of the times when thinking about content addressing.
There is a global registry with all the addresses to files,


# Top Priority
1. Haskell binding for libcontainer
