# Virtual Synchrony
virtual synchrony is a interprocess message passing (ordered, reliable multicast) technique that allows messages to be sent to a group of processes, where all processes within a group see the messages in the same order. it is achieved via **state machine replication**.

## Fault tolerance
If one replica crashes, others remain and can continue to provide responses. Members of the replica group can also be programmed to subdivide the workload. This permits a group of N members to run as much as N times faster than a single member.

## Implementation details
The key idea is to create a form of distributed state machine associated with the replicated data item. Called a *process group*. These state machines share copies of the data, and updates are delivered as events that occur in the same order at all copies (I'd imagine using totally ordered broadcast). If a process fails or crashes, this is reported to the other processes in the group. If a process joins, this is similarly reported, and a state transfer is used to initialize the joining member.

# Replica
- **Active replication** - processing the same request at every replica. This typically forwards all messages to a central coordinator (leader), and the coordinator sequentially chooses an unique sequence number for each message, then sends the sequence number along with the message to all replicas. Operations are carried out by the order of the sequence number.

Protocols:
- Primary backup protocol
  - Pro: guarantees sequential consistency
  - Cons: Delay answer due to blocking execution
- Local-write primary backup
  - Pro: Does not block
- Quorum System
  - is a set S of quorums that are mutually intersecting.
  - Consider N replicas:
  - Upon read, forward the request to at least Nr servers (a read quorum)
  - Upon write, forward the request to at least Nw servers (a write quorum) s.t.
    - Nr + Nw > N
    - Nw > N/2

- Passive replication - process each single request at one replica and transfer resultant state to other replicas.
- lazy replication - allow two transactions to commit and run a conflict resolution during resynchronization. Resolution may be based on a timestamp or other more complex logic. Used by databases.
- Disk storage

## Models
- Transactional replication - replicating transactional data.
- State machine replication - Assumes that replicated process is a deterministic finite automaton, and that atomic braodcast of every event is possible. Usually implemented by replicated log consisting of multiple subsequent round of the *Paxos algorithm*.
- Virtual synchrony - used when a group of processes coorporate to replicate in-memory data or to coordinate actions.

# Paxos Algorithm
