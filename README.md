Level 2 Inversion
=================

See [DEVELOPMENT.md](./DEVELOPMENT.md)


Unknowns
--------

- [ ] Reading the Metadata store and also having the Metadata Store read from us feels strange. Circular dependency. But updating whatever service is behind Metadata is 
- [ ] (Tony) We need local persistence to know the state of what worked has already been performed, but a new DB may increase the workload for Tony. Using an existing database creates an unwanted dependency. 
- [ ] Who chooses which OP to process next? Do we manually choose one and notify them, or can they work on any ready OP?
- [ ] Han wants to work on OPs, not datasets. How do we know when an OP is complete? Are all datasets activated at once?

Service Graph
-------------

```mermaid
graph LR;
    L2[Level 2]
    PG[(Postgres)]
    Meta[Metadata Store]
    Events[Event Bus]
    Globus[GLOBUS]
    Sci[Science / CU Blanca]

    Meta --> Portal
    Meta --- L2
    L2 --- PG
    L2 --- Events
    L2 --- Globus
    Events --- Sci
    Globus --- Sci
```



Process
-------

```mermaid
flowchart LR;
    Scan[[Scan]]
    Qualify[[Qualify]]
    Preprocess[[Preprocess]]
    Invert[[Invert]]
    Publish[[Publish]]

    Scan --> Qualify
    Qualify --> Preprocess
    Preprocess --> Invert
    subgraph Science
      Invert
    end
    Invert --> Publish
```

_Scan_ - Check the entire metadata store for OP candidates and add to the database if they don't exist. Update if necessary

_Qualify_ - Check criteria for invertibility and record if OPs are invertible or not

_Preprocess_ - Preprocess data if necessary, saving intermediate files as needed

_Invert_ - Han's Science Team manually inverts the OP

_Publish_ - Make L2 data available to metadata store / portal












Inversion States
----------------

Observing Programs are identified, then pass through the following states.

```mermaid
stateDiagram-v2
    dis: Discovered
    inv: Invertible
    no: Not Invertible
    pre: Preprocessed
    qd: Queued
    done: Inverted
    pub: Published
    err: Error

    dis --> inv
    dis --> no
    inv --> pre
    pre --> qd
    qd --> done: Science
    done --> pub
```
States are cumulative, meaning an OP at the `Inverted` state, would have the states:

    Discovered
    Invertible
    Preprocessed
    Queued
    Inverted

 And an OP that errored during preprocessing would have the states:

    Discovered
    Invertible
    Error "Preprocessing ____"
