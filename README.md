# ProofGate

Nice try, LLM. Now prove it.

ProofGate is a Scala-native review conveyor for LLM-written data-pipeline code.
The core idea is simple:

1. proposal
2. proof
3. policy
4. pin
5. people

The first reviewer should be machine-enforced structure, not a human and not another LLM.

## Review flow

```mermaid
flowchart LR
  A[LLM draft] --> B[proposal]
  B --> C[proof<br/>macros inline typestate]
  C --> D[policy<br/>Scalafix rules]
  D --> E[pin<br/>runtime sink checks]
  E --> F[people<br/>AI and human review]

  C -. fail .-> X[reject early]
  D -. fail .-> X
  E -. fail .-> X
```

## Current stack

This repository currently targets the stable stack:

- JDK `21`
- Scala `3.8.3`
- sbt `1.12.11`
- Scalafmt `3.11.1`
- Scalafix / sbt-scalafix `0.14.6`
- MUnit `1.3.0`

## Design notes

- [ADR 0001: review conveyor](docs/adr/0001-review-conveyor.md)

## Module layout

- `modules/model`: core ADTs, typed errors, report model
- `modules/proof`: proposal DSL, typestate builder, `SchemaConforms` proof
- `modules/runtime-spark`: runtime pin abstractions and Spark-facing boundary
- `modules/cli`: CLI entry point
- `modules/examples`: tiny usage examples
- `modules/fixtures-compile-fail`: reference snippets for compile-fail demos

## Commands

```bash
sbt test
sbt reviewGates
sbt reviewPolicy
sbt reviewConveyor
```

## Status

This is an early POC scaffold.
The current code proves the build, module wiring, typed-error model, typestate assembly, and the first exact structural
contract proof.
