# ADR 0001: Build a thin review conveyor for LLM-written pipelines

- Status: Accepted
- Date: 2026-05-12

## Context

LLMs can draft data-pipeline code that looks plausible but still violates structural, architectural, or runtime constraints.
That is the main problem this repository exists to address.

The project assumes three things:

1. LLM output is useful as a proposal, not as truth.
2. The first review should be machine-enforced structure, not human opinion.
3. Some failures can be rejected at compile time, while others must still be checked at runtime.

For this repository, the goal is not to build a general AI platform.
The goal is to create a small, understandable review layer that rejects bad pipeline code as early as possible.

## Decision

Build ProofGate as a thin, standalone, Scala 3-first review conveyor with five stages:

1. `proposal`
2. `proof`
3. `policy`
4. `pin`
5. `people`

The intended flow is:

- `proposal`: accept pipeline intent through a typed Scala surface
- `proof`: use Scala types, typestate, and narrow compile-time proof to reject invalid structure
- `policy`: use static rules to reject forbidden patterns and unsafe architecture
- `pin`: run explicit sink-boundary checks for facts that compile time cannot know
- `people`: only after machine gates pass, allow AI and human review

The repository will stay local-first and vendor-neutral.
The core will not depend on a hosted service, database, web UI, or LLM vendor SDK.

The `people` stage is intentionally conjunctive, not optional.
AI review can assist, summarize, and challenge, but it does not replace human ownership.

## Implementation shape

The repository is organized as a small sbt multi-module build:

- `modules/model`: shared ADTs, report model, typed errors
- `modules/proof`: proposal DSL and compile-time review surface
- `modules/runtime-spark`: runtime sink-boundary checks
- `modules/cli`: command entry point
- `modules/examples`: small usage examples
- `modules/fixtures-compile-fail`: negative fixtures and compile-fail cases

The core design rules are:

- errors are values
- invalid states should be made hard to represent
- side effects stay at the edge
- compile-time proof should stay narrow and readable
- runtime checks remain for runtime facts

## Scope for v0

Version `0.x` is a POC.
It should prove the conveyor, not pretend to be a finished platform.

The v0 scope is:

- a typed proposal DSL
- typestate-driven pipeline assembly
- at least one real compile-time proof path
- policy checks through Scalafix
- a runtime sink-boundary pin
- a small review report model
- a CI-friendly command surface

## Non-goals

The following are out of scope for v0:

- autonomous pipeline execution
- proving business correctness
- general-purpose agent orchestration
- a web product or control plane
- a database-backed service
- MCP integration
- vendor-specific review workflows in the core
- a YAML-first or config-first source of truth

## Consequences

This decision has clear trade-offs.

Positive:

- the artifact stays small enough to explain, test, and evolve
- the review order is explicit and defensible
- typed errors and typestate reduce sloppy orchestration paths
- vendor-neutral core logic can survive changes in AI tooling

Negative:

- the initial DSL is narrower than free-form pipeline code
- compile-time machinery must be kept disciplined or it will become hard to maintain
- runtime checks are still necessary, so compile-time proof is not the whole story
- some teams may want integrations or services earlier than this repository will provide

## Follow-up

If the POC proves useful, later ADRs can decide:

- report format stabilization
- repository adapters
- CI annotation outputs
- optional integration layers outside the core
