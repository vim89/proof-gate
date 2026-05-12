package proofgate.proof

import proofgate.model.*

enum SchemaPolicy:
  case Exact, Backward, Forward

trait Source[A]

trait Sink[A]

trait Transform[-A, +B]:
  def apply(in: A): B

final case class ProposalName private (value: String) extends AnyVal

object ProposalName:
  def from(value: String): Result[ProposalName] =
    val trimmed = value.trim
    Either.cond(
      trimmed.nonEmpty,
      ProposalName(trimmed),
      ReviewError.ProposalParse(None, "Proposal names cannot be blank")
    )

sealed trait Empty
sealed trait HasSource
sealed trait HasTransform
sealed trait Complete

final case class PipelineProposal[In, Out](
    source: ProposalName,
    transforms: Vector[ProposalName],
    sink: ProposalName,
    contractId: ContractId
)

trait SchemaConforms[Out, Contract, P <: SchemaPolicy]

object SchemaConforms:
  given identity[A]: SchemaConforms[A, A, SchemaPolicy.Exact.type] with {}

final class EmptyBuilder private[proof] ():
  def source[A](name: String): Result[SourceBuilder[A, A]] =
    ProposalName.from(name).map { parsed =>
      SourceBuilder[A, A](
        sourceName = parsed,
        transforms = Vector.empty
      )
    }

final class SourceBuilder[In, Out] private[proof] (
    private val sourceName: ProposalName,
    private val transforms: Vector[ProposalName]
):
  def transform[Next](name: String): Result[SourceBuilder[In, Next]] =
    ProposalName.from(name).map { parsed =>
      SourceBuilder[In, Next](
        sourceName = sourceName,
        transforms = transforms :+ parsed
      )
    }

  def sink(name: String, contract: ContractId): Result[CompleteBuilder[In, Out]] =
    ProposalName.from(name).map { parsed =>
      CompleteBuilder[In, Out](
        sourceName = sourceName,
        transforms = transforms,
        sinkName = parsed,
        contractId = contract
      )
    }

final class CompleteBuilder[In, Out] private[proof] (
    private val sourceName: ProposalName,
    private val transforms: Vector[ProposalName],
    private val sinkName: ProposalName,
    private val contractId: ContractId
):
  def build: PipelineProposal[In, Out] =
    PipelineProposal(
      source = sourceName,
      transforms = transforms,
      sink = sinkName,
      contractId = contractId
    )

object Builder:
  def empty: EmptyBuilder = EmptyBuilder()
