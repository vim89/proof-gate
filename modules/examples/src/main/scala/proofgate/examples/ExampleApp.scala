package proofgate.examples

import proofgate.model.ContractId
import proofgate.proof.*

object ExampleApp:
  final case class DemoOut(id: Long, email: String)
  final case class DemoContract(id: Long, email: String)

  def sample(): Either[String, String] =
    val built =
      for
        contract <- ContractId.from("demo.contract").left.map(identity)
        withSource <- Builder.empty.source[DemoOut]("demo-source").left.map(_.message)
        withTransform <- withSource.transform[DemoOut]("normalize").left.map(_.message)
        complete <- withTransform
          .sink[DemoContract, SchemaPolicy.Exact.type]("demo-sink", contract)
          .left
          .map(_.message)
      yield complete.build

    built.map(_.contractId.value)

  // Backward demo: Out adds a column beyond Contract. Old consumers keep working.
  final case class BackwardOut(id: Long, email: String, segment: String)
  final case class BackwardContract(id: Long, email: String)

  def backwardSample(): Either[String, String] =
    val built =
      for
        contract <- ContractId.from("backward.contract")
        withSource <- Builder.empty
          .source[BackwardOut]("backward-source")
          .left
          .map(_.message)
        complete <- withSource
          .sink[BackwardContract, SchemaPolicy.Backward.type]("backward-sink", contract)
          .left
          .map(_.message)
      yield complete.build

    built.map(_.contractId.value)

  // Forward demo: Out drops a column Contract declares. New consumers can default it.
  final case class ForwardOut(id: Long, email: String)
  final case class ForwardContract(id: Long, email: String, segment: String)

  def forwardSample(): Either[String, String] =
    val built =
      for
        contract <- ContractId.from("forward.contract")
        withSource <- Builder.empty
          .source[ForwardOut]("forward-source")
          .left
          .map(_.message)
        complete <- withSource
          .sink[ForwardContract, SchemaPolicy.Forward.type]("forward-sink", contract)
          .left
          .map(_.message)
      yield complete.build

    built.map(_.contractId.value)
