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
