package proofgate.examples

import proofgate.model.ContractId
import proofgate.proof.Builder

object ExampleApp:
  def sample(): Either[String, String] =
    val built =
      for
        contract <- ContractId.from("demo.contract").left.map(identity)
        withSource <- Builder.empty.source[String]("demo-source").left.map(_.message)
        withTransform <- withSource.transform[String]("normalize").left.map(_.message)
        complete <- withTransform.sink("demo-sink", contract).left.map(_.message)
      yield complete.build

    built.map(_.contractId.value)
