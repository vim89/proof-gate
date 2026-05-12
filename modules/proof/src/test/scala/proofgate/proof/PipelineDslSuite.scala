package proofgate.proof

import munit.FunSuite
import proofgate.model.ContractId

final class PipelineDslSuite extends FunSuite:
  test("proposal names reject blanks"):
    assert(ProposalName.from(" ").isLeft)

  test("complete builders can be built"):
    val proposal =
      for
        contract <- ContractId.from("orders.v1")
        withSource <- Builder.empty.source[String]("orders-source")
        withTransform <- withSource.transform[String]("normalize-orders")
        complete <- withTransform.sink("orders-sink", contract)
      yield complete.build

    assert(proposal.isRight)
