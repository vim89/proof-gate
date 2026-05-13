package proofgate.proof

import munit.FunSuite
import proofgate.model.ContractId

final class PipelineDslSuite extends FunSuite:
  final case class OrderOut(id: Long, email: String, amount: BigDecimal)
  final case class OrderContract(id: Long, email: String, amount: BigDecimal)

  test("proposal names reject blanks"):
    assert(ProposalName.from(" ").isLeft)

  test("complete builders can be built"):
    val proposal =
      for
        contract <- ContractId.from("orders.v1")
        withSource <- Builder.empty.source[OrderOut]("orders-source")
        withTransform <- withSource.transform[OrderOut]("normalize-orders")
        complete <- withTransform
          .sink[OrderContract, SchemaPolicy.Exact.type]("orders-sink", contract)
      yield complete.build

    assert(proposal.isRight)

  test("SchemaConforms accepts exact matching case class shapes"):
    val evidence = SchemaConforms.conforms[OrderOut, OrderContract, SchemaPolicy.Exact.type]
    assert(evidence != null)

  test("SchemaConforms rejects missing fields"):
    val errors = compileErrors("""
      import proofgate.proof.*

      final case class Out(id: Long)
      final case class Contract(id: Long, email: String)

      val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Exact.type]]
    """)

    assert(errors.contains("Missing attributes: email : String"))

  test("SchemaConforms rejects extra fields"):
    val errors = compileErrors("""
      import proofgate.proof.*

      final case class Out(id: Long, email: String, segment: String)
      final case class Contract(id: Long, email: String)

      val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Exact.type]]
    """)

    assert(errors.contains("Extra attributes: segment"))

  test("SchemaConforms rejects type drift"):
    val errors = compileErrors("""
      import proofgate.proof.*

      final case class Out(id: Long, email: Long)
      final case class Contract(id: Long, email: String)

      val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Exact.type]]
    """)

    assert(errors.contains("email expected String, found Long"))

  test("builders require sink contract evidence"):
    val errors = compileErrors("""
      import proofgate.model.*
      import proofgate.proof.*

      final case class Out(id: Long)
      final case class Contract(id: Long, email: String)

      val contract = ContractId.unsafe("orders.v1")
      val built =
        Builder.empty
          .source[Out]("orders-source")
          .toOption
          .get
          .sink[Contract, SchemaPolicy.Exact.type]("orders-sink", contract)
    """)

    assert(errors.contains("Missing attributes: email : String"))

  final case class OrderOutWithExtra(
      id: Long,
      email: String,
      amount: BigDecimal,
      segment: String
  )

  test("SchemaConforms accepts Backward when Out adds fields beyond Contract"):
    val evidence =
      SchemaConforms.conforms[OrderOutWithExtra, OrderContract, SchemaPolicy.Backward.type]
    assert(evidence != null)

  test("SchemaConforms rejects Backward when Contract fields are missing in Out"):
    val errors = compileErrors("""
      import proofgate.proof.*

      final case class Out(id: Long, email: String)
      final case class Contract(id: Long, email: String, amount: BigDecimal)

      val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Backward.type]]
    """)

    assert(errors.contains("Missing attributes: amount : BigDecimal"))

  test("SchemaConforms rejects Backward when field types drift"):
    val errors = compileErrors("""
      import proofgate.proof.*

      final case class Out(id: Long, email: Long, amount: BigDecimal)
      final case class Contract(id: Long, email: String, amount: BigDecimal)

      val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Backward.type]]
    """)

    assert(errors.contains("email expected String, found Long"))
