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

  test("SchemaConforms accepts Exact when names only differ by case"):
    final case class Out(id: Long, Email: String, amount: BigDecimal)

    val evidence = SchemaConforms.conforms[Out, OrderContract, SchemaPolicy.Exact.type]
    assert(evidence != null)

  test("SchemaConforms accepts Exact when field-level optionality differs"):
    final case class Out(id: Long, email: String, amount: BigDecimal)
    final case class Contract(id: Long, email: Option[String], amount: BigDecimal)

    val evidence = SchemaConforms.conforms[Out, Contract, SchemaPolicy.Exact.type]
    assert(evidence != null)

  test("SchemaConforms accepts ExactUnorderedCI when order and case differ"):
    final case class Out(AMOUNT: BigDecimal, Email: String, id: Long)

    val evidence =
      SchemaConforms.conforms[Out, OrderContract, SchemaPolicy.ExactUnorderedCI.type]
    assert(evidence != null)

  test("SchemaConforms rejects duplicate names under case-insensitive Exact"):
    val errors = compileErrors("""
      import proofgate.proof.*

      final case class Out(Email: String, email: String)
      final case class Contract(email: String)

      val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Exact.type]]
    """)

    assert(errors.contains("duplicate Out field names [Email, email]"))

  test("SchemaConforms rejects ExactOrdered when fields are reordered"):
    val errors = compileErrors("""
      import proofgate.proof.*

      final case class Out(email: String, id: Long, amount: BigDecimal)
      final case class Contract(id: Long, email: String, amount: BigDecimal)

      val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.ExactOrdered.type]]
    """)

    assert(errors.contains("@0(name)") || errors.contains("id expected"))

  test("SchemaConforms accepts ExactOrderedCI when order matches and case differs"):
    final case class Out(ID: Long, EMAIL: String, AMOUNT: BigDecimal)

    val evidence =
      SchemaConforms.conforms[Out, OrderContract, SchemaPolicy.ExactOrderedCI.type]
    assert(evidence != null)

  test("SchemaConforms accepts ExactByPosition when names differ but positions match"):
    final case class Out(a: Long, b: String, c: BigDecimal)

    val evidence =
      SchemaConforms.conforms[Out, OrderContract, SchemaPolicy.ExactByPosition.type]
    assert(evidence != null)

  test("SchemaConforms accepts Full even when shapes drift"):
    final case class Out(id: Long)
    final case class Contract(id: String, email: String)

    val evidence = SchemaConforms.conforms[Out, Contract, SchemaPolicy.Full.type]
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

  final case class OrderContractWithOptional(
      id: Long,
      email: String,
      amount: BigDecimal,
      segment: Option[String]
  )

  test("SchemaConforms accepts Backward when Out adds fields beyond Contract"):
    val evidence =
      SchemaConforms.conforms[OrderOutWithExtra, OrderContract, SchemaPolicy.Backward.type]
    assert(evidence != null)

  test("SchemaConforms accepts Backward when a missing Contract field is optional"):
    val evidence =
      SchemaConforms.conforms[OrderOut, OrderContractWithOptional, SchemaPolicy.Backward.type]
    assert(evidence != null)

  test("SchemaConforms accepts Backward when a missing Contract field has a default"):
    final case class ContractWithDefault(
        id: Long,
        email: String,
        amount: BigDecimal,
        region: String = "IN"
    )

    val evidence =
      SchemaConforms.conforms[OrderOut, ContractWithDefault, SchemaPolicy.Backward.type]
    assert(evidence != null)

  test("SchemaConforms rejects Backward when Contract fields are missing in Out"):
    val errors = compileErrors("""
      import proofgate.proof.*

      final case class Out(id: Long, email: String)
      final case class Contract(id: Long, email: String, amount: BigDecimal)

      val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Backward.type]]
    """)

    assert(errors.contains("Missing attributes: amount : BigDecimal"))

  test("SchemaConforms accepts Forward when Out drops fields Contract declares"):
    val evidence =
      SchemaConforms.conforms[OrderOut, OrderContractWithExtra, SchemaPolicy.Forward.type]
    assert(evidence != null)

  final case class OrderContractWithExtra(
      id: Long,
      email: String,
      amount: BigDecimal,
      segment: String
  )

  test("SchemaConforms rejects Forward when Out adds fields beyond Contract"):
    val errors = compileErrors("""
      import proofgate.proof.*

      final case class Out(id: Long, email: String, segment: String)
      final case class Contract(id: Long, email: String)

      val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Forward.type]]
    """)

    assert(errors.contains("Extra attributes: segment"))

  test("SchemaConforms rejects Backward when field types drift"):
    val errors = compileErrors("""
      import proofgate.proof.*

      final case class Out(id: Long, email: Long, amount: BigDecimal)
      final case class Contract(id: Long, email: String, amount: BigDecimal)

      val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Backward.type]]
    """)

    assert(errors.contains("email expected String, found Long"))
