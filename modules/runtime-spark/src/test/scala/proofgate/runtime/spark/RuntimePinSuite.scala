package proofgate.runtime.spark

import munit.FunSuite
import proofgate.proof.SchemaPolicy

final class RuntimePinSuite extends FunSuite:
  final case class Address(city: String, zip: Option[Int])
  final case class OrderActual(
      id: Long,
      email: String,
      tags: List[Option[String]],
      address: Address
  )
  final case class OrderExpected(
      id: Long,
      email: String,
      tags: List[Option[String]],
      address: Address
  )

  test("runtime shape derivation produces exact shapes for matching contracts"):
    val actual = RuntimeShapeEncoder.shapeOf[OrderActual]
    val expected = RuntimeShapeEncoder.shapeOf[OrderExpected]

    val findings = summon[RuntimePin[SchemaPolicy.Exact.type]].validate(actual, expected)

    assertEquals(findings, Vector.empty)

  test("runtime pin reports missing fields with paths"):
    final case class Actual(id: Long)
    final case class Expected(id: Long, email: String)

    val findings =
      summon[RuntimePin[SchemaPolicy.Exact.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings.map(_.path), Vector(Some("email")))
    assert(findings.head.message.contains("expected String, found <missing>"))

  test("runtime pin reports type drift with paths"):
    final case class Actual(id: Long, email: Long)
    final case class Expected(id: Long, email: String)

    val findings =
      summon[RuntimePin[SchemaPolicy.Exact.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings.map(_.path), Vector(Some("email")))
    assert(findings.head.message.contains("expected String, found Long"))

  test("runtime pin reports nested optionality drift"):
    final case class Actual(tags: List[String])
    final case class Expected(tags: List[Option[String]])

    val findings =
      summon[RuntimePin[SchemaPolicy.Exact.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings.map(_.path), Vector(Some("tags[]")))
    assert(findings.head.message.contains("expected Option[String], found String"))

  test("runtime pin ignores field-level nullability drift"):
    final case class ActualAddress(city: String, zip: Int)
    final case class ExpectedAddress(city: String, zip: Option[Int])
    final case class Actual(address: ActualAddress)
    final case class Expected(address: ExpectedAddress)

    val findings =
      summon[RuntimePin[SchemaPolicy.Exact.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings, Vector.empty)

  test("runtime pin rejects nested optionality drift inside arrays"):
    final case class Actual(tags: List[String])
    final case class Expected(tags: List[Option[String]])

    val findings =
      summon[RuntimePin[SchemaPolicy.Exact.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings.map(_.path), Vector(Some("tags[]")))
    assert(findings.head.message.contains("expected Option[String], found String"))

  test("runtime pin accepts ExactUnorderedCI when order and case differ"):
    final case class Actual(EMAIL: String, id: Long)
    final case class Expected(id: Long, email: String)

    val findings =
      summon[RuntimePin[SchemaPolicy.ExactUnorderedCI.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings, Vector.empty)

  test("runtime pin rejects case-insensitive duplicate names"):
    val actual = RuntimeShape(
      Vector(
        RuntimeField("Email", RuntimeType.Primitive("String"), nullable = false),
        RuntimeField("email", RuntimeType.Primitive("String"), nullable = false)
      )
    )
    val expected =
      RuntimeShape(Vector(RuntimeField("email", RuntimeType.Primitive("String"), nullable = false)))

    val findings =
      summon[RuntimePin[SchemaPolicy.Exact.type]].validate(actual, expected)

    assert(findings.exists(_.message.contains("duplicate actual field names [Email, email]")))

  test("runtime pin rejects ExactOrdered when fields are reordered"):
    final case class Actual(email: String, id: Long)
    final case class Expected(id: Long, email: String)

    val findings =
      summon[RuntimePin[SchemaPolicy.ExactOrdered.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assert(findings.exists(_.path.contains("id.@0(name)")))

  test("runtime pin accepts ExactByPosition when names differ but positions match"):
    final case class Actual(a: Long, b: String)
    final case class Expected(id: Long, email: String)

    val findings =
      summon[RuntimePin[SchemaPolicy.ExactByPosition.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings, Vector.empty)

  test("runtime pin accepts Backward when actual adds fields"):
    final case class Actual(id: Long, email: String, segment: String)
    final case class Expected(id: Long, email: String)

    val findings =
      summon[RuntimePin[SchemaPolicy.Backward.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings, Vector.empty)

  test("runtime pin rejects duplicate names under Backward"):
    val actual = RuntimeShape(
      Vector(
        RuntimeField("id", RuntimeType.Primitive("Long"), nullable = false),
        RuntimeField("id", RuntimeType.Primitive("Long"), nullable = false)
      )
    )
    val expected =
      RuntimeShape(Vector(RuntimeField("id", RuntimeType.Primitive("Long"), nullable = false)))

    val findings =
      summon[RuntimePin[SchemaPolicy.Backward.type]].validate(actual, expected)

    assert(findings.exists(_.message.contains("duplicate actual field names [id, id]")))

  test("runtime pin accepts Backward when expected missing field is nullable"):
    final case class Actual(id: Long)
    final case class Expected(id: Long, email: Option[String])

    val findings =
      summon[RuntimePin[SchemaPolicy.Backward.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings, Vector.empty)

  test("runtime pin accepts Backward when expected missing field has default metadata"):
    val actual =
      RuntimeShape(Vector(RuntimeField("id", RuntimeType.Primitive("Long"), nullable = false)))
    val expected = RuntimeShape(
      Vector(
        RuntimeField("id", RuntimeType.Primitive("Long"), nullable = false),
        RuntimeField(
          "region",
          RuntimeType.Primitive("String"),
          nullable = false,
          hasDefault = true
        )
      )
    )

    val findings =
      summon[RuntimePin[SchemaPolicy.Backward.type]].validate(actual, expected)

    assertEquals(findings, Vector.empty)

  test("runtime pin accepts Forward when actual drops fields"):
    final case class Actual(id: Long)
    final case class Expected(id: Long, email: String)

    val findings =
      summon[RuntimePin[SchemaPolicy.Forward.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings, Vector.empty)

  test("runtime pin accepts Full even when shapes drift"):
    final case class Actual(id: Long)
    final case class Expected(id: String, email: String)

    val findings =
      summon[RuntimePin[SchemaPolicy.Full.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings, Vector.empty)
