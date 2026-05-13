package proofgate.runtime.spark

import munit.FunSuite
import proofgate.proof.SchemaPolicy

final class RuntimePinSuite extends FunSuite:
  final case class Address(city: String, zip: Option[Int])
  final case class OrderActual(id: Long, email: String, tags: List[Option[String]], address: Address)
  final case class OrderExpected(id: Long, email: String, tags: List[Option[String]], address: Address)

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

  test("runtime pin reports nested field nullability drift"):
    final case class ActualAddress(city: String, zip: Int)
    final case class ExpectedAddress(city: String, zip: Option[Int])
    final case class Actual(address: ActualAddress)
    final case class Expected(address: ExpectedAddress)

    val findings =
      summon[RuntimePin[SchemaPolicy.Exact.type]]
        .validate(RuntimeShapeEncoder.shapeOf[Actual], RuntimeShapeEncoder.shapeOf[Expected])

    assertEquals(findings.map(_.path), Vector(Some("address.zip")))
    assert(findings.head.message.contains("expected Int nullable, found Int"))
