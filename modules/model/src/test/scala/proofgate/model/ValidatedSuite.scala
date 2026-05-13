package proofgate.model

import munit.FunSuite

final class ValidatedSuite extends FunSuite:
  test("valid carries the value"):
    assertEquals(Validated.valid(42).map(_ + 1), Validated.Valid(43))

  test("invalid short-circuits map"):
    val errors = Vector("bad")
    assertEquals(
      Validated.fromErrors(errors).map(_.map(_.toString).toEither),
      Some(Left(errors))
    )

  test("andThen sequences over Valid and stops at Invalid"):
    val ok = Validated.valid(1).andThen(value => Validated.valid(value + 1))
    assertEquals(ok, Validated.Valid(2))

    val fail =
      Validated.invalid("first").andThen(_ => Validated.valid(99))
    assertEquals(fail.toEither, Left(Vector("first")))

  test("zip accumulates errors from both sides"):
    val combined = Validated.invalid("a").zip(Validated.invalid("b"))
    assertEquals(combined.toEither, Left(Vector("a", "b")))

  test("zip passes both values when both valid"):
    val combined = Validated.valid(1).zip(Validated.valid("x"))
    assertEquals(combined, Validated.Valid((1, "x")))

  test("sequence collects all errors across independent validations"):
    val items: Vector[Validated[String, Int]] =
      Vector(
        Validated.valid(1),
        Validated.invalid("first"),
        Validated.valid(3),
        Validated.invalid("second")
      )

    assertEquals(
      Validated.sequence(items).toEither,
      Left(Vector("first", "second"))
    )

  test("sequence returns the collected values when every entry is valid"):
    val items: Vector[Validated[String, Int]] =
      Vector(Validated.valid(1), Validated.valid(2), Validated.valid(3))

    assertEquals(Validated.sequence(items), Validated.Valid(Vector(1, 2, 3)))

  test("fromErrors rejects empty error vectors without throwing"):
    assertEquals(Validated.fromErrors(Vector.empty[String]), None)

  test("invalidAll requires at least one error by construction"):
    assertEquals(Validated.invalidAll("first", "second").toEither, Left(Vector("first", "second")))

  test("Invalid constructor is not public outside proofgate"):
    val errors = compileErrors("""
      import proofgate.model.Validated

      val invalid = Validated.Invalid(Vector.empty[String])
    """)

    assert(errors.nonEmpty)
    assert(errors.contains("Invalid"))

  test("fromEither bridges Either"):
    assertEquals(Validated.fromEither(Right(1)), Validated.Valid(1))
    assertEquals(
      Validated.fromEither(Left("bad")).toEither,
      Left(Vector("bad"))
    )

  test("toEither bridges back"):
    assertEquals(Validated.valid(1).toEither, Right(1))
    assertEquals(
      Validated.invalid("bad").toEither,
      Left(Vector("bad"))
    )
