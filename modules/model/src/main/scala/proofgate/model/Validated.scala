package proofgate.model

/** Accumulating result type for stages that should report all findings before stopping, not just
  * the first one. `Either` short-circuits at the first `Left`; `Validated` carries every error from
  * independent steps.
  *
  * The invariant is: `Invalid` always holds a non-empty error vector. Use the companion factory
  * methods to build values; the public extension `zip` keeps the invariant under composition.
  */
enum Validated[+E, +A]:
  case Valid(value: A)
  case Invalid(errors: Vector[E])

  def map[B](f: A => B): Validated[E, B] =
    this match
      case Valid(value) => Valid(f(value))
      case Invalid(es)  => Invalid(es)

  def andThen[E1 >: E, B](f: A => Validated[E1, B]): Validated[E1, B] =
    this match
      case Valid(value) => f(value)
      case Invalid(es)  => Invalid(es)

  def isValid: Boolean =
    this match
      case Valid(_)   => true
      case Invalid(_) => false

  def toEither: Either[Vector[E], A] =
    this match
      case Valid(value) => Right(value)
      case Invalid(es)  => Left(es)

object Validated:
  def valid[A](value: A): Validated[Nothing, A] = Valid(value)

  def invalid[E](error: E): Validated[E, Nothing] = Invalid(Vector(error))

  def invalidAll[E](errors: Vector[E]): Validated[E, Nothing] =
    require(errors.nonEmpty, "Invalid must hold at least one error")
    Invalid(errors)

  def fromEither[E, A](either: Either[E, A]): Validated[E, A] =
    either match
      case Right(value) => Valid(value)
      case Left(error)  => Invalid(Vector(error))

  /** Combine independent results, accumulating every error from both sides. */
  extension [E, A](self: Validated[E, A])
    def zip[B](other: Validated[E, B]): Validated[E, (A, B)] =
      (self, other) match
        case (Valid(a), Valid(b))       => Valid((a, b))
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
        case (Invalid(e), Valid(_))     => Invalid(e)
        case (Valid(_), Invalid(e))     => Invalid(e)

  /** Collect a sequence of independent validations, accumulating every error. */
  def sequence[E, A](values: Vector[Validated[E, A]]): Validated[E, Vector[A]] =
    values.foldLeft[Validated[E, Vector[A]]](Valid(Vector.empty)): (acc, next) =>
      acc.zip(next).map((collected, value) => collected :+ value)
