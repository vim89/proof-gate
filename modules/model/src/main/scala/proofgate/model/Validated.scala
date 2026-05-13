package proofgate.model

/** Accumulating result type for stages that should report all findings before stopping, not just
  * the first one. `Either` short-circuits at the first `Left`; `Validated` carries every error from
  * independent steps.
  *
  * The invariant is: invalid values always hold a non-empty error vector. Use the companion factory
  * methods to build values; the public `Invalid` member is an extractor, not a constructor.
  */
sealed trait Validated[+E, +A]:
  import Validated.*

  def map[B](f: A => B): Validated[E, B] =
    this match
      case Valid(value)    => Valid(f(value))
      case InvalidImpl(es) => InvalidImpl(es)

  def andThen[E1 >: E, B](f: A => Validated[E1, B]): Validated[E1, B] =
    this match
      case Valid(value)    => f(value)
      case InvalidImpl(es) => InvalidImpl(es)

  def isValid: Boolean =
    this match
      case Valid(_)       => true
      case InvalidImpl(_) => false

  def toEither: Either[Vector[E], A] =
    this match
      case Valid(value)    => Right(value)
      case InvalidImpl(es) => Left(es)

object Validated:
  final case class Valid[+A](value: A) extends Validated[Nothing, A]

  private final case class InvalidImpl[+E](errors: Vector[E]) extends Validated[E, Nothing]

  object Invalid:
    def unapply[E](value: Validated[E, ?]): Option[Vector[E]] =
      value match
        case InvalidImpl(errors) => Some(errors)
        case _                   => None

  def valid[A](value: A): Validated[Nothing, A] = Valid(value)

  def invalid[E](error: E): Validated[E, Nothing] = InvalidImpl(Vector(error))

  def invalidAll[E](errors: Vector[E]): Validated[E, Nothing] =
    require(errors.nonEmpty, "Invalid must hold at least one error")
    InvalidImpl(errors)

  def fromEither[E, A](either: Either[E, A]): Validated[E, A] =
    either match
      case Right(value) => Valid(value)
      case Left(error)  => InvalidImpl(Vector(error))

  /** Combine independent results, accumulating every error from both sides. */
  extension [E, A](self: Validated[E, A])
    def zip[B](other: Validated[E, B]): Validated[E, (A, B)] =
      (self, other) match
        case (Valid(a), Valid(b))               => Valid((a, b))
        case (InvalidImpl(e1), InvalidImpl(e2)) => InvalidImpl(e1 ++ e2)
        case (InvalidImpl(e), Valid(_))         => InvalidImpl(e)
        case (Valid(_), InvalidImpl(e))         => InvalidImpl(e)

  /** Collect a sequence of independent validations, accumulating every error. */
  def sequence[E, A](values: Vector[Validated[E, A]]): Validated[E, Vector[A]] =
    values.foldLeft[Validated[E, Vector[A]]](Valid(Vector.empty)): (acc, next) =>
      acc.zip(next).map((collected, value) => collected :+ value)
