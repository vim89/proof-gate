package proofgate.proof

import proofgate.model.*
import scala.quoted.*

enum SchemaPolicy:
  case Exact, ExactUnorderedCI, ExactOrdered, ExactOrderedCI, ExactByPosition, Backward, Forward,
    Full

object SchemaPolicy:
  type Exact = SchemaPolicy.Exact.type
  type ExactUnorderedCI = SchemaPolicy.ExactUnorderedCI.type
  type ExactOrdered = SchemaPolicy.ExactOrdered.type
  type ExactOrderedCI = SchemaPolicy.ExactOrderedCI.type
  type ExactByPosition = SchemaPolicy.ExactByPosition.type
  type Backward = SchemaPolicy.Backward.type
  type Forward = SchemaPolicy.Forward.type
  type Full = SchemaPolicy.Full.type

trait Source[A]

trait Sink[A]

trait Transform[-A, +B]:
  def apply(in: A): B

final case class ProposalName private (value: String) extends AnyVal

object ProposalName:
  def from(value: String): Result[ProposalName] =
    val trimmed = value.trim
    Either.cond(
      trimmed.nonEmpty,
      ProposalName(trimmed),
      ReviewError.ProposalParse(None, "Proposal names cannot be blank")
    )

sealed trait Empty
sealed trait HasSource
sealed trait HasTransform
sealed trait Complete

final case class PipelineProposal[In, Out](
    source: ProposalName,
    transforms: Vector[ProposalName],
    sink: ProposalName,
    contractId: ContractId
)

trait SchemaConforms[Out, Contract, P <: SchemaPolicy]

object SchemaConforms:
  inline given derived[Out, Contract, P <: SchemaPolicy]: SchemaConforms[Out, Contract, P] =
    ${ SchemaConformsMacro.derive[Out, Contract, P] }

  inline def conforms[Out, Contract, P <: SchemaPolicy](using
      evidence: SchemaConforms[Out, Contract, P]
  ): SchemaConforms[Out, Contract, P] =
    evidence

final class EmptyBuilder private[proof] ():
  def source[A](name: String): Result[SourceBuilder[A, A]] =
    ProposalName.from(name).map { parsed =>
      SourceBuilder[A, A](
        sourceName = parsed,
        transforms = Vector.empty
      )
    }

final class SourceBuilder[In, Out] private[proof] (
    private val sourceName: ProposalName,
    private val transforms: Vector[ProposalName]
):
  def transform[Next](name: String): Result[SourceBuilder[In, Next]] =
    ProposalName.from(name).map { parsed =>
      SourceBuilder[In, Next](
        sourceName = sourceName,
        transforms = transforms :+ parsed
      )
    }

  def sink[Contract, P <: SchemaPolicy](name: String, contract: ContractId)(using
      SchemaConforms[Out, Contract, P]
  ): Result[CompleteBuilder[In, Out]] =
    ProposalName.from(name).map { parsed =>
      CompleteBuilder[In, Out](
        sourceName = sourceName,
        transforms = transforms,
        sinkName = parsed,
        contractId = contract
      )
    }

final class CompleteBuilder[In, Out] private[proof] (
    private val sourceName: ProposalName,
    private val transforms: Vector[ProposalName],
    private val sinkName: ProposalName,
    private val contractId: ContractId
):
  def build: PipelineProposal[In, Out] =
    PipelineProposal(
      source = sourceName,
      transforms = transforms,
      sink = sinkName,
      contractId = contractId
    )

object Builder:
  def empty: EmptyBuilder = EmptyBuilder()

private object SchemaConformsMacro:
  def derive[Out: Type, Contract: Type, P <: SchemaPolicy: Type](using
      q: Quotes
  ): Expr[SchemaConforms[Out, Contract, P]] =
    import q.reflect.*

    val isExact =
      TypeRepr.of[P] =:= TypeRepr.of[SchemaPolicy.Exact.type] ||
        TypeRepr.of[P] =:= TypeRepr.of[SchemaPolicy.ExactUnorderedCI.type]
    val isExactOrdered = TypeRepr.of[P] =:= TypeRepr.of[SchemaPolicy.ExactOrdered.type]
    val isExactOrderedCI = TypeRepr.of[P] =:= TypeRepr.of[SchemaPolicy.ExactOrderedCI.type]
    val isExactByPosition = TypeRepr.of[P] =:= TypeRepr.of[SchemaPolicy.ExactByPosition.type]
    val isBackward = TypeRepr.of[P] =:= TypeRepr.of[SchemaPolicy.Backward.type]
    val isForward = TypeRepr.of[P] =:= TypeRepr.of[SchemaPolicy.Forward.type]
    val isFull = TypeRepr.of[P] =:= TypeRepr.of[SchemaPolicy.Full.type]

    if !(isExact || isExactOrdered || isExactOrderedCI || isExactByPosition || isBackward || isForward || isFull)
    then
      report.errorAndAbort(
        s"Unsupported SchemaPolicy requested: ${Type.show[P]}"
      )

    val outShape = shapeOf(using q)(TypeRepr.of[Out])
    val contractShape = shapeOf(using q)(TypeRepr.of[Contract])
    val caseInsensitive = isExact || isExactOrderedCI
    val rawDiffs =
      if isExactByPosition then compareByPosition("", outShape, contractShape)
      else if isExactOrdered || isExactOrderedCI then
        compareOrdered("", outShape, contractShape, caseInsensitive)
      else compareByName("", outShape, contractShape, caseInsensitive)

    // Backward: Out may add fields beyond Contract. Missing required fields and type drift fail.
    // Forward: Out may drop fields Contract declares. Extras and type drift fail.
    val diffs =
      if isFull then Nil
      else if isBackward then
        rawDiffs.filter {
          case Diff.Extra(_)          => false
          case Diff.Missing(_, field) => !(field.optional || field.hasDefault)
          case _                      => true
        }
      else if isForward then
        rawDiffs.filter {
          case Diff.Missing(_, _) => false
          case _                  => true
        }
      else rawDiffs

    if diffs.nonEmpty then
      val missing = diffs.collect { case Diff.Missing(path, field) =>
        s"$path : ${renderField(field)}"
      }
      val extra = diffs.collect { case Diff.Extra(path) => path }
      val mismatches = diffs.collect { case Diff.Mismatch(path, expected, found) =>
        s"$path expected ${render(expected)}, found ${render(found)}"
      }

      report.errorAndAbort(
        s"""Compile-time contract drift (policy: ${Type.show[P]}).
           |Out: ${Type.show[Out]} vs Contract: ${Type.show[Contract]}
           |Missing attributes: ${missing.mkString(", ")}
           |Extra attributes: ${extra.mkString(", ")}
           |Mismatch attributes: ${mismatches.mkString("; ")}
           |""".stripMargin
      )

    '{ new SchemaConforms[Out, Contract, P] {} }

  private enum Shape:
    case Primitive(name: String)
    case Optional(inner: Shape)
    case Sequence(element: Shape)
    case MapShape(key: Shape.Primitive, value: Shape)
    case Struct(fields: List[Field])

  private final case class Field(name: String, shape: Shape, hasDefault: Boolean, optional: Boolean)

  private enum Diff:
    case Missing(path: String, field: Field)
    case Extra(path: String)
    case Mismatch(path: String, expected: Shape, found: Shape)

  private def shapeOf(using q: Quotes)(tpe: q.reflect.TypeRepr): Shape =
    import q.reflect.*

    def appliedArgs(t: TypeRepr): List[TypeRepr] =
      t match
        case AppliedType(_, args) => args
        case _                    => Nil

    def optionArg(t: TypeRepr): Option[TypeRepr] =
      if t <:< TypeRepr.of[Option[?]] then appliedArgs(t).headOption else None

    def sequenceArg(t: TypeRepr): Option[TypeRepr] =
      val isSequence =
        t <:< TypeRepr.of[List[?]] ||
          t <:< TypeRepr.of[Seq[?]] ||
          t <:< TypeRepr.of[Vector[?]] ||
          t <:< TypeRepr.of[Array[?]] ||
          t <:< TypeRepr.of[Set[?]]

      if isSequence then
        Some(
          appliedArgs(t).headOption.getOrElse(
            report.errorAndAbort(s"Missing type argument for sequence type: ${t.show}")
          )
        )
      else None

    def mapArgs(t: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      if t <:< TypeRepr.of[Map[?, ?]] then
        appliedArgs(t) match
          case key :: value :: Nil => Some(key -> value)
          case _ => report.errorAndAbort(s"Map requires two type arguments: ${t.show}")
      else None

    def isCaseClass(t: TypeRepr): Boolean =
      val symbol = t.typeSymbol
      symbol.isClassDef && symbol.flags.is(Flags.Case)

    def isSupportedPrimitive(t: TypeRepr): Boolean =
      t =:= TypeRepr.of[String] ||
        t =:= TypeRepr.of[Int] ||
        t =:= TypeRepr.of[Long] ||
        t =:= TypeRepr.of[Short] ||
        t =:= TypeRepr.of[Byte] ||
        t =:= TypeRepr.of[Double] ||
        t =:= TypeRepr.of[Float] ||
        t =:= TypeRepr.of[Boolean] ||
        t =:= TypeRepr.of[BigDecimal] ||
        t =:= TypeRepr.of[java.math.BigDecimal] ||
        t =:= TypeRepr.of[java.sql.Date] ||
        t =:= TypeRepr.of[java.time.LocalDate] ||
        t =:= TypeRepr.of[java.sql.Timestamp] ||
        t =:= TypeRepr.of[java.time.Instant] ||
        t =:= TypeRepr.of[java.time.LocalDateTime]

    def isAtomicMapKey(t: TypeRepr): Boolean =
      t =:= TypeRepr.of[String] ||
        t =:= TypeRepr.of[Int] ||
        t =:= TypeRepr.of[Long] ||
        t =:= TypeRepr.of[Short] ||
        t =:= TypeRepr.of[Byte] ||
        t =:= TypeRepr.of[Boolean]

    optionArg(tpe)
      .map(arg => Shape.Optional(shapeOf(using q)(arg)))
      .orElse(sequenceArg(tpe).map(arg => Shape.Sequence(shapeOf(using q)(arg))))
      .orElse {
        mapArgs(tpe).map { case (key, value) =>
          if !isAtomicMapKey(key) then
            report.errorAndAbort(
              s"Unsupported Map key type for ${tpe.show}. Allowed keys: String, Int, Long, Short, Byte, Boolean."
            )

          Shape.MapShape(Shape.Primitive(key.show), shapeOf(using q)(value))
        }
      }
      .getOrElse {
        if isCaseClass(tpe) then
          val params = tpe.typeSymbol.primaryConstructor.paramSymss.flatten
          Shape.Struct(
            params.map { param =>
              val fieldType = tpe.memberType(param)
              val maybeOption = optionArg(fieldType)
              val fieldShape = shapeOf(using q)(maybeOption.getOrElse(fieldType))
              Field(
                name = param.name,
                shape = fieldShape,
                hasDefault = param.flags.is(Flags.HasDefault),
                optional = maybeOption.isDefined
              )
            }
          )
        else if isSupportedPrimitive(tpe) then Shape.Primitive(tpe.show)
        else
          report.errorAndAbort(
            s"Unsupported structural type in SchemaConforms derivation: ${tpe.show}"
          )
      }

  private def compareByName(
      path: String,
      found: Shape,
      expected: Shape,
      caseInsensitive: Boolean
  ): List[Diff] =
    (found, expected) match
      case (Shape.Primitive(left), Shape.Primitive(right)) if left == right =>
        Nil

      case (Shape.Optional(left), Shape.Optional(right)) =>
        compareByName(path, left, right, caseInsensitive)

      case (Shape.Sequence(left), Shape.Sequence(right)) =>
        compareByName(s"$path[]", left, right, caseInsensitive)

      case (Shape.MapShape(leftKey, leftValue), Shape.MapShape(rightKey, rightValue)) =>
        val keyDiffs =
          if sameName(leftKey.name, rightKey.name, caseInsensitive) then Nil
          else List(Diff.Mismatch(s"$path<key>", rightKey, leftKey))

        keyDiffs ++ compareByName(s"$path<value>", leftValue, rightValue, caseInsensitive)

      case (Shape.Struct(foundFields), Shape.Struct(expectedFields)) =>
        val duplicateDiffs =
          duplicateNameDiffs(path, foundFields, "Out", caseInsensitive) ++
            duplicateNameDiffs(path, expectedFields, "Contract", caseInsensitive)

        val foundByName =
          foundFields.map(field => normalize(field.name, caseInsensitive) -> field).toMap
        val expectedByName =
          expectedFields.map(field => normalize(field.name, caseInsensitive) -> field).toMap

        val missing =
          expectedFields.collect {
            case field if !foundByName.contains(normalize(field.name, caseInsensitive)) =>
              Diff.Missing(pathOf(path, field.name), field)
          }

        val extra =
          foundFields.collect {
            case field if !expectedByName.contains(normalize(field.name, caseInsensitive)) =>
              Diff.Extra(pathOf(path, field.name))
          }

        val nested =
          expectedFields.flatMap { expectedField =>
            foundByName.get(normalize(expectedField.name, caseInsensitive)).toList.flatMap {
              foundField =>
                compareByName(
                  pathOf(path, expectedField.name),
                  foundField.shape,
                  expectedField.shape,
                  caseInsensitive
                )
            }
          }

        duplicateDiffs ++ missing ++ extra ++ nested

      case _ =>
        List(Diff.Mismatch(path, expected, found))

  private def compareOrdered(
      path: String,
      found: Shape,
      expected: Shape,
      caseInsensitive: Boolean
  ): List[Diff] =
    (found, expected) match
      case (Shape.Primitive(left), Shape.Primitive(right)) if left == right =>
        Nil

      case (Shape.Optional(left), Shape.Optional(right)) =>
        compareOrdered(path, left, right, caseInsensitive)

      case (Shape.Sequence(left), Shape.Sequence(right)) =>
        compareOrdered(s"$path[]", left, right, caseInsensitive)

      case (Shape.MapShape(leftKey, leftValue), Shape.MapShape(rightKey, rightValue)) =>
        val keyDiffs =
          if sameName(leftKey.name, rightKey.name, caseInsensitive) then Nil
          else List(Diff.Mismatch(s"$path<key>", rightKey, leftKey))

        keyDiffs ++ compareOrdered(s"$path<value>", leftValue, rightValue, caseInsensitive)

      case (Shape.Struct(foundFields), Shape.Struct(expectedFields)) =>
        val min = math.min(foundFields.length, expectedFields.length)

        val pairDiffs =
          (0 until min).toList.flatMap { index =>
            val foundField = foundFields(index)
            val expectedField = expectedFields(index)
            val nameDiff =
              if sameName(foundField.name, expectedField.name, caseInsensitive) then Nil
              else
                List(
                  Diff.Mismatch(
                    s"${pathOf(path, expectedField.name)}.@$index(name)",
                    Shape.Primitive(expectedField.name),
                    Shape.Primitive(foundField.name)
                  )
                )

            nameDiff ++ compareOrdered(
              pathOf(path, expectedField.name),
              foundField.shape,
              expectedField.shape,
              caseInsensitive
            )
          }

        val missing =
          if expectedFields.length > foundFields.length then
            expectedFields.drop(min).map(field => Diff.Missing(pathOf(path, field.name), field))
          else Nil

        val extra =
          if foundFields.length > expectedFields.length then
            foundFields.drop(min).map(field => Diff.Extra(pathOf(path, field.name)))
          else Nil

        missing ++ extra ++ pairDiffs

      case _ =>
        List(Diff.Mismatch(path, expected, found))

  private def compareByPosition(path: String, found: Shape, expected: Shape): List[Diff] =
    (found, expected) match
      case (Shape.Primitive(left), Shape.Primitive(right)) if left == right =>
        Nil

      case (Shape.Optional(left), Shape.Optional(right)) =>
        compareByPosition(path, left, right)

      case (Shape.Sequence(left), Shape.Sequence(right)) =>
        compareByPosition(s"$path[]", left, right)

      case (Shape.MapShape(leftKey, leftValue), Shape.MapShape(rightKey, rightValue)) =>
        val keyDiffs =
          if leftKey.name == rightKey.name then Nil
          else List(Diff.Mismatch(s"$path<key>", rightKey, leftKey))

        keyDiffs ++ compareByPosition(s"$path<value>", leftValue, rightValue)

      case (Shape.Struct(foundFields), Shape.Struct(expectedFields)) =>
        val min = math.min(foundFields.length, expectedFields.length)

        val nested =
          (0 until min).toList.flatMap { index =>
            compareByPosition(
              s"$path.@$index",
              foundFields(index).shape,
              expectedFields(index).shape
            )
          }

        val missing =
          if expectedFields.length > foundFields.length then
            expectedFields.drop(min).map(field => Diff.Missing(s"$path.@$min", field))
          else Nil

        val extra =
          if foundFields.length > expectedFields.length then
            foundFields.drop(min).map(field => Diff.Extra(s"$path.@$min"))
          else Nil

        missing ++ extra ++ nested

      case _ =>
        List(Diff.Mismatch(path, expected, found))

  private def pathOf(base: String, segment: String): String =
    if base.isEmpty then segment else s"$base.$segment"

  private def normalize(name: String, caseInsensitive: Boolean): String =
    if caseInsensitive then name.toLowerCase else name

  private def sameName(left: String, right: String, caseInsensitive: Boolean): Boolean =
    if caseInsensitive then left.equalsIgnoreCase(right) else left == right

  private def duplicateNameDiffs(
      path: String,
      fields: List[Field],
      side: String,
      caseInsensitive: Boolean
  ): List[Diff] =
    fields
      .groupBy(field => normalize(field.name, caseInsensitive))
      .values
      .collect {
        case duplicates if duplicates.length > 1 =>
          val names = duplicates.map(_.name).sorted.mkString("[", ", ", "]")
          Diff.Mismatch(
            pathOf(path, "<fields>"),
            Shape.Primitive(s"unique $side field names"),
            Shape.Primitive(s"duplicate $side field names $names")
          )
      }
      .toList

  private def renderField(field: Field): String =
    val optional = if field.optional then " (optional)" else ""
    val default = if field.hasDefault then " (default)" else ""
    s"${render(field.shape)}$optional$default"

  private def render(shape: Shape): String =
    shape match
      case Shape.Primitive(name) =>
        name
          .stripPrefix("scala.Predef.")
          .stripPrefix("scala.")
          .stripPrefix("java.lang.")
      case Shape.Optional(inner)      => s"Option[${render(inner)}]"
      case Shape.Sequence(element)    => s"Seq[${render(element)}]"
      case Shape.MapShape(key, value) => s"Map[${render(key)} -> ${render(value)}]"
      case Shape.Struct(fields)       =>
        fields.map(f => s"${f.name}: ${render(f.shape)}").mkString("{", ", ", "}")
