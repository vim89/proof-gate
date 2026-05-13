package proofgate.runtime.spark

import proofgate.model.*
import proofgate.proof.SchemaPolicy
import scala.quoted.*

enum RuntimeType:
  case Primitive(name: String)
  case Optional(inner: RuntimeType)
  case Sequence(element: RuntimeType)
  case MapType(key: RuntimeType, value: RuntimeType)
  case Struct(fields: Vector[RuntimeField])

final case class RuntimeField(name: String, dataType: RuntimeType, nullable: Boolean)
final case class RuntimeShape(fields: Vector[RuntimeField])

trait RuntimeShapeEncoder[A]:
  def shape: RuntimeShape

object RuntimeShapeEncoder:
  inline given derived[A]: RuntimeShapeEncoder[A] =
    ${ RuntimeShapeEncoderMacro.derive[A] }

  inline def shapeOf[A](using encoder: RuntimeShapeEncoder[A]): RuntimeShape =
    encoder.shape

trait RuntimePin[P <: SchemaPolicy]:
  def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding]

object RuntimePin:
  val runtimePinRuleId: RuleId = RuleId.unsafe("runtime-pin.shape-mismatch")

  given exact: RuntimePin[SchemaPolicy.Exact.type] with
    def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding] =
      RuntimeShapeDiff.compare(actual, expected).map(toFinding)

  private def toFinding(diff: RuntimeShapeDiff): Finding =
    Finding(
      stage = StageName.Pin,
      ruleId = runtimePinRuleId,
      severity = FindingSeverity.Blocker,
      message =
        s"Runtime shape drift at ${diff.path}: expected ${diff.expected}, found ${diff.found}.",
      path = Some(diff.path),
      hint = Some("Inspect field names, types, and nullability before shipping.")
    )

final case class RuntimeShapeDiff(path: String, expected: String, found: String)

object RuntimeShapeDiff:
  def compare(actual: RuntimeShape, expected: RuntimeShape): Vector[RuntimeShapeDiff] =
    compareFields("", actual.fields, expected.fields)

  private def compareFields(
      basePath: String,
      actualFields: Vector[RuntimeField],
      expectedFields: Vector[RuntimeField]
  ): Vector[RuntimeShapeDiff] =
    val actualByName = actualFields.map(field => field.name -> field).toMap
    val expectedByName = expectedFields.map(field => field.name -> field).toMap

    val missing =
      expectedFields.collect {
        case field if !actualByName.contains(field.name) =>
          RuntimeShapeDiff(pathOf(basePath, field.name), renderField(field), "<missing>")
      }

    val extra =
      actualFields.collect {
        case field if !expectedByName.contains(field.name) =>
          RuntimeShapeDiff(pathOf(basePath, field.name), "<absent>", renderField(field))
      }

    val nested =
      expectedFields.flatMap { expectedField =>
        actualByName.get(expectedField.name).toVector.flatMap { actualField =>
          compareField(pathOf(basePath, expectedField.name), actualField, expectedField)
        }
      }

    missing ++ extra ++ nested

  private def compareField(
      path: String,
      actual: RuntimeField,
      expected: RuntimeField
  ): Vector[RuntimeShapeDiff] =
    val nullability =
      Option
        .when(actual.nullable != expected.nullable) {
          RuntimeShapeDiff(path, renderField(expected), renderField(actual))
        }
        .toVector

    nullability ++ compareType(path, actual.dataType, expected.dataType)

  private def compareType(
      path: String,
      actual: RuntimeType,
      expected: RuntimeType
  ): Vector[RuntimeShapeDiff] =
    (actual, expected) match
      case (RuntimeType.Primitive(left), RuntimeType.Primitive(right)) if left == right =>
        Vector.empty

      case (RuntimeType.Optional(left), RuntimeType.Optional(right)) =>
        compareType(path, left, right)

      case (RuntimeType.Sequence(left), RuntimeType.Sequence(right)) =>
        compareType(s"$path[]", left, right)

      case (RuntimeType.MapType(leftKey, leftValue), RuntimeType.MapType(rightKey, rightValue)) =>
        val keyDiffs =
          if renderType(leftKey) == renderType(rightKey) then Vector.empty
          else Vector(RuntimeShapeDiff(s"$path<key>", renderType(rightKey), renderType(leftKey)))

        keyDiffs ++ compareType(s"$path<value>", leftValue, rightValue)

      case (RuntimeType.Struct(actualFields), RuntimeType.Struct(expectedFields)) =>
        compareFields(path, actualFields, expectedFields)

      case _ =>
        Vector(RuntimeShapeDiff(path, renderType(expected), renderType(actual)))

  private def pathOf(base: String, segment: String): String =
    if base.isEmpty then segment else s"$base.$segment"

  private def renderField(field: RuntimeField): String =
    val nullable = if field.nullable then " nullable" else ""
    s"${renderType(field.dataType)}$nullable"

  private def renderType(dataType: RuntimeType): String =
    dataType match
      case RuntimeType.Primitive(name)     => name
      case RuntimeType.Optional(inner)     => s"Option[${renderType(inner)}]"
      case RuntimeType.Sequence(element)   => s"Seq[${renderType(element)}]"
      case RuntimeType.MapType(key, value) => s"Map[${renderType(key)} -> ${renderType(value)}]"
      case RuntimeType.Struct(fields)      =>
        fields.map(field => s"${field.name}: ${renderField(field)}").mkString("{", ", ", "}")

private object RuntimeShapeEncoderMacro:
  def derive[A: Type](using q: Quotes): Expr[RuntimeShapeEncoder[A]] =
    import q.reflect.*

    val fields = fieldsOf(using q)(TypeRepr.of[A])
    val shapeExpr = '{ RuntimeShape(${ Expr.ofSeq(fields).asExprOf[Seq[RuntimeField]] }.toVector) }

    '{
      new RuntimeShapeEncoder[A]:
        def shape: RuntimeShape = $shapeExpr
    }

  private def fieldsOf(using q: Quotes)(tpe: q.reflect.TypeRepr): List[Expr[RuntimeField]] =
    import q.reflect.*

    if !isCaseClass(using q)(tpe) then
      report.errorAndAbort(s"RuntimeShapeEncoder requires a case class: ${tpe.show}")

    tpe.typeSymbol.primaryConstructor.paramSymss.flatten.map { param =>
      val fieldType = tpe.memberType(param)
      val (dataType, nullable) = fieldShape(using q)(fieldType)

      '{ RuntimeField(${ Expr(param.name) }, $dataType, ${ Expr(nullable) }) }
    }

  private def fieldShape(using
      q: Quotes
  )(
      tpe: q.reflect.TypeRepr
  ): (Expr[RuntimeType], Boolean) =
    optionArg(using q)(tpe) match
      case Some(inner) => runtimeType(using q)(inner) -> true
      case None        => runtimeType(using q)(tpe) -> false

  private def runtimeType(using q: Quotes)(tpe: q.reflect.TypeRepr): Expr[RuntimeType] =
    import q.reflect.*

    optionArg(using q)(tpe)
      .map(inner => '{ RuntimeType.Optional(${ runtimeType(using q)(inner) }) })
      .orElse(
        sequenceArg(using q)(tpe).map(inner =>
          '{ RuntimeType.Sequence(${ runtimeType(using q)(inner) }) }
        )
      )
      .orElse {
        mapArgs(using q)(tpe).map { case (key, value) =>
          if !isAtomicMapKey(using q)(key) then
            report.errorAndAbort(
              s"Unsupported Map key type for ${tpe.show}. Allowed keys: String, Int, Long, Short, Byte, Boolean."
            )

          '{
            RuntimeType.MapType(${ primitiveType(using q)(key) }, ${ runtimeType(using q)(value) })
          }
        }
      }
      .getOrElse {
        if isCaseClass(using q)(tpe) then
          '{
            RuntimeType.Struct(${
              Expr.ofSeq(fieldsOf(using q)(tpe)).asExprOf[Seq[RuntimeField]]
            }.toVector)
          }
        else primitiveType(using q)(tpe)
      }

  private def primitiveType(using q: Quotes)(tpe: q.reflect.TypeRepr): Expr[RuntimeType] =
    import q.reflect.*

    val name =
      if tpe =:= TypeRepr.of[String] then Some("String")
      else if tpe =:= TypeRepr.of[Int] then Some("Int")
      else if tpe =:= TypeRepr.of[Long] then Some("Long")
      else if tpe =:= TypeRepr.of[Short] then Some("Short")
      else if tpe =:= TypeRepr.of[Byte] then Some("Byte")
      else if tpe =:= TypeRepr.of[Double] then Some("Double")
      else if tpe =:= TypeRepr.of[Float] then Some("Float")
      else if tpe =:= TypeRepr.of[Boolean] then Some("Boolean")
      else if tpe =:= TypeRepr.of[BigDecimal] then Some("BigDecimal")
      else if tpe =:= TypeRepr.of[java.math.BigDecimal] then Some("java.math.BigDecimal")
      else if tpe =:= TypeRepr.of[java.sql.Date] then Some("java.sql.Date")
      else if tpe =:= TypeRepr.of[java.time.LocalDate] then Some("java.time.LocalDate")
      else if tpe =:= TypeRepr.of[java.sql.Timestamp] then Some("java.sql.Timestamp")
      else if tpe =:= TypeRepr.of[java.time.Instant] then Some("java.time.Instant")
      else if tpe =:= TypeRepr.of[java.time.LocalDateTime] then Some("java.time.LocalDateTime")
      else None

    name match
      case Some(value) => '{ RuntimeType.Primitive(${ Expr(value) }) }
      case None        => report.errorAndAbort(s"Unsupported runtime shape type: ${tpe.show}")

  private def isCaseClass(using q: Quotes)(tpe: q.reflect.TypeRepr): Boolean =
    import q.reflect.*

    val symbol = tpe.typeSymbol
    symbol.isClassDef && symbol.flags.is(Flags.Case)

  private def appliedArgs(using q: Quotes)(tpe: q.reflect.TypeRepr): List[q.reflect.TypeRepr] =
    import q.reflect.*

    tpe match
      case AppliedType(_, args) => args
      case _                    => Nil

  private def optionArg(using q: Quotes)(tpe: q.reflect.TypeRepr): Option[q.reflect.TypeRepr] =
    import q.reflect.*

    if tpe <:< TypeRepr.of[Option[?]] then appliedArgs(using q)(tpe).headOption else None

  private def sequenceArg(using q: Quotes)(tpe: q.reflect.TypeRepr): Option[q.reflect.TypeRepr] =
    import q.reflect.*

    val isSequence =
      tpe <:< TypeRepr.of[List[?]] ||
        tpe <:< TypeRepr.of[Seq[?]] ||
        tpe <:< TypeRepr.of[Vector[?]] ||
        tpe <:< TypeRepr.of[Array[?]] ||
        tpe <:< TypeRepr.of[Set[?]]

    if isSequence then
      Some(
        appliedArgs(using q)(tpe).headOption.getOrElse(
          report.errorAndAbort(s"Missing type argument for sequence type: ${tpe.show}")
        )
      )
    else None

  private def mapArgs(using
      q: Quotes
  )(
      tpe: q.reflect.TypeRepr
  ): Option[(q.reflect.TypeRepr, q.reflect.TypeRepr)] =
    import q.reflect.*

    if tpe <:< TypeRepr.of[Map[?, ?]] then
      appliedArgs(using q)(tpe) match
        case key :: value :: Nil => Some(key -> value)
        case _ => report.errorAndAbort(s"Map requires two type arguments: ${tpe.show}")
    else None

  private def isAtomicMapKey(using q: Quotes)(tpe: q.reflect.TypeRepr): Boolean =
    import q.reflect.*

    tpe =:= TypeRepr.of[String] ||
    tpe =:= TypeRepr.of[Int] ||
    tpe =:= TypeRepr.of[Long] ||
    tpe =:= TypeRepr.of[Short] ||
    tpe =:= TypeRepr.of[Byte] ||
    tpe =:= TypeRepr.of[Boolean]
