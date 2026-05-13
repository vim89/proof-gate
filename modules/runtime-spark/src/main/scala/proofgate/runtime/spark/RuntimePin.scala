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

final case class RuntimeField(
    name: String,
    dataType: RuntimeType,
    nullable: Boolean,
    hasDefault: Boolean = false
)
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
      RuntimeShapeDiff.compare(actual, expected, RuntimeShapePolicy.Exact).map(toFinding)

  given exactUnorderedCI: RuntimePin[SchemaPolicy.ExactUnorderedCI.type] with
    def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding] =
      RuntimeShapeDiff.compare(actual, expected, RuntimeShapePolicy.Exact).map(toFinding)

  given exactOrdered: RuntimePin[SchemaPolicy.ExactOrdered.type] with
    def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding] =
      RuntimeShapeDiff
        .compare(actual, expected, RuntimeShapePolicy.Ordered(caseInsensitive = false))
        .map(toFinding)

  given exactOrderedCI: RuntimePin[SchemaPolicy.ExactOrderedCI.type] with
    def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding] =
      RuntimeShapeDiff
        .compare(actual, expected, RuntimeShapePolicy.Ordered(caseInsensitive = true))
        .map(toFinding)

  given exactByPosition: RuntimePin[SchemaPolicy.ExactByPosition.type] with
    def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding] =
      RuntimeShapeDiff.compare(actual, expected, RuntimeShapePolicy.ByPosition).map(toFinding)

  given backward: RuntimePin[SchemaPolicy.Backward.type] with
    def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding] =
      RuntimeShapeDiff.compare(actual, expected, RuntimeShapePolicy.Backward).map(toFinding)

  given forward: RuntimePin[SchemaPolicy.Forward.type] with
    def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding] =
      RuntimeShapeDiff.compare(actual, expected, RuntimeShapePolicy.Forward).map(toFinding)

  given full: RuntimePin[SchemaPolicy.Full.type] with
    def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding] =
      Vector.empty

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

private enum RuntimeShapePolicy:
  case Exact
  case Ordered(caseInsensitive: Boolean)
  case ByPosition
  case Backward
  case Forward

object RuntimeShapeDiff:
  def compare(actual: RuntimeShape, expected: RuntimeShape): Vector[RuntimeShapeDiff] =
    compare(actual, expected, RuntimeShapePolicy.Exact)

  private[runtime] def compare(
      actual: RuntimeShape,
      expected: RuntimeShape,
      policy: RuntimeShapePolicy
  ): Vector[RuntimeShapeDiff] =
    policy match
      case RuntimeShapePolicy.Exact =>
        compareFieldsByName("", actual.fields, expected.fields, caseInsensitive = true)
      case RuntimeShapePolicy.Ordered(caseInsensitive) =>
        compareFieldsOrdered("", actual.fields, expected.fields, caseInsensitive)
      case RuntimeShapePolicy.ByPosition =>
        compareFieldsByPosition("", actual.fields, expected.fields)
      case RuntimeShapePolicy.Backward =>
        compareFieldsBackward("", actual.fields, expected.fields)
      case RuntimeShapePolicy.Forward =>
        compareFieldsByName("", actual.fields, expected.fields, caseInsensitive = false).filter {
          case RuntimeShapeDiff(_, _, "<missing>") => false
          case _                                   => true
        }

  private def compareFieldsByName(
      basePath: String,
      actualFields: Vector[RuntimeField],
      expectedFields: Vector[RuntimeField],
      caseInsensitive: Boolean
  ): Vector[RuntimeShapeDiff] =
    val duplicateDiffs =
      duplicateNameDiffs(basePath, actualFields, "actual", caseInsensitive) ++
        duplicateNameDiffs(basePath, expectedFields, "expected", caseInsensitive)

    val actualByName =
      actualFields.map(field => normalize(field.name, caseInsensitive) -> field).toMap
    val expectedByName =
      expectedFields.map(field => normalize(field.name, caseInsensitive) -> field).toMap

    val missing =
      expectedFields.collect {
        case field if !actualByName.contains(normalize(field.name, caseInsensitive)) =>
          RuntimeShapeDiff(pathOf(basePath, field.name), renderField(field), "<missing>")
      }

    val extra =
      actualFields.collect {
        case field if !expectedByName.contains(normalize(field.name, caseInsensitive)) =>
          RuntimeShapeDiff(pathOf(basePath, field.name), "<absent>", renderField(field))
      }

    val nested =
      expectedFields.flatMap { expectedField =>
        actualByName.get(normalize(expectedField.name, caseInsensitive)).toVector.flatMap {
          actualField =>
            compareFieldByName(
              pathOf(basePath, expectedField.name),
              actualField,
              expectedField,
              caseInsensitive
            )
        }
      }

    duplicateDiffs ++ missing ++ extra ++ nested

  private def compareFieldsBackward(
      basePath: String,
      actualFields: Vector[RuntimeField],
      expectedFields: Vector[RuntimeField]
  ): Vector[RuntimeShapeDiff] =
    val duplicateDiffs =
      duplicateNameDiffs(basePath, actualFields, "actual", caseInsensitive = false) ++
        duplicateNameDiffs(basePath, expectedFields, "expected", caseInsensitive = false)

    val actualByName = actualFields.map(field => field.name -> field).toMap

    val missing =
      expectedFields.collect {
        case field if !actualByName.contains(field.name) && !(field.nullable || field.hasDefault) =>
          RuntimeShapeDiff(pathOf(basePath, field.name), renderField(field), "<missing>")
      }

    val nested =
      expectedFields.flatMap { expectedField =>
        actualByName.get(expectedField.name).toVector.flatMap { actualField =>
          compareFieldBackward(pathOf(basePath, expectedField.name), actualField, expectedField)
        }
      }

    duplicateDiffs ++ missing ++ nested

  private def compareFieldsOrdered(
      basePath: String,
      actualFields: Vector[RuntimeField],
      expectedFields: Vector[RuntimeField],
      caseInsensitive: Boolean
  ): Vector[RuntimeShapeDiff] =
    val min = math.min(actualFields.length, expectedFields.length)

    val pairDiffs =
      (0 until min).toVector.flatMap { index =>
        val actual = actualFields(index)
        val expected = expectedFields(index)
        val nameDiff =
          Option
            .when(!sameName(actual.name, expected.name, caseInsensitive)) {
              RuntimeShapeDiff(
                s"${pathOf(basePath, expected.name)}.@$index(name)",
                expected.name,
                actual.name
              )
            }
            .toVector

        nameDiff ++ compareFieldOrdered(
          pathOf(basePath, expected.name),
          actual,
          expected,
          caseInsensitive
        )
      }

    val missing =
      if expectedFields.length > actualFields.length then
        expectedFields
          .drop(min)
          .map(field =>
            RuntimeShapeDiff(pathOf(basePath, field.name), renderField(field), "<missing>")
          )
      else Vector.empty

    val extra =
      if actualFields.length > expectedFields.length then
        actualFields
          .drop(min)
          .map(field =>
            RuntimeShapeDiff(pathOf(basePath, field.name), "<absent>", renderField(field))
          )
      else Vector.empty

    missing ++ extra ++ pairDiffs

  private def compareFieldsByPosition(
      basePath: String,
      actualFields: Vector[RuntimeField],
      expectedFields: Vector[RuntimeField]
  ): Vector[RuntimeShapeDiff] =
    val min = math.min(actualFields.length, expectedFields.length)

    val nested =
      (0 until min).toVector.flatMap { index =>
        compareFieldByPosition(s"$basePath.@$index", actualFields(index), expectedFields(index))
      }

    val missing =
      if expectedFields.length > actualFields.length then
        expectedFields
          .drop(min)
          .map(field => RuntimeShapeDiff(s"$basePath.@$min", renderField(field), "<missing>"))
      else Vector.empty

    val extra =
      if actualFields.length > expectedFields.length then
        actualFields
          .drop(min)
          .map(field => RuntimeShapeDiff(s"$basePath.@$min", "<absent>", renderField(field)))
      else Vector.empty

    missing ++ extra ++ nested

  private def compareFieldByName(
      path: String,
      actual: RuntimeField,
      expected: RuntimeField,
      caseInsensitive: Boolean
  ): Vector[RuntimeShapeDiff] =
    compareTypeByName(path, actual.dataType, expected.dataType, caseInsensitive)

  private def compareFieldOrdered(
      path: String,
      actual: RuntimeField,
      expected: RuntimeField,
      caseInsensitive: Boolean
  ): Vector[RuntimeShapeDiff] =
    compareTypeOrdered(path, actual.dataType, expected.dataType, caseInsensitive)

  private def compareFieldByPosition(
      path: String,
      actual: RuntimeField,
      expected: RuntimeField
  ): Vector[RuntimeShapeDiff] =
    compareTypeByPosition(path, actual.dataType, expected.dataType)

  private def compareFieldBackward(
      path: String,
      actual: RuntimeField,
      expected: RuntimeField
  ): Vector[RuntimeShapeDiff] =
    compareTypeBackward(path, actual.dataType, expected.dataType)

  private def compareTypeByName(
      path: String,
      actual: RuntimeType,
      expected: RuntimeType,
      caseInsensitive: Boolean
  ): Vector[RuntimeShapeDiff] =
    (actual, expected) match
      case (RuntimeType.Primitive(left), RuntimeType.Primitive(right)) if left == right =>
        Vector.empty

      case (RuntimeType.Optional(left), RuntimeType.Optional(right)) =>
        compareTypeByName(path, left, right, caseInsensitive)

      case (RuntimeType.Sequence(left), RuntimeType.Sequence(right)) =>
        compareTypeByName(s"$path[]", left, right, caseInsensitive)

      case (RuntimeType.MapType(leftKey, leftValue), RuntimeType.MapType(rightKey, rightValue)) =>
        val keyDiffs =
          if sameName(renderType(leftKey), renderType(rightKey), caseInsensitive) then Vector.empty
          else Vector(RuntimeShapeDiff(s"$path<key>", renderType(rightKey), renderType(leftKey)))

        keyDiffs ++ compareTypeByName(s"$path<value>", leftValue, rightValue, caseInsensitive)

      case (RuntimeType.Struct(actualFields), RuntimeType.Struct(expectedFields)) =>
        compareFieldsByName(path, actualFields, expectedFields, caseInsensitive)

      case _ =>
        Vector(RuntimeShapeDiff(path, renderType(expected), renderType(actual)))

  private def compareTypeOrdered(
      path: String,
      actual: RuntimeType,
      expected: RuntimeType,
      caseInsensitive: Boolean
  ): Vector[RuntimeShapeDiff] =
    (actual, expected) match
      case (RuntimeType.Struct(actualFields), RuntimeType.Struct(expectedFields)) =>
        compareFieldsOrdered(path, actualFields, expectedFields, caseInsensitive)
      case _ =>
        compareTypeByName(path, actual, expected, caseInsensitive)

  private def compareTypeByPosition(
      path: String,
      actual: RuntimeType,
      expected: RuntimeType
  ): Vector[RuntimeShapeDiff] =
    (actual, expected) match
      case (RuntimeType.Struct(actualFields), RuntimeType.Struct(expectedFields)) =>
        compareFieldsByPosition(path, actualFields, expectedFields)
      case (RuntimeType.Sequence(left), RuntimeType.Sequence(right)) =>
        compareTypeByPosition(s"$path[]", left, right)
      case (RuntimeType.MapType(leftKey, leftValue), RuntimeType.MapType(rightKey, rightValue)) =>
        val keyDiffs =
          if renderType(leftKey) == renderType(rightKey) then Vector.empty
          else Vector(RuntimeShapeDiff(s"$path<key>", renderType(rightKey), renderType(leftKey)))

        keyDiffs ++ compareTypeByPosition(s"$path<value>", leftValue, rightValue)
      case _ =>
        compareTypeByName(path, actual, expected, caseInsensitive = false)

  private def compareTypeBackward(
      path: String,
      actual: RuntimeType,
      expected: RuntimeType
  ): Vector[RuntimeShapeDiff] =
    (actual, expected) match
      case (RuntimeType.Struct(actualFields), RuntimeType.Struct(expectedFields)) =>
        compareFieldsBackward(path, actualFields, expectedFields)
      case _ =>
        compareTypeByName(path, actual, expected, caseInsensitive = false)

  private def pathOf(base: String, segment: String): String =
    if base.isEmpty then segment else s"$base.$segment"

  private def normalize(name: String, caseInsensitive: Boolean): String =
    if caseInsensitive then name.toLowerCase else name

  private def sameName(left: String, right: String, caseInsensitive: Boolean): Boolean =
    if caseInsensitive then left.equalsIgnoreCase(right) else left == right

  private def duplicateNameDiffs(
      basePath: String,
      fields: Vector[RuntimeField],
      side: String,
      caseInsensitive: Boolean
  ): Vector[RuntimeShapeDiff] =
    fields
      .groupBy(field => normalize(field.name, caseInsensitive))
      .values
      .collect {
        case duplicates if duplicates.length > 1 =>
          val names = duplicates.map(_.name).sorted.mkString("[", ", ", "]")
          RuntimeShapeDiff(
            pathOf(basePath, "<fields>"),
            s"unique $side field names",
            s"duplicate $side field names $names"
          )
      }
      .toVector

  private def renderField(field: RuntimeField): String =
    val nullable = if field.nullable then " nullable" else ""
    val default = if field.hasDefault then " default" else ""
    s"${renderType(field.dataType)}$nullable$default"

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
