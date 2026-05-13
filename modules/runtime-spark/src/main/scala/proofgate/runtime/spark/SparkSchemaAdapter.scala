package proofgate.runtime.spark

import org.apache.spark.sql.types.*

/** Field-level information extracted from a Spark `StructType`.
  *
  * Callers obtain this from a live Spark `StructType` without forcing this module to depend on
  * Spark at compile time:
  *
  * {{{
  * structType.fields.map(f => SparkFieldInfo(f.name, f.dataType.simpleString, f.nullable)).toVector
  * }}}
  *
  * Prefer `SparkSchemaAdapter.fromStructType` when Spark SQL types are available. The
  * `simpleString` path remains for callers that want to pass a tiny DTO across an integration
  * boundary.
  */
final case class SparkFieldInfo(name: String, sparkType: String, nullable: Boolean)

object SparkSchemaAdapter:
  val HasDefaultMetadataKey: String = "ctdc.hasDefault"

  /** Convert a real Spark `StructType` into a ProofGate `RuntimeShape`.
    *
    * This path preserves nested struct field nullability, array `containsNull`, map
    * `valueContainsNull`, and the `ctdc.hasDefault` metadata used by the contract proof lineage.
    */
  def fromStructType(schema: StructType): RuntimeShape =
    RuntimeShape(schema.fields.toVector.map(fromStructField))

  /** Convert a Spark `simpleString` field description into a ProofGate `RuntimeShape`.
    *
    * The adapter supports Spark primitive types and the standard wrappers Spark surfaces through
    * `simpleString`: `array<T>`, `map<K,V>`, and nested `struct<name:T,...>`. Unknown Spark types
    * are reported as `RuntimeType.Primitive` carrying the raw Spark name; downstream diff reports
    * the raw name so the reviewer can fix the type or extend the adapter.
    */
  def fromSparkFields(fields: Vector[SparkFieldInfo]): RuntimeShape =
    RuntimeShape(fields.map(toRuntimeField))

  private def toRuntimeField(info: SparkFieldInfo): RuntimeField =
    RuntimeField(
      name = info.name,
      dataType = parseType(info.sparkType.trim),
      nullable = info.nullable
    )

  private def fromStructField(field: StructField): RuntimeField =
    RuntimeField(
      name = field.name,
      dataType = fromDataType(field.dataType),
      nullable = field.nullable,
      hasDefault = hasDefault(field.metadata)
    )

  private def fromDataType(dataType: DataType): RuntimeType =
    dataType match
      case StringType           => RuntimeType.Primitive("String")
      case ByteType             => RuntimeType.Primitive("Byte")
      case ShortType            => RuntimeType.Primitive("Short")
      case IntegerType          => RuntimeType.Primitive("Int")
      case LongType             => RuntimeType.Primitive("Long")
      case FloatType            => RuntimeType.Primitive("Float")
      case DoubleType           => RuntimeType.Primitive("Double")
      case BooleanType          => RuntimeType.Primitive("Boolean")
      case DateType             => RuntimeType.Primitive("java.sql.Date")
      case TimestampType        => RuntimeType.Primitive("java.sql.Timestamp")
      case decimal: DecimalType =>
        RuntimeType.Primitive("BigDecimal")
      case ArrayType(elementType, containsNull) =>
        val element = fromDataType(elementType)
        RuntimeType.Sequence(if containsNull then RuntimeType.Optional(element) else element)
      case MapType(keyType, valueType, valueContainsNull) =>
        val value = fromDataType(valueType)
        RuntimeType.MapType(
          fromDataType(keyType),
          if valueContainsNull then RuntimeType.Optional(value) else value
        )
      case struct: StructType =>
        RuntimeType.Struct(struct.fields.toVector.map(fromStructField))
      case other =>
        RuntimeType.Primitive(other.typeName)

  private def hasDefault(metadata: Metadata): Boolean =
    metadata.contains(HasDefaultMetadataKey) && metadata.getBoolean(HasDefaultMetadataKey)

  /** Parse a Spark `DataType.simpleString` representation. */
  private[spark] def parseType(input: String): RuntimeType =
    val trimmed = input.trim

    if trimmed.startsWith("array<") && trimmed.endsWith(">") then
      RuntimeType.Sequence(parseType(trimmed.substring(6, trimmed.length - 1)))
    else if trimmed.startsWith("map<") && trimmed.endsWith(">") then
      parseMap(trimmed.substring(4, trimmed.length - 1))
    else if trimmed.startsWith("struct<") && trimmed.endsWith(">") then
      parseStruct(trimmed.substring(7, trimmed.length - 1))
    else parsePrimitive(trimmed)

  private def parseMap(body: String): RuntimeType =
    splitTopLevel(body, ',') match
      case key +: value +: Nil =>
        RuntimeType.MapType(parseType(key), parseType(value))
      case _ =>
        RuntimeType.Primitive(s"map<$body>")

  private def parseStruct(body: String): RuntimeType =
    val fields = splitTopLevel(body, ',').map(parseStructField)
    RuntimeType.Struct(fields.toVector)

  private def parseStructField(entry: String): RuntimeField =
    val colon = indexOfTopLevel(entry, ':')
    if colon < 0 then RuntimeField(entry.trim, RuntimeType.Primitive(entry.trim), nullable = true)
    else
      val name = entry.substring(0, colon).trim
      val typeExpr = entry.substring(colon + 1).trim
      RuntimeField(name, parseType(typeExpr), nullable = true)

  private def parsePrimitive(typeName: String): RuntimeType =
    val canonical = typeName.toLowerCase
    val mapped = canonical match
      case "string"                             => "String"
      case "byte" | "tinyint"                   => "Byte"
      case "short" | "smallint"                 => "Short"
      case "int" | "integer"                    => "Int"
      case "long" | "bigint"                    => "Long"
      case "float"                              => "Float"
      case "double"                             => "Double"
      case "boolean"                            => "Boolean"
      case "date"                               => "java.sql.Date"
      case "timestamp"                          => "java.sql.Timestamp"
      case other if other.startsWith("decimal") =>
        "BigDecimal"
      case _ =>
        typeName

    RuntimeType.Primitive(mapped)

  private def splitTopLevel(input: String, separator: Char): List[String] =
    val builder = collection.mutable.ListBuffer.empty[String]
    val current = StringBuilder()
    var depth = 0
    var index = 0

    while index < input.length do
      val char = input.charAt(index)
      char match
        case '<' =>
          depth += 1
          current.append(char)
        case '>' =>
          depth -= 1
          current.append(char)
        case c if c == separator && depth == 0 =>
          builder += current.toString
          current.clear()
        case other =>
          current.append(other)
      index += 1

    builder += current.toString
    builder.toList.map(_.trim).filter(_.nonEmpty)

  private def indexOfTopLevel(input: String, separator: Char): Int =
    var depth = 0
    var index = 0

    while index < input.length do
      val char = input.charAt(index)
      char match
        case '<'                               => depth += 1
        case '>'                               => depth -= 1
        case c if c == separator && depth == 0 =>
          return index
        case _ =>
      index += 1

    -1
