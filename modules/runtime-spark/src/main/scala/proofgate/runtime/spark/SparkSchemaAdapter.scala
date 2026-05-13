package proofgate.runtime.spark

/** Field-level information from a Spark `StructType`.
  *
  * Callers obtain this from a live Spark `StructType` without forcing this module to depend on
  * Spark at compile time:
  *
  * {{{
  * structType.fields.map(f => SparkFieldInfo(f.name, f.dataType.simpleString, f.nullable)).toVector
  * }}}
  *
  * The caller keeps the Spark dependency; the runtime-pin module stays free of Spark binaries,
  * which keeps Scala 3 builds clean.
  */
final case class SparkFieldInfo(name: String, sparkType: String, nullable: Boolean)

object SparkSchemaAdapter:
  /** Convert a flat Spark field description into a ProofGate `RuntimeShape`.
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
