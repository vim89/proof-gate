package proofgate.runtime.spark

import munit.FunSuite

final class SparkSchemaAdapterSuite extends FunSuite:
  test("primitive types map to canonical Scala names"):
    val shape = SparkSchemaAdapter.fromSparkFields(
      Vector(
        SparkFieldInfo("id", "long", nullable = false),
        SparkFieldInfo("email", "string", nullable = true),
        SparkFieldInfo("amount", "decimal(18,4)", nullable = true),
        SparkFieldInfo("joined_at", "timestamp", nullable = false)
      )
    )

    assertEquals(
      shape.fields.map(field => field.name -> renderType(field.dataType)),
      Vector(
        "id" -> "Long",
        "email" -> "String",
        "amount" -> "BigDecimal",
        "joined_at" -> "java.sql.Timestamp"
      )
    )
    assertEquals(shape.fields.map(_.nullable), Vector(false, true, true, false))

  test("array types decode into Sequence"):
    val shape = SparkSchemaAdapter.fromSparkFields(
      Vector(SparkFieldInfo("tags", "array<string>", nullable = true))
    )

    assertEquals(
      shape.fields.head.dataType,
      RuntimeType.Sequence(RuntimeType.Primitive("String"))
    )

  test("map types decode into MapType"):
    val shape = SparkSchemaAdapter.fromSparkFields(
      Vector(SparkFieldInfo("metadata", "map<string,long>", nullable = true))
    )

    assertEquals(
      shape.fields.head.dataType,
      RuntimeType.MapType(RuntimeType.Primitive("String"), RuntimeType.Primitive("Long"))
    )

  test("nested struct types decode into Struct"):
    val shape = SparkSchemaAdapter.fromSparkFields(
      Vector(SparkFieldInfo("address", "struct<city:string,zip:int>", nullable = true))
    )

    val expected = RuntimeType.Struct(
      Vector(
        RuntimeField("city", RuntimeType.Primitive("String"), nullable = true),
        RuntimeField("zip", RuntimeType.Primitive("Int"), nullable = true)
      )
    )

    assertEquals(shape.fields.head.dataType, expected)

  test("nested struct fields are nullable because simpleString omits nested nullability"):
    val parsed = SparkSchemaAdapter.parseType("struct<city:string,zip:int>")

    val fields =
      parsed match
        case RuntimeType.Struct(fields) => fields
        case other                      => fail(s"Expected struct, found $other")

    assertEquals(fields.map(_.nullable), Vector(true, true))

  test("nested array of struct retains type detail"):
    val parsed = SparkSchemaAdapter.parseType("array<struct<id:long,email:string>>")

    val expected = RuntimeType.Sequence(
      RuntimeType.Struct(
        Vector(
          RuntimeField("id", RuntimeType.Primitive("Long"), nullable = true),
          RuntimeField("email", RuntimeType.Primitive("String"), nullable = true)
        )
      )
    )

    assertEquals(parsed, expected)

  test("unknown spark types are surfaced verbatim"):
    val shape = SparkSchemaAdapter.fromSparkFields(
      Vector(SparkFieldInfo("payload", "binary", nullable = true))
    )

    assertEquals(shape.fields.head.dataType, RuntimeType.Primitive("binary"))

  test("adapter output feeds the runtime pin diff cleanly"):
    val actual = SparkSchemaAdapter.fromSparkFields(
      Vector(
        SparkFieldInfo("id", "long", nullable = false),
        SparkFieldInfo("email", "string", nullable = true)
      )
    )
    val expected = SparkSchemaAdapter.fromSparkFields(
      Vector(
        SparkFieldInfo("id", "long", nullable = false),
        SparkFieldInfo("email", "string", nullable = true),
        SparkFieldInfo("amount", "decimal(18,4)", nullable = true)
      )
    )

    val diffs = RuntimeShapeDiff.compare(actual, expected)

    assertEquals(diffs.size, 1)
    assertEquals(diffs.head.path, "amount")

  private def renderType(dataType: RuntimeType): String =
    dataType match
      case RuntimeType.Primitive(name) => name
      case other                       => other.toString
