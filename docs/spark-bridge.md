# Spark bridge example

ProofGate keeps Spark at the runtime adapter boundary. The core proof and model
modules stay Spark-free, while `runtime-spark` can traverse a real Spark
`StructType` for sink-time pins.

## The StructType bridge

In a project that already has `org.apache.spark:spark-sql:_:Provided` on the
classpath:

```scala
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.types.StructType
import proofgate.runtime.spark.{RuntimeShape, SparkSchemaAdapter}

def runtimeShapeOf[A](dataset: Dataset[A]): RuntimeShape =
  SparkSchemaAdapter.fromStructType(dataset.schema)
```

The adapter understands Spark primitives, arrays, maps, and nested structs.
It preserves top-level and nested `StructField.nullable`, array `containsNull`,
map `valueContainsNull`, and `ctdc.hasDefault` metadata.

There is also a DTO bridge, `fromSparkFields`, for integration boundaries where
passing Spark classes is inconvenient. That path uses `DataType.simpleString`;
it is useful for thin wiring, but it cannot preserve nested struct nullability.

## End-to-end at the sink

```scala
import proofgate.proof.SchemaPolicy
import proofgate.runtime.spark.RuntimePin

val actual = runtimeShapeOf(producedDataset)
val expected = SparkSchemaAdapter.fromSparkFields(contractFields)

val findings =
  summon[RuntimePin[SchemaPolicy.Exact.type]].validate(actual, expected)
```

`findings` is a `Vector[proofgate.model.Finding]` that drops straight into a
`ReviewReport`. The CLI and the GitHub workflow already render it.

## Dependency posture

Spark 3.5 ships for Scala 2.13, so this Scala 3 build uses
`CrossVersion.for3Use2_13` with Spark marked `Provided`. That keeps Spark out
of the core proof/model artifact graph while allowing the runtime adapter and
tests to compile against real Spark SQL types.
