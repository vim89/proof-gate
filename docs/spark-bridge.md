# Spark bridge example

ProofGate keeps the `runtime-spark` module free of Spark binaries so the
Scala 3 build stays fast and clean. Downstream projects that already depend on
Spark provide their own `StructType` and bridge into `RuntimeShape` with one
line.

## The one-line bridge

In a project that already has `org.apache.spark:spark-sql:_:Provided` on the
classpath:

```scala
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.types.StructType
import proofgate.runtime.spark.{
  RuntimeShape,
  SparkFieldInfo,
  SparkSchemaAdapter
}

def runtimeShapeOf[A](dataset: Dataset[A]): RuntimeShape =
  fromStructType(dataset.schema)

def fromStructType(schema: StructType): RuntimeShape =
  val infos = schema.fields.iterator
    .map: field =>
      SparkFieldInfo(
        name = field.name,
        sparkType = field.dataType.simpleString,
        nullable = field.nullable
      )
    .toVector
  SparkSchemaAdapter.fromSparkFields(infos)
```

The adapter understands Spark primitives plus `array<T>`, `map<K,V>`, and
nested `struct<...>`. Unknown types pass through as raw Spark names so the
runtime diff still surfaces them in the review report.

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

## Why this is not a sub-module yet

Spark 3.5 and Spark 4 ship for Scala 2.13. Pulling them into this Scala 3
build needs the `for3Use2_13` cross modifier. That works for source-level
usage of `StructType` and `Dataset`, but it ties this repository's CI to a
specific Spark line and inflates compile time. Until the talk demo requires a
runnable Spark example inside the repository, the bridge stays a six-line
recipe a downstream project copies into its own module that already pays the
Spark cost.

When the demo needs a runnable example, the natural addition is:

```scala
lazy val examplesSpark = module("examplesSpark", "examples-spark")
  .settings(
    libraryDependencies += (
      "org.apache.spark" %% "spark-sql" % "3.5.1" % Provided
    ).cross(CrossVersion.for3Use2_13)
  )
  .dependsOn(model, proof, runtimeSpark)
```

That keeps Spark out of the core artifact graph while letting one example
module exercise the bridge against a real `StructType`.
