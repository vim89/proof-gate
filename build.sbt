val scala3Version = "3.8.3"
val munitVersion = "1.3.0"

ThisBuild / organization := "com.vitthalmirji"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / scalaVersion := scala3Version
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:imports",
  "-Xmax-inlines:256"
)

ThisBuild / Test / parallelExecution := false

lazy val commonSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
)

def module(id: String): Project =
  module(id, id)

def module(id: String, path: String): Project =
  Project(id, file(s"modules/$path"))
    .settings(commonSettings *)
    .settings(
      publish / skip := true
    )

lazy val model = module("model")
  .settings(
    name := "proof-gate-model",
    moduleName := "proof-gate-model"
  )

lazy val proof = module("proof")
  .settings(
    name := "proof-gate-proof",
    moduleName := "proof-gate-proof"
  )
  .dependsOn(model)

lazy val runtimeSpark = module("runtimeSpark", "runtime-spark")
  .settings(
    name := "proof-gate-runtime-spark",
    moduleName := "proof-gate-runtime-spark"
  )
  .dependsOn(model, proof)

lazy val cli = module("cli")
  .settings(
    name := "proof-gate-cli",
    moduleName := "proof-gate-cli"
  )
  .dependsOn(model, proof, runtimeSpark)

lazy val examples = module("examples")
  .settings(
    name := "proof-gate-examples",
    moduleName := "proof-gate-examples"
  )
  .dependsOn(model, proof, runtimeSpark)

lazy val fixturesCompileFail = module("fixturesCompileFail", "fixtures-compile-fail")
  .settings(
    name := "proof-gate-fixtures-compile-fail",
    moduleName := "proof-gate-fixtures-compile-fail"
  )
  .dependsOn(model, proof)

lazy val fixturesPolicyFail = module("fixturesPolicyFail", "fixtures-policy-fail")
  .settings(
    name := "proof-gate-fixtures-policy-fail",
    moduleName := "proof-gate-fixtures-policy-fail"
  )
  .dependsOn(model)

// fixturesPolicyFail is intentionally not aggregated. Its sources hold
// patterns that Scalafix must reject. scripts/check-policy-fixtures.sh runs
// it on demand and inverts the exit code so the conveyor proves the rules
// are still wired up.
lazy val root = project
  .in(file("."))
  .aggregate(model, proof, runtimeSpark, cli, examples, fixturesCompileFail)
  .settings(
    name := "proof-gate",
    publish / skip := true,
    addCommandAlias(
      "reviewGates",
      ";scalafmtSbtCheck;scalafmtCheckAll;test"
    ),
    addCommandAlias(
      "reviewPolicy",
      ";scalafixAll --check"
    ),
    addCommandAlias(
      "reviewConveyor",
      ";reviewGates;reviewPolicy"
    )
  )
