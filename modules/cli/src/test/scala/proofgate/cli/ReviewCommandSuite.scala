package proofgate.cli

import java.nio.file.Files
import java.nio.file.Path

import munit.FunSuite

final class ReviewCommandSuite extends FunSuite:
  test("review command renders passing report to stdout"):
    val result = ReviewCommand.run(Vector("review", "--revision", "abc123"))

    assertEquals(result.exitCode, 0)
    assert(result.stdout.contains("# ProofGate review report"))
    assert(result.stdout.contains("- Verdict: `Pass`"))
    assert(result.stderr.isEmpty)

  test("review command exits with gate failure for blocker finding"):
    val result = ReviewCommand.run(
      Vector(
        "review",
        "--revision",
        "abc123",
        "--finding",
        "proof|blocker|proof.schema-exact|Missing customer_id|pipelines/orders.scala|Add the field"
      )
    )

    assertEquals(result.exitCode, 1)
    assert(result.stdout.contains("- Verdict: `Reject`"))
    assert(result.stdout.contains("`proof.schema-exact`"))
    assert(result.stdout.contains("Path: `pipelines/orders.scala`"))

  test("review command writes report to file"):
    val dir = Files.createTempDirectory("proof-gate-cli")
    val out = dir.resolve("report.md")

    val result = ReviewCommand.run(
      Vector(
        "review",
        "--revision",
        "abc123",
        "--summary",
        "CI review completed.",
        "--risk",
        "api-breaking-change=High",
        "--question",
        "Should this sink be append-only?",
        "--out",
        out.toString
      )
    )

    val markdown = Files.readString(out)

    assertEquals(result.exitCode, 0)
    assertEquals(result.stdout, s"Wrote ProofGate report to ${out.toString}\n")
    assert(markdown.contains("CI review completed."))
    assert(markdown.contains("| API breaking change | High |"))
    assert(markdown.contains("- Should this sink be append-only?"))

  test("review command rejects invalid input"):
    val result = ReviewCommand.run(Vector("review", "--revision"))

    assertEquals(result.exitCode, 2)
    assert(result.stdout.isEmpty)
    assert(result.stderr.contains("Missing value for option: --revision"))

  test("review command reports write failure"):
    val result = ReviewCommand.run(
      Vector("review", "--revision", "abc123", "--out", "/tmp/report.md"),
      (_: Path, _: String) => Left("disk full")
    )

    assertEquals(result.exitCode, 2)
    assertEquals(result.stderr, "disk full\n")
