package proofgate.model

import munit.FunSuite

final class ReviewReportsSuite extends FunSuite:
  test("empty report passes and renders no findings"):
    val report = ReviewReport(
      revision = Revision.unsafe("abc123"),
      summary = "No contract drift.",
      risk = RiskAssessment.empty,
      stageOutcomes = Vector.empty,
      optionalQuestions = Vector.empty
    )

    assertEquals(ReviewReports.finalVerdict(report), Verdict.Pass)
    assertEquals(ReviewReports.severityCounts(report), SeverityCounts.empty)
    assert(ReviewReportMarkdown.render(report).contains("No findings."))

  test("blocker finding rejects report"):
    val report = ReviewReport(
      revision = Revision.unsafe("abc123"),
      summary = "Schema proof failed.",
      risk = RiskAssessment.empty,
      stageOutcomes = Vector(StageOutcome(Verdict.Reject, Vector(blockerFinding), None)),
      optionalQuestions = Vector.empty
    )

    assertEquals(ReviewReports.finalVerdict(report), Verdict.Reject)
    assertEquals(ReviewReports.severityCounts(report).blocker, 1)
    assert(ReviewReportMarkdown.render(report).contains("- Verdict: `Reject`"))

  test("error stage keeps final verdict as error"):
    val report = ReviewReport(
      revision = Revision.unsafe("abc123"),
      summary = "Runtime pin crashed.",
      risk = RiskAssessment.empty,
      stageOutcomes = Vector(StageOutcome(Verdict.Error, Vector.empty, None)),
      optionalQuestions = Vector.empty
    )

    assertEquals(ReviewReports.finalVerdict(report), Verdict.Error)

  test("explicit stage rejection rejects report"):
    val report = ReviewReport(
      revision = Revision.unsafe("abc123"),
      summary = "Policy rejected the proposal.",
      risk = RiskAssessment.empty,
      stageOutcomes = Vector(StageOutcome(Verdict.Reject, Vector.empty, None)),
      optionalQuestions = Vector.empty
    )

    assertEquals(ReviewReports.finalVerdict(report), Verdict.Reject)

  test("findings are rendered in stage order"):
    val proofFinding = blockerFinding.copy(stage = StageName.Proof)
    val pinFinding = blockerFinding.copy(
      stage = StageName.Pin,
      ruleId = RuleId.unsafe("pin.runtime-shape"),
      message = "Runtime shape does not match the contract."
    )
    val report = ReviewReport(
      revision = Revision.unsafe("abc123"),
      summary = "Two gates failed.",
      risk = RiskAssessment.empty.copy(apiBreakingChange = RiskLevel.High),
      stageOutcomes = Vector(
        StageOutcome(Verdict.Reject, Vector(pinFinding, proofFinding), None)
      ),
      optionalQuestions = Vector.empty
    )
    val markdown = ReviewReportMarkdown.render(report)

    assert(markdown.indexOf("### Proof") < markdown.indexOf("### Pin"))
    assert(markdown.contains("| API breaking change | High |"))
    assert(markdown.contains("`proof.schema-exact`"))
    assert(markdown.contains("`pin.runtime-shape`"))

  test("optional questions are rendered"):
    val report = ReviewReport(
      revision = Revision.unsafe("abc123"),
      summary = "Review passed with question.",
      risk = RiskAssessment.empty,
      stageOutcomes = Vector.empty,
      optionalQuestions = Vector("Should this sink be append-only?")
    )

    assert(ReviewReportMarkdown.render(report).contains("- Should this sink be append-only?"))

  test("json report renders canonical machine-readable fields"):
    val report = ReviewReport(
      revision = Revision.unsafe("abc123"),
      summary = "Schema proof failed.",
      risk = RiskAssessment.empty.copy(security = RiskLevel.Medium),
      stageOutcomes = Vector(StageOutcome(Verdict.Reject, Vector(blockerFinding), None)),
      optionalQuestions = Vector("Should this sink be append-only?")
    )
    val json = ReviewReportJson.render(report)

    assert(json.contains(""""schemaVersion":"proof-gate.report.v1""""))
    assert(json.contains(""""revision":"abc123""""))
    assert(json.contains(""""verdict":"Reject""""))
    assert(json.contains(""""blocker":1"""))
    assert(json.contains(""""security":"Medium""""))
    assert(json.contains(""""ruleId":"proof.schema-exact""""))
    assert(json.contains(""""path":"pipelines/orders.scala""""))
    assert(json.contains(""""optionalQuestions":["Should this sink be append-only?"]"""))

  test("json report escapes strings"):
    val report = ReviewReport(
      revision = Revision.unsafe("abc123"),
      summary = "Quote \" and newline\nare escaped.",
      risk = RiskAssessment.empty,
      stageOutcomes = Vector.empty,
      optionalQuestions = Vector.empty
    )
    val json = ReviewReportJson.render(report)

    assert(json.contains("""Quote \" and newline\nare escaped."""))

  private val blockerFinding: Finding =
    Finding(
      stage = StageName.Proof,
      ruleId = RuleId.unsafe("proof.schema-exact"),
      severity = FindingSeverity.Blocker,
      message = "Output schema is missing required field customer_id.",
      path = Some("pipelines/orders.scala"),
      hint = Some("Add customer_id or change the declared contract.")
    )
