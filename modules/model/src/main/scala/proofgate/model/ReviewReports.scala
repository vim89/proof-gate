package proofgate.model

final case class SeverityCounts(
    blocker: Int,
    mustFix: Int,
    shouldFix: Int,
    nit: Int
):
  def total: Int = blocker + mustFix + shouldFix + nit

object SeverityCounts:
  val empty: SeverityCounts = SeverityCounts(0, 0, 0, 0)

  def from(findings: Iterable[Finding]): SeverityCounts =
    findings.foldLeft(empty): (counts, finding) =>
      finding.severity match
        case FindingSeverity.Blocker =>
          counts.copy(blocker = counts.blocker + 1)
        case FindingSeverity.MustFix =>
          counts.copy(mustFix = counts.mustFix + 1)
        case FindingSeverity.ShouldFix =>
          counts.copy(shouldFix = counts.shouldFix + 1)
        case FindingSeverity.Nit =>
          counts.copy(nit = counts.nit + 1)

object ReviewReports:
  def findings(report: ReviewReport): Vector[Finding] =
    report.stageOutcomes.flatMap(_.findings)

  def severityCounts(report: ReviewReport): SeverityCounts =
    SeverityCounts.from(findings(report))

  def finalVerdict(report: ReviewReport): Verdict =
    if report.stageOutcomes.exists(_.verdict == Verdict.Error) then Verdict.Error
    else if report.stageOutcomes.exists(_.verdict == Verdict.Reject) then Verdict.Reject
    else if findings(report).exists(isRejecting) then Verdict.Reject
    else if report.stageOutcomes.nonEmpty && report.stageOutcomes.forall(_.verdict == Verdict.Skip)
    then Verdict.Skip
    else Verdict.Pass

  def findingsByStage(report: ReviewReport): Vector[(StageName, Vector[Finding])] =
    StageName.values.toVector.flatMap: stage =>
      val stageFindings = findings(report).filter(_.stage == stage)
      Option.when(stageFindings.nonEmpty)(stage -> stageFindings)

  private def isRejecting(finding: Finding): Boolean =
    finding.severity match
      case FindingSeverity.Blocker | FindingSeverity.MustFix => true
      case FindingSeverity.ShouldFix | FindingSeverity.Nit   => false

object ReviewReportMarkdown:
  def render(report: ReviewReport): String =
    val counts = ReviewReports.severityCounts(report)
    val verdict = ReviewReports.finalVerdict(report)

    Vector(
      "# ProofGate review report",
      "",
      "## Summary",
      report.summary.trim,
      "",
      s"- Revision: `${report.revision.value}`",
      s"- Verdict: `${renderVerdict(verdict)}`",
      s"- Findings: Blocker=${counts.blocker}, MustFix=${counts.mustFix}, ShouldFix=${counts.shouldFix}, Nit=${counts.nit}",
      "",
      renderRiskAssessment(report.risk),
      "",
      renderFindings(report),
      "",
      renderOptionalQuestions(report.optionalQuestions)
    ).mkString("\n").stripTrailing + "\n"

  private def renderRiskAssessment(risk: RiskAssessment): String =
    Vector(
      "## Risk assessment",
      "",
      "| Category | Risk |",
      "| --- | --- |",
      s"| API breaking change | ${renderRisk(risk.apiBreakingChange)} |",
      s"| Binary compatibility | ${renderRisk(risk.binaryCompatibility)} |",
      s"| Source compatibility | ${renderRisk(risk.sourceCompatibility)} |",
      s"| Cross build | ${renderRisk(risk.crossBuild)} |",
      s"| Performance | ${renderRisk(risk.performance)} |",
      s"| Concurrency | ${renderRisk(risk.concurrency)} |",
      s"| Security | ${renderRisk(risk.security)} |"
    ).mkString("\n")

  private def renderFindings(report: ReviewReport): String =
    val grouped = ReviewReports.findingsByStage(report)
    if grouped.isEmpty then "## Findings\n\nNo findings."
    else
      val sections = grouped.map: (stage, findings) =>
        val renderedFindings = findings.map(renderFinding).mkString("\n")
        s"### ${renderStage(stage)}\n\n$renderedFindings"

      ("## Findings" +: "" +: sections).mkString("\n")

  private def renderFinding(finding: Finding): String =
    val base =
      s"- [${renderSeverity(finding.severity)}] `${finding.ruleId.value}`: ${finding.message}"
    val path = finding.path.map(value => s"  Path: `${value}`")
    val hint = finding.hint.map(value => s"  Hint: $value")

    (Vector(base) ++ path ++ hint).mkString("\n")

  private def renderOptionalQuestions(questions: Vector[String]): String =
    if questions.isEmpty then "## Optional questions\n\nNone."
    else
      val renderedQuestions = questions.map(question => s"- $question").mkString("\n")
      s"## Optional questions\n\n$renderedQuestions"

  private def renderVerdict(verdict: Verdict): String =
    verdict.toString

  private def renderSeverity(severity: FindingSeverity): String =
    severity.toString

  private def renderStage(stage: StageName): String =
    stage.toString

  private def renderRisk(risk: RiskLevel): String =
    risk.toString

object ReviewReportJson:
  def render(report: ReviewReport): String =
    val counts = ReviewReports.severityCounts(report)
    val verdict = ReviewReports.finalVerdict(report)
    val findings = ReviewReports.findings(report)

    obj(
      "schemaVersion" -> string("proof-gate.report.v1"),
      "revision" -> string(report.revision.value),
      "summary" -> string(report.summary.trim),
      "verdict" -> string(verdict.toString),
      "severityCounts" -> renderSeverityCounts(counts),
      "risk" -> renderRiskAssessment(report.risk),
      "findings" -> array(findings.map(renderFinding)),
      "optionalQuestions" -> array(report.optionalQuestions.map(string))
    ) + "\n"

  private def renderSeverityCounts(counts: SeverityCounts): String =
    obj(
      "blocker" -> number(counts.blocker),
      "mustFix" -> number(counts.mustFix),
      "shouldFix" -> number(counts.shouldFix),
      "nit" -> number(counts.nit),
      "total" -> number(counts.total)
    )

  private def renderRiskAssessment(risk: RiskAssessment): String =
    obj(
      "apiBreakingChange" -> string(risk.apiBreakingChange.toString),
      "binaryCompatibility" -> string(risk.binaryCompatibility.toString),
      "sourceCompatibility" -> string(risk.sourceCompatibility.toString),
      "crossBuild" -> string(risk.crossBuild.toString),
      "performance" -> string(risk.performance.toString),
      "concurrency" -> string(risk.concurrency.toString),
      "security" -> string(risk.security.toString)
    )

  private def renderFinding(finding: Finding): String =
    obj(
      "stage" -> string(finding.stage.toString),
      "ruleId" -> string(finding.ruleId.value),
      "severity" -> string(finding.severity.toString),
      "message" -> string(finding.message),
      "path" -> optionalString(finding.path),
      "hint" -> optionalString(finding.hint)
    )

  private def obj(fields: (String, String)*): String =
    fields.map((name, value) => s"${string(name)}:$value").mkString("{", ",", "}")

  private def array(values: Iterable[String]): String =
    values.mkString("[", ",", "]")

  private def string(value: String): String =
    "\"" + escape(value) + "\""

  private def optionalString(value: Option[String]): String =
    value.fold("null")(string)

  private def number(value: Int): String =
    value.toString

  private def escape(value: String): String =
    value.flatMap:
      case '"'                    => "\\\""
      case '\\'                   => "\\\\"
      case '\b'                   => "\\b"
      case '\f'                   => "\\f"
      case '\n'                   => "\\n"
      case '\r'                   => "\\r"
      case '\t'                   => "\\t"
      case char if char.isControl =>
        "\\u%04x".format(char.toInt)
      case char =>
        char.toString
