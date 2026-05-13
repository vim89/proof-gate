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
