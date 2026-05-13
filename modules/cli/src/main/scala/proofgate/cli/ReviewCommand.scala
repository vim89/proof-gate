package proofgate.cli

import proofgate.model.*

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.util.Try

final case class CliResult(exitCode: Int, stdout: String, stderr: String)

object ReviewCommand:
  private val Success = 0
  private val GateFailed = 1
  private val UsageError = 2

  def run(args: Vector[String]): CliResult =
    run(args, writeFile)

  def run(args: Vector[String], write: (Path, String) => Either[String, Unit]): CliResult =
    args match
      case Vector() | Vector("help") | Vector("--help") =>
        CliResult(Success, usage, "")
      case "review" +: options =>
        renderReview(parseOptions(options), write)
      case command +: _ =>
        CliResult(UsageError, "", s"Unknown command: $command\n\n$usage")
      case _ =>
        CliResult(UsageError, "", usage)

  private def renderReview(
      parsed: Either[String, ReviewOptions],
      write: (Path, String) => Either[String, Unit]
  ): CliResult =
    parsed match
      case Left(error) =>
        CliResult(UsageError, "", s"$error\n\n$usage")
      case Right(options) =>
        val report = options.toReport
        val rendered = options.formatOrDefault.render(report)
        val verdict = ReviewReports.finalVerdict(report)
        val exitCode =
          if verdict == Verdict.Pass || verdict == Verdict.Skip then Success else GateFailed

        options.out match
          case Some(path) =>
            write(path, rendered) match
              case Right(()) =>
                CliResult(exitCode, s"Wrote ProofGate report to ${path.toString}\n", "")
              case Left(error) =>
                CliResult(UsageError, "", s"$error\n")
          case None =>
            CliResult(exitCode, rendered, "")

  private def parseOptions(args: Vector[String]): Either[String, ReviewOptions] =
    parsePairs(args).flatMap: pairs =>
      // Each pair is validated independently so a user passing several malformed options
      // sees every problem in one report, not just the first.
      val perPair: Vector[proofgate.model.Validated[String, ReviewOptions => ReviewOptions]] =
        pairs.map(applyPair)

      proofgate.model.Validated
        .sequence(perPair)
        .toEither
        .left
        .map(errors => errors.mkString("; "))
        .map(updates => updates.foldLeft(ReviewOptions.empty)((acc, update) => update(acc)))
        .flatMap: options =>
          Either.cond(options.revision.nonEmpty, options, "Missing required option: --revision")

  private def applyPair(
      pair: (String, String)
  ): proofgate.model.Validated[String, ReviewOptions => ReviewOptions] =
    pair match
      case ("--revision", value) =>
        proofgate.model.Validated.valid(_.copy(revision = Some(Revision.unsafe(value))))
      case ("--summary", value) =>
        proofgate.model.Validated.valid(_.copy(summary = value))
      case ("--out", value) =>
        proofgate.model.Validated.valid(_.copy(out = Some(Path.of(value))))
      case ("--format", value) =>
        proofgate.model.Validated
          .fromEither(parseFormat(value))
          .map: format =>
            (options: ReviewOptions) => options.copy(format = Some(format))
      case ("--question", value) =>
        proofgate.model.Validated.valid: options =>
          options.copy(optionalQuestions = options.optionalQuestions :+ value)
      case ("--finding", value) =>
        proofgate.model.Validated
          .fromEither(parseFinding(value))
          .map: finding =>
            (options: ReviewOptions) => options.copy(findings = options.findings :+ finding)
      case ("--risk", value) =>
        proofgate.model.Validated.fromEither(parseRiskUpdate(value))
      case (name, _) =>
        proofgate.model.Validated.invalid(s"Unknown option: $name")

  private def parseRiskUpdate(value: String): Either[String, ReviewOptions => ReviewOptions] =
    value.split("=", 2).toVector match
      case Vector(category, level) =>
        parseRisk(level).flatMap(risk => updateRiskFn(category, risk))
      case _ =>
        Left("Invalid --risk. Use category=level")

  private def updateRiskFn(
      category: String,
      level: RiskLevel
  ): Either[String, ReviewOptions => ReviewOptions] =
    normalize(category) match
      case "api" | "apibreakingchange" =>
        Right(options => options.copy(risk = options.risk.copy(apiBreakingChange = level)))
      case "binarycompatibility" =>
        Right(options => options.copy(risk = options.risk.copy(binaryCompatibility = level)))
      case "sourcecompatibility" =>
        Right(options => options.copy(risk = options.risk.copy(sourceCompatibility = level)))
      case "crossbuild" =>
        Right(options => options.copy(risk = options.risk.copy(crossBuild = level)))
      case "performance" =>
        Right(options => options.copy(risk = options.risk.copy(performance = level)))
      case "concurrency" =>
        Right(options => options.copy(risk = options.risk.copy(concurrency = level)))
      case "security" =>
        Right(options => options.copy(risk = options.risk.copy(security = level)))
      case _ =>
        Left(s"Unknown risk category: $category")

  private def parsePairs(args: Vector[String]): Either[String, Vector[(String, String)]] =
    def loop(
        remaining: Vector[String],
        parsed: Vector[(String, String)]
    ): Either[String, Vector[(String, String)]] =
      remaining match
        case Vector() =>
          Right(parsed)
        case name +: value +: tail if name.startsWith("--") && !value.startsWith("--") =>
          loop(tail, parsed :+ (name -> value))
        case name +: _ if name.startsWith("--") =>
          Left(s"Missing value for option: $name")
        case value +: _ =>
          Left(s"Unexpected argument: $value")
        case _ =>
          Left("Invalid argument list")

    loop(args, Vector.empty)

  private def parseFinding(value: String): Either[String, Finding] =
    value.split("\\|", -1).toVector match
      case Vector(stage, severity, ruleId, message) =>
        buildFinding(stage, severity, ruleId, message, None, None)
      case Vector(stage, severity, ruleId, message, path) =>
        buildFinding(stage, severity, ruleId, message, nonBlank(path), None)
      case Vector(stage, severity, ruleId, message, path, hint) =>
        buildFinding(stage, severity, ruleId, message, nonBlank(path), nonBlank(hint))
      case _ =>
        Left("Invalid --finding. Use stage|severity|ruleId|message[|path[|hint]]")

  private def buildFinding(
      stage: String,
      severity: String,
      ruleId: String,
      message: String,
      path: Option[String],
      hint: Option[String]
  ): Either[String, Finding] =
    for
      parsedStage <- parseStage(stage)
      parsedSeverity <- parseSeverity(severity)
      parsedRuleId <- RuleId.from(ruleId).left.map(error => s"Invalid finding rule id: $error")
      _ <- Either.cond(message.trim.nonEmpty, (), "Finding message cannot be empty")
    yield Finding(parsedStage, parsedRuleId, parsedSeverity, message.trim, path, hint)

  private def parseStage(value: String): Either[String, StageName] =
    normalize(value) match
      case "proposal" => Right(StageName.Proposal)
      case "proof"    => Right(StageName.Proof)
      case "policy"   => Right(StageName.Policy)
      case "pin"      => Right(StageName.Pin)
      case "people"   => Right(StageName.People)
      case _          => Left(s"Unknown stage: $value")

  private def parseSeverity(value: String): Either[String, FindingSeverity] =
    normalize(value) match
      case "blocker"   => Right(FindingSeverity.Blocker)
      case "mustfix"   => Right(FindingSeverity.MustFix)
      case "shouldfix" => Right(FindingSeverity.ShouldFix)
      case "nit"       => Right(FindingSeverity.Nit)
      case _           => Left(s"Unknown severity: $value")

  private def parseRisk(value: String): Either[String, RiskLevel] =
    normalize(value) match
      case "none"    => Right(RiskLevel.None)
      case "low"     => Right(RiskLevel.Low)
      case "medium"  => Right(RiskLevel.Medium)
      case "high"    => Right(RiskLevel.High)
      case "blocker" => Right(RiskLevel.Blocker)
      case _         => Left(s"Unknown risk level: $value")

  private def parseFormat(value: String): Either[String, ReportFormat] =
    normalize(value) match
      case "markdown" | "md" => Right(ReportFormat.Markdown)
      case "json"            => Right(ReportFormat.Json)
      case _                 => Left(s"Unknown report format: $value")

  private def writeFile(path: Path, content: String): Either[String, Unit] =
    Try:
      val parent = path.toAbsolutePath.getParent
      if parent != null then Files.createDirectories(parent)
      Files.writeString(path, content, StandardCharsets.UTF_8)
      ()
    .toEither.left.map(error => s"Could not write report to ${path.toString}: ${error.getMessage}")

  private def normalize(value: String): String =
    value.toLowerCase.replace("-", "").replace("_", "").replace(" ", "")

  private def nonBlank(value: String): Option[String] =
    Option.when(value.trim.nonEmpty)(value.trim)

  private val usage: String =
    """Usage:
      |  proof-gate review --revision <sha> [options]
      |
      |Options:
      |  --summary <text>       Report summary. Defaults to "ProofGate review completed."
      |  --out <path>           Write Markdown report to a file. Prints to stdout when omitted.
      |  --format <format>      Output format: markdown or json. Inferred from .json output paths.
      |  --finding <value>      stage|severity|ruleId|message[|path[|hint]]
      |                         Pipe-delimited. Fields cannot contain the | character.
      |  --risk <value>         category=level, for example api-breaking-change=High
      |  --question <text>      Optional human-review question.
      |
      |Exit codes:
      |  0  Pass or Skip
      |  1  Reject or Error
      |  2  Invalid command, invalid input, or write failure
      |""".stripMargin

private final case class ReviewOptions(
    revision: Option[Revision],
    summary: String,
    risk: RiskAssessment,
    findings: Vector[Finding],
    optionalQuestions: Vector[String],
    out: Option[Path],
    format: Option[ReportFormat]
):
  def formatOrDefault: ReportFormat =
    format.orElse(out.flatMap(ReportFormat.fromPath)).getOrElse(ReportFormat.Markdown)

  def toReport: ReviewReport =
    val stageOutcome =
      if findings.isEmpty then Vector.empty
      else Vector(StageOutcome(stageVerdict, findings, None))

    ReviewReport(
      revision = revision.getOrElse(Revision.unsafe("unknown")),
      summary = summary,
      risk = risk,
      stageOutcomes = stageOutcome,
      optionalQuestions = optionalQuestions
    )

  private def stageVerdict: Verdict =
    if findings.exists(finding =>
        finding.severity == FindingSeverity.Blocker || finding.severity == FindingSeverity.MustFix
      )
    then Verdict.Reject
    else Verdict.Pass

private object ReviewOptions:
  val empty: ReviewOptions =
    ReviewOptions(
      revision = None,
      summary = "ProofGate review completed.",
      risk = RiskAssessment.empty,
      findings = Vector.empty,
      optionalQuestions = Vector.empty,
      out = None,
      format = None
    )

private enum ReportFormat:
  case Markdown, Json

  def render(report: ReviewReport): String =
    this match
      case Markdown => ReviewReportMarkdown.render(report)
      case Json     => ReviewReportJson.render(report)

private object ReportFormat:
  def fromPath(path: Path): Option[ReportFormat] =
    Option(path.getFileName)
      .map(_.toString.toLowerCase)
      .collect:
        case name if name.endsWith(".json") => Json
