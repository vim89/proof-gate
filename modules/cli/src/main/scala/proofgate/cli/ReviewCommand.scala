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
        val markdown = ReviewReportMarkdown.render(report)
        val verdict = ReviewReports.finalVerdict(report)
        val exitCode = if verdict == Verdict.Pass || verdict == Verdict.Skip then Success else GateFailed

        options.out match
          case Some(path) =>
            write(path, markdown) match
              case Right(()) =>
                CliResult(exitCode, s"Wrote ProofGate report to ${path.toString}\n", "")
              case Left(error) =>
                CliResult(UsageError, "", s"$error\n")
          case None =>
            CliResult(exitCode, markdown, "")

  private def parseOptions(args: Vector[String]): Either[String, ReviewOptions] =
    parsePairs(args).flatMap: pairs =>
      pairs.foldLeft[Either[String, ReviewOptions]](Right(ReviewOptions.empty)):
        case (Right(options), ("--revision", value)) =>
          Right(options.copy(revision = Some(Revision.unsafe(value))))
        case (Right(options), ("--summary", value)) =>
          Right(options.copy(summary = value))
        case (Right(options), ("--out", value)) =>
          Right(options.copy(out = Some(Path.of(value))))
        case (Right(options), ("--question", value)) =>
          Right(options.copy(optionalQuestions = options.optionalQuestions :+ value))
        case (Right(options), ("--finding", value)) =>
          parseFinding(value).map(finding =>
            options.copy(findings = options.findings :+ finding)
          )
        case (Right(options), ("--risk", value)) =>
          applyRisk(options, value)
        case (Right(_), (name, _)) =>
          Left(s"Unknown option: $name")
        case (Left(error), _) =>
          Left(error)
      .flatMap: options =>
        Either.cond(options.revision.nonEmpty, options, "Missing required option: --revision")

  private def parsePairs(args: Vector[String]): Either[String, Vector[(String, String)]] =
    def loop(remaining: Vector[String], parsed: Vector[(String, String)]): Either[String, Vector[(String, String)]] =
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

  private def applyRisk(options: ReviewOptions, value: String): Either[String, ReviewOptions] =
    value.split("=", 2).toVector match
      case Vector(category, level) =>
        parseRisk(level).flatMap(risk => updateRisk(options, category, risk))
      case _ =>
        Left("Invalid --risk. Use category=level")

  private def updateRisk(
      options: ReviewOptions,
      category: String,
      level: RiskLevel
  ): Either[String, ReviewOptions] =
    val risk = options.risk
    normalize(category) match
      case "api" | "apibreakingchange" =>
        Right(options.copy(risk = risk.copy(apiBreakingChange = level)))
      case "binarycompatibility" =>
        Right(options.copy(risk = risk.copy(binaryCompatibility = level)))
      case "sourcecompatibility" =>
        Right(options.copy(risk = risk.copy(sourceCompatibility = level)))
      case "crossbuild" =>
        Right(options.copy(risk = risk.copy(crossBuild = level)))
      case "performance" =>
        Right(options.copy(risk = risk.copy(performance = level)))
      case "concurrency" =>
        Right(options.copy(risk = risk.copy(concurrency = level)))
      case "security" =>
        Right(options.copy(risk = risk.copy(security = level)))
      case _ =>
        Left(s"Unknown risk category: $category")

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
      |  --finding <value>      stage|severity|ruleId|message[|path[|hint]]
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
    out: Option[Path]
):
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
      out = None
    )
