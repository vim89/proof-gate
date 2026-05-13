package proofgate.model

enum Verdict:
  case Pass, Reject, Error, Skip

enum StageName:
  case Proposal, Proof, Policy, Pin, People

enum FindingSeverity:
  case Blocker, MustFix, ShouldFix, Nit

enum RiskLevel:
  case None, Low, Medium, High, Blocker

opaque type RuleId = String
opaque type Revision = String
opaque type PipelineId = String
opaque type ContractId = String

object RuleId:
  def from(value: String): Either[String, RuleId] =
    val trimmed = value.trim
    Either.cond(trimmed.nonEmpty, trimmed, "RuleId cannot be empty")

  def unsafe(value: String): RuleId = value

  extension (ruleId: RuleId) def value: String = ruleId

object Revision:
  def unsafe(value: String): Revision = value
  extension (revision: Revision) def value: String = revision

object PipelineId:
  def from(value: String): Either[String, PipelineId] =
    val trimmed = value.trim
    Either.cond(trimmed.nonEmpty, trimmed, "PipelineId cannot be empty")

  def unsafe(value: String): PipelineId = value

  extension (pipelineId: PipelineId) def value: String = pipelineId

object ContractId:
  def from(value: String): Either[String, ContractId] =
    val trimmed = value.trim
    Either.cond(trimmed.nonEmpty, trimmed, "ContractId cannot be empty")

  def unsafe(value: String): ContractId = value

  extension (contractId: ContractId) def value: String = contractId

final case class Finding(
    stage: StageName,
    ruleId: RuleId,
    severity: FindingSeverity,
    message: String,
    path: Option[String],
    hint: Option[String]
)

final case class RiskAssessment(
    apiBreakingChange: RiskLevel,
    binaryCompatibility: RiskLevel,
    sourceCompatibility: RiskLevel,
    crossBuild: RiskLevel,
    performance: RiskLevel,
    concurrency: RiskLevel,
    security: RiskLevel
)

object RiskAssessment:
  val empty: RiskAssessment =
    RiskAssessment(
      apiBreakingChange = RiskLevel.None,
      binaryCompatibility = RiskLevel.None,
      sourceCompatibility = RiskLevel.None,
      crossBuild = RiskLevel.None,
      performance = RiskLevel.None,
      concurrency = RiskLevel.None,
      security = RiskLevel.None
    )

final case class StageOutcome[+A](
    verdict: Verdict,
    findings: Vector[Finding],
    value: Option[A]
)

final case class ReviewReport(
    revision: Revision,
    summary: String,
    risk: RiskAssessment,
    stageOutcomes: Vector[StageOutcome[?]],
    optionalQuestions: Vector[String]
)

sealed trait ReviewError extends Product with Serializable:
  def stage: StageName
  def message: String

object ReviewError:
  final case class ProposalParse(path: Option[String], message: String) extends ReviewError:
    val stage: StageName = StageName.Proposal

  final case class PolicyExecution(ruleId: RuleId, message: String) extends ReviewError:
    val stage: StageName = StageName.Policy

  final case class RuntimePin(path: Option[String], message: String) extends ReviewError:
    val stage: StageName = StageName.Pin

  final case class Infrastructure(message: String, cause: Option[Throwable] = None)
      extends ReviewError:
    val stage: StageName = StageName.People

type Result[A] = Either[ReviewError, A]
