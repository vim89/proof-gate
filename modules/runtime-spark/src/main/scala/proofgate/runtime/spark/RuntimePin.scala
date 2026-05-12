package proofgate.runtime.spark

import proofgate.model.*
import proofgate.proof.SchemaPolicy

final case class RuntimeField(name: String, dataType: String, nullable: Boolean)
final case class RuntimeShape(fields: Vector[RuntimeField])

trait RuntimeShapeEncoder[A]:
  def encode(value: A): RuntimeShape

trait RuntimePin[P <: SchemaPolicy]:
  def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding]

object RuntimePin:
  val runtimePinRuleId: RuleId = RuleId.unsafe("runtime-pin.shape-mismatch")

  given exact: RuntimePin[SchemaPolicy.Exact.type] with
    def validate(actual: RuntimeShape, expected: RuntimeShape): Vector[Finding] =
      if actual == expected then Vector.empty
      else
        Vector(
          Finding(
            stage = StageName.Pin,
            ruleId = runtimePinRuleId,
            severity = FindingSeverity.Blocker,
            message = "Runtime shape drift detected at the sink boundary.",
            path = None,
            hint = Some("Inspect field names, types, and nullability before shipping.")
          )
        )
