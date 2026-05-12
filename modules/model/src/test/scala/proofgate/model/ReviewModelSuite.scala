package proofgate.model

import munit.FunSuite

final class ReviewModelSuite extends FunSuite:
  test("RuleId rejects blank ids"):
    assertEquals(RuleId.from("   ").left.toOption, Some("RuleId cannot be empty"))

  test("RiskAssessment.empty is zeroed"):
    assertEquals(RiskAssessment.empty.security, RiskLevel.None)
