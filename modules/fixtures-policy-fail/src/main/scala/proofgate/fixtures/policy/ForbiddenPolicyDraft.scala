package proofgate.fixtures.policy

object ForbiddenPolicyDraft:
  def directSysEnv(): Option[String] =
    sys.env.get("WAREHOUSE_PATH")

  def directSystemGetenv(): String | Null =
    System.getenv("WAREHOUSE_PATH")

  def directConfigFactory(): String =
    ConfigFactory.toString

  def rawTryCatch(value: String): Int =
    try {
      value.toInt
    } catch {
      case _: NumberFormatException => 0
    }

  def rawThrow(): Nothing =
    throw new IllegalStateException("policy fixture rejected")

  def rawRequire(value: String): String =
    require(value.nonEmpty, "value required")
    value

  private object ConfigFactory
