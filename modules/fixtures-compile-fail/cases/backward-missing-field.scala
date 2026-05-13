import proofgate.proof.*

final case class Out(id: Long, email: String)
final case class Contract(id: Long, email: String, amount: BigDecimal)

val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Backward.type]]
