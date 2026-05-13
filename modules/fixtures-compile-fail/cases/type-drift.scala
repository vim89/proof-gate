import proofgate.proof.*

final case class Out(id: Long, email: Long)
final case class Contract(id: Long, email: String)

val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Exact.type]]
