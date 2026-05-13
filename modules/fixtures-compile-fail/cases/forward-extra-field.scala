import proofgate.proof.*

final case class Out(id: Long, email: String, segment: String)
final case class Contract(id: Long, email: String)

val evidence = summon[SchemaConforms[Out, Contract, SchemaPolicy.Forward.type]]
