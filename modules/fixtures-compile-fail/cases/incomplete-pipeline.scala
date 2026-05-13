import proofgate.proof.*

final case class Out(id: Long, email: String)

val incomplete =
  Builder.empty
    .source[Out]("orders-source")
    .toOption
    .get

val pipeline = incomplete.build
