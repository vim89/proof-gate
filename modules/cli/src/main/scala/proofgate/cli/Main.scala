package proofgate.cli

object Main:
  def main(args: Array[String]): Unit =
    val result = ReviewCommand.run(args.toVector)

    if result.stdout.nonEmpty then Console.out.print(result.stdout)
    if result.stderr.nonEmpty then Console.err.print(result.stderr)
    if result.exitCode != 0 then sys.exit(result.exitCode)
