
object TableDrivenParser extends App {
  val scanner = new Scanner(args(0))

  def parse_error() = {
    println(s"Parser error!")
    System.exit(1)
  }


}