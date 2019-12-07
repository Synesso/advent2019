

class Amplifier(computer: IntCodeComputer, phase: Int) {

  private var piped: Option[Amplifier] = None

  def pipeTo(r: Amplifier): Amplifier = {
    piped = Some(r)
    this
  }

  private var init = true

  def input(i: Int): Result = input(List(i))

  def input(in: List[Int]): Result = {
//    println(s"P$phase")
    val result = if (init) {
      init = false
      computer.execute(phase +: in)
    } else computer.execute(in)
    result match {
      case t@ Term(output) => piped.map(_.input(output)).getOrElse(t)
      case c@ Continue(output) => piped.map(_.input(output)).getOrElse(c)
    }
  }

}
