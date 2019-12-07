import scala.annotation.tailrec

case class Op(code: Int, modeP1: Int, modeP2: Int, modeP3: Int)

sealed trait Result {
  val output: List[Int]
  def answer: Int = output.head
}
case class Continue(output: List[Int]) extends Result
case class Term(output: List[Int]) extends Result

class IntCodeComputer(prog: Array[Int]) {
  var ptr: Int = 0

  def execute(input: List[Int]): Result = {

    @tailrec
    def exec(in: List[Int], out: List[Int]): Result = {
      val raw = prog(ptr)
      val code = raw % 100
      val op = Op(code, raw / 100 % 10, raw / 1000 % 10, raw / 10000 % 10)
      code match {
        case 1 =>
          val p1 = prog(ptr + 1)
          val p2 = prog(ptr + 2)
          val p3 = prog(ptr + 3)
          val v1 = if (op.modeP1 == 0) prog(p1) else p1
          val v2 = if (op.modeP2 == 0) prog(p2) else p2
          //          println(s"$op $p1($v1), $p2($v2), $p3")
          //          println(s"  $p1($v1) + $p2($v2) = ${v1 + v2}. Storing in $p3)")
          prog(p3) = v1 + v2
          ptr += 4
          exec(in, out)

        case 2 =>
          val p1 = prog(ptr + 1)
          val p2 = prog(ptr + 2)
          val p3 = prog(ptr + 3)
          val v1 = if (op.modeP1 == 0) prog(p1) else p1
          val v2 = if (op.modeP2 == 0) prog(p2) else p2
          //          println(s"$op $p1($v1), $p2($v2), $p3")
          //          println(s"  $p1($v1) * $p2($v2) = ${v1 * v2}. Storing in $p3")
          prog(p3) = v1 * v2
          ptr += 4
          exec(in, out)

        case 3 =>
          in match {
            case Nil => Continue(out)
            case h +: t =>
              val p1 = prog(ptr + 1)
              prog(p1) = h
              //          println(s"$op $p1")
              //          println(s"  Updating location $p1 to $input")
              ptr += 2
              exec(t, out)
          }

        case 4 =>
          val p1 = prog(ptr + 1)
          //          println(s"$op $p1")
//          println(s">> location $p1 = ${prog(p1)}")
          ptr += 2
          exec(in, prog(p1) +: out)

        // jump if true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
        case 5 =>
          val p1 = prog(ptr + 1)
          val p2 = prog(ptr + 2)
          val v1 = if (op.modeP1 == 0) prog(p1) else p1
          val v2 = if (op.modeP2 == 0) prog(p2) else p2
          ptr = if (v1 == 0) ptr + 3 else v2
          exec(in, out)

        // jummp if false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
        case 6 =>
          val p1 = prog(ptr + 1)
          val p2 = prog(ptr + 2)
          val v1 = if (op.modeP1 == 0) prog(p1) else p1
          val v2 = if (op.modeP2 == 0) prog(p2) else p2
          ptr = if (v1 == 0) v2 else ptr + 3
          exec(in, out)

        // less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
        case 7 =>
          val p1 = prog(ptr + 1)
          val p2 = prog(ptr + 2)
          val p3 = prog(ptr + 3)
          val v1 = if (op.modeP1 == 0) prog(p1) else p1
          val v2 = if (op.modeP2 == 0) prog(p2) else p2
          val store = if (v1 < v2) 1 else 0
          prog(p3) = store
          ptr += 4
          exec(in, out)

        // equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
        case 8 =>
          val p1 = prog(ptr + 1)
          val p2 = prog(ptr + 2)
          val p3 = prog(ptr + 3)
          val v1 = if (op.modeP1 == 0) prog(p1) else p1
          val v2 = if (op.modeP2 == 0) prog(p2) else p2
          val store = if (v1 == v2) 1 else 0
          prog(p3) = store
          ptr += 4
          exec(in, out)

        case 99 =>
          Term(out)
      }
    }

    exec(input, Nil)
  }
}