

object d05 extends App {

  private val xs = List(3,225,1,225,6,6,1100,1,238,225,104,0,1101,90,64,225,1101,15,56,225,1,14,153,224,101,-147,224,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,2,162,188,224,101,-2014,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,1001,18,81,224,1001,224,-137,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,1102,16,16,224,101,-256,224,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,101,48,217,224,1001,224,-125,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,1002,158,22,224,1001,224,-1540,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,83,31,225,1101,56,70,225,1101,13,38,225,102,36,192,224,1001,224,-3312,224,4,224,1002,223,8,223,1001,224,4,224,1,224,223,223,1102,75,53,225,1101,14,92,225,1101,7,66,224,101,-73,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,77,60,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,226,677,224,1002,223,2,223,1005,224,329,1001,223,1,223,1007,226,677,224,1002,223,2,223,1005,224,344,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,359,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,374,101,1,223,223,8,677,677,224,1002,223,2,223,1005,224,389,1001,223,1,223,107,677,677,224,102,2,223,223,1006,224,404,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,419,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,434,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,449,1001,223,1,223,1107,226,226,224,1002,223,2,223,1005,224,464,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,479,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,494,1001,223,1,223,1107,226,677,224,1002,223,2,223,1005,224,509,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,524,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1108,677,677,224,1002,223,2,223,1005,224,554,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,569,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,108,677,677,224,102,2,223,223,1006,224,614,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,629,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,644,101,1,223,223,107,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,674,1001,223,1,223,4,223,99,226)
  private val input = 5

  case class Op(code: Int, modeP1: Int, modeP2: Int, modeP3: Int)

  def exec(prog: Array[Int], ptr: Int = 0): Unit = {
    val raw = prog(ptr)
    val code = raw % 100
    val op  = Op(code, raw / 100 % 10, raw / 1000 % 10, raw / 10000 % 10)
    code match {
      case 1 =>
        val p1 = prog(ptr + 1)
        val p2 = prog(ptr + 2)
        val p3 = prog(ptr + 3)
        val v1 = if (op.modeP1 == 0) prog(p1) else p1
        val v2 = if (op.modeP2 == 0) prog(p2) else p2
        println(s"$op $p1($v1), $p2($v2), $p3")
        println(s"  $p1($v1) + $p2($v2) = ${v1 + v2}. Storing in $p3)")
        prog(p3) = v1 + v2
        exec(prog, ptr + 4)

      case 2 =>
        val p1 = prog(ptr + 1)
        val p2 = prog(ptr + 2)
        val p3 = prog(ptr + 3)
        val v1 = if (op.modeP1 == 0) prog(p1) else p1
        val v2 = if (op.modeP2 == 0) prog(p2) else p2
        println(s"$op $p1($v1), $p2($v2), $p3")
        println(s"  $p1($v1) * $p2($v2) = ${v1 * v2}. Storing in $p3")
        prog(p3) = v1 * v2
        exec(prog, ptr + 4)

      case 3 =>
        val p1 = prog(ptr + 1)
        prog(p1) = input
        println(s"$op $p1")
        println(s"  Updating location $p1 to $input")
        exec(prog, ptr + 2)

      case 4 =>
        val p1 = prog(ptr + 1)
        println(s"$op $p1")
        println(s">> location $p1 = ${prog(p1)}")
        exec(prog, ptr + 2)

      // jump if true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
      case 5 =>
        val p1 = prog(ptr + 1)
        val p2 = prog(ptr + 2)
        val v1 = if (op.modeP1 == 0) prog(p1) else p1
        val v2 = if (op.modeP2 == 0) prog(p2) else p2
        if (v1 != 0) exec(prog, v2)
        else exec(prog, ptr + 3)

      // jummp if false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
      case 6 =>
        val p1 = prog(ptr + 1)
        val p2 = prog(ptr + 2)
        val v1 = if (op.modeP1 == 0) prog(p1) else p1
        val v2 = if (op.modeP2 == 0) prog(p2) else p2
        if (v1 == 0) exec(prog, v2)
        else exec(prog, ptr + 3)

      // less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
      case 7 =>
        val p1 = prog(ptr + 1)
        val p2 = prog(ptr + 2)
        val p3 = prog(ptr + 3)
        val v1 = if (op.modeP1 == 0) prog(p1) else p1
        val v2 = if (op.modeP2 == 0) prog(p2) else p2
        val store = if (v1 < v2) 1 else 0
        prog(p3) = store
        exec(prog, ptr + 4)

      // equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
      case 8 =>
        val p1 = prog(ptr + 1)
        val p2 = prog(ptr + 2)
        val p3 = prog(ptr + 3)
        val v1 = if (op.modeP1 == 0) prog(p1) else p1
        val v2 = if (op.modeP2 == 0) prog(p2) else p2
        val store = if (v1 == v2) 1 else 0
        prog(p3) = store
        exec(prog, ptr + 4)

      case 99 =>
        println(op)

      case x =>
        println(s"$x?!")
    }
  }

  exec(xs.toArray)
}