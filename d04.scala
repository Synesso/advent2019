  def adjSame(s: String) = ("_" + s + "_").sliding(4).exists(t => t(0) != t(1) && t(1) == t(2) && t(2) != t(3))
  def increasing(s: String) = s.foldLeft(true -> 0) { case ((b, last), next) =>
    val nInt = next.toInt
    (b && nInt >= last, nInt)
  }._1
  println(
    (152085 to 670283).map(_.toString)
    .filter(adjSame).count(increasing))

