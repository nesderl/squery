object Main {
  def main(args: Array[String]) {
    val node : Computation[Int] = new Computation(
      new Field("a"),
      new Computation(gt, new Value(5))
    )

    val n = 3

    println(node)
    print(s"node eval $n = ")
    println(node eval n)
  }

  def _gt (left : Int, right : Expression[Int]) : Boolean = {
    right match {
      case value : Value[Int] => left > value.get
      case _ => false
    }
  }

  def gt () : Operator[Int] = new Operator[Int]("$gt", _gt)
}