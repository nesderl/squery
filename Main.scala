object Main {
  def main(args: Array[String]) {
    val predicate : Predicate[Int] = new Predicate(
      new Field("a"),
      new Predicate(gt, new Value(5))
    )

    val n = 3

    println(predicate)
    print(s"predicate eval $n = ")
    println(predicate eval n)
  }

  def _gt (left : Int, right : Expression[Int]) : Boolean = {
    right match {
      case value : Value[Int] => left > value.get
      case _ => false
    }
  }

  def gt () : Operator[Int] = new Operator[Int]("$gt", _gt)
}