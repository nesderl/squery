/** LEFT NODE */
trait Key[T] {
  def prepareWith (param : T) : Key[T]
  def applyTo (node : Expression[T]) : Boolean
}

class Field[T](name : String)  extends Key[T] {
  var myValue : Option[T] = None

  def prepareWith (value : T) : Key[T] = {
    myValue = Some(value)
    this
  }

  def applyTo (node : Expression[T]) : Boolean = myValue match {
    case Some(value) => node eval value
    case None => false
  }

  override def toString() = s"'$name'"
}

class Operator[T] (name : String, operation : ((T, Expression[T]) => Boolean))  extends Key[T] {
  var myOperator : Option[Expression[T] => Boolean] = None

  def prepareWith (param : T) : Key[T] = {
    myOperator = Some(operation(param, _ : Expression[T]))
    this
  }

  def applyTo (node : Expression[T]) : Boolean = {
    myOperator match {
      case Some(operator) => operator(node)
      case None => false
    }
  }

  override def toString() = name
}

/** RIGHT NODE */
trait Expression[T] {
  def eval (value : T) : Boolean
}

class Value [T] (value : T) extends Expression[T] {
  def eval (other : T) : Boolean = other == value
  def get () : T = value
  override def toString() = s"$value"
}

class Predicate[T] (left : Key[T], right : Expression[T]) extends Expression[T] {
  def eval (value : T) : Boolean = {
    left prepareWith value applyTo right
  }

  override def toString() = s"{$left : $right}"
}