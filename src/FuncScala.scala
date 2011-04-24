object FuncScala {
  def main(args: Array[String]): Unit = {
    println("Start time: " + new java.util.Date)
    println(1 +: 2 +: Nl)
  }
}

sealed abstract class Lst[+A] {
  def +:[S >: A](x: S): Lst[S] = new +:(x, this)
  def empty(lst: Lst[_]) = lst == Nl
  def head[A](lst: Lst[A]): Option[A] = lst match {
    case a +: _ => Some(a)
    case _      => None
  }
  def tail[A](lst: Lst[A]): Lst[A] = lst match {
    case _ +: as => as
    case _       => lst
  }
  def init[A](lst: Lst[A]): Lst[A] = lst match {
    case Nl              => Nl
    case a +: last +: Nl => a +: Nl
    case a +: as         => a +: init(as)
  }
  def repeat[A](a: A, times: Int): Lst[A] = if (times == 0) Nl else a +: repeat(a, times - 1)
}

case object Nl extends Lst[Nothing]
case class +:[A](x: A, tail: Lst[A]) extends Lst[A]