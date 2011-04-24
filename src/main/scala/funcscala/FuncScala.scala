package funcscala

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

  def interval(start: Int, end: Int, step: Int): Lst[Int] = (start, end) match {
    case (s, e) if s > e => Nl
    case (s, e)          => s +: interval(s + step, e, step)
  }

  def insertAt[A](index: Int, a: A, lst: Lst[A]): Lst[A] = lst match {
    case _ if (index == 0) => a +: lst;
    case x +: xs           => x +: insertAt(index - 1, a, xs)
    case Nl                => Nl
  }

  def length[A](lst: Lst[A]): Int = lst match {
    case Nl      => 0
    case x +: xs => 1 + length(xs)
  }
  //def append[A](a: A, lst: Lst[A]): Lst[A] = insertAt(length(lst), a, lst)
}

object FuncScala extends Application {
  println(Nl.length(1 +: 2 +: Nl))
  println("Start time: " + new java.util.Date)
  //  println("index out of bounds: " + Nl.insertAt(6, 0, 1 +: 2 +: 3 +: Nl))
  //  println(Nl.insertAt(1, 0, 1 +: 2 +: 3 +: Nl))
  //  println(1 +: 2 +: Nl)
  //  println(Nl.repeat("2", 8))
  //  println(Nl.interval(start = 3, end = 27, step = 3))
}

case object Nl extends Lst[Nothing]
case class +:[A](x: A, tail: Lst[A]) extends Lst[A]