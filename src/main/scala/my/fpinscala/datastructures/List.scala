package my.fpinscala.datastructures

/**
*  Created by Jared Nagle on 31-10-15.
*/
import scala.collection.immutable.{List => _}
import scala.language.implicitConversions

trait List[+A]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  def isEmpty[A](list: List[A]): Boolean = list == Nil

  def tail[A](list: List[A]): List[A] = drop(list, 1)

  def head[A](list: List[A]): A = list match {
    case Cons(x, _) => x
    case Nil => sys.error("head on empty list")
  }

  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Cons(x, xs) => Cons(head, xs)
    case Nil => sys.error("set head on empty list")
  }

  def drop[A](list: List[A], n: Int): List[A] = if (n > 0) list match {
    case Cons(x, xs) => drop(xs, n-1)
    case Nil => sys.error("drop on empty list")
  } else list

  def apply[A](a: A*): List[A] = {
    if(a.isEmpty) Nil
    else Cons(a.head, apply(a.tail: _*))
  }

  implicit def toListOps[A](l: List[A]): ListOps[A] = new ListOps(l)

  class ListOps[A](that: List[A]) {
    def tail: List[A] = List.tail(that)
    def head: A = List.head(that)
    def isEmpty: Boolean = List.isEmpty(that)
    def setHead(head: A): List[A] = List.setHead(that, head)
  }
}
