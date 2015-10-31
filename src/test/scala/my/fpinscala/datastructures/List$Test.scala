package my.fpinscala.datastructures

/**
*  Created by Jared Nagle on 31-10-15.
*/
class List$Test extends org.scalatest.FunSuite {
  test("isEmpty performs correctly") {
    assert (Nil.isEmpty)
    assert (List().isEmpty)
  }
  test("tail returns correctly") {
    val tail1 = List(1, 2).tail
    assert(tail1 == List(2))
    val tail2 = tail1.tail
    assert(tail2 == Nil)
  }
  test("tail throws error on empty List") {
    intercept[Exception] { Nil.tail; fail() }
  }
  test("list head on empty throws error") {
    intercept[Exception] { Nil.head; fail() }
  }
}
