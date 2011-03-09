package boxes.test

import org.specs.Specification

//import org.scalatest.WordSpec
//import scala.collection.mutable.Stack

class ExampleSpecs extends Specification {
  "'hello world' has 11 characters" in {
     "hello world".size must_== 11
  }
  "'hello world' matches 'h.* w.*'" in {
     "hello world" must be matching("h.* w.*")
  }
}

//class ExampleSpec extends WordSpec {
//
//  "A Stack" should {
//
//    "pop values in last-in-first-out order" in {
//      val stack = new Stack[Int]
//      stack.push(1)
//      stack.push(2)
//      assert(stack.pop() === 2)
//      assert(stack.pop() === 1)
//    }
//
//    "throw NoSuchElementException if an empty stack is popped" in {
//      val emptyStack = new Stack[String]
//      intercept[NoSuchElementException] {
//        emptyStack.pop()
//      }
//    }
//  }
//}