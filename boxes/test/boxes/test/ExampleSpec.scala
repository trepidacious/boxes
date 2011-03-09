package boxes.test

import org.scalatest.WordSpec
import scala.collection.mutable.Stack


class ExampleSpec extends WordSpec {

  "A Stack" should {

    "pop values in last-in-first-out order" in {
      val stack = new Stack[Int]
      stack.push(1)
      stack.push(2)
      assert(stack.pop() === 2)
      assert(stack.pop() === 1)
    }

    "throw NoSuchElementException if an empty stack is popped" in {
      val emptyStack = new Stack[String]
      intercept[NoSuchElementException] {
        emptyStack.pop()
      }
    }
  }

  "A String" should {
    "have correct length" in {
      assert("bob".length === 3)
    }
  }
}