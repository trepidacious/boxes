/*
 * Created by IntelliJ IDEA.
 * User: trepidacious
 * Date: 22/02/2011
 * Time: 20:37
 */
package boxes.demo

import boxes._

object BoxesDemo {

  class Person {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Person] = Var(null)

    //override def toString = name() + ", " + age() + ", friend: " + friend()
  }

  def simpleCalc() = {

    println()
    println("simpleCalc")

    val a = Var(2)
    val b = Cal(a() + 3)
    val c = Cal(a() + b())

    println("b = " + b() + ", c = " + c())

    a() = 4

    println("b = " + b() + ", c = " + c())

  }

  def simplePath() = {

    println()
    println("simplePath")

    val cate = new Person()
    cate.name() = "Cate"
    val alice = new Person()
    alice.name() = "Alice"
    val bob = new Person()
    bob.name() = "Bob"
    bob.friend() = cate

    val bobsFriendsName = Cal(bob.friend().name())

    //FIXME implement test - should see no changes to bobsFriendsName when something not
    //in the path changes, even if it is deeply referenced, or used to be part of path, etc.
    //Reaction(bobsFriendsName, {bob.friend().name()})

    println("Before change: " + bobsFriendsName())

    bob.friend() = alice

    println("After change: " + bobsFriendsName())

    println("About to change cate's name")
    cate.name() = "Katey"

    println("After changing cate's name")

    println("About to change alice's name")
    alice.name() = "Alicia"

    println("After changing alice's name: " + bobsFriendsName())
  }

  def separateBIDIReactions = {

    println()
    println("separateBIDIReactions")

    val x = Var(2d)
    val doubleX = Var(0d)

    println("x = " + x() + ", doubleX = " + doubleX())

    println("About to create reaction doubleX = 2 * x")
    Reaction(doubleX, x()*2d, "doubleX = 2 * x")
    println("Created reaction doubleX = 2 * x")
    println("x = " + x() + ", doubleX = " + doubleX())
    println("About to create reaction x = doubleX / 2")
    Reaction(x, doubleX()/2d, "x = doubleX / 2")
    println("Created both reactions")

    println("x = " + x() + ", doubleX = " + doubleX())

    println("About to set x() = 4")
    x() = 4
    println("Just set x() = 4")

    println("x = " + x() + ", doubleX = " + doubleX())

    println("About to set doubleX() = 10")
    doubleX() = 10
    println("Just set doubleX() = 10")

    println("x = " + x() + ", doubleX = " + doubleX())
  }

  def conflictingReactions() = {

    println()
    println("conflictingReactions")

    val x = Var(2d)
    val y = Var(0d)

    Reaction(y, x() * 2, "double")

    println("Applied first constraint")

    Reaction(y, x() * 4, "quadruple")

    println("Applied second constraint - should not get here...")
  }

  def nonConflictingReactions() = {

    println()
    println("nonConflictingReactions")

    val x = Var(2d)
    val y = Var(0d)

    Reaction(y, x() * 2, "double")

    println("Applied first constraint")

    Reaction(y, x() * 2, "also double")

    println("Applied second constraint - should get here fine...")
  }

  def bidiPath() = {

    println()
    println("bidiPath")

    val cate = new Person()
    cate.name() = "Cate"
    val alice = new Person()
    alice.name() = "Alice"
    val bob = new Person()
    bob.name() = "Bob"
    bob.friend() = cate

    val bobsFriendsName = Path(bob.friend().name)

    //FIXME implement test - should see no changes to bobsFriendsName when something not
    //in the path changes, even if it is deeply referenced, or used to be part of path, etc.
    //Reaction(bobsFriendsName, {bob.friend().name()})

    println("Before change: " + bobsFriendsName())

    bob.friend() = alice

    println("After change: " + bobsFriendsName())

    println("About to change cate's name")
    cate.name() = "Katey"

    println("After changing cate's name")

    println("About to change alice's name")
    alice.name() = "Alicia"

    println("After changing alice's name: " + bobsFriendsName())

    println("About to change bobsFriendsName")
    bobsFriendsName() = "Alucard"

    println("After changing bobsFriendsName: " + bobsFriendsName() + ", and alice's name is " + alice.name())


  }

  def main(args: Array[String]) {
    simpleCalc
    simplePath
    separateBIDIReactions
    nonConflictingReactions

    bidiPath

//    conflictingReactions
  }

}