package boxes.im

import scalaz._
import Scalaz._
import scala.runtime.ScalaRunTime
import scalaz.CoStateT._
import scalaz.Lens._
//import scalaz.State._

case class Address(number: Int, street: String)
case class Person(name: String, age: Int, address: Address) {
  //Simple "non-settable" calculation - just lazy val, or def
  lazy val decades = (age/10)
  override def toString = ScalaRunTime._toString(this) + "(" + decades + " decades)"
}

object Person {
//  val name = "sdfsdfsdf"
  val name: Lens[Person, String] = Lens((r: Person) => coState((v: String) => r.copy(name=v), r.name))
  val age: Lens[Person, Int] = Lens((r: Person) => coState((v: Int) => r.copy(age=v), r.age))
  val address: Lens[Person, Address] = Lens((r: Person) => coState((v: Address) => r.copy(address=v), r.address))
  
  //Bidirectional "settable" calculations are a custom lens with the mapping embedded
  val agePlusOne: Lens[Person, Int] = Lens((r: Person) => coState((agePlusOne: Int) => r.copy(age=agePlusOne-1), r.age + 1))
}

object Address {
  val number: Lens[Address, Int] = Lens((r: Address) => coState((v: Int) => r.copy(number=v), r.number))
  val street: Lens[Address, String] = Lens((r: Address) => coState((street: String) => r.copy(street=street), r.street))
}

trait View[M, V] {
  //Produce a new View of a new model, allowing re-use of existing
  //views, values etc.
  def of(newM: M): View[M, V]
  def v: V
}

case class StringStringView(m: String) extends View[String, String]{
  {println("Made a new " + toString)}
  def of(newM: String) = if (m == newM) this else StringStringView(newM)
  lazy val v = m
}

case class IntStringView(m: Int) extends View[Int, String]{
  {println("Made a new " + toString)}
  def of(newM: Int) = if (m == newM) this else IntStringView(newM)
  lazy val v = m.toString
}

object PersonStringView {
  def apply(m: Person) = new PersonStringView(m, new StringStringView(m.name), new IntStringView(m.age), AddressStringView(m.address))
}

case class PersonStringView(val m: Person, val nameV: View[String, String], val ageV: View[Int, String], val addressV: View[Address, String]) extends View[Person, String]{
  {println("Made a new " + toString)}
  def of(newM: Person) = new PersonStringView(newM, nameV.of(newM.name), ageV.of(newM.age), addressV.of(newM.address))
  
  //Models for example compositing of images from child views to larger image, or compositing images plus map from focus areas to input handlers, etc.
  lazy val v = "name: " + nameV.v + ", age: " + ageV.v + ", address: " + addressV.v
}

object AddressStringView {
  def apply(m: Address) = new AddressStringView(m, new IntStringView(m.number), new StringStringView(m.street))
}

case class AddressStringView(val m: Address, val numberV: View[Int, String], val streetV: View[String, String]) extends View[Address, String] {
  {println("Made a new " + toString)}
  def of(newM: Address) = if (m == newM) this else new AddressStringView(newM, numberV.of(newM.number), streetV.of(newM.street))
  lazy val v = "number: " + numberV.v + ", street: " + streetV.v
}

//class MV[M, V](val model: M, viewer: M=>V) {
//  lazy val view = viewer(model)
//}

object ModelView {
    def modelLens[M, V] = Lens((r: ModelView[M, V]) => coState((model: M) => r.copy(model), r.model))
}

class ModelView[M, V](val model: M, val view: View[M, V]) {
  def copy(newM: M) = if (model == newM) this else new ModelView(newM, view.of(newM))
}


object LensProto {

  def main(args:Array[String]) {
    val p = Person("bob", 32, Address(4, "Main Street"))
//    System.out.println(Person.nameLens.get(p))
//    System.out.println(Person.nameLens.set(p, "bobobobobo"))
//    
//    //Paths are just composed Lenses, and are automatically bidirectional since they
//    //are a "view", but delegate back to the correct data when "writing"
//    val personsStreetLens = Address.streetLens.compose(Person.addressLens)
//    System.out.println(personsStreetLens.get(p))
//    System.out.println(personsStreetLens.set(p, "Cherry Street"))
//    
//    System.out.println(Person.agePlusOneLens.get(p))
//    System.out.println(Person.agePlusOneLens.set(p, 11))
//
//    val pV = PersonStringView(p)
//    println(pV.v)
//    
//    val pV2 = pV.of(Person.nameLens.set(p, "bobobobobobobobo"))
//    println(pV2.v)
//    
//    val mv = new ModelView(p, PersonStringView(p))
//    println("Initial person state")
//    println(mv.view.v)
//    println("Finished viewing initial person state")
//    
//    val modelPersonsStreetLens = Address.streetLens.compose(Person.addressLens.compose(ModelView.modelLens[Person, String]))
//    println("About to set street")
//    val mv2 = modelPersonsStreetLens.set(mv, "New Street in Model View!") 
//    println("Set street")
//    println(mv2.view.v)
//    println("Finished viewing new street state")

    val v = for {
      n <- Person.name
      _ <- Person.name := n + " hjhjhjhjh"
      a <- Person.age
      aPlus <- Person.age := a + 42
    } yield aPlus
    
    println(v(p))
    
    val op = Person.name.flatMap((n: String) => Person.name.%=((currentName: String) => currentName + " suffix"))
    println(op(p))
    
  }

}



//case class IntState[A](apply: Int => (Int, A)) {
//  
//  def map[B](f: A => B): IntState[B] = IntState[B] {
//    (state:Int) => {
//      val newState = apply(state)
//      val fOnNewState = f(newState._2)
//      (newState._1, fOnNewState)
//    }
//  }
//
//  def flatMap[B](f: A => IntState[B]): IntState[B] = IntState[B] {
//    (state:Int) => {
//      val newState = apply(state)
//      val fOnNewState = f(newState._2)
//      fOnNewState.apply(newState._1)
//    }
//  }
//  
//}
