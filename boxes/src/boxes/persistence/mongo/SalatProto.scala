package boxes.persistence.mongo

import boxes.Node
import boxes.Var
import boxes.persistence.json.JSONIO
import com.novus.salat._
import com.novus.salat.global._
import com.mongodb.casbah.Imports._
import scala.tools.scalap.scalax.rules.scalasig.MethodSymbol

class A extends Node {
  val a = Var("a")
}

class B extends A {
  val b = Var("b")
}

case class CB(a: String = "aVal", b: String = "bVal")

object SalatProto {

  def fieldNames[T](implicit m: Manifest[T]) = {
    val ca = ClassAnalyzer(m.erasure)
    // don't use allTheChildren here!  this is the indexed fields for clazz and clazz alone
    ca.sym.children
      .filter(c => c.isCaseAccessor && !c.isPrivate)
      .map(_.asInstanceOf[MethodSymbol])
      .map (_.name)
  }
  
  def fields[T <: Product](t: T)(implicit m: Manifest[T]) = {
    t.productIterator.zip(fieldNames[T].iterator).toList
  }
    
  def main(args: Array[String]) {
    val io = JSONIO()
    println(io.write(new B))
    
    val cb = CB()
    
    //List of (value, name) for all fields
    println(fields(cb).toList)
    
    val dbo = grater[CB].asDBObject(cb) 
    println(dbo)
    val cb_* = grater[CB].asObject(dbo)
    println(cb_*)
  }
}

