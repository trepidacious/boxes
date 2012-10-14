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

//  def fieldNames[T](implicit m: Manifest[T]) = {
//    val ca = ClassAnalyzer(m.erasure)
//    // don't use allTheChildren here!  this is the indexed fields for clazz and clazz alone
//    ca.sym.children
//      .filter(c => c.isCaseAccessor && !c.isPrivate)
//      .map(_.asInstanceOf[MethodSymbol])
//      .map (_.name)
//  }
//  
//  def fields[T <: Product](t: T)(implicit m: Manifest[T]) = {
//    Map(fieldNames[T].iterator.zip(t.productIterator).toList:_*)
//  }
//    
//  def asObject[T](fields: Map[String, AnyRef])(implicit m: Manifest[T]): T = {
//    val ca = ClassAnalyzer[T](m.erasure)
//    if (ca.sym.isModule) {
//      ca.companionObject.asInstanceOf[T]
//    } 
//    else {
//      //TODO use default values
//      val names = fieldNames[T](m)
//      val args = names.flatMap(name => fields.get(name))
//      if (args.size < names.size) throw new RuntimeException("Too few fields in data")
//      ca.constructor.newInstance(args: _*).asInstanceOf[T]
//    }
//  }
  
//  type SalatObject = T forSome { type T <: AnyRef }

  def genericGrater(t: AnyRef)(implicit ctx: Context) = grater[AnyRef](ctx, Manifest.classType(t.getClass()))
  
  def main(args: Array[String]) {
    val io = JSONIO()
    println(io.write(new B))
    
    val cb = CB("aaa", "bbb")
    
//    //List of (value, name) for all fields
//    val f = fields(cb)
//    println(f)
    
//    type GraterType = T forSome {type T <: AnyRef}
    
//    val g = grater[SalatObject]
    
    val g = genericGrater(cb)
    
    val map = g.toMap(cb) 
    println(map)
    val map2 = map - "b"
    val cb_* = g.fromMap(map2).asInstanceOf[CB]
    println(cb_*)
  }
}

