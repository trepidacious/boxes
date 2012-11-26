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

  def genericGrater(t: AnyRef)(implicit ctx: Context) = grater[AnyRef](ctx, Manifest.classType(t.getClass()))
  
  def main(args: Array[String]) {
    val io = JSONIO()
    println(io.write(new B))
    
    val cb = CB("aaa", "bbb")
        
    val g = genericGrater(cb)
    
    val map = g.toMap(cb) 
    println(map)
    val map2 = map - "b"
    val cb_* = g.fromMap(map2).asInstanceOf[CB]
    println(cb_*)
  }
}

