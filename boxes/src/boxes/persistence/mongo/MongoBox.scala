package boxes.persistence.mongo

import com.mongodb.casbah.Imports._
import boxes.persistence.json.JSONIO
import scala.collection.mutable.WeakHashMap
import boxes.demo.Person
import com.mongodb.DBObject
import com.mongodb.util.JSON
import boxes._
import boxes.persistence.ClassAliases
import boxes.persistence.CodecByClass
import boxes.util.WeakKeysBIDIMap

object MongoBox {
  def main(args: Array[String]) {
    val aliases = new ClassAliases
    aliases.alias(classOf[Person], "Person")
    val mb = new MongoBox("boxes", aliases)

//    val p = Person.testPerson
//    println("Kept with id " + mb.keep(p))
//    
//    println(p)
//    p.name() = p.name() + "b"
//    println(p)

//    val idA = new ObjectId("50688bdc3004c7e2cc92a11b")
//    val idB = new ObjectId("5068649b3004c70bee79b45b")
    
    val p = mb.findById[Person](new ObjectId("50688bdc3004c7e2cc92a11b"))
    println(p)
    p.foreach(_.name() = "Renamed again After findById")
    println(p)

    val p2 = mb.findById[Person](new ObjectId("50688bdc3004c7e2cc92a11b"))
    println(p2)

    for (a <- p; b <- p2) {println("p identical to p2?" + (a eq b))}
    
    val p3 = mb.findOne[Person](MongoDBObject("name" -> "Renamed again After findById"))
    println(p3)
    p3.foreach(person => println(mb.id(person)))
    
    val namedB = mb.find[Person](MongoDBObject("name" -> "nameb"))
    namedB.foreach(person => println(person.name()))
    
//    val containsMed = mb.find[Person]("name" $exists true)
//    namedB.foreach(person => println(person.name()))

  }
}

class MongoBox(dbName: String, aliases: ClassAliases) {

  private val mongoConn = MongoConnection()
  private val db = mongoConn(dbName)
  private val io = JSONIO(aliases)
  
  //TODO might be better as soft references, to reduce unneeded db access?
  private val m = new WeakKeysBIDIMap[Node, ObjectId]()
  
  def id(t: Node) = Box.transact{m.toValue(t)}

  private def toNode[T <: Node](alias: String, dbo: MongoDBObject) = {
    for {
      id <- dbo._id
    } yield {
      m.toKey(id).map(_.asInstanceOf[T]).getOrElse {
        val fromMongo = io.readDBO(dbo).asInstanceOf[T]
        track(alias, fromMongo, id)
        fromMongo
      }
    }    
  }

  def findById[T <: Node](id: ObjectId)(implicit man: Manifest[T]): Option[T] = findOne(MongoDBObject("_id" -> id))(man) 
    
  def findOne[T <: Node](query: MongoDBObject)(implicit man: Manifest[T]): Option[T] = {
    Box.transact {
      val alias = aliases.forClass(man.erasure)
      for {
        dbo <- db(alias).findOne(query)
        n <- toNode[T](alias, dbo)
      } yield n
    }
  }
  
  def find[T <: Node](query: MongoDBObject)(implicit man: Manifest[T]): Iterator[T] = {
    Box.transact {
      val alias = aliases.forClass(man.erasure)
      val cursor = db(alias).find(query)
      cursor.flatMap(dbo => toNode[T](alias, dbo))
    }
  }
  
  private def track(alias:String, t: Node, id: ObjectId) {
      //Set up a View that writes any changes to mongo
      val query = MongoDBObject("_id" -> id)
      t.retainReaction(View {
        val dbo = io.writeDBO(t).asInstanceOf[MongoDBObject]
        db(alias).update(query, dbo)
      })
      m.put(t, id)
  }
  
  //Register a Node to be kept in mongodb. Returns the ObjectId used. If the
  //Node was already kept, nothing is done, but the ObjectId is still returned.
  def keep(t: Node) = {
    Box.transact{
      //Get the existing id for the node, or else add the
      //node to mongo and return the new id
      m.toValue(t).getOrElse{        
        val alias = aliases.forClass(t.getClass())
        
        //Make a DB object with new id, and insert it to mongo
        val id = new ObjectId()
        val dbo = io.writeDBO(t).asInstanceOf[MongoDBObject]
        dbo.put("_id", id)
        db(alias).insert(dbo)

        //Set up View and add to map
        track(alias, t, id)
        id        
      }
    }
  }
  
  def forget(t: Node) {
    Box.transact{
      //Get the existing id for the node
      m.toValue(t).foreach(id => {
        val alias = aliases.forClass(t.getClass())
        //Remove from mongo and our map
        db(alias).remove(MongoDBObject("_id" -> id))
        m.removeKey(t)
      })
    }
  }
  
}