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
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.MongoException

object MongoBox {
  def main(args: Array[String]) {
    val aliases = {
      val a = new ClassAliases
      a.alias(classOf[TestNode], "TestNode")
      a
    }
    val mb = new MongoBox("boxestest", aliases)
    
    val bob = new TestNode
    bob.index() = 1
    val bobDup = new TestNode
    bobDup.index() = 2
    
    val bill = new TestNode
    bill.name() = "bill"
    bill.index() = 42

    mb.keep(bobDup)
    mb.keep(bob)
    mb.keep(bill)
  }
  
  def tryOption[T](f: =>T): Option[T] = {
    try {
      Some(f)
    } catch {
      case c => None
    }
  }
}

import MongoBox._

object TestNode extends MongoMetaNode {
    override val indices = List(MongoNodeIndex("name"))
}

class TestNode extends MongoNode {
  def meta = TestNode
  val name = Var("bob")
  val index = Var(0)
}

case class MongoNodeIndex(key: String, unique: Boolean = true, ascending: Boolean = true)

trait MongoMetaNode {
  def indices: List[MongoNodeIndex] = List()  
}

trait MongoNode extends Node {
  def meta: MongoMetaNode
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

  def findById[T <: Node](id: String)(implicit man: Manifest[T]): Option[T] = 
    tryOption(new ObjectId(id)).flatMap(oid => findById(oid)(man))
  
  def findById[T <: Node](id: ObjectId)(implicit man: Manifest[T]): Option[T] = findOne(MongoDBObject("_id" -> id))(man) 

  def findOne[T <: Node](key: String, value: Any)(implicit man: Manifest[T]): Option[T] = findOne(MongoDBObject(key -> value))(man)
    
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
  
  private def useMongoNode(alias: String, t: Node) {
    t match {
      case mn: MongoNode => mn.meta.indices.foreach(
          i => db(alias).ensureIndex(
            MongoDBObject(i.key -> (if (i.ascending) "1" else "-1")), 
            i.key, 
            i.unique))
      case _ => {}
    }
  }
  
  private def track(alias:String, t: Node, id: ObjectId) {
    //First make sure any indices are in place, so we respect them from the View we will create
    useMongoNode(alias, t)

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
        
        //First make sure any indices are in place, so we respect them when writing the
        //new record to DB
        useMongoNode(alias, t)
        
        //Make a DB object with new id, and insert it to mongo
        val id = new ObjectId()
        val dbo = io.writeDBO(t).asInstanceOf[MongoDBObject]
        dbo.put("_id", id)
        
        //TODO detect errors, e.g. index problems (duplicate ObjectId, or conflict with an index from MongoNode)
        db(alias).insert(dbo)
        
        val leOrNull = db(alias).lastError.getException()
        if (leOrNull != null) throw leOrNull 
          
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