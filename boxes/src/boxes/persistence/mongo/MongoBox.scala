package boxes.persistence.mongo

import com.mongodb.casbah._
import com.mongodb.casbah.commons._
import boxes.persistence.json.JSONIO
import scala.collection.mutable.WeakHashMap
import org.bson.types.ObjectId
import boxes.demo.Person
import com.mongodb.DBObject
import com.mongodb.util.JSON
import boxes.View
import boxes.persistence.ClassAliases
import boxes.persistence.CodecByClass

object MongoBox {
  lazy val default = new MongoBox("boxes", "default")
  
  def main(args: Array[String]) {
    
    val p = default.person

    
    
    println(p)
    p.name() = p.name() + "b"
    println(p)
  }
}

class MongoBox(db: String, collection: String) {

  private val mongoConn = MongoConnection()
  private val boxesDB = mongoConn(db)
  private val boxes = boxesDB(collection)
  private val aliases = new ClassAliases
  private val io = JSONIO()
  
  //Make sure there is at least one Person
  if (boxes.isEmpty) {
    val p = Person.testPerson
    val json = io.write(p)
    val obj = JSON.parse(json).asInstanceOf[DBObject]
    boxes.insert(obj)
  }
  
  //Use first Person for demo
  private val dbo = boxes.head
  val personIdQuery = MongoDBObject("_id" -> dbo.get("_id"))

  val person = {
    io.readDBO(dbo).asInstanceOf[Person]
  }
  
  //As long as Person is reachable, the view will be retained and commit changes
  //to mongo
  person.retainReaction(View {
    //TODO: Note that for efficiency it would be nice to support conversion of
    //tokens directly to a DBObject, avoiding the print/parse cycle
    val json = io.write(person)
    val obj = JSON.parse(json).asInstanceOf[DBObject]
    boxes.update(personIdQuery, obj)
  })
  
//  private val map = WeakHashMap[ObjectId, AnyRef]()
  
}