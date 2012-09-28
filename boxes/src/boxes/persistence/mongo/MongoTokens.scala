package boxes.persistence.mongo

import scala.collection._
import boxes.persistence._
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.commons.MongoDBList
import com.mongodb.BasicDBObject
import com.mongodb.casbah.commons.MongoDBObject$
import com.mongodb.casbah.Imports._

//Handles conversion of BasicDBObject, BasicDBList and String, Int, Double, Boolean, Float and Long
//to/from Tokens. Essentially allows for persistence of anything with a Token code in MongoDB
object MongoTokens {
  
  private def writeThing(value: Any, tokens: TokenWriter, aliases: ClassAliases) {
    value match {
      case obj: BasicDBObject => writeObjOrSecondClassPrim(obj, tokens, aliases)
      case arr: BasicDBList => writeArr(arr, tokens, aliases)
      case v: String => tokens.write(StringToken(v))
      case v: Int => tokens.write(IntToken(v))
      case v: Double => tokens.write(DoubleToken(v))
      case v: Boolean => tokens.write(BooleanToken(v))
      case other:Any => throw new RuntimeException("Unexpected value in MongoDBObject '" + other + "' class '" + other.getClass + "'")
    }
  }
  
  private def writeArr(arr: MongoDBList, tokens: TokenWriter, aliases: ClassAliases) {
    tokens.write(OpenArr)
    arr.foreach(t => writeThing(t, tokens, aliases))
    tokens.write(CloseArr)
  }
  
  private val skippedFieldNames = Set("_id", "_id_", "_ref_", "_type_")
  
  private def writeObjOrSecondClassPrim(obj: MongoDBObject, tokens: TokenWriter, aliases: ClassAliases) {
    
    //See if we have a second class prim
    obj.getAs[String]("_val_type_") match {
      case Some(s) => {
        s match {
          case "Long" => obj.getAs[Long]("_val_") match {
            case Some(l) => tokens.write(LongToken(l))
            case None => throw new RuntimeException("Missing val for Long prim")
          }
          case "Float" => obj.getAs[Float]("_val_") match {
            case Some(l) => tokens.write(FloatToken(l))
            case None => throw new RuntimeException("Missing val for Float prim")
          }
        }
      }
      
      //Assume we have a real obj
      case None => {
        val classNameOption = obj.getAs[String]("_type_")
        val id = obj.getAs[Int]("_id_")
        val ref = obj.getAs[Int]("_ref_")
        classNameOption match {
          case Some(className) => {
            val link = id.map(LinkId(_)).getOrElse(ref.map(LinkRef(_)).getOrElse(LinkEmpty))
            val clazz = aliases.forAlias(className)
            tokens.write(OpenObj(clazz, link))
            obj.foreach{case (fieldName, value) => {
              if (!skippedFieldNames.contains(fieldName)) {
                tokens.write(OpenField(fieldName))
                writeThing(value, tokens, aliases)
              }
            }}
            tokens.write(CloseObj)
          }
          case None => throw new RuntimeException("No _type_ for an obj")
        }
        
      }
    }
    
  }
  
  //TODO - make this stream, so the tokens are made on demand. sending this to
  //mongo in a stream would be the final stage to give minimum overhead for serialisation
  def toTokenReader(obj: Any, aliases: ClassAliases) = {
    val tokens = new StoringTokenWriter
    writeThing(obj, tokens, aliases)
    println(tokens.tokens)
    new StoringTokenReader(tokens.tokens:_*)
  }
  
}

class StoringTokenWriter extends TokenWriter {
  private val tokensM = mutable.ListBuffer[Token]()
  def write(t: Token) {
    tokensM.append(t)
  }
  def tokens = List(tokensM:_*)
}

class StoringTokenReader(tokens: Token*) extends TokenReader {
  private val tokenStack = mutable.Stack(tokens:_*)
  def peek: Token = tokenStack.head
  def pull(): Token = {
    val t = tokenStack.pop()
    print(t + ", ")
    t
  }
}

