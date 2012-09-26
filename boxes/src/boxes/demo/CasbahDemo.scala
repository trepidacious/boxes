package boxes.demo

import boxes._
import general.{RadioReaction, SetOp}
import graph._
import list._
import java.awt.{Dimension, BorderLayout, GridLayout, Color}
import boxes.VarLens.apply
import swing.{BoxesPopupView, EmbossedLabel, TabBuilder, SheetBuilder, GraphSwingBGView, GraphSwingView, SwingButtonBar, SwingOp, SwingBarButton}
import boxes.BoxImplicits._
import javax.swing._
import com.explodingpixels.macwidgets.{SourceList, SourceListItem, SourceListCategory, SourceListModel}
import com.mongodb.casbah.Imports._
import com.mongodb.DBObject
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.util.JSON
import boxes.persistence.json.JSONIO

object CasbahDemo {

  def main(args: Array[String]) {
    val mongoConn = MongoConnection()
    
    val nanos = mongoConn("nanos")
    
//    val boxes = nanos.createCollection("boxes", MongoDBObject())
    
    val boxes = nanos("boxes")
    
    val io = JSONIO()
    
    val p = Person.testPerson
    
    val json = io.write(p)
    
    val obj = JSON.parse(json).asInstanceOf[DBObject]
    
//    boxes.insert(obj)

    boxes.foreach(o => {
      println("Object id: " + o.get("_id"))
      o.removeField("_id")
      println(io.read(o.toString).asInstanceOf[Person])
    })
  }

}