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

object CasbahDemo {

  def main(args: Array[String]) {
    val mongoConn = MongoConnection()
    
    val nanos = mongoConn("nanos")
    val plates = nanos("plates")
    plates.foreach(println(_))
    
    val boxes = nanos.createCollection("boxes", MongoDBObject())
//    boxes.
  }

}