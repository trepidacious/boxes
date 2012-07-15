package boxes.jfx

import javafx.event.ActionEvent
import javafx.geometry.Pos
import javafx.scene.control.ButtonBase
import boxes.VarBox
import boxes.util.GConverter
import javafx.scene.layout.StackPane
import javafx.scene.control.ToggleButton
import boxes.jfx.JFXImplicits._
import boxes.View
import boxes.util.TConverter
import boxes.util.OptionTConverter
import boxes.BooleanControlType
import boxes.Box
import boxes.Val
import boxes.BooleanControlType
import javafx.scene.layout.GridPane


object SlideCheck {
  def apply(v:VarBox[Boolean,_], toggle:Boolean = true) = new SlideCheck(v, new TConverter[Boolean], toggle).asInstanceOf[JFXView]
  def option(v:VarBox[Option[Boolean],_], toggle:Boolean = true) = new SlideCheck(v,new OptionTConverter[Boolean], toggle).asInstanceOf[JFXView]
}

class SlideCheck[G](v:VarBox[G,_], c:GConverter[G, Boolean], toggle:Boolean) extends JFXView {

  val btn = new ToggleButton
  btn.setId("SlideCheck")
  btn.setMinWidth(72)
  
  btn.selectedProperty.addListener((selected: java.lang.Boolean) => {
    if (toggle) {
      v() = c.toG(selected)
    } else {
      if (selected) {
        v() = c.toG(selected)
      } else {
        val newV = v()
        //This will be called from Swing Thread
        replaceUpdate { display(newV) } 
      }
    }
  })
  
  val view = View{
    val newV = v()
    replaceUpdate { display(newV) }
  }

  //Update display if necessary
  private def display(newV:G) {
    c.toOption(newV) match {  
      case None => {
        btn.setDisable(true)
        btn.setSelected(false)
      }
      case Some(b) => {
        btn.setDisable(false)
        btn.setSelected(b)
      }
    }
  } 
  
  val node = new StackPane() {
    val view = this
    
    val firstW = widthProperty().divide(2)
    val secondW = widthProperty().subtract(firstW)
    val h = heightProperty

    // inc/dec buttons
    val buttons = new GridPane()
    buttons.setHgap(0)
    buttons.setVgap(0)
    buttons.setMouseTransparent(true)
  
    val l = new StackPane()
    l.setId("SlideOn")
    l.prefWidthProperty.bind(firstW)
    l.prefHeightProperty().bind(h)
    l.setMouseTransparent(true)
    val r = new StackPane()
    r.setId("SlideOff")
    r.prefWidthProperty.bind(secondW)
    r.prefHeightProperty().bind(h)
    r.setMouseTransparent(true)
    
    buttons.add(l, 0, 0)
    buttons.add(r, 1, 0)

    val cover = new StackPane
    cover.setId("SlideCover")
    cover.prefWidthProperty.set(36)
    cover.minWidthProperty.set(36)
    cover.maxWidthProperty.set(36)
    cover.prefHeightProperty().bind(h)
    cover.setMouseTransparent(true)
    
    getChildren.addAll(btn, buttons, cover)

  }

}

