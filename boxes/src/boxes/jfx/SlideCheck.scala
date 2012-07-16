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
import javafx.scene.layout.HBox
import javafx.scene.Group
import javafx.scene.shape.Path
import javafx.scene.shape.Rectangle
import javafx.animation.PathTransition
import javafx.animation.PathTransition.OrientationType
import javafx.scene.shape.MoveTo
import javafx.util.Duration
import javafx.animation.Timeline
import javafx.scene.input.MouseEvent
import javafx.scene.shape.CubicCurveTo
import javafx.scene.paint.Color
import javafx.animation.Animation
import javafx.scene.shape.LineTo
import javafx.beans.value.ObservableDoubleValue
import javafx.scene.shape.Circle


object SlideCheck {
  def apply(v:VarBox[Boolean,_], toggle:Boolean = true) = new SlideCheck(v, new TConverter[Boolean], toggle).asInstanceOf[JFXView]
  def option(v:VarBox[Option[Boolean],_], toggle:Boolean = true) = new SlideCheck(v,new OptionTConverter[Boolean], toggle).asInstanceOf[JFXView]
  
  val width = 72
  val slideTime = Duration.seconds(0.05)
}

class SlideCheck[G](v:VarBox[G,_], c:GConverter[G, Boolean], toggle:Boolean) extends JFXView {

  val btn = new ToggleButton
  btn.setId("SlideCheck")
  btn.setMaxHeight(10000)
  btn.setMaxWidth(10000)
  
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

  val pt = new PathTransition()

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
        pt.setRate(if (b) 1 else -1)
        pt.play
      }
    }
  } 
  
  val node = new StackPane() {
    val view = this
    
    setMinWidth(SlideCheck.width)
    setMaxWidth(SlideCheck.width)
    setPrefWidth(SlideCheck.width)

    val firstW = widthProperty().divide(2)
    val secondW = widthProperty().subtract(firstW)
    val coverW = widthProperty().divide(2).add(4)
    val h = heightProperty

    // inc/dec buttons
    val buttons = new HBox()
//    buttons.setHgap(0)
//    buttons.setVgap(0)
    buttons.setMouseTransparent(true)
  
    val l = new StackPane()
    l.setId("SlideOn")
    l.prefWidthProperty.bind(firstW)
    l.setMouseTransparent(true)
    val r = new StackPane()
    r.setId("SlideOff")
    r.prefWidthProperty.bind(secondW)
    r.setMouseTransparent(true)
    
//    buttons.add(l, 0, 0)
//    buttons.add(r, 1, 0)
    buttons.getChildren.addAll(l, r)

        val cover = new StackPane
    cover.setId("SlideCover")
    cover.setMouseTransparent(true)

    cover.prefWidthProperty.bind(coverW);    
    cover.minWidthProperty.bind(coverW);    
    cover.maxWidthProperty.bind(coverW);    
    cover.prefHeightProperty.bind(h);
    cover.minHeightProperty.bind(h);
    cover.maxHeightProperty.bind(h);
    
    val path = new Path()
    val s = new MoveTo(4,0)
    s.yProperty.bind(h.multiply(0.5))
    path.getElements().add(s)
    val e = new LineTo(0,0)
    e.yProperty.bind(h.multiply(0.5))
    e.xProperty.bind(firstW)
    path.getElements().add(e)
    
    pt.setDuration(SlideCheck.slideTime)
    pt.setPath(path)
    pt.setNode(cover)

    //TODO clipping - note need to animate the clip shape as the cover moves
//    val clip = new Circle(10,10,10);
//    clip.setMouseTransparent(true)
//    cover.setClip(clip)
    
    //    pt.setOrientation(OrientationType.ORTHOGONAL_TO_TANGENT)
//    pt.setCycleCount(Animation.INDEFINITE)
//    pt.setAutoReverse(true)
//    pt.play()
  
    getChildren.addAll(btn, buttons, cover)

  }

}

