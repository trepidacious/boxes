package boxes.swing

import java.awt.event.ActionEvent
import boxes.{SwingView, View, Op}
import boxes.swing.{ListMultiDeleteOp, ListDeleteOp, ListMultiAddOp, ListAddOp}
import com.explodingpixels.painter.Painter
import javax.swing._
import border.EmptyBorder
import com.explodingpixels.swingx.{EPPanel, EPButton}
import java.awt.{Component, Graphics2D, Color}

object SwingOpAction {
  def apply(name:String, icon:Icon, op:Op) = new SwingOpAction(name, icon, op)
}

class SwingOpAction(name:String, icon:Icon, op:Op) extends AbstractAction(name, icon) {
  def actionPerformed(e:ActionEvent) {
    op()
  }
  View {
    val enabled = op.canApply()
    SwingView.replaceUpdate(
      this,
      setEnabled(enabled)
    )
  }
}

object SwingOp {

  val add = new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Plus.png"))
  val delete = new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Minus.png"))


  def apply(op:Op):SwingOpAction = {
    op match {
      case o:ListAddOp[_] => SwingOpAction("", add, op)
      case o:ListMultiAddOp[_] => SwingOpAction("", add, op)
      case o:ListDeleteOp[_] => SwingOpAction("", delete, op)
      case o:ListMultiDeleteOp[_] => SwingOpAction("", delete, op)
      //FIXME use implicits
      case _ => throw new IllegalArgumentException("Unknown op")
    }
  }

}

object SwingButton {
  def apply(op:Op):EPButton = {
    ListStyleButton(SwingOp(op))
  }

  def ListStyleButton(a:Action) = {
    val button = new EPButton(a)
    button.setBorder(new EmptyBorder(4,2,3,2))
    button.setContentAreaFilled(false)
    button.setBackgroundPainter(ListStylePainter[AbstractButton]())
    button
  }
  def buttonPadding() = {
    val panel = new EPPanel()
    panel.setBackgroundPainter(ListStylePainter[Component](false, false))
    panel
  }
}

object ListStylePainter {
  val dividerColor = new Color(0, 0, 0, 51)
  val topColor = new Color(0xaaaaaa)
  val image = new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/ListButton.png")).getImage

  def apply[T](paintLeft:Boolean = false, paintRight:Boolean = true) = new ListStylePainter[T](paintLeft, paintRight)
}

class ListStylePainter[T](paintLeft:Boolean = false, paintRight:Boolean = true) extends Painter[T] {
  override def paint(g:Graphics2D, t:T, w:Int, h:Int) {
    g.drawImage(ListStylePainter.image, 0, 0, w, h, null)

    g.setColor(ListStylePainter.dividerColor)
    if (paintLeft) {
      g.drawLine(0, 0, 0, h-1)
    }
    if (paintRight) {
      g.drawLine(w-1, 0, w-1, h-1)
    }

    g.setColor(ListStylePainter.topColor)
    g.drawLine(0, 0, w-1, 0)
  }
}


