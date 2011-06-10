package boxes.swing

import java.awt.event.ActionEvent
import boxes.{SwingView, View, Op}
import boxes.swing.{ListMultiDeleteOp, ListDeleteOp, ListMultiAddOp, ListAddOp, ListMoveOp, ListMultiMoveOp}
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
  val up = new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Up.png"))
  val down = new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Down.png"))


  def apply(op:Op):SwingOpAction = {
    op match {
      case o:ListAddOp[_] => SwingOpAction("", add, op)
      case o:ListMultiAddOp[_] => SwingOpAction("", add, op)
      case o:ListDeleteOp[_] => SwingOpAction("", delete, op)
      case o:ListMultiDeleteOp[_] => SwingOpAction("", delete, op)
      case o:ListMoveOp[_] => {
        if (o.up) {
          SwingOpAction("", up, op)
        } else {
          SwingOpAction("", down, op)
        }
      }
      case o:ListMultiMoveOp[_] => {
        if (o.up) {
          SwingOpAction("", up, op)
        } else {
          SwingOpAction("", down, op)
        }
      }
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
    button.setBackgroundPainter(new ListStyleButtonPainter())
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
  val pressedColor = new Color(0, 0, 0, 51)
  val dividerBright = new Color(1f, 1f, 1f, 0.4f)
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

    g.setColor(ListStylePainter.dividerBright)
    if (paintLeft) {
      g.drawLine(1, 0, 1, h-1)
    } else {
      g.drawLine(0, 0, 0, h-1)
    }
    if (paintRight) {
      g.drawLine(w-2, 0, w-2, h-1)
    } else {
      g.drawLine(w-1, 0, w-1, h-1)
    }

    g.setColor(ListStylePainter.topColor)
    g.drawLine(0, 0, w-1, 0)
  }
}

class ListStyleButtonPainter(paintLeft:Boolean = false, paintRight:Boolean = true) extends ListStylePainter[AbstractButton] {
  override def paint(g:Graphics2D, t:AbstractButton, w:Int, h:Int) {
    super.paint(g, t, w, h)
    if (t.getModel.isPressed) {
      println("pressed")
      g.setColor(ListStylePainter.pressedColor)
      g.fillRect(0, 0, w, h)
    }
  }
}


