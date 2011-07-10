package boxes.swing

import java.awt.event.ActionEvent
import boxes.{SwingView, View, Op}
import boxes.list.{ListMultiDeleteOp, ListDeleteOp, ListMultiAddOp, ListAddOp, ListMoveOp, ListMultiMoveOp}
import com.explodingpixels.painter.Painter
import javax.swing._
import border.EmptyBorder
import com.explodingpixels.swingx.{EPPanel, EPButton}
import java.awt.{BorderLayout, Component, Graphics2D, Color}

class SwingOpAction(name:String, icon:Icon, op:Op) extends AbstractAction(name, icon) {
  def actionPerformed(e:ActionEvent) {
    op()
  }
  View {
    val enabled = op.canApply()
    SwingView.replaceUpdate(
      this,
      {
        setEnabled(enabled)
      }
    )
  }
}

object SwingOp {

  val add = new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Plus.png"))
  val delete = new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Minus.png"))
  val up = new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Up.png"))
  val down = new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Down.png"))

  def apply(name:String, icon:Icon, op:Op):SwingOpAction = new SwingOpAction(name, icon, op)

  def apply(op:Op):SwingOpAction = {
    op match {
      case o:ListAddOp[_] => SwingOp("", add, op)
      case o:ListMultiAddOp[_] => SwingOp("", add, op)
      case o:ListDeleteOp[_] => SwingOp("", delete, op)
      case o:ListMultiDeleteOp[_] => SwingOp("", delete, op)
      case o:ListMoveOp[_] => {
        if (o.up) {
          SwingOp("", up, op)
        } else {
          SwingOp("", down, op)
        }
      }
      case o:ListMultiMoveOp[_] => {
        if (o.up) {
          SwingOp("", up, op)
        } else {
          SwingOp("", down, op)
        }
      }
      //FIXME use implicits
      case _ => throw new IllegalArgumentException("Unknown op")
    }
  }

}

object SwingButton {
  def apply(name:String, icon:Icon, op:Op):EPButton = {
    ListStyleButton(SwingOp(name, icon, op))
  }
  def apply(op:Op):EPButton = {
    ListStyleButton(SwingOp(op))
  }
  def apply(op:SwingOpAction):EPButton = {
    ListStyleButton(op)
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

object SwingButtonBar {
  def apply() = new SwingButtonBarBuilder(List[JComponent]())
}

class SwingButtonBarBuilder(val components:List[JComponent]) {
  def add(c:JComponent) = new SwingButtonBarBuilder(components ::: List(c))
  def add(op:Op):SwingButtonBarBuilder = add(SwingButton(op))

  def buildWithComponent(c:JComponent) = {
    val padding = SwingButton.buttonPadding
    padding.setBorder(new EmptyBorder(2, 5, 2, 5))
    padding.setLayout(new BorderLayout)
    padding.add(c)
    c.setOpaque(false)
    build(padding)
  }

  def build(padding:JComponent = SwingButton.buttonPadding) = {
    val buttonPanel = new JPanel()
    buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS))
    components.foreach(c => buttonPanel.add(c))

    val bottom = new JPanel(new BorderLayout())
    bottom.add(buttonPanel, BorderLayout.WEST)
    bottom.add(padding, BorderLayout.CENTER)

    bottom
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
      g.setColor(ListStylePainter.pressedColor)
      g.fillRect(0, 0, w, h)
    }
  }
}




