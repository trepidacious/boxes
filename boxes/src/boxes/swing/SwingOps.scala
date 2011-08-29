package boxes.swing

import java.awt.event.ActionEvent
import boxes.{SwingView, View, Op}
import boxes.list.{ListMultiDeleteOp, ListDeleteOp, ListMultiAddOp, ListAddOp, ListMoveOp, ListMultiMoveOp}
import javax.swing._
import border.EmptyBorder
import com.explodingpixels.swingx.{EPToggleButton, EPPanel, EPButton}
import java.awt.{BorderLayout, Component}

//TODO should make an ExtendedOp that has a name:Ref[String] and icon:Ref[Icon] (not sure about
//icon, maybe make return an image?, and an Action that is a view of these.
class SwingOpAction(name:String, icon:Option[Icon], op:Op) extends AbstractAction(name, icon.getOrElse(null)) {
  def actionPerformed(e:ActionEvent) {
    op()
  }
  val view = View {
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

  val add = Some(new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Plus.png")))
  val delete = Some(new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Minus.png")))
  val up = Some(new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Up.png")))
  val down = Some(new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/Down.png")))

  def apply(name:String = "", icon:Option[Icon] = None, op:Op):SwingOpAction = new SwingOpAction(name, icon, op)

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

object SwingBarButton {
  def apply(name:String, icon:Option[Icon] = None, op:Op):EPButton = {
    createButton(SwingOp(name, icon, op))
  }
  def apply(op:Op):EPButton = {
    createButton(SwingOp(op))
  }
  def apply(op:SwingOpAction):EPButton = {
    createButton(op)
  }

  def createButton(a:Action) = {
    val button = new EPButton(a)
    button.setBorder(new EmptyBorder(4,2,3,2))
    button.setContentAreaFilled(false)
    button.setBackgroundPainter(new BarStyleButtonPainter())
    button
  }

  def buttonPadding() = {
    val panel = new EPPanel()
    panel.setBackgroundPainter(BarStylePainter[Component](false, false))
    panel
  }
}

object SwingButton {
  def apply(name:String, icon:Option[Icon] = None, op:Op):EPButton = new SwingButton(SwingOp(name, icon, op))
  def apply(op:Op):EPButton = new SwingButton(SwingOp(op))
  def apply(op:SwingOpAction):EPButton = new SwingButton(op)
}

class SwingBarToggleButton extends EPToggleButton{
  {
    setBorder(new EmptyBorder(4,2,3,2))
    setContentAreaFilled(false)
    setBackgroundPainter(new BarStyleToggleButtonPainter())
  }
}

class SwingToggleButton extends EPToggleButton{
  {
    setBorder(new EmptyBorder(7,12,6,12))
    setContentAreaFilled(false)
    setBackgroundPainter(new ButtonPainter())
  }
}

class SwingButton(a:Action) extends EPButton(a) {
  {
    setBorder(new EmptyBorder(7,12,6,12))
    setContentAreaFilled(false)
    setBackgroundPainter(new ButtonPainter())
  }
}

object SwingButtonBar {
  def apply() = new SwingButtonBarBuilder(List[JComponent]())
}

class SwingButtonBarBuilder(val components:List[JComponent]) {
  def add(c:JComponent) = new SwingButtonBarBuilder(components ::: List(c))
  def add(op:Op):SwingButtonBarBuilder = add(SwingBarButton(op))
  def add(v:SwingView) = new SwingButtonBarBuilder(components ::: List(v.component))

  def buildWithListStyleComponent(c:JComponent) = {
    val padding = SwingBarButton.buttonPadding
    padding.setBorder(new EmptyBorder(2, 5, 2, 5))
    padding.setLayout(new BorderLayout)
    padding.add(c)
    c.setOpaque(false)
    build(padding)
  }

  def build(padding:JComponent = SwingBarButton.buttonPadding) = {
    val buttonPanel = new JPanel()
    buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS))
    components.foreach(c => buttonPanel.add(c))

    val bottom = new JPanel(new BorderLayout())
    bottom.add(buttonPanel, BorderLayout.WEST)
    bottom.add(padding, BorderLayout.CENTER)

    bottom
  }
}





