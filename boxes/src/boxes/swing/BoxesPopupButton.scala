package boxes.swing

import javax.swing.event.{PopupMenuEvent, PopupMenuListener}
import java.awt.event.{ActionEvent, ActionListener}
import com.explodingpixels.swingx.EPToggleButton
import javax.swing.border.{EmptyBorder}
import java.awt.{Graphics, Graphics2D, BorderLayout, Component}
import javax.swing.{Icon, JComponent, SwingUtilities, JPopupMenu}
import boxes.{SwingView, Val, RefGeneral, View}
import java.lang.ref.PhantomReference

private class BoxesPopupButtonHandler(popupComponent:Component, focusComponent:Component, invoker:Component) extends PopupMenuListener {

  private val xOffset = -16

  val popup = new JPopupMenu();
  popup.removeAll()
  popup.setLayout(new BorderLayout())
  popup.add(popupComponent, BorderLayout.CENTER)
  popup.addPopupMenuListener(this)

  popup.pack()


  def hide() {
    popup.setVisible(false)
  }

  def show() = {
    //Use size of component and add on the border ourselves - the JPopupMenu has size 0,0 until shown for first time
//    val ph = popup.getHeight
    val ph = popupComponent.getPreferredSize.height + 2

    //Find position relative to invoker - if we would appear (partially) off screen top, display below
    //instead of above
    var y = - ph + 1;
    var top = false;
    if (invoker.getLocationOnScreen.getY + y < 0) {
      y = invoker.getHeight;
      top = true;
    }

    popup.setBorder(new PopupBorder(4 - xOffset, top))
    popup.pack();

    popup.show(invoker, xOffset, y);

    //Start with correct component focused
    SwingUtilities.invokeLater(new Runnable() {
      override def run() {
        if (focusComponent != null) {
          focusComponent.requestFocus()
        }
      }
    })

    top
  }

  override def popupMenuWillBecomeVisible(e:PopupMenuEvent) {
    //Note - height still not known here, so no chance to reposition
  }

  override def popupMenuWillBecomeInvisible(e:PopupMenuEvent) {
    invoker match {
      case button:ToolbarPopupButton => {
        button.setSelected(false)
        button.indicator(0)
      }
    }
  }

  override def popupMenuCanceled(e:PopupMenuEvent) {}
}

object BoxesPopupView {
  def apply(n:RefGeneral[String,_] = Val(""), icon:RefGeneral[Option[Icon], _] = Val(None), popupContents:JComponent) = {
    new BoxesPopupView(n, icon, popupContents)
  }
}

class BoxesPopupView(n:RefGeneral[String,_] = Val(""), icon:RefGeneral[Option[Icon], _] = Val(None), popupContents:JComponent) extends SwingView {

  //Only support types that make sense, otherwise default to a plain toggle button
  val component = new ToolbarPopupButton(this)

  val view = View {
    //Store the values for later use on Swing Thread
    val newN = n()
    val newIcon = icon()
    //This will be called from Swing Thread
    replaceUpdate { display(newN, newIcon) }
  }

  //Update display if necessary
  private def display(newN:String, newIcon:Option[Icon]) {
    if (newN != component.getText) {
      component.setText(newN)
    }
    val iconOrNull = newIcon.getOrElse(null)
    if (iconOrNull != component.getIcon) {
      component.setIcon(iconOrNull)
    }
  }

  private val handler = new BoxesPopupButtonHandler(popupContents, popupContents, component)

  component.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      if (component.isSelected) {
        val top = handler.show()
        //Handle the direction of the popup indicator (to link with popup above or below)
        component.indicator(if (top) 1 else -1)
      }
    }
  })

}

object ToolbarPopupButton {
  val popupIndicator = SwingView.icon("PopupIndicator").getImage
  val popupBorderCutout = SwingView.icon("PopupBorderCutout").getImage
}

class ToolbarPopupButton(val sv:SwingView) extends EPToggleButton{
  {
    setBorder(new EmptyBorder(4,2,3,2))
    setContentAreaFilled(false)
    setBackgroundPainter(new BarStyleToggleButtonPainter())
  }

  var indicator = 0

  def indicator(i:Int) {
    indicator = i
    repaint()
  }

  override def paintComponent(g:Graphics) {
    super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]
    if (getModel.isSelected || getModel.isPressed) {
      val i = ToolbarPopupButton.popupIndicator
      if (indicator > 0) {
        g2.drawImage(i, 0, getHeight, i.getWidth(null), -i.getHeight(null), null)
      } else if (indicator < 0) {
        g2.drawImage(i, 0, 1, null)
      }
    }
  }
}


class PopupButton(val sv:SwingView) extends SwingToggleButton

class PopupBorder(val xOffset:Int, val topGap:Boolean) extends EmptyBorder(1,1,1,1) {

  override def paintBorder(c: Component, g: Graphics, x: Int, y: Int, width: Int, height: Int): Unit = {
    val oldColor = g.getColor
    g.setColor(SwingView.dividingColor)
    g.drawRect(x, y, width-1, height-1)
    g.setColor(oldColor)
    if (topGap) {
      g.drawImage(ToolbarPopupButton.popupBorderCutout, x + xOffset, 0, null)
    } else {
      g.drawImage(ToolbarPopupButton.popupBorderCutout, x + xOffset, y + height - 1, null)
    }
  }

  override def isBorderOpaque = true
}


