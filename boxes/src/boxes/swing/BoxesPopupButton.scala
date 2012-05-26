package boxes.swing

import javax.swing.event.{PopupMenuEvent, PopupMenuListener}
import java.awt.event.{ActionEvent, ActionListener}
import com.explodingpixels.swingx.EPToggleButton
import javax.swing.border.{EmptyBorder}
import java.awt.{Graphics, Graphics2D, BorderLayout, Component}
import javax.swing.{Icon, JComponent, SwingUtilities, JPopupMenu, JDialog, JPanel}
import boxes.{Val, RefGeneral, View}
import java.lang.ref.PhantomReference
import boxes.swing.icons.IconFactory
import java.awt.event.{WindowFocusListener, WindowEvent, WindowListener, WindowStateListener, ComponentListener, ComponentEvent}
import java.awt.Point

private class BoxesPopupButtonHandler(popupComponent:Component, focusComponent:Option[Component], invoker:Component) extends WindowListener with ComponentListener {

  private val xOffset = 0

  var popupOption = None: Option[JDialog]

  override def windowOpened(e: WindowEvent) {}
  override def windowClosing(e: WindowEvent) {}
  override def windowClosed(e: WindowEvent) {}
  override def windowIconified(e: WindowEvent) {}
  override def windowDeiconified(e: WindowEvent) {}
  override def windowActivated(e: WindowEvent) {}
  override def windowDeactivated(e: WindowEvent) {
    //Hide when user deactivates dialog (e.g. alt-tab, clicking outside dialog, etc.)
    popupOption.foreach{popup => {
        popup.setVisible(false)
      }
    }
  }

  override def componentResized(e: ComponentEvent) {}
  override def componentMoved(e: ComponentEvent) {}
  override def componentShown(e: ComponentEvent) {
    SwingUtilities.invokeLater(new Runnable() {
      override def run() {
        //Start with correct focus component selected
        focusComponent.foreach(_.requestFocus())
      }
    })
  }
  
  override def componentHidden(e: ComponentEvent) {
    invoker match {
      case button:ToolbarPopupButton => {
        button.setSelected(false)
        //TODO make this simpler
        //This is a little hacky... if the dialog loses focus from a click, and that click
        //is on the button, we will hide, and then set the button not selected, then the button
        //can get the click and reselect itself, showing the dialog again. Note that this happens
        //unpredictably. So to avoid it, we disable the button while we hide the dialog, and then reenable
        //with invoke later, which will only happen after the click has definitely left the event queue.
        if (button.isEnabled()) {
          button.setEnabled(false)
          SwingUtilities.invokeLater(new Runnable() {
            override def run() {
              button.setEnabled(true)
            }
          })
        }        
      }
    }
    
    //We are now finished with this dialog - don't listen to it,
    //remove our component, and dispose of it so it can be GCed, and
    //clear our Option to avoid retaining it
    popupOption.foreach{popup => {
      popup.removeWindowListener(BoxesPopupButtonHandler.this)
      popup.removeComponentListener(BoxesPopupButtonHandler.this)
      popup.removeAll();
      popup.dispose();
    }}
    popupOption = None

  }

  def show() = {
    
    //Use a fresh dialog each time. We can't reuse a dialog sensibly, since it will never be GCed,
    //and will hold on to other stuff.
    val popup = new JDialog()
    popupOption = Some(popup)
    popup.setUndecorated(true)
    popup.getContentPane().add(popupComponent)
    popup.pack()

    popup.addWindowListener(this)
    popup.addComponentListener(this)

    val ph = popupComponent.getPreferredSize.height + 4

    //Find position relative to invoker - if we would appear (partially) off screen top, display below
    //instead of above
    var y = - ph + 1;
    var top = false;
    if (invoker.getLocationOnScreen.getY + y < 0) {
      y = invoker.getHeight + 4;
      top = true;
    }

//    popup.show(invoker, xOffset, y);
    //popup.setLocationRelativeTo(invoker)
    popup.setLocation(new Point(invoker.getLocationOnScreen().x + xOffset, invoker.getLocationOnScreen().y + y))
    popup.setVisible(true)

    top
  }
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
    val newNPadded = if (newN == "") "" else newN + " "
    if (newNPadded != component.getText) {
      component.setText(newNPadded)
    }
    val iconOrNull = newIcon.getOrElse(null)
    if (iconOrNull != component.getIcon) {
      component.setIcon(iconOrNull)
    }
  }

  private val handler = new BoxesPopupButtonHandler(popupContents, None, component)

  component.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      if (component.isSelected) {
        val top = handler.show()
      }
    }
  })

}

object ToolbarPopupButton {
  val popupIndicator = IconFactory.image("PopupIndicator")
  val popupBorderCutout = IconFactory.image("PopupBorderCutout")
}

class ToolbarPopupButton(val sv:SwingView) extends EPToggleButton{
  {
    setBorder(new EmptyBorder(4,2,3,2))
    setContentAreaFilled(false)
    setBackgroundPainter(new BarStyleToggleButtonPainter())
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


